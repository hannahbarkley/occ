library(lubridate)
library(dplyr)
library(tools)

trimDS <- function(adcp = NULL,
                   ctd = NULL,
                   pH = NULL,
                   par = NULL,
                   do = NULL,
                   tcm = NULL,
                   filename,
                   output.dir,
                   start.time = NULL,
                   end.time = NULL,
                   read_from_csv = FALSE, 
                   write_to_csv = TRUE) { 
  
  # --- 1. SETUP & LOAD ---
  
  # Helper to read CSV if path provided, else return object
  load_data <- function(obj, name) {
    if (is.null(obj)) return(NULL)
    
    # Handle CSV input
    if (read_from_csv && is.character(obj)) {
      if (!file.exists(obj)) {
        warning(paste("File not found for", name, ":", obj))
        return(NULL)
      }
      message(paste("Loading", name, "from CSV..."))
      return(read.csv(obj, stringsAsFactors = FALSE))
    }
    
    # If it's already a dataframe, return it
    if (is.data.frame(obj)) return(obj)
    
    return(NULL)
  }
  
  # Load all datasets safely
  adcp  <- load_data(adcp, "ADCP")
  ctd   <- load_data(ctd, "CTD")
  pH    <- load_data(pH, "PH")
  par   <- load_data(par, "PAR")
  do    <- load_data(do, "DO")
  tcm   <- load_data(tcm, "TCM")
  
  # Helper to standardize time to UTCDATETIME
  standardize_time <- function(df, name) {
    if (is.null(df)) return(NULL)
    if (nrow(df) == 0) return(NULL)
    
    names(df) <- toupper(names(df))
    
    # 1. Robustly find the Time column
    t_col <- grep("UTCDATETIME|TIMESTAMP|DATE_TIME", names(df), ignore.case = TRUE, value = TRUE)[1]
    
    if (is.na(t_col)) {
      warning(paste(name, "- No time column found. Skipping."))
      return(NULL)
    }
    
    # 2. Rename to standard UTCDATETIME if needed
    if (t_col != "UTCDATETIME") {
      df$UTCDATETIME <- df[[t_col]]
    }
    
    # 3. Parse and Enforce UTC
    if (!inherits(df$UTCDATETIME, "POSIXct")) {
      # Try standard formats
      parsed <- as.POSIXct(df$UTCDATETIME, tz = "UTC", tryFormats = c("%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S", "%m/%d/%Y %H:%M:%S"))
      df$UTCDATETIME <- parsed
    } else {
      # Ensure TZ is UTC
      df$UTCDATETIME <- with_tz(df$UTCDATETIME, tz = "UTC")
    }
    
    # Drop rows with invalid time
    df <- df[!is.na(df$UTCDATETIME), ]
    return(df)
  }
  
  # Apply time standardization
  adcp  <- standardize_time(adcp, "ADCP")
  ctd   <- standardize_time(ctd, "CTD")
  pH    <- standardize_time(pH, "PH")
  par   <- standardize_time(par, "PAR")
  do    <- standardize_time(do, "DO")
  tcm   <- standardize_time(tcm, "TCM")
  
  # --- 2. DETERMINE TIME WINDOW ---
  
  start_win <- NULL
  end_win   <- NULL
  
  # Priority 1: User provided manual start/end times
  if (!is.null(start.time) && !is.null(end.time)) {
    message("Using manually provided time window.")
    start_win <- as.POSIXct(start.time, tz = "UTC")
    end_win   <- as.POSIXct(end.time, tz = "UTC")
    
  } else if (!is.null(ctd) && nrow(ctd) > 0) {
    # Priority 2: Auto-detect from CTD Pressure
    pres_col <- grep("PRESSURE|DEPTH", names(ctd), ignore.case = TRUE, value = TRUE)[1]
    
    if (!is.na(pres_col)) {
      message(paste("Auto-detecting time window using column:", pres_col))
      
      p_vals <- ctd[[pres_col]]
      valid_pres <- !is.na(p_vals)
      
      if (sum(valid_pres) > 10) { 
        med_pres <- median(p_vals, na.rm = TRUE)
        
        # Define "In Water" as Median +/- 2 db 
        in_range <- which(p_vals >= (med_pres - 2) & p_vals <= (med_pres + 2))
        
        if (length(in_range) > 0) {
          start_win <- ctd$UTCDATETIME[min(in_range)]
          end_win   <- ctd$UTCDATETIME[max(in_range)]
        }
      }
    }
  } 
  
  # Priority 3: Fallback Logic
  if (is.null(start_win)) {
    available_data <- list(adcp=adcp, ctd=ctd, pH=pH, par=par, do=do, tcm=tcm)
    available_data <- available_data[!sapply(available_data, is.null)]
    
    if (length(available_data) > 0) {
      fallback_name <- names(available_data)[1]
      message(paste("No CTD Pressure found. Setting time window from:", fallback_name))
      
      df_fallback <- available_data[[1]]
      start_win <- min(df_fallback$UTCDATETIME, na.rm = TRUE)
      end_win   <- max(df_fallback$UTCDATETIME, na.rm = TRUE)
    } else {
      warning("No valid data loaded. Cannot determine time window.")
      return(NULL) 
    }
  }
  
  message(paste("Trim Window:", start_win, "to", end_win))
  
  # --- 3. TRIM DATASETS ---
  
  trim_data <- function(df, start, end) {
    if (is.null(df)) return(NULL)
    df %>% filter(UTCDATETIME >= start & UTCDATETIME <= end)
  }
  
  adcp.trim   <- trim_data(adcp, start_win, end_win)
  ctd.trim    <- trim_data(ctd, start_win, end_win)
  pH.trim     <- trim_data(pH, start_win, end_win)
  par.trim    <- trim_data(par, start_win, end_win)
  do.trim     <- trim_data(do, start_win, end_win)
  tcm.trim    <- trim_data(tcm, start_win, end_win)
  
  # --- 4. EXPORT CSV ---
  
  if (write_to_csv) {
    if (!dir.exists(output.dir)) {
      dir.create(output.dir, recursive = TRUE)
    }
    
    save_csv <- function(df, suffix) {
      if (!is.null(df) && nrow(df) > 0) {
        
        if ("UTCDATETIME" %in% names(df)) {
          df$UTCDATETIME <- format(df$UTCDATETIME, "%Y-%m-%d %H:%M:%S")
        }
        # ------------------------------------
        
        # Clean filename to ensure no paths are duplicated
        base_fn <- basename(filename)
        out_path <- file.path(output.dir, paste0(base_fn, suffix))
        
        write.csv(df, out_path, row.names = FALSE, na = "")
      }
    }
    
    save_csv(adcp.trim, '_ADCP.csv')
    save_csv(ctd.trim, '_CTD.csv')
    save_csv(pH.trim, '_pH.csv')
    save_csv(par.trim, '_PAR.csv')
    save_csv(do.trim, '_DO.csv')
    save_csv(tcm.trim, '_TCM.csv')
    
    message(paste("Trimmed files saved to:", output.dir))
  }
  
  # --- 5. RETURN LIST ---
  return(list(
    adcp   = adcp.trim,
    ctd    = ctd.trim,
    pH     = pH.trim,
    par    = par.trim,
    do     = do.trim,
    tcm    = tcm.trim
  ))
}