library(lubridate)
library(dplyr)
library(tools)
library(readr)

processpH <- function(file,
                      ctd_data = NULL,       # Dataframe with CTD salinity (Required for SAMI reprocessing)
                      reprocess = FALSE,     # TRUE = Apply Salinity Correction (SAMI) or Use Internal (SeaFET)
                      keep_original = FALSE, # NEW: If TRUE, keeps 'PH_ORIGINAL' and 'SAL_CTD' columns
                      sal_constant = 35,     # The fixed salinity used by SAMI originally (usually 35)
                      tz.in = "UTC",
                      julianday = FALSE,
                      write_to_csv = TRUE,
                      average = FALSE,
                      pH_min = 7) { 
  
  # --- 1. SETUP & DETECTION ---
  if (!file.exists(file)) stop(paste("File not found:", file))
  
  cat(paste0("Reading pH file: ", basename(file), "\n"))
  
  # Read header to detect instrument type
  header_lines <- readLines(file, n = 20)
  is_sami <- any(grepl("pHConstSal|SAMI", header_lines, ignore.case = TRUE))
  processed <- NULL
  
  # ============================================================================
  # OPTION A: SAMI INSTRUMENT PROCESSING
  # ============================================================================
  if (is_sami) {
    cat("    -> Detected Instrument: SAMI\n")
    
    # Read SAMI Data
    raw_data <- read.table(file, 
                           header = TRUE, 
                           sep = "\t", 
                           stringsAsFactors = FALSE,
                           check.names = FALSE, 
                           fill = TRUE, 
                           quote = "", 
                           na.strings = c("NA", "NaN", "-nan(ind).0000")) 
    
    # A1. Parse Time
    if ("DateStr" %in% names(raw_data) && "TimeStr" %in% names(raw_data)) {
      dt_str <- paste(raw_data$DateStr, raw_data$TimeStr)
      UTCDateTime <- ymd_hms(dt_str, tz = "UTC", quiet = TRUE)
    } else {
      UTCDateTime <- tryCatch({
        ymd_hms(raw_data[[1]], tz = "UTC", quiet = TRUE)
      }, error = function(e) NULL)
    }
    
    if (is.null(UTCDateTime) || all(is.na(UTCDateTime))) {
      stop("Could not parse SAMI timestamps. Check column names.")
    }
    
    # A2. Parse pH and Temp
    pH_col <- grep("pHConstSal|pH", names(raw_data), value = TRUE)[1]
    temp_col <- grep("Temperature", names(raw_data), value = TRUE)[1]
    
    # Initial Load
    processed <- data.frame(UTCDATETIME = UTCDateTime,
                            PH = as.numeric(raw_data[[pH_col]]),
                            TEMP_DEGC = as.numeric(raw_data[[temp_col]]))
    
    # Placeholder for original if needed
    processed$PH_ORIGINAL <- processed$PH
    
    # A3. REPROCESSING LOGIC (Salinity Correction)
    if (reprocess) {
      if (is.null(ctd_data)) {
        warning("   ! Reprocess requested (SAMI) but 'ctd_data' is missing. Skipping correction.")
      } else {
        cat("    -> Reprocessing SAMI pH using CTD Salinity...\n")
        
        names(ctd_data) <- toupper(names(ctd_data))
        
        if (all(c("UTCDATETIME", "SALN_PSU") %in% names(ctd_data))) {
          
          # Clean and Interpolate CTD Data
          ctd_clean <- ctd_data %>% 
            filter(!is.na(UTCDATETIME) & !is.na(SALN_PSU)) %>%
            arrange(UTCDATETIME)
          
          if (nrow(ctd_clean) > 1) {
            # Create Salinity Interpolation Function
            sal_fun <- approxfun(ctd_clean$UTCDATETIME, ctd_clean$SALN_PSU, rule = 2)
            
            # Map CTD Salinity to SAMI Time
            processed$SAL_CTD <- sal_fun(processed$UTCDATETIME)
            
            # --- PHYSICS CALCULATION ---
            get_pKa <- function(T_K, S) {
              return(1245.69 / T_K + 3.8275 + 0.00211 * (35 - S))
            }
            
            T_K <- processed$TEMP_DEGC + 273.15
            pKa_old <- get_pKa(T_K, sal_constant)
            pKa_new <- get_pKa(T_K, processed$SAL_CTD)
            
            # Apply Delta
            processed$PH <- processed$PH + (pKa_new - pKa_old)
            
            cat("    -> SAMI Salinity Correction applied successfully.\n")
            
          } else {
            warning("   ! Not enough valid CTD data points to interpolate.")
          }
        } else {
          warning("   ! CTD data missing 'UTCDATETIME' or 'SALN_PSU'.")
        }
      }
    }
    
  } else {
    # ==========================================================================
    # OPTION B: SEAFET INSTRUMENT PROCESSING
    # ==========================================================================
    cat("    -> Detected Instrument: SeaFET\n")
    
    header_idx <- grep("^SAT", header_lines)[1]
    if (is.na(header_idx)) header_idx <- 1
    
    file_content <- readLines(file)
    data_lines <- file_content[header_idx:length(file_content)]
    
    data <- read.table(text = data_lines, 
                       sep = ",", 
                       stringsAsFactors = FALSE, 
                       fill = TRUE, 
                       header = FALSE)
    
    UTCDateTime <- NULL
    pH_final <- NULL
    pH_other <- NULL
    temp_raw <- NULL
    
    if (julianday) {
      year_str <- substr(data$V2, 1, 4)
      doy_str  <- substr(data$V2, 5, 7)
      origin_dates <- as.Date(paste0(year_str, "-01-01"))
      date_val <- origin_dates + (as.numeric(doy_str) - 1)
      seconds_offset <- as.numeric(data$V3) * 3600 
      
      full_datetime <- as.POSIXct(paste(date_val), tz = tz.in) + seconds_offset
      UTCDateTime <- with_tz(full_datetime, tz = "UTC")
      
      # Julian Mapping (Approx): V5=Int, V4=Ext
      if (reprocess) {
        pH_final <- as.numeric(data$V5) # Internal
        pH_other <- as.numeric(data$V4) # External
      } else {
        pH_final <- as.numeric(data$V4) # External
        pH_other <- as.numeric(data$V5) # Internal
      }
      temp_raw <- as.numeric(data$V6)
      
    } else {
      # Standard Format
      dt_parsed <- dmy_hms(data$V2, tz = tz.in, quiet = TRUE)
      if (all(is.na(dt_parsed))) dt_parsed <- ymd_hms(data$V2, tz = tz.in, quiet = TRUE)
      UTCDateTime <- with_tz(dt_parsed, tz = "UTC")
      
      if (reprocess) {
        cat("    ... Selecting Internal/Corrected pH (Col 4)\n")
        pH_final <- as.numeric(data$V4)
        pH_other <- as.numeric(data$V5)
      } else {
        cat("    ... Selecting External/Raw pH (Col 5)\n")
        pH_final <- as.numeric(data$V5)
        pH_other <- as.numeric(data$V4)
      }
      temp_raw <- as.numeric(data$V6)
    }
    
    processed <- data.frame(UTCDATETIME = UTCDateTime, 
                            PH = pH_final, 
                            TEMP_DEGC = temp_raw,
                            PH_ORIGINAL = pH_other)
  }
  
  # --- 3. CLEANUP & EXPORT ---
  
  # Remove Invalid Rows
  processed <- processed %>%
    filter(!is.na(UTCDATETIME),
           !is.na(PH),
           PH >= pH_min,
           PH <= 14) 
  
  # Cleanup: If keep_original is FALSE, drop extra columns
  if (!keep_original) {
    if ("PH_ORIGINAL" %in% names(processed)) processed$PH_ORIGINAL <- NULL
    if ("SAL_CTD" %in% names(processed)) processed$SAL_CTD <- NULL
  }
  
  # Optional Averaging
  if (average) {
    processed <- processed %>%
      mutate(TimeRound = floor_date(UTCDATETIME, "1 minute")) %>%
      group_by(TimeRound) %>%
      summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE))) %>%
      rename(UTCDATETIME = TimeRound) 
    cat("    ... Averaged to 1-minute intervals\n")
  }
  
  # Export to CSV
  if (write_to_csv) {
    name <- file_path_sans_ext(basename(file))
    out_dir <- dirname(file)
    
    suffix <- "_processed"
    
    out_path <- file.path(out_dir, paste0(name, suffix, ".csv"))
    
    export_df <- processed
    if ("UTCDATETIME" %in% names(export_df)) {
      export_df$UTCDATETIME <- format(export_df$UTCDATETIME, "%Y-%m-%d %H:%M:%S")
    }
    
    write.csv(export_df, out_path, row.names = FALSE, na = "")
    cat(paste0("    -> Saved: ", basename(out_path), "\n"))
  }
  
  return(processed)
}