library(oce)
library(lubridate)
library(tools)
library(data.table)

processDO <- function(file, 
                      ctd, 
                      read.csv = FALSE, 
                      write.csv = TRUE) {
  
  # --- 1. Read DO Data ---
  # Swapped message for cat to avoid red text
  cat(paste0("Reading DO file: ", basename(file), "\n"))
  do.file <- read.oce(file)
  
  # Get Time (Checks 'time' first, then 'tstamp' for RSK files)
  raw_time <- if (!is.null(do.file[['time']])) do.file[['time']] else do.file[['tstamp']]
  
  # Get Saturation (Finds first column with "saturation" in name)
  sat_col <- grep("saturation", names(do.file@data), ignore.case = TRUE, value = TRUE)[1]
  
  if (is.na(sat_col)) stop("Could not find a saturation column in DO file.")
  
  # Create DO DataFrame with UTCDATETIME (all caps)
  do_df <- data.frame(UTCDATETIME = raw_time, 
                      OXYGEN_PERCENTSAT = do.file[[sat_col]])
  
  # Ensure UTC
  do_df$UTCDATETIME <- with_tz(do_df$UTCDATETIME, tz = "UTC")
  
  # --- 2. Prepare CTD Data ---
  if (read.csv) {
    ctd_data <- read.csv(ctd)
  } else {
    ctd_data <- ctd 
  }
  
  # Identify the time column in CTD data (case-insensitive grep)
  time_col_idx <- grep("time", names(ctd_data), ignore.case = TRUE)[1]
  
  # Rename CTD time column to UTCDATETIME
  names(ctd_data)[time_col_idx] <- "UTCDATETIME"
  
  # Rename other key columns to ALL CAPS for consistency
  t_idx <- grep("temp", names(ctd_data), ignore.case = TRUE)[1]
  s_idx <- grep("sal", names(ctd_data), ignore.case = TRUE)[1]
  
  if(!is.na(t_idx)) names(ctd_data)[t_idx] <- "TEMP_DEGC"
  if(!is.na(s_idx)) names(ctd_data)[s_idx] <- "SALN_PSU"
  
  # Ensure POSIXct format
  if (!inherits(ctd_data$UTCDATETIME, "POSIXct")) {
    ctd_data$UTCDATETIME <- parse_date_time(ctd_data$UTCDATETIME, 
                                            orders = c("ymd HMS", "mdy HMS", "dmy HMS", "ymd"),
                                            tz = "UTC")
  }
  
  # --- 3. Merge Data (Rolling Join) ---
  setDT(do_df)
  setDT(ctd_data)
  
  setkey(do_df, UTCDATETIME)
  setkey(ctd_data, UTCDATETIME)
  
  # Join nearest CTD point to DO point
  combined <- ctd_data[do_df, roll = "nearest"]
  
  # --- 4. Calculate Dissolved Oxygen (mg/L) ---
  # Garcia and Gordon (1992) - Weiss formulation coefficients
  
  A1 <- -173.4292
  A2 <-  249.6339
  A3 <-  143.3483
  A4 <-  -21.8492
  B1 <- -0.033096
  B2 <-  0.014259
  B3 <- -0.001700
  
  # Absolute Temp (Kelvin)
  Temp_DegK <- combined$TEMP_DEGC + 273.15
  
  # Calculate Solubility (ml/L) using Weiss formula
  T100 <- Temp_DegK / 100
  
  exponent <- A1 + 
    (A2 * 100 / Temp_DegK) + 
    (A3 * log(T100)) + 
    (A4 * T100) + 
    combined$SALN_PSU * (B1 + (B2 * T100) + (B3 * (T100^2)))
  
  O2_Solubility_mlL <- exp(exponent)
  
  # Convert Saturation % to Concentration (mg/L)
  # Formula: (Sat% / 100) * Solubility * 1.42903
  combined$OXYGEN_MGL <- (combined$OXYGEN_PERCENTSAT / 100) * O2_Solubility_mlL * 1.42903
  
  # --- 5. Export ---
  
  # Define columns to keep
  keep_cols <- c("UTCDATETIME", "OXYGEN_PERCENTSAT", "OXYGEN_MGL", "TEMP_DEGC", "SALN_PSU")
  
  # Subset and convert back to data.frame
  final_df <- as.data.frame(combined[, ..keep_cols])
  
  if (write.csv) {
    name <- file_path_sans_ext(basename(file))
    out_path <- file.path(dirname(file), paste0(name, "_processed.csv"))
    write.csv(final_df, out_path, row.names = FALSE, na = "")
    # Swapped message for cat to avoid red text
    cat(paste0("Saved: ", out_path, "\n"))
  }
  
  return(final_df)
}