library(oce)
library(lubridate)
library(tools)

processCTD_moored <- function(file,
                              tz.in = "UTC",
                              model = c("RBR", "SBE"), 
                              write.csv = TRUE,
                              SN3029 = FALSE,
                              oxygen = FALSE) {
  
  # Ensure model is selected (defaults to "RBR")
  model <- match.arg(model)
  
  # Initialize variables to NULL to prevent scoping errors
  name <- file_path_sans_ext(basename(file))
  pressure <- NULL
  depth <- NULL
  temperature <- NULL
  conductivity <- NULL
  salinity <- NULL
  sigmaT <- NULL
  date <- NULL
  
  # --- 1. PROCESS RBR (Default) ---
  if (model == "RBR") {
    rsk_data <- read.rsk(file)
    data <- as.ctd(rsk_data)
    
    pressure <- data[['pressure']]
    depth <- data[['depth']]
    temperature <- data[['temperature']]
    
    # RBR conductivity is usually mS/cm, convert to S/m if needed
    cond_raw <- data[['conductivity']]
    if (!is.null(cond_raw)) {
      conductivity <- cond_raw / 10 # Convert mS/cm to S/m
    }
    
    salinity <- data[['salinity']]
    sigmaT <- data[['density']] - 1000
    
    date <- data[['time']]
  }
  
  # --- 2. PROCESS SBE (Sea-Bird) ---
  if (model == "SBE") {
    # SBE instruments often use .cnv or .asc formats
    data <- suppressWarnings(read.ctd.sbe(file))
    
    pressure <- data[['pressure']]
    depth <- data[['depth']]
    temperature <- data[['temperature']]
    conductivity <- data[['conductivity']]
    salinity <- data[['salinity']]
    
    # SBE Time Processing Logic
    start.time <- ymd_hms(data[['startTime']])
    year_val <- year(start.time)
    
    if (!SN3029) {
      # Standard SBE logic
      if (is.null(data[['timeSCP']])) {
        time.raw <- data[['timeJV2']]
      } else {
        time.raw <- data[['timeSCP']]
      }
      
      # Julian Day calculation (days since Jan 1)
      origin <- ymd_hms(paste0(year_val, "-01-01 00:00:00"), tz = tz.in)
      date_calc <- origin + (time.raw * 86400) - 86400 
      date <- with_tz(date_calc, tz = 'UTC')
      
      sigmaT <- data[['sigmaT']] 
      
    } else {
      # Special logic for SN3029 (Time Elapsed)
      if (!is.null(data[['timeS']])) {
        time.elapsed <- data[['timeS']]
        date_calc <- start.time + seconds(time.elapsed)
      } else {
        time.elapsed <- data[['timeH']]
        date_calc <- start.time + seconds(time.elapsed * 3600)
      }
      
      date_calc <- force_tz(date_calc, tz = tz.in)
      date <- with_tz(date_calc, tz = 'UTC')
      sigmaT <- data[['density']] - 1000
    }
  }
  
  # --- 3. BUILD DATAFRAME ---
  df.raw <- data.frame(
    ShallowCTDID = name,
    UTCDATETIME = date,
    PRESSURE_DB = if(is.null(pressure)) NA else pressure,
    DEPTH_M = if(is.null(depth)) NA else depth,
    TEMP_DEGC = temperature,
    SALN_PSU = salinity,
    DENSITY_SIGMAT = sigmaT,
    COND_S_PER_M = if(is.null(conductivity)) NA else conductivity
  )
  
  # Add Oxygen if requested
  if (oxygen) {
    oxy_data <- data[['oxygen']] 
    if (!is.null(oxy_data)) {
      df.raw$OXYGEN_MGL <- oxy_data
    } else {
      cat("Warning: Oxygen requested but not found in data file.\n")
      df.raw$OXYGEN_MGL <- NA
    }
  }
  
  # Filter Bad Salinity
  df <- subset(df.raw, SALN_PSU > 20)
  
  # --- 4. EXPORT FILES ---
  if (write.csv) {
    out_dir <- dirname(file)
    
    # 1. Main Processed File
    write.csv(df,
              file.path(out_dir, paste0(name, '_processed.csv')),
              row.names = FALSE)
    
    # 2. SeaFET Format File
    seafet_time <- format(date, format = "%Y-%m-%d %H:%M:%S")
    
    df.seafet.raw <- data.frame(date_time = seafet_time,
                                temperature = temperature,
                                salinity = salinity)
    
    df.seafet <- subset(df.seafet.raw, salinity > 20)
    
    write.table(
      df.seafet,
      file.path(out_dir, paste0(name, '_SEAFET.csv')),
      row.names = FALSE,
      col.names = FALSE, 
      sep = ','
    )
    
    # Changed message to cat to avoid red text
    cat(paste0("Saved processed files for: ", name, "\n"))
  }
  
  return(df)
}