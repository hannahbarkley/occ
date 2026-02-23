library(lubridate)
library(tools)
library(dplyr)

processTCM <- function(file, write.csv = TRUE) {
  
  # --- 1. Read Data ---
  cat(paste0("Reading TCM file: ", basename(file), "\n"))
  
  if (!file.exists(file)) stop("File not found: ", file)
  
  # Read CSV
  data <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
  
  # --- 2. Parse Time ---
  # Check standard Lowell TCM time column names
  if ("ISO.8601.Time" %in% names(data)) {
    # Standard format: "2023-01-01T12:00:00"
    data$UTCDateTime <- ymd_hms(data$ISO.8601.Time, tz = "UTC", quiet = TRUE)
  } else if ("Time" %in% names(data)) {
    # Fallback for some exports
    data$UTCDateTime <- ymd_hms(data$Time, tz = "UTC", quiet = TRUE)
  } else {
    stop("Could not find a valid Time column (expected 'ISO.8601.Time').")
  }
  
  # --- 3. Extract & Convert Variables ---
  
  # Helper to safely extract and convert units (cm/s to m/s)
  get_velocity <- function(col_name) {
    if (col_name %in% names(data)) {
      return(data[[col_name]] / 100) 
    } else {
      return(rep(NA, nrow(data)))
    }
  }
  
  # Create new clean dataframe
  processed <- data.frame(
    UTCDATETIME   = data$UTCDateTime,
    SPEED_MS      = get_velocity("Speed..cm.s."),
    DIRECTION_DEG = if("Heading..degrees." %in% names(data)) data$Heading..degrees. else NA,
    VELOCITY_N_MS = get_velocity("Velocity.N..cm.s."),
    VELOCITY_E_MS = get_velocity("Velocity.E..cm.s.")
  )
  
  # Remove rows with invalid time
  processed <- processed[!is.na(processed$UTCDATETIME), ]
  
  # --- 4. Export ---
  if (write.csv) {
    out_path <- file.path(dirname(file), 
                          paste0(file_path_sans_ext(basename(file)), '_processed.csv'))
    
    # Format time for CSV output
    export_df <- processed
    export_df$UTCDATETIME <- format(export_df$UTCDATETIME, "%Y-%m-%d %H:%M:%S")
    
    write.csv(export_df, out_path, row.names = FALSE, na = "")
    cat(paste0("Saved processed TCM file: ", out_path, "\n"))
  }
  
  cat("Tilt meter data processed successfully.\n")
  return(processed)
}