library(oce)
library(tidyverse)
library(tools)

# --- HELPER FUNCTION: Radians to Degrees ---
rad2deg <- function(rad) {
  return(rad * 180 / pi)
}

processADCP <- function(file,
                        write.csv = TRUE,
                        tz.in = "UTC",
                        format = c("long", "wide"),
                        na.rm = TRUE) {
  
  # --- 1. Read Data ---
  # Replaced message() with cat() to avoid red text in consoles
  cat(paste0("Reading: ", basename(file), "\n"))
  
  data.all <- read.aquadoppProfiler(file, tz = tz.in, despike = TRUE)
  
  # --- 2. Smart Trimming ---
  valid_indices <- which(
    data.all[['heading']] <= median(data.all[['heading']], na.rm = TRUE) + 5 &
      data.all[['heading']] >= median(data.all[['heading']], na.rm = TRUE) - 5 &
      data.all[['pressure']] <= median(data.all[['pressure']], na.rm = TRUE) + 2 &
      data.all[['pressure']] >= median(data.all[['pressure']], na.rm = TRUE) - 2
  )
  
  if (length(valid_indices) == 0) stop("No valid data found based on trimming criteria.")
  
  first <- min(valid_indices)
  last  <- max(valid_indices)
  
  data.xyz <- oce::subset(data.all, first <= seq_along(data.all[['time']]) & seq_along(data.all[['time']]) <= last)
  
  # --- 3. Coordinate Conversion ---
  if (data.xyz[["oceCoordinate"]] == "enu") {
    data.enu <- data.xyz
  } else {
    data.enu <- xyzToEnuAdp(data.xyz, declination = 0) 
  }
  
  # --- 4. Extract Variables ---
  UTCDateTime <- data.enu[['time']]
  Distance    <- data.enu[['distance']] 
  Pressure    <- data.enu[['pressure']]
  
  v_east  <- data.enu[["v"]][, , 1]
  v_north <- data.enu[["v"]][, , 2]
  v_up    <- data.enu[["v"]][, , 3]
  
  speed_mat <- sqrt(v_east^2 + v_north^2 + v_up^2)
  
  direction_mat <- rad2deg(atan2(v_north, v_east))
  direction_mat <- ifelse(direction_mat > 0, direction_mat, direction_mat + 360)
  
  # --- 5. Surface Masking (Vectorized) ---
  if (na.rm) {
    dist_mat <- matrix(rep(Distance, nrow(speed_mat)), 
                       nrow = nrow(speed_mat), 
                       byrow = TRUE)
    
    mask <- dist_mat > Pressure 
    
    speed_mat[mask] <- NA
    direction_mat[mask] <- NA
  }
  
  # --- 6. Formatting DataFrames ---
  col_names <- as.character(round(Distance, 2))
  
  speed_df <- as.data.frame(speed_mat)
  colnames(speed_df) <- col_names
  speed_df <- cbind(UTCDateTime, speed_df)
  
  direction_df <- as.data.frame(direction_mat)
  colnames(direction_df) <- col_names
  direction_df <- cbind(UTCDateTime, direction_df)
  
  pressure_df <- data.frame(UTCDATETIME = UTCDateTime, 
                            PRESSURE_DB = Pressure)
  
  # --- 7. Remove Empty Columns ---
  valid_cols <- sapply(speed_df[, -1], function(x) !all(is.na(x)))
  
  speed_trim <- speed_df[, c(TRUE, valid_cols)]
  direction_trim <- direction_df[, c(TRUE, valid_cols)]
  
  name <- file_path_sans_ext(basename(file))
  out_dir <- dirname(file)
  
  # --- 8. Export Logic ---
  long_speed <- speed_trim %>%
    pivot_longer(cols = -UTCDateTime, names_to = "HEIGHTBIN_M", values_to = "SPEED_MS")
  
  long_dir <- direction_trim %>%
    pivot_longer(cols = -UTCDateTime, names_to = "HEIGHTBIN_M", values_to = "DIRECTION_DEG")
  
  adcp_long <- left_join(long_speed, long_dir, by = c("UTCDateTime", "HEIGHTBIN_M")) %>%
    rename(UTCDATETIME = UTCDateTime) %>%
    left_join(pressure_df, by = "UTCDATETIME") %>%
    arrange(UTCDATETIME, as.numeric(HEIGHTBIN_M))
  
  # --- 9. Writing Files ---
  if (write.csv) {
    cat("Writing CSV files...\n")
    
    if ("wide" %in% format) {
      write.csv(speed_trim, file.path(out_dir, paste0(name, "_speed.csv")), row.names = FALSE, na = "")
      write.csv(direction_trim, file.path(out_dir, paste0(name, "_direction.csv")), row.names = FALSE, na = "")
      write.csv(pressure_df, file.path(out_dir, paste0(name, "_pressure.csv")), row.names = FALSE, na = "")
    }
    
    if ("long" %in% format) {
      write.csv(adcp_long, file.path(out_dir, paste0(name, "_processed.csv")), row.names = FALSE, na = "")
    }
  }
  
  # --- 10. Return ---
  cat("ADCP data processed successfully.\n")
  
  if (length(format) == 1 && format == "wide") {
    return(list(speed = speed_trim, direction = direction_trim, pressure = pressure_df))
  } else {
    return(adcp_long)
  }
}