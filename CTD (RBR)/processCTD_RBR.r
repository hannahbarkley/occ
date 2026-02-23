library(lubridate)
library(oce)
library(stringr)
library(tools)
library(DBI)
library(RSQLite)
library(data.table) # Added for high-speed I/O

# --- HELPER FUNCTION: Isolate Downcast in Shallow Water ---
ctdRemoveInversions <- function(x) {
  p <- x[['pressure']]
  if (length(p) == 0) return(x)
  
  idx_max <- which.max(p)
  x_down <- oce::subset(x, seq_along(p) <= idx_max)
  
  return(x_down)
}

processCTD_RBR <- function(rsk.file,
                           CRUISEID,
                           tzone,
                           use_twist_log = TRUE,
                           cutoff = 0.5,
                           minLength = 10,
                           minHeight = 0.5,
                           start.time = NULL,
                           end.time = NULL,
                           cast.num = NULL,
                           method = "downcast",
                           plot_profiles = FALSE,
                           output_dir = NULL) {
  
  # --- HELPER FUNCTION: Get Times for Start Twists (24) -> End Twists (25)  ---
  get_twist_segments_sql <- function(rsk_path) {
    if (!file.exists(rsk_path)) return(NULL)
    
    con <- tryCatch({ dbConnect(RSQLite::SQLite(), rsk_path) }, error = function(e) NULL)
    if (is.null(con)) return(NULL)
    on.exit(dbDisconnect(con))
    
    if (!dbExistsTable(con, "events")) return(NULL)
    
    # Fetch events & data range
    query <- "SELECT tstamp, type FROM events WHERE type IN (24, 25) ORDER BY tstamp"
    events <- dbGetQuery(con, query)
    data_range <- dbGetQuery(con, "SELECT MIN(tstamp) as tmin, MAX(tstamp) as tmax FROM data")
    
    if (is.na(data_range$tmin)) return(NULL)
    
    file_start_time <- as.POSIXct(data_range$tmin / 1000, origin="1970-01-01", tz="UTC")
    file_end_time   <- as.POSIXct(data_range$tmax / 1000, origin="1970-01-01", tz="UTC")
    
    # Virtual Start Logic
    if (nrow(events) > 0) {
      events$time <- as.POSIXct(events$tstamp / 1000, origin="1970-01-01", tz="UTC")
      if (events$type[1] == 25) { 
        if (file_start_time < events$time[1]) {
          events <- rbind(data.frame(tstamp=data_range$tmin, type=24, time=file_start_time), events)
        }
      }
    } else {
      return(list(list(start = file_start_time, end = file_end_time)))
    }
    
    # Pairing Logic
    segs <- list()
    current_start <- NULL
    for (i in 1:nrow(events)) {
      ev_type <- events$type[i]
      ev_time <- events$time[i]
      
      if (ev_type == 24) {
        current_start <- ev_time 
      } else if (ev_type == 25) {
        if (!is.null(current_start)) {
          if (ev_time > current_start) {
            segs[[length(segs) + 1]] <- list(start = current_start, end = ev_time)
          }
          current_start <- NULL
        }
      }
    }
    
    if (!is.null(current_start)) {
      if (file_end_time > current_start) {
        segs[[length(segs) + 1]] <- list(start = current_start, end = file_end_time)
      }
    }
    return(segs)
  }
  
  # --- 1. SETUP ---
  message(paste("Reading:", basename(rsk.file)))
  if (file.size(rsk.file) == 0) return(NULL)
  
  ctd_raw <- tryCatch({ oce::read.rsk(rsk.file) }, error = function(e) NULL)
  if (is.null(ctd_raw)) { warning("Failed to read file."); return(NULL) }
  
  ctd <- as.ctd(ctd_raw)
  ctd <- oce::subset(ctd, pressure >= 0)
  
  ctd[['UTCDateTime']] <- ctd[['time']] 
  ctd[['LocalDateTime']] <- with_tz(ctd[['UTCDateTime']], tzone = tzone)
  ctd[['LocalDate']] <- as_date(ctd[['LocalDateTime']])
  
  if (is.null(ctd[['latitude']])) ctd <- oceSetMetadata(ctd, "latitude", NA_real_)
  
  # --- 2. DEFINE CASTS ---
  casts_list <- list()
  
  if (!is.null(start.time) && !is.null(end.time)) {
    casts_list <- list(oce::subset(ctd, time >= start.time & time <= end.time))
  } else if (use_twist_log) {
    message("   ... Scanning for Twist Start (24) -> Twist End (25) pairs")
    segments <- get_twist_segments_sql(rsk.file)
    
    if (length(segments) > 0) {
      message(paste("   > Found", length(segments), "intervals. Checking depth..."))
      valid_count <- 0
      for (seg in segments) {
        chunk <- oce::subset(ctd, time >= seg$start & time <= seg$end)
        
        if (length(chunk[['pressure']]) == 0) next
        max_p <- max(chunk[['pressure']], na.rm = TRUE)
        
        if (!is.infinite(max_p) && max_p > 0.5) {
          valid_count <- valid_count + 1
          casts_list[[valid_count]] <- chunk
        }
      }
      if (length(casts_list) == 0) message("   ! Segments found, but too shallow (<0.5m).")
    } else {
      message("   ! No valid recording intervals found.")
    }
  }
  
  # Fallback
  if (length(casts_list) == 0) {
    message("   ... Using Automated Profile Detection")
    if (is.null(minHeight)) minHeight <- 0.1 * diff(range(ctd[["pressure"]], na.rm = TRUE))
    tryCatch({
      casts_list <- ctdFindProfiles(ctd, cutoff=cutoff, minLength=minLength, minHeight=minHeight)
    }, error = function(e) warning("Detection failed."))
  }
  
  if (length(casts_list) == 0) { warning("No casts detected."); return(NULL) }
  
  # --- 3. OUTPUT DIRECTORIES ---
  if (!is.null(output_dir)) {
    dir_processed <- output_dir
    dir_summary   <- file.path(dirname(output_dir), "Summary")
  } else {
    file_dir <- dirname(rsk.file)
    base_path <- if (grepl("Raw Data", file_dir, ignore.case = TRUE)) dirname(file_dir) else file_dir
    dir_summary <- file.path(base_path, "Summary")
    dir_processed <- file.path(base_path, "Processed")
  }
  if (!dir.exists(dir_summary)) dir.create(dir_summary, recursive = TRUE)
  if (!dir.exists(dir_processed)) dir.create(dir_processed, recursive = TRUE)
  
  filename_clean <- tools::file_path_sans_ext(basename(rsk.file))
  parts <- unlist(strsplit(filename_clean, "_"))
  sn_extracted <- if(length(parts) > 0) parts[1] else "UnknownSN"
  
  all_summaries <- list()
  
  # --- 4. PROCESSING LOOP ---
  for (i in seq_along(casts_list)) {
    prof.i <- casts_list[[i]]
    
    # Safety Check: Empty Data
    if (length(prof.i[['pressure']]) == 0 || all(is.na(prof.i[['pressure']]))) next
    
    max_p <- max(prof.i[['pressure']], na.rm = TRUE)
    if (is.infinite(max_p) || max_p < 0.5) next
    
    # Trim
    is_shallow <- max_p < 5.0
    ctd.trim <- prof.i
    if (is_shallow) {
      ctd.trim <- ctdRemoveInversions(prof.i)
    } else {
      tryCatch({ ctd.trim <- ctdTrim(prof.i, method=method) }, error=function(e) { ctd.trim <- prof.i })
    }
    
    # Record max depth for the summary log
    max_depth_trim <- max(ctd.trim[['depth']], na.rm = TRUE)
    
    # --- Bin (1 dbar ~ 1m) ---
    max_p_trim <- max(ctd.trim[['pressure']], na.rm = TRUE)
    
    if (max_p_trim < 1.0) {
      message(paste0("   > Cast ", i, " is shallow (<1 dbar). Skipping decimation."))
      ctd.processed <- ctd.trim
    } else {
      tryCatch({
        ctd.processed <- ctdDecimate(ctd.trim, p = 1)
      }, error = function(e) {
        message(paste0("   ! Decimation failed for Cast ", i, ". Using un-binned data."))
        ctd.processed <- ctd.trim
      })
    }
    
    # Cast Numbering 
    current_cast_num <- if (!is.null(cast.num)) (cast.num + i - 1) else i
    
    # --- PLOT PROCESSED DATA ---
    if (plot_profiles) {
      plot(ctd.processed, which="temperature", main=paste("Cast", current_cast_num, "(Processed)"))
    }
    
    # --- OCEANOGRAPHIC PHYSICS LOGIC ---
    
    # 1. Conductivity
    cond <- ctd.processed[['conductivity']] 
    # RBR reports mS/cm. If values are ~50, it's mS/cm. If ~5, it's S/m.
    if (!is.null(cond) && mean(cond, na.rm=TRUE) > 10) {
      cond <- cond / 10 
    }
    
    # 2. Salinity (Derive if missing from RSK)
    saln <- ctd.processed[['salinity']]
    if (is.null(saln) && !is.null(cond)) {
      saln <- swSCTp(cond, ctd.processed[['temperature']], ctd.processed[['pressure']], conductivityUnit = "S/m")
    }
    
    # 3. Density (Sigma-t)
    sigma_t <- NA
    if (!is.null(saln)) {
      rho <- swRho(salinity = saln, temperature = ctd.processed[['temperature']], pressure = ctd.processed[['pressure']])
      sigma_t <- rho - 1000
    }
    
    # --- OUTPUT DATA FRAME ---
    df_out <- data.frame(
      Pressure_db    = round(ctd.processed[['pressure']], 6), 
      Depth_m        = round(ctd.processed[['depth']], 0),    
      Temp_DegC      = round(ctd.processed[['temperature']], 6),
      Cond_S_per_m   = if(!is.null(cond)) round(cond, 6) else NA,
      Saln_PSU       = if(!is.null(saln)) round(saln, 6) else NA,
      Density_Sigmat = if(!is.na(sigma_t[1])) round(sigma_t, 6) else NA
    )
    
    date_str <- min(ctd.trim[['LocalDate']], na.rm = TRUE)
    cast_name <- paste0(CRUISEID, "_SN", sn_extracted, "_", date_str, "_", sprintf("%03d", current_cast_num))
    
    # High-speed export using data.table
    fwrite(df_out, file.path(dir_processed, paste0(cast_name, ".csv")), na = "")
    
    # Summary Row
    summary_row <- data.frame(
      CastNum          = current_cast_num,                      
      Type             = if(is_shallow) "SHALLOW" else "DEEP", 
      Method           = if(!is.null(start.time)) "MANUAL_TIME" else if(use_twist_log) "TWIST" else "AUTO", 
      MaxDepth         = round(max_depth_trim, 2),
      Local_DateTime   = format(min(ctd.trim[['LocalDateTime']], na.rm = TRUE), "%Y-%m-%d %H:%M:%S"),
      UTC_DateTime     = format(min(ctd.trim[['UTCDateTime']], na.rm = TRUE), "%Y-%m-%d %H:%M:%S"),
      stringsAsFactors = FALSE
    )
    all_summaries[[length(all_summaries) + 1]] <- summary_row
    
    message(paste0("   -> Saved: ", basename(cast_name)))
  }
  
  # --- 5. FINALIZE & PRINT SUMMARY ---
  if (length(all_summaries) > 0) {
    full_summary_df <- rbindlist(all_summaries) 
    
    summary_file_name <- paste0(filename_clean, "_Summary_Log.csv")
    fwrite(full_summary_df, file.path(dir_summary, summary_file_name), na = "")
    message(paste0("📝 Summary Log saved: ", summary_file_name))
    
    cat("\n--- CAST SUMMARY ---\n")
    print(as.data.frame(full_summary_df))
    cat("--------------------\n")
  }
  
  message("✅ Processing Complete.")
}