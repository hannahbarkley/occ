process_ds <- function(site_dir, output_dir = NULL) {
  
  site_name <- basename(site_dir)
  cat(paste0("\n==============================================\n"))
  cat(paste0("   PROCESSING DEPLOYMENT: ", site_name, "\n"))
  cat(paste0("==============================================\n"))
  
  # 1. SETUP OUTPUT DIRECTORY
  if (is.null(output_dir)) {
    output_dir <- file.path(site_dir, "Processed Data")
  }
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Initialize Status Tracker
  status <- list(
    CTD = "Missing",
    PAR = "Missing",
    ADCP = "Missing",
    pH = "Missing",
    DO = "Missing",
    TCM = "Missing",
    Trim = "Skipped",
    Plot = "Skipped"
  )
  
  # 2. HELPER TO FIND FILES
  find_sensor_file <- function(subfolder, pattern) {
    target_path <- file.path(site_dir, subfolder)
    if (!dir.exists(target_path)) return(NULL)
    
    files <- list.files(target_path, pattern = pattern, full.names = TRUE)
    files <- files[!grepl("^~", basename(files))]
    
    if (length(files) == 0) return(NULL)
    if (length(files) > 1) {
      cat(paste0("   ! Note: Multiple files found in ", subfolder, ". Using: ", basename(files[1]), "\n"))
    }
    return(files[1])
  }
  
  # 3. LOCATE FILES
  file_ctd  <- find_sensor_file("CTD-Moored", "\\.rsk$|\\.cnv$")
  file_par  <- find_sensor_file("PAR", "\\.rsk$|\\.txt$")
  file_adcp <- find_sensor_file("ADCP", "\\.prf$|\\.000$|\\.adp$")
  file_ph   <- find_sensor_file("SAMI", "\\.txt$")
  if (is.null(file_ph)) file_ph <- find_sensor_file("SeaFET", "\\.csv$")
  file_do   <- find_sensor_file("DO", "\\.rsk$|\\.txt$")
  file_tcm  <- find_sensor_file("TCM", "\\.csv$")
  
  # 4. PROCESS INSTRUMENTS
  obj_ctd <- NULL; obj_par <- NULL; obj_adcp <- NULL; obj_ph <- NULL; obj_do <- NULL; obj_tcm <- NULL
  
  # -- CTD --
  if (!is.null(file_ctd)) {
    cat("   -> Found CTD. Processing...\n")
    tryCatch({
      is_rbr <- grepl("\\.rsk$", file_ctd, ignore.case = TRUE)
      # Ensure processCTD_moored call matches your definition (write.csv vs write_to_csv)
      # Assuming standard definition uses write.csv:
      obj_ctd <- processCTD_moored(file_ctd, model = ifelse(is_rbr, "RBR", "SBE"), write.csv = TRUE) 
      status$CTD <- "Success"
    }, error = function(e) { 
      cat(paste("       CTD Error:", e$message, "\n"))
      status$CTD <<- "Failed"
    })
  }
  
  # -- PAR --
  if (!is.null(file_par)) {
    cat("   -> Found PAR. Processing...\n")
    tryCatch({
      obj_par <- processPAR(file_par, write.csv = TRUE)
      status$PAR <- "Success"
    }, error = function(e) { 
      cat(paste("       PAR Error:", e$message, "\n"))
      status$PAR <<- "Failed"
    })
  }
  
  # -- ADCP --
  if (!is.null(file_adcp)) {
    cat("   -> Found ADCP. Processing...\n")
    tryCatch({
      obj_adcp <- processADCP(file_adcp, na.rm = TRUE, format = "long", write.csv = TRUE)
      status$ADCP <- "Success"
    }, error = function(e) { 
      cat(paste("       ADCP Error:", e$message, "\n"))
      status$ADCP <<- "Failed"
    })
  }
  
  # -- pH --
  if (!is.null(file_ph)) {
    cat("   -> Found pH. Processing...\n")
    tryCatch({
      # Pass CTD object for salinity correction
      # Enable 'reprocess' to trigger the correction (SAMI) or internal selection (SeaFET)
      obj_ph <- processpH(file_ph, 
                          ctd_data = obj_ctd, 
                          reprocess = TRUE, 
                          pH_min = 7.8, 
                          write_to_csv = TRUE) # Updated argument name
      status$pH <- "Success"
    }, error = function(e) { 
      cat(paste("       pH Error:", e$message, "\n"))
      status$pH <<- "Failed"
    })
  }
  
  # -- DO --
  if (!is.null(file_do)) {
    if (!is.null(obj_ctd)) {
      cat("   -> Found DO. Processing (using CTD)...\n")
      tryCatch({
        obj_do <- processDO(file_do, ctd = obj_ctd, write.csv = TRUE)
        status$DO <- "Success"
      }, error = function(e) { 
        cat(paste("       DO Error:", e$message, "\n"))
        status$DO <<- "Failed"
      })
    } else {
      cat("   ! Skipping DO: CTD required but missing/failed.\n")
      status$DO <- "Failed (No CTD)"
    }
  }
  
  # -- TCM --
  if (!is.null(file_tcm)) {
    cat("   -> Found TCM. Processing...\n")
    tryCatch({
      obj_tcm <- processTCM(file_tcm, write.csv = TRUE)
      status$TCM <- "Success"
    }, error = function(e) { 
      cat(paste("       TCM Error:", e$message, "\n"))
      status$TCM <<- "Failed"
    })
  }
  
  # 5. TRIM & PLOT
  cat("   -> Running trimDS...\n")
  trim_results <- NULL
  
  tryCatch({
    trim_results <- trimDS(
      adcp = obj_adcp,
      ctd = obj_ctd,
      pH = obj_ph,
      par = obj_par,
      do = obj_do,
      tcm = obj_tcm, # Include TCM in trim
      filename = site_name,
      output.dir = output_dir,
      read_from_csv = FALSE,
      write_to_csv = TRUE
    )
    status$Trim <- "Success"
  }, error = function(e) {
    cat(paste("       Trim Error:", e$message, "\n"))
    status$Trim <<- "Failed"
  })
  
  if (!is.null(trim_results)) {
    cat("   -> Running plotDS...\n")
    tryCatch({
      final_grid_plot <- plotDS(
        adcp = trim_results$adcp,
        ctd = trim_results$ctd,
        pH = trim_results$pH,
        par = trim_results$par,
        do = trim_results$do,
        tcm = trim_results$tcm, 
        read_from_csv = FALSE, 
        sal.range = c(34, 35),
        title = site_name
      )
      
      if (!is.null(final_grid_plot)) {
        print(final_grid_plot)
        cat("       ... Plot displayed in Viewer.\n")
      }
      
      status$Plot <- "Success"
    }, error = function(e) {
      cat(paste("       Plot Error:", e$message, "\n"))
      status$Plot <<- "Failed"
    })
  }
  
  # --- SUMMARY REPORT ---
  cat(paste0("\n----------------------------------------------\n"))
  cat(paste0("   SUMMARY FOR: ", site_name, "\n"))
  cat(paste0("----------------------------------------------\n"))
  
  for (inst in names(status)) {
    cat(sprintf("   %-10s : %s\n", inst, status[[inst]]))
  }
  cat(paste0("----------------------------------------------\n\n"))
  
  # --- FINAL RETURN ---
  # Return the trimmed results if successful, otherwise return the raw objects
  if (!is.null(trim_results)) {
    return(invisible(trim_results))
  } else {
    # Fallback list if trimming failed
    return(invisible(list(
      ctd = obj_ctd,
      par = obj_par,
      adcp = obj_adcp,
      pH = obj_ph,
      do = obj_do,
      tcm = obj_tcm
    )))
  }
}