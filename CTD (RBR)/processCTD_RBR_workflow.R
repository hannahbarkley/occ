## Workflow for processing RBR CTD profiles

library(lubridate)
library(utils)
library(crayon) 

# --- CONFIGURATION ---
source("C:/Users/Hannah.Barkley/Desktop/Diel Suites/Instrument Data Processing/CTD (RBR)/processCTD_RBR.r")

run_ctd_workflow <- function(cruise_id,
                             time_zone,
                             cast_folder,
                             start.time = NULL, # Option to define manual start time (default = NULL)
                             end.time = NULL,   # Option to define manual end time (default = NULL)   
                             cast.num = NULL,   # Option to manually assign a cast number (default = NULL)
                             use_twist_log = TRUE, # Identify casts using cap twist events (default = TRUE)
                             plot_profiles = TRUE) { 
  
  # --- 1. SETUP OUTPUT DIRECTORY ---
  clean_cast_folder <- normalizePath(cast_folder, winslash = "/", mustWork = FALSE)
  parent_folder <- dirname(clean_cast_folder)
  output_folder <- file.path(parent_folder, "Processed")
  
  if (!dir.exists(output_folder)) {
    cat(blue(paste0("📂 Creating output directory: ", output_folder, "\n")))
    dir.create(output_folder, recursive = TRUE)
  } else {
    cat(blue(paste0("📂 Output directory found: ", output_folder, "\n")))
  }
  
  # --- 2. HANDLE TIME ARGUMENTS ---
  if (!is.null(start.time) && is.character(start.time)) {
    start.time <- force_tz(ymd_hms(start.time), tzone = "UTC")
    cat(yellow(paste0("⏰ Manual Start Time set: ", start.time, " (UTC)\n")))
  }
  
  if (!is.null(end.time) && is.character(end.time)) {
    end.time <- force_tz(ymd_hms(end.time), tzone = "UTC")
    cat(yellow(paste0("⏰ Manual End Time set:   ", end.time, " (UTC)\n")))
  }
  
  # --- 3. SELECT FILES ---
  cat(blue("\n📂 Opening file selection window... (Hold Ctrl or Shift to select multiple)\n"))
  
  rsk_files <- choose.files(default = file.path(clean_cast_folder, "*.rsk"),
                            caption = "Select RSK Files (Multi-select allowed)",
                            multi = TRUE, 
                            filters = matrix(c("RSK Files", "*.rsk", "All Files", "*.*"),
                                             nrow = 2, byrow = TRUE))
  
  if (length(rsk_files) == 0) {
    cat(magenta("❌ No files selected. Operation cancelled.\n"))
    return(NULL)
  }
  
  cat(green(paste0("✅ Selected ", length(rsk_files), " files.\n")))
  
  # --- 4. LOOP THROUGH FILES ---
  for (i in seq_along(rsk_files)) {
    
    current_file <- rsk_files[i]
    
    cat(cyan("\n========================================================\n"))
    cat(blue(paste0("Processing File ", i, " of ", length(rsk_files), "\n")))
    cat(blue(paste0("File: ", basename(current_file), "\n")))
    
    tryCatch({
      
      processCTD_RBR(rsk.file = current_file, 
                     CRUISEID = cruise_id, 
                     tzone = time_zone,
                     start.time = start.time, 
                     end.time = end.time,  
                     cast.num = cast.num,
                     use_twist_log = use_twist_log,
                     plot_profiles = plot_profiles,
                     output_dir = output_folder) 
      
      cat(green(paste0("✅ Processed: ", basename(current_file), "\n")))
      
    }, error = function(e) {
      cat(magenta(paste0("❌ Error processing: ", basename(current_file), "\n")))
      cat(magenta(paste0("   Reason: ", e$message, "\n")))
    })
  }
  
  cat(cyan("\n========================================================\n"))
  cat(green("🎉 Batch Processing Complete!\n"))
  cat(silver(paste0("Files saved to: ", output_folder, "\n")))
}

# ==============================================================================
# 🚀 EXECUTION 
# ==============================================================================

# Standard Auto-Detect Casts (uses twist events (default) or auto-detects casts from profile data)
run_ctd_workflow(cruise_id = "SE2503",
                 time_zone = "Pacific/Guam",
                 cast_folder = "C:/Users/Hannah.Barkley/Desktop/Diel Suites/")


# Process Cast Using Manual Time Window (use this if the code above doesn't identify a specific cast you know exists, e.g. shallow offshore)
# run_ctd_workflow(cruise_id = "SE2503",
#                  time_zone = "Pacific/Guam",
#                  cast_folder = "C:/Users/Hannah.Barkley/Desktop/Diel Suites/",
#                  cast.num = 3,
#                  start.time = "2025-05-23 08:26:00", # You can get these times from the .xslx file
#                  end.time = "2025-05-23 08:28:00")