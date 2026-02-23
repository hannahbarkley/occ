library(shiny)
library(tools)
library(utils) 
library(shinythemes)

# ==============================================================================
# FUNCTION DEFINITION: process_str_data
# ==============================================================================

process_str_data <- function(meta_file, 
                                    cruise_id = "SE2503", 
                                    default_start_folder = "T:/",
                                    source_script = "C:/Users/Hannah.Barkley/Desktop/Diel Suites/Instrument Data Processing/STR/processSTR.r") {
  
  # --- 1. LOAD PROCESSING SOURCE ---
  if (!file.exists(source_script)) {
    stop("❌ Could not find the processing script at: ", source_script)
  }
  source(source_script)
  
  # --- 2. SELECT FILE ---
  message("📂 Opening file selection window...")
  
  current_file <- choose.files(default = paste0(default_start_folder, "*"),
                               caption = "Select an Instrument File (.rsk or .csv)",
                               multi = FALSE,
                               filters = matrix(c("Instrument Files", "*.rsk;*.csv", "All Files", "*.*"),
                                                nrow = 2, byrow = TRUE))
  
  if (length(current_file) == 0) {
    message("❌ No file selected. Operation cancelled.")
    return(NULL)
  }
  
  message(paste("✅ Selected:", basename(current_file)))
  
  # --- 3. DETERMINE MODEL & PATHS ---
  
  # A. Determine Model
  file_ext <- tolower(tools::file_ext(current_file))
  if (file_ext == "rsk") {
    model_type <- "RBR"
  } else if (file_ext == "csv") {
    model_type <- "SBE"
  } else {
    stop("❌ Unknown file type. Please select .rsk or .csv")
  }
  message(paste("⚙️  Detected Model:", model_type))
  
  # B. Determine Output Directory (Robust Logic)
  file_dir <- dirname(current_file)
  
  # If inside a "Raw Data" folder, step up one level to find the Mooring root
  if (grepl("Raw Data", file_dir, ignore.case = TRUE)) {
    mooring_root <- dirname(file_dir) 
  } else {
    mooring_root <- file_dir
  }
  
  final_out_dir <- file.path(mooring_root, "Processed Data")
  
  # C. Extract Serial Number (SN)
  filename_only <- basename(current_file)
  raw_sn <- unlist(strsplit(filename_only, "_"))[1]
  search_sn <- if (model_type == "RBR") paste0("RBR", raw_sn) else raw_sn
  
  # --- 4. CHECK EXISTING & ASK OVERWRITE ---
  
  skip_processing <- FALSE
  
  if (dir.exists(final_out_dir)) {
    existing_files <- list.files(path = final_out_dir, pattern = search_sn, full.names = FALSE)
    
    if (length(existing_files) > 0) {
      message("⚠️  FOUND EXISTING FILES:")
      print(existing_files)
      
      user_choice <- winDialog(type = "yesno", 
                               message = paste0("Processed file(s) for SN ", search_sn, " already exist.\n\n",
                                                "Do you want to OVERWRITE them?"))
      
      if (user_choice == "YES") {
        message("🔄 User selected OVERWRITE. Proceeding...")
      } else {
        message("⏭️  User selected SKIP.")
        skip_processing <- TRUE
      }
    }
  } else {
    dir.create(final_out_dir, recursive = TRUE)
  }
  
  # --- 5. EXECUTE LOCAL PROCESSING ---
  
  if (!skip_processing) {
    
    # Create unique temp folder
    local_work_dir <- file.path(tempdir(), "Instrument_Work")
    if (dir.exists(local_work_dir)) unlink(local_work_dir, recursive = TRUE)
    dir.create(local_work_dir)
    
    message("⬇️  Copying to local drive for speed...")
    local_file_path <- file.path(local_work_dir, basename(current_file))
    
    # COPY File to Temp
    if (file.copy(current_file, local_file_path)) {
      
      tryCatch({
        
        # --- RUN APP ---
        # Note: We run on the LOCAL copy
        app <- processSTR(file = local_file_path, 
                          model = model_type, 
                          meta.file = meta_file,
                          RETRIEVE_CRUISE = cruise_id)
        
        message("🚀 Launching App...")
        runApp(app, launch.browser = TRUE)
        
        # --- MOVE RESULTS ---
        # The app creates "Processed Data" inside the temp folder
        local_output_folder <- file.path(local_work_dir, "Processed Data")
        
        if (dir.exists(local_output_folder)) {
          generated_files <- list.files(local_output_folder, full.names = TRUE)
          
          if (length(generated_files) > 0) {
            message(paste("⬆️  Moving", length(generated_files), "files back to network..."))
            file.copy(generated_files, final_out_dir, overwrite = TRUE)
            message(paste("✅ Success! Files saved to:", final_out_dir))
          } else {
            warning("⚠️ App closed but no output files were found.")
          }
        }
        
      }, error = function(e) {
        message("❌ Error during processing:")
        message(e)
      }, finally = {
        # --- CLEANUP ---
        unlink(local_work_dir, recursive = TRUE)
      })
      
    } else {
      stop("❌ Failed to copy file to local temp directory.")
    }
  } else {
    message("✅ Script finished (File was skipped).")
  }
}

# ==============================================================================
# 🚀 SETUP & EXECUTION (RUN THIS PART)
# ==============================================================================

# 1. Define your paths here
meta_file <- 'C:/Users/Hannah.Barkley/Desktop/Diel Suites/Instrument Data Processing/STR/V_OCC_CURRENT_DEPLOYMENTS_MARIAN_STR.csv'
dir <- "C:/Users/Hannah.Barkley/Desktop/Diel Suites/Instrument Data Processing/Training Data/"
source_script <- "C:/Users/Hannah.Barkley/Desktop/Diel Suites/Instrument Data Processing/STR/processSTR.r"

# 2. Run the function
process_str_data(meta_file = meta_file, 
                        default_start_folder = dir,
                        source_script = source_script,
                        cruise_id = "SE2503")

