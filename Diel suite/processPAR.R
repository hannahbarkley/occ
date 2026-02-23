library(oce)
library(lubridate)
library(tools)

processPAR <- function(file, write.csv = TRUE) {
  
  # --- 1. Read Data ---
  # Swapped message for cat to avoid red text
  cat(paste0("Reading PAR file: ", basename(file), "\n"))
  
  # Check if file exists and is not empty
  if (!file.exists(file) || file.size(file) == 0) {
    stop("File does not exist or is empty: ", file)
  }
  
  par.file <- read.oce(file)
  
  # --- 2. Extract Data ---
  if (is.null(par.file[['par']])) {
    if (!is.null(par.file[['PAR']])) {
      par.data <- par.file[['PAR']]
    } else {
      # Swapped warning for cat
      cat("Warning: Variable 'par' not found in file. Returning raw object structure.\n")
      print(names(par.file@data))
      return(NULL)
    }
  } else {
    par.data <- par.file[['par']]
  }
  
  UTCDateTime <- par.file[['time']]
  
  # --- 3. Build DataFrame ---
  par_df <- data.frame(UTCDATETIME = UTCDateTime, 
                       PAR = par.data)
  
  # Ensure UTC Timezone
  if (!inherits(par_df$UTCDATETIME, "POSIXct")) {
    par_df$UTCDATETIME <- ymd_hms(par_df$UTCDATETIME, tz = "UTC")
  } else {
    par_df$UTCDATETIME <- with_tz(par_df$UTCDATETIME, tz = "UTC")
  }
  
  # --- 4. Export ---
  if (write.csv) {
    name <- file_path_sans_ext(basename(file))
    out_dir <- dirname(file)
    out_path <- file.path(out_dir, paste0(name, "_processed.csv"))
    
    write.csv(par_df, 
              out_path, 
              row.names = FALSE, 
              na = "")
    
    # Swapped message for cat
    cat(paste0("Saved processed PAR file: ", out_path, "\n"))
  }
  
  return(par_df)
}