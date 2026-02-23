
# Source all processing scripts 
script_files <- list.files("C:/Users/Hannah.Barkley/Desktop/Diel Suites/Instrument Data Processing/Diel suite/", pattern = "\\.r$|\\.R$", full.names = TRUE)
for (f in script_files) source(f)

process_ds("C:/Users/Hannah.Barkley/Desktop/Diel Suites/OCC-MAU-016/")
