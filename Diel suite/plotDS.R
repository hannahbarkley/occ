library(ggplot2)
library(scales)
library(viridis)
library(cowplot)
library(dplyr)
library(lubridate)

plotDS <- function(adcp = NULL,
                   ctd = NULL,
                   pH = NULL,
                   do = NULL,
                   par = NULL,
                   puc = NULL,
                   tcm = NULL,
                   read_from_csv = FALSE, 
                   plot.tz = "UTC",
                   time.step = "4 hour",
                   time.format = '%H:%M',
                   temp.range = NULL,
                   sal.range = NULL,
                   pH.range = NULL,
                   title = NULL) {
  
  # --- 1. DATA PREP HELPER ---
  prep_df <- function(obj, name = "Unknown") { 
    if (is.null(obj)) return(NULL)
    
    # Handle CSV reading
    if (read_from_csv && is.character(obj)) {
      if(file.exists(obj)) {
        obj <- read.csv(obj, stringsAsFactors = FALSE)
      } else {
        warning(paste("File not found:", obj))
        return(NULL)
      }
    }
    
    names(obj) <- toupper(names(obj))
    
    # Flexible time column search
    t_col <- grep("UTCDATETIME|TIMESTAMP|DATE_TIME", names(obj), ignore.case = TRUE, value = TRUE)[1]
    
    if (is.na(t_col)) {
      warning(paste("No time column found in", name))
      return(NULL)
    }
    
    # FORCE POSIXct CONVERSION
    if (!inherits(obj[[t_col]], "POSIXct")) {
      try_time <- as.POSIXct(obj[[t_col]], tz = "UTC", tryFormats = c("%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S"))
    } else {
      try_time <- obj[[t_col]]
    }
    
    obj$UTCDATETIME <- try_time
    
    # FINAL SAFETY CHECK
    if (!inherits(obj$UTCDATETIME, "POSIXct")) {
      warning(paste("Time conversion failed for", name, "- Check date format."))
      return(NULL)
    }
    
    obj <- obj %>% 
      filter(!is.na(UTCDATETIME)) %>% 
      arrange(UTCDATETIME)
    
    if (nrow(obj) == 0) {
      warning(paste("Data frame empty after removing NA times:", name))
      return(NULL)
    }
    
    # GAP DETECTION 
    if (nrow(obj) > 1) {
      dt <- as.numeric(median(diff(as.numeric(obj$UTCDATETIME)), na.rm = TRUE))
      if (is.na(dt) || dt == 0) dt <- 3600 
      
      obj <- obj %>%
        mutate(gap = c(0, diff(as.numeric(UTCDATETIME))) > (dt * 3),
               LINE_GROUP = cumsum(gap))
    } else {
      obj$LINE_GROUP <- 1
    }
    return(obj)
  }
  
  # Process inputs with names for better error messages
  adcp_df   <- prep_df(adcp, "ADCP")
  ctd_df    <- prep_df(ctd, "CTD")
  pH_df     <- prep_df(pH, "pH")
  do_df     <- prep_df(do, "DO")
  par_df    <- prep_df(par, "PAR")
  tcm_df    <- prep_df(tcm, "TCM")
  
  # --- 2. GLOBAL TIME ANCHOR ---
  all_times <- c(adcp_df$UTCDATETIME, ctd_df$UTCDATETIME, pH_df$UTCDATETIME, 
                 do_df$UTCDATETIME, par_df$UTCDATETIME, tcm_df$UTCDATETIME)
  
  if (length(all_times) > 0 && !inherits(all_times, "POSIXct")) {
    all_times <- as.POSIXct(all_times, origin = "1970-01-01", tz = "UTC")
  }
  
  if (length(all_times) == 0) {
    message("No valid data found to plot.")
    return(NULL)
  }
  
  global_limits <- c(min(all_times, na.rm = TRUE), max(all_times, na.rm = TRUE))
  
  # --- 3. PLOTTING TOOLS ---
  plot_list <- list()
  
  common_theme <- theme_bw() + theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    plot.margin = unit(c(0.1, 0.2, 0.1, 0.2), "cm"),
    legend.position = "right",
    legend.margin = margin(0,0,0,0),
    legend.box.margin = margin(0,0,0,0)
  )
  
  apply_x <- function(p) {
    p + scale_x_datetime(
      limits = global_limits,
      date_breaks = time.step,
      labels = date_format(time.format, tz = plot.tz),
      expand = c(0, 0)
    )
  }
  
  # --- 4. SENSOR BLOCKS ---
  
  # >> ADCP
  if (!is.null(adcp_df)) {
    h_col <- grep("HEIGHT|BIN|DEPTH", names(adcp_df), value = TRUE)[1]
    if(!is.na(h_col)) {
      p_spd <- ggplot(adcp_df, aes(x = UTCDATETIME, y = as.numeric(.data[[h_col]]), fill = SPEED_MS)) +
        geom_tile() + 
        scale_fill_viridis(name = "Speed\n(m/s)", na.value = "transparent") +
        ylab("Height (m)") + ggtitle(title) + common_theme
      plot_list[["adcp_speed"]] <- apply_x(p_spd)
      
      p_dir <- ggplot(adcp_df, aes(x = UTCDATETIME, y = as.numeric(.data[[h_col]]), fill = DIRECTION_DEG)) +
        geom_tile() + 
        scale_fill_viridis(name = "Dir\n(deg)", na.value = "transparent", option = "plasma") +
        ylab("Height (m)") + common_theme
      plot_list[["adcp_direction"]] <- apply_x(p_dir)
    }
  }
  
  # >> CTD
  if (!is.null(ctd_df)) {
    if("PRESSURE_DB" %in% names(ctd_df)) {
      p_pres <- ggplot(ctd_df, aes(x = UTCDATETIME, y = PRESSURE_DB, group = LINE_GROUP)) +
        geom_line(col = "dodgerblue") + scale_y_reverse() + ylab("Pres (db)") + common_theme
      plot_list[["pressure"]] <- apply_x(p_pres)
    }
    p_temp <- ggplot(ctd_df, aes(x = UTCDATETIME, y = TEMP_DEGC, group = LINE_GROUP)) +
      geom_line(col = "dodgerblue") + ylab("Temp (C)") + common_theme
    if (!is.null(temp.range)) p_temp <- p_temp + coord_cartesian(ylim = temp.range)
    plot_list[["temp"]] <- apply_x(p_temp)
    
    p_sal <- ggplot(ctd_df, aes(x = UTCDATETIME, y = SALN_PSU, group = LINE_GROUP)) +
      geom_line(col = "dodgerblue") + ylab("Sal (PSU)") + common_theme
    if (!is.null(sal.range)) p_sal <- p_sal + coord_cartesian(ylim = sal.range)
    plot_list[["salinity"]] <- apply_x(p_sal)
  }
  
  # >> DO
  if (!is.null(do_df)) {
    p_do <- ggplot(do_df, aes(x = UTCDATETIME, y = OXYGEN_MGL, group = LINE_GROUP)) +
      geom_line(col = "dodgerblue") + ylab("DO (mg/L)") + common_theme
    plot_list[["do"]] <- apply_x(p_do)
  }
  
  # >> pH
  if (!is.null(pH_df)) {
    p_ph <- ggplot(pH_df, aes(x = UTCDATETIME, y = PH, group = LINE_GROUP)) +
      geom_line(col = "dodgerblue") + ylab("pH") + common_theme
    if (!is.null(pH.range)) p_ph <- p_ph + coord_cartesian(ylim = pH.range)
    plot_list[["ph"]] <- apply_x(p_ph)
  }
  
  # >> PAR
  if (!is.null(par_df)) {
    p_par <- ggplot(par_df, aes(x = UTCDATETIME, y = PAR, group = LINE_GROUP)) +
      geom_line(col = "dodgerblue") + ylab("PAR") + common_theme
    plot_list[["par"]] <- apply_x(p_par)
  }
  
  # >> TCM
  if (!is.null(tcm_df)) {
    p_tcm <- ggplot(tcm_df, aes(x = UTCDATETIME, y = SPEED_MS, color = DIRECTION_DEG)) +
      geom_point(size = 0.5) + 
      scale_color_viridis(name = "Dir", option = "plasma") +
      ylab("TCM\n(m/s)") + common_theme
    plot_list[["tcm"]] <- apply_x(p_tcm)
  }
  
  # --- 5. ALIGN & COMBINE ---
  last_idx <- length(plot_list)
  
  if (last_idx > 0) {
    plot_list[[last_idx]] <- plot_list[[last_idx]] + 
      theme(axis.text.x = element_text(color = "black", angle = 0, hjust = 0.5)) + 
      xlab(paste("Time (", plot.tz, ")", sep = ""))
    
    final_grid <- plot_grid(
      plotlist = plot_list,
      ncol = 1,
      align = "v", 
      axis = "lr",
      rel_heights = rep(1, last_idx)
    )
    return(final_grid) 
  } else {
    return(NULL)
  }
}