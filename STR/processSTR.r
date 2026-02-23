library(lubridate)
library(shiny)
library(plotly)
library(shinyalert)
library(shinyjs)
library(htmltools)
library(htmlwidgets)
library(tools)
library(scales)
library(ggpubr)
library(stringr)
library(oce)
library(shinythemes)

# Set environment variables to UTC
Sys.setenv(TZ = 'UTC')
options(scipen = 999)

processSTR <- function(file, 
                       meta.file = NULL, 
                       model_type = c("SBE", "RBR", "SBE39", "SBE39.special", "HOBO"), 
                       RETRIEVE_CRUISE = NULL) {
  
  model_type <- match.arg(model_type)
  
  # ============================================================================
  # 1. SHINY APP UI
  # ============================================================================
  jscode <- "shinyjs.closeWindow = function() { window.close(); }"
  
  ui <- fluidPage(
    theme = shinythemes::shinytheme("flatly"),
    useShinyjs(),
    extendShinyjs(text = jscode, functions = c("closeWindow")),
    useShinyalert(force = TRUE),
    
    tags$head(
      tags$style(HTML("
        body { background-color: #f4f7f6; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; }
        .well { background-color: #ffffff; border-radius: 12px; box-shadow: 0 4px 15px rgba(0,0,0,0.06); border: 1px solid #e3e6e8; margin-bottom: 30px; padding: 20px; }
        #meta_styled { background: #2c3e50; color: #ffffff; padding: 15px; border-radius: 8px; font-weight: bold; text-align: center; min-height: 55px; display: flex; align-items: center; justify-content: center; font-size: 1.1em; }
        .plotly .cursor-ew-resize { cursor: crosshair !important; }
        .btn-stack { display: flex; flex-direction: column; gap: 8px; max-width: 260px; margin: 15px auto; }
        .btn { border-radius: 6px; font-weight: 600; transition: transform 0.1s; }
        .section-header { border-bottom: 2px solid #18bc9c; padding-bottom: 10px; margin-bottom: 20px; font-weight: 800; color: #2c3e50; text-transform: uppercase; letter-spacing: 1px; }
        
        /* Specific styling to make the disabled purple button look truly disabled */
        #btn_meta_start:disabled { background-color: #bca0dc !important; color: #f4f7f6 !important; cursor: not-allowed; opacity: 0.65; box-shadow: none; }
      "))
    ),
    
    fluidRow(
      column(10, offset = 1,
             br(),
             div(id = "meta_styled", icon("ship"), " ", textOutput("meta", inline = TRUE)),
             br(),
             div(style = "display: flex; gap: 15px; justify-content: center;",
                 actionButton("revise.SN", "Change Serial Number", icon = icon("tag"), class = "btn-info"),
                 actionButton("revise", "Edit Metadata Details", icon = icon("edit"), class = "btn-info")
             ),
             hr(style="border-top: 1px solid #dce1e5;"),
             
             div(class = "well", 
                 h4(class = "section-header", icon("chart-area"), "1. Full Series Preview"),
                 plotlyOutput("whole.ts", height = "240px")),
             
             div(class = "well",
                 h4(class = "section-header", icon("arrows-alt-h"), "2. Select DATA START AND END"),
                 fluidRow(
                   column(6, align="center", 
                          h5(style="font-weight:bold; color:#34495e;", "Select Valid Data Start (First Month)"),
                          plotlyOutput("start.plot", height = "380px"),
                          div(class = "btn-stack",
                              actionButton("btn_meta_start", "Use Deploy Date and Time", icon = icon("database"), 
                                           style="background-color:#6f42c1; color:white; border:none;"),
                              actionButton("btn_use_first", "Select First Time Point", icon = icon("step-backward"), class = "btn-primary")
                          )),
                   column(6, align="center", 
                          h5(style="font-weight:bold; color:#34495e;", "Select Valid Data End (Last Month)"),
                          plotlyOutput("end.plot", height = "380px"),
                          div(class = "btn-stack",
                              div(style="height: 40px;"), 
                              actionButton("btn_use_last", "Select Last Time Point", icon = icon("step-forward"), class = "btn-primary")
                          ))
                 ),
                 br(),
                 div(align="center", actionButton("btn_reset", "Reset Selection Points", icon = icon("sync"), class = "btn-danger", style="width:200px;"))
             ),
             
             div(class = "well", 
                 h4(class = "section-header", icon("check-double"), "3. Final Trimmed Review"),
                 plotlyOutput("cut.ts", height = "280px"),
                 br(),
                 div(style = "display: flex; justify-content: center; gap: 20px;",
                     actionButton("save", "Save Processed Data", icon = icon("save"), class = "btn-lg btn-success", style="padding: 18px 50px; font-size: 1.2em;"),
                     actionButton("close", "Finish & Exit", icon = icon("power-off"), class = "btn-lg btn-default")
                 )
             ),
             br()
      )
    )
  )
  
  # ============================================================================
  # 2. SHINY SERVER
  # ============================================================================
  server <- function(input, output, session) {
    session$onSessionEnded(function() { stopApp() })
    
    rv <- reactiveValues(str = NULL, meta.data.all = NULL, SNs = NULL)
    vals <- reactiveValues(start.time = NULL, end.time = NULL)
    spike_config <- list(showspikes = TRUE, spikemode = "across", spikethickness = 1, spikedash = "dash", spikecolor = "#18bc9c")
    
    get_meta_start <- function(sn) {
      if (is.null(rv$meta.data.all) || is.null(sn) || is.na(sn) || sn == "") return(NULL)
      col_idx <- grep("^UTCDATETIME$", names(rv$meta.data.all), ignore.case = TRUE)
      if (length(col_idx) == 0) return(NULL)
      row_match <- rv$meta.data.all[as.character(rv$meta.data.all$INST_SN) == as.character(sn), ]
      if(nrow(row_match) > 0) {
        m_time <- parse_date_time(row_match[[col_idx[1]]][1], orders = c("ymd HMS", "mdy HMS", "dmy HMS", "ymd HM", "mdy HM", "ymd IMp", "mdy IMp"), tz = "UTC", truncated = 3)
        if(!is.na(m_time)) {
          idx <- which.min(abs(as.numeric(rv$str$UTCDateTime) - as.numeric(m_time)))
          return(list(x = rv$str$UTCDateTime[idx], y = rv$str$Temperature[idx]))
        }
      }
      return(NULL)
    }
    
    # ==========================================================================
    # DATA LOADING & SN DETECTION
    # ==========================================================================
    observe({
      withProgress(message = 'Crunching Logger Data...', value = 0, {
        
        # --- 1. ROUTE FILE READING BY MODEL TYPE ---
        if (model_type == "RBR") {
          rsk_data <- oce::read.rsk(file)
          df_raw <- as.data.frame(rsk_data@data)
          rv$str <- data.frame(UTCDateTime = df_raw$time, Temperature = df_raw$temperature)
          
        } else if (grepl("SBE", model_type, ignore.case = TRUE)) {
          headers <- readLines(file, n = 100)
          header_idx <- grep("^\"?Date", headers, ignore.case = TRUE)[1]
          if(is.na(header_idx)) header_idx <- 1
          
          df_raw <- read.csv(file, skip = header_idx - 1, stringsAsFactors = FALSE, check.names = FALSE)
          date_col <- grep("date", colnames(df_raw), ignore.case = TRUE)[1]
          time_col <- grep("time", colnames(df_raw), ignore.case = TRUE)[1]
          temp_col <- grep("temp", colnames(df_raw), ignore.case = TRUE)[1]
          
          df_raw$UTCDateTime <- parse_date_time(paste(df_raw[[date_col]], df_raw[[time_col]]), 
                                                orders = c("ymd HMS", "mdy HMS", "dmy HMS"), tz = "UTC", truncated = 3)
          rv$str <- data.frame(UTCDateTime = df_raw$UTCDateTime, Temperature = df_raw[[temp_col]])
          
        } else if (model_type == "HOBO") {
          df_raw <- read.csv(file, skip = 1, stringsAsFactors = FALSE, check.names = FALSE)
          date_col <- grep("date|time", colnames(df_raw), ignore.case = TRUE)[1]
          temp_col <- grep("temp", colnames(df_raw), ignore.case = TRUE)[1]
          
          df_raw$UTCDateTime <- parse_date_time(df_raw[[date_col]], 
                                                orders = c("ymd HMS", "mdy HMS", "dmy HMS", "mdy IMp"), tz = "UTC", truncated = 3)
          rv$str <- data.frame(UTCDateTime = df_raw$UTCDateTime, Temperature = df_raw[[temp_col]])
        }
        
        # Clean up any missing valid timestamps
        rv$str <- rv$str[!is.na(rv$str$UTCDateTime) & !is.na(rv$str$Temperature), ]
        
        # --- 2. METADATA SYNC ---
        incProgress(0.5, detail = "Syncing Metadata...")
        if (!is.null(meta.file) && file.exists(meta.file)) {
          rv$meta.data.all <- read.csv(meta.file, stringsAsFactors = FALSE)
          rv$SNs <- unique(as.character(rv$meta.data.all$INST_SN))
        }
        
        # --- 3. AUTO-EXTRACT SERIAL NUMBER ---
        incProgress(0.9, detail = "Identifying Logger SN...")
        filename_only <- basename(file)
        tokens <- unlist(strsplit(filename_only, "[_\\-\\.]")) 
        def_sn <- NULL
        
        for (t in tokens) {
          clean_t <- sub("^0+", "", gsub("^SBE[0-9]*|^SBE", "", t, ignore.case = TRUE))
          if (clean_t %in% rv$SNs) { def_sn <- clean_t; break }
          if (paste0("RBR", clean_t) %in% rv$SNs) { def_sn <- paste0("RBR", clean_t); break }
        }
        if (is.null(def_sn)) def_sn <- sub("^0+", "", gsub("^SBE", "", tokens[1], ignore.case = TRUE))
        
        showModal(modalDialog(
          title = "Step 1: Confirm Serial Number",
          selectizeInput("SN.input", "Select Logger Serial Number:", choices = rv$SNs, selected = def_sn),
          footer = actionButton("run.SN", "Apply SN & Continue", class="btn-success"),
          easyClose = FALSE, size = "m"
        ))
      })
    })
    
    observeEvent(input$run.SN, {
      req(input$SN.input)
      match_row <- rv$meta.data.all[as.character(rv$meta.data.all$INST_SN) == as.character(input$SN.input), ]
      d_transect <- if(nrow(match_row) > 0) match_row$TRANSECTID[1] else ""
      d_site <- if(nrow(match_row) > 0) match_row$ASSOC_OCC_SITEID[1] else ""
      d_depth <- if(nrow(match_row) > 0) match_row$DEPTH_M[1] else ""
      
      # Determine Metadata Match and Toggle Button
      meta_match <- get_meta_start(input$SN.input)
      if (!is.null(meta_match)) {
        shinyjs::enable("btn_meta_start")
        vals$start.time <- meta_match
      } else {
        shinyjs::disable("btn_meta_start")
        vals$start.time <- list(x = rv$str$UTCDateTime[1], y = rv$str$Temperature[1]) # Fallback to absolute start
      }
      
      # Ensure end time remains blank initially
      vals$end.time <- NULL
      
      removeModal()
      showModal(modalDialog(
        title = "Step 2: Deployment Details",
        textInput("cruise.input", "Recovery Cruise Name:", value = RETRIEVE_CRUISE),
        textInput("transect.input", "Transect ID:", value = d_transect),
        textInput("site.input", "Site ID:", value = d_site),
        textInput("depth.input", "Depth (m):", value = d_depth),
        footer = actionButton("run.meta", "Finalize Metadata", class="btn-success"),
        easyClose = TRUE, size = "m"
      ))
    })
    
    observeEvent(input$run.meta, { removeModal() })
    observeEvent(input$revise.SN, { showModal(modalDialog(title = "Change Serial Number", selectizeInput("SN.input", "SN:", choices = rv$SNs, selected = input$SN.input), footer = actionButton("run.SN", "Apply"), easyClose = TRUE)) })
    observeEvent(input$revise, { showModal(modalDialog(title = "Edit Details", textInput("cruise.input", "Cruise:", value = input$cruise.input), textInput("transect.input", "Transect:", value = input$transect.input), textInput("site.input", "Site:", value = input$site.input), textInput("depth.input", "Depth:", value = input$depth.input), footer = actionButton("run.meta", "OK"), easyClose = TRUE)) })
    
    output$meta <- renderText({
      req(vals$start.time, input$SN.input)
      # Display an 'NA' for end time visually if it hasn't been set yet
      end_disp <- if(!is.null(vals$end.time)) as.character(as.Date(vals$end.time$x)) else "Pending End Date..."
      paste(input$cruise.input, input$SN.input, as.Date(vals$start.time$x), end_disp, paste0(input$depth.input, "m"), input$site.input, input$transect.input, sep = " | ")
    })
    
    # ==========================================================================
    # PLOTTING & CLICKS 
    # ==========================================================================
    output$whole.ts <- renderPlotly({
      req(rv$str)
      
      p <- plot_ly(rv$str, x = ~UTCDateTime, y = ~Temperature, type = 'scatter', mode = 'lines', line = list(color = '#3498db')) %>% 
        layout(xaxis = spike_config, yaxis = spike_config, hovermode = "closest")
      
      # Add a refined semi-transparent box with crisp dotted borders
      if (!is.null(vals$start.time) && !is.null(vals$end.time)) {
        p <- p %>% layout(
          shapes = list(
            list(
              type = "rect",
              fillcolor = "rgba(24, 188, 156, 0.20)", # 20% opacity teal for better visibility
              line = list(color = "rgba(24, 188, 156, 0.8)", width = 2, dash = "dot"), # Crisp dotted borders
              x0 = vals$start.time$x,
              x1 = vals$end.time$x,
              xref = "x",
              y0 = 0,
              y1 = 1,
              yref = "paper", # Stretches from the bottom to the top of the plot
              layer = "below" # Puts the box behind the data lines
            )
          )
        )
      }
      p
    })
    
    output$start.plot <- renderPlotly({ 
      req(rv$str)
      
      min_date <- rv$str$UTCDateTime[1]
      max_date <- rv$str$UTCDateTime[nrow(rv$str)]
      start_rng <- c(min_date, min(min_date + days(30), max_date))
      
      p <- plot_ly(rv$str, x = ~UTCDateTime, y = ~Temperature, type = 'scatter', mode = 'lines', source = 'S', line = list(color = '#3498db')) %>% 
        layout(showlegend = F, xaxis = c(list(rangeslider = list(type="date"), range=start_rng), spike_config), yaxis = spike_config, hovermode = "closest")
      
      if (!is.null(vals$start.time)) {
        p <- add_trace(p, x = vals$start.time$x, y = vals$start.time$y, type = 'scatter', mode = 'markers', marker = list(size=14, symbol="circle-cross", color="#e74c3c", line=list(color="white", width=2)))
      }
      p 
    })
    
    output$end.plot <- renderPlotly({ 
      req(rv$str)
      
      min_date <- rv$str$UTCDateTime[1]
      max_date <- rv$str$UTCDateTime[nrow(rv$str)]
      end_rng <- c(max(max_date - days(30), min_date), max_date)
      
      p <- plot_ly(rv$str, x = ~UTCDateTime, y = ~Temperature, type = 'scatter', mode = 'lines', source = 'E', line = list(color = '#3498db')) %>% 
        layout(showlegend = F, xaxis = c(list(rangeslider = list(type="date"), range=end_rng), spike_config), yaxis = spike_config, hovermode = "closest")
      
      if (!is.null(vals$end.time)) {
        p <- add_trace(p, x = vals$end.time$x, y = vals$end.time$y, type = 'scatter', mode = 'markers', marker = list(size=14, symbol="circle-cross", color="#e74c3c", line=list(color="white", width=2)))
      }
      p 
    })
    
    observe({ 
      d_s <- event_data("plotly_click", source = 'S'); if (!is.null(d_s)) vals$start.time <- list(x = as.POSIXct(d_s$x, tz = "UTC", origin = "1970-01-01"), y = d_s$y)
      d_e <- event_data("plotly_click", source = 'E'); if (!is.null(d_e)) vals$end.time <- list(x = as.POSIXct(d_e$x, tz = "UTC", origin = "1970-01-01"), y = d_e$y)
    })
    
    observeEvent(input$btn_meta_start, { 
      res <- get_meta_start(input$SN.input)
      if(!is.null(res)) vals$start.time <- res 
    })
    
    observeEvent(input$btn_use_first, { vals$start.time <- list(x = rv$str$UTCDateTime[1], y = rv$str$Temperature[1]) })
    observeEvent(input$btn_use_last, { vals$end.time <- list(x = rv$str$UTCDateTime[nrow(rv$str)], y = rv$str$Temperature[nrow(rv$str)]) })
    
    observeEvent(input$btn_reset, { 
      meta_match <- get_meta_start(input$SN.input)
      if (!is.null(meta_match)) {
        vals$start.time <- meta_match
      } else {
        vals$start.time <- list(x = rv$str$UTCDateTime[1], y = rv$str$Temperature[1]) 
      }
      vals$end.time <- NULL
    })
    
    observe({
      # Require BOTH to be selected before drawing the final cut box
      req(vals$start.time, vals$end.time, rv$str)
      sub_data <- subset(rv$str, UTCDateTime >= vals$start.time$x & UTCDateTime <= vals$end.time$x)
      output$cut.ts <- renderPlotly({ plot_ly(sub_data, x = ~UTCDateTime, y = ~Temperature, type = 'scatter', mode = 'lines', line = list(color = '#18bc9c')) %>% layout(xaxis = spike_config, yaxis = spike_config, hovermode = "closest") })
    })
    
    # ==========================================================================
    # SAVE WITH CUSTOM INSTRUMENT MESSAGES
    # ==========================================================================
    observeEvent(input$save, {
      req(vals$start.time, vals$end.time, rv$str)
      sub_save <- subset(rv$str, UTCDateTime >= vals$start.time$x & UTCDateTime <= vals$end.time$x)
      
      # Calc stats
      duration_days <- round(as.numeric(difftime(vals$end.time$x, vals$start.time$x, units = "days")), 1)
      duration_years <- duration_days / 365.25
      avg_temp <- round(mean(sub_save$Temperature, na.rm = TRUE), 2)
      
      # Logic for custom messages
      custom_msg <- "Data successfully processed!"
      
      if (model_type == "RBR") {
        if (duration_years < 1) custom_msg <- "Ew, definitely a Really Bad Recorder."
        if (duration_years > 2.5) custom_msg <- "Wow, now that's a Really Brilliant Recorder."
      } else if (grepl("SBE", model_type, ignore.case = TRUE)) {
        if (duration_years < 1) custom_msg <- "Ugh, definitely some Seriously Bad Equipment."
        if (duration_years > 2.5) custom_msg <- "This Seabird is really soaring! Now that's some Super Brilliant Equipment."
      }
      
      fname <- paste(input$cruise.input, input$SN.input, as.Date(vals$start.time$x), as.Date(vals$end.time$x), paste0(input$depth.input, "m"), input$site.input, input$transect.input, sep = "_")
      out_dir <- file.path(dirname(file), "Processed Data"); if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
      
      write.csv(sub_save, file.path(out_dir, paste0(fname, "_processed.csv")), row.names = FALSE)
      
      shinyalert(
        title = "STR Data Saved!",
        text = paste0(
          custom_msg, "\n\n",
          "Time Series Stats:\n",
          "⚓ Duration: ", duration_days, " days (", round(duration_years, 2), " years)\n",
          "🌡️ Avg Temp: ", avg_temp, "°C\n\n"
        ),
        type = "success"
      )
    })
    
    observeEvent(input$close, { js$closeWindow(); stopApp() })
  }
  
  shinyApp(ui, server)
}