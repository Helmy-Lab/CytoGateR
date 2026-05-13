# cellIdentificationModuleUI.R
cellIdentificationModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinydashboard::box(
      title = "Cell Identification & Gating", status = "warning", solidHeader = TRUE,
      width = 12, collapsible = TRUE,
      
      # QC Section
      div(class = "parameter-group",
          h5(icon("shield-alt"), "Quality Control"),
          checkboxInput(ns("performQC"), "Perform Quality Control", value = TRUE),
          # Replace conditionalPanel with server-side rendering
          uiOutput(ns("qcOptionsUI"))
      ),
      
      tags$hr(),
      
      # Gating Section
      div(class = "parameter-group",
          h5(icon("filter"), "Gating"),
          checkboxInput(ns("performGating"), "Perform Gating", value = TRUE),
          # Replace conditionalPanel with server-side rendering
          uiOutput(ns("gatingOptionsUI"))
      ),
      
      tags$hr(),
      
      # Marker Selection
      div(class = "parameter-group",
          h5(icon("crosshairs"), "Marker Selection"),
          selectizeInput(ns("markerSelection"), "Markers for Dimensionality Reduction / Clustering:",
                         choices = NULL, multiple = TRUE)
      ),
      
      # Optional custom gating
      div(class = "parameter-group",
          h5(icon("code"), "Custom Gating Expression"),
          textInput(ns("customGate"), "Custom Expression (e.g., CD3 > 500 & CD19 < 200)")
      )
    )
  )
}

# cellIdentificationModuleServer.R
cellIdentificationModuleServer <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ============================================================================
    # SERVER-SIDE CONDITIONAL UI RENDERING
    # ============================================================================
    
    # QC options UI
    output$qcOptionsUI <- renderUI({
      if (isTRUE(input$performQC)) {
        tagList(
          selectInput(session$ns("qcVariable"), "QC Variable", choices = NULL),
          sliderInput(session$ns("qcThreshold"), "QC Threshold Range", min = 0, max = 100000, value = c(100, 10000))
        )
      }
    })
    
    # Gating options UI
    output$gatingOptionsUI <- renderUI({
      if (isTRUE(input$performGating)) {
        tagList(
          selectizeInput(session$ns("debrisGate"), "FSC/SSC Parameters", choices = NULL, multiple = TRUE),
          selectInput(session$ns("liveDeadGate"), "Live/Dead Marker", choices = NULL, selected = "None"),
          # Live/Dead threshold conditionally rendered
          uiOutput(session$ns("liveDeadThresholdUI"))
        )
      }
    })
    
    # Live/Dead threshold UI
    output$liveDeadThresholdUI <- renderUI({
      if (!is.null(input$liveDeadGate) && input$liveDeadGate != "None") {
        tagList(
          numericInput(session$ns("liveDeadThreshold"), "Live/Dead Threshold", value = 1000, min = 0, max = 10000),
          helpText("Cells with values below this threshold will be considered live.")
        )
      }
    })
    
    # ============================================================================
    # END OF SERVER-SIDE CONDITIONAL UI RENDERING
    # ============================================================================
    
    # Populate choices dynamically based on uploaded data
    observe({
      req(data_reactive())
      vars <- colnames(data_reactive())
      
      updateSelectizeInput(session, "debrisGate", choices = vars, selected = c("FSC-A", "SSC-A"))
      updateSelectInput(session, "liveDeadGate", choices = c("None", vars))
      updateSelectInput(session, "qcVariable", choices = vars)
      updateSelectizeInput(session, "markerSelection", choices = vars, selected = guessMarkers(vars))
    })
    
    # Reactive: processed data after QC and gating
    processed_data <- reactive({
      df <- data_reactive()
      req(df)
      
      # Quality Control
      if (input$performQC && input$qcVariable %in% names(df)) {
        qc_var <- df[[input$qcVariable]]
        df <- df[qc_var >= input$qcThreshold[1] & qc_var <= input$qcThreshold[2], ]
      }
      
      # Debris gating
      if (input$performGating && length(input$debrisGate) == 2 &&
          all(input$debrisGate %in% names(df))) {
        debris_x <- df[[input$debrisGate[1]]]
        debris_y <- df[[input$debrisGate[2]]]
        gate <- debris_x > quantile(debris_x, 0.05) & debris_y > quantile(debris_y, 0.05)
        df <- df[gate, ]
      }
      
      # Live/Dead gating
      if (input$performGating && input$liveDeadGate != "None" &&
          input$liveDeadGate %in% names(df)) {
        ld <- df[[input$liveDeadGate]]
        df <- df[ld >= input$liveDeadThreshold, ]
      }
      
      # Custom expression
      if (nzchar(input$customGate)) {
        try({
          df <- df[eval(parse(text = input$customGate), envir = df), ]
        }, silent = TRUE)
      }
      
      df
    })
    
    # Output: return reactive values for use in downstream modules
    return(list(
      data = processed_data,
      selected_markers = reactive(input$markerSelection)
    ))
  })
}

# Helper to guess markers 
guessMarkers <- function(vars) {
  grep("CD|HLA|CXCR|CCR|TCR", vars, value = TRUE)
}

