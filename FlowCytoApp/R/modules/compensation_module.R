# Advanced Spillover Compensation Module for Flow Cytometry Analysis

#' UI for the Advanced Spillover Compensation Module
#' @param id Module ID
#' @return UI elements for spillover compensation workflow
compensationModuleUI <- function(id) {
  ns <- NS(id)
  
  shinydashboard::dashboardPage(
    header = shinydashboard::dashboardHeader(disable = TRUE),
    sidebar = shinydashboard::dashboardSidebar(disable = TRUE),
    body = shinydashboard::dashboardBody(
      shinyjs::useShinyjs(),
      
      # Custom CSS for better styling
      tags$head(
        tags$style(HTML("
          .compensation-workflow {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 15px;
            border-radius: 8px;
            margin-bottom: 20px;
          }
          .step-indicator {
            display: flex;
            justify-content: space-between;
            margin-bottom: 20px;
          }
          .step {
            flex: 1;
            text-align: center;
            padding: 10px;
            background: rgba(255,255,255,0.1);
            margin: 0 5px;
            border-radius: 5px;
            transition: all 0.3s ease;
          }
          .step.active {
            background: rgba(255,255,255,0.3);
            transform: scale(1.05);
          }
          .step.completed {
            background: rgba(76, 175, 80, 0.8);
          }
          .matrix-editor .dataTables_wrapper {
            font-size: 12px;
          }
          .qc-metric {
            background: white;
            padding: 15px;
            border-radius: 5px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            margin-bottom: 10px;
          }
        "))
      ),
      
      # Workflow Progress Header
      div(class = "compensation-workflow",
          h3(icon("flask"), "Spillover Compensation Workflow"),
          p("Upload control files, compute spillover matrices, validate compensation quality, and export compensated data")
      ),
      
      # Main Tabset Panel
      tabsetPanel(
        id = ns("main_tabs"),
        type = "tabs",
        
        # Tab 1: Upload and Import
        tabPanel("📁 Upload & Import", 
                 value = "upload",
                 br(),
                 fluidRow(
                   # File Upload Section
                   column(6,
                          shinydashboard::box(
                            title = "FCS File Upload",
                            status = "primary", 
                            solidHeader = TRUE,
                            width = 12,
                            
                            fileInput(ns("fcs_files"), 
                                      "Upload FCS Files:",
                                      accept = c(".fcs"),
                                      multiple = TRUE,
                                      buttonLabel = "Browse FCS Files...",
                                      placeholder = "No files selected"),
                            
                                        helpText(icon("info-circle"), 
                     "Upload unstained control, single-stain controls, and experimental files."),
            
            div(class = "alert alert-light", style = "margin-top: 10px; font-size: 12px;",
                icon("check-circle"), 
                strong(" Supported formats: "), 
                "Standard (FL1-A, FL2-H), FJComp (FJComp-A, FJComp-H), and other naming conventions"),
                            
                            # File validation status
                            shinycssloaders::withSpinner(
                              verbatimTextOutput(ns("file_validation"))
                            ),
                            
                            # Quick file info
                            conditionalPanel(
                              condition = paste0("output['", ns("file_validation"), "'] != null"),
                              hr(),
                              h5("Uploaded Files Summary:"),
                              DT::dataTableOutput(ns("file_summary_table"))
                            )
                          )
                   ),
                   
                   # File Role Assignment
                   column(6,
                          shinydashboard::box(
                            title = "File Role Assignment", 
                            status = "warning", 
                            solidHeader = TRUE,
                            width = 12,
                            
                            # Use server-side rendering instead of conditionalPanel for better reliability
                            uiOutput(ns("file_assignment_ui"))
                          )
                   )
                 )
        ),
        
        # Tab 2: Compensation Matrix
        tabPanel("🧮 Compensation", 
                 value = "compensation",
                 br(),
                 fluidRow(
                   # Auto-computation section
                   column(4,
                          shinydashboard::box(
                            title = "Matrix Generation", 
                            status = "success", 
                            solidHeader = TRUE,
                            width = 12,
                            
                            # Auto-compute section
                            h5(icon("magic"), "Automatic Computation:"),
                            
                            actionButton(ns("compute_matrix"), 
                                         "Compute Spillover Matrix",
                                         class = "btn-primary btn-lg",
                                         icon = icon("calculator"),
                                         style = "width: 100%; margin-bottom: 15px;"),
                            
                            shinycssloaders::withSpinner(
                              verbatimTextOutput(ns("computation_status"))
                            ),
                            
                            hr(),
                            
                            # Manual upload section
                            h5(icon("upload"), "Import Existing Matrix:"),
                            
                            fileInput(ns("matrix_upload"), 
                                      "Upload CSV Matrix:",
                                      accept = c(".csv", ".xlsx")),
                            
                            # Matrix format example
                            conditionalPanel(
                              condition = paste0("input['", ns("matrix_upload"), "'] != null"),
                              div(class = "alert alert-info",
                                  icon("info-circle"), 
                                  "Matrix should have fluorophores as both row and column names.")
                            )
                          )
                   ),
                   
                   # Matrix viewer/editor
                   column(8,
                          shinydashboard::box(
                            title = "Matrix Viewer & Editor", 
                            status = "info", 
                            solidHeader = TRUE,
                            width = 12,
                            
                            conditionalPanel(
                              condition = paste0("output['", ns("computation_status"), "'] != null || input['", ns("matrix_upload"), "'] != null"),
                              
                              # Matrix editing controls
                              fluidRow(
                                column(6,
                                       checkboxInput(ns("enable_editing"), 
                                                     "Enable Matrix Editing", 
                                                     value = FALSE),
                                       # Editing status indicator
                                       conditionalPanel(
                                         condition = paste0("input['", ns("enable_editing"), "'] == true"),
                                         div(class = "alert alert-info", style = "margin-top: 5px; padding: 5px 8px; font-size: 12px;",
                                             icon("edit"), 
                                             " Double-click cells to edit matrix values")
                                       ),
                                       conditionalPanel(
                                         condition = paste0("input['", ns("enable_editing"), "'] == false"),
                                         div(class = "alert alert-secondary", style = "margin-top: 5px; padding: 5px 8px; font-size: 12px;",
                                             icon("lock"), 
                                             " Matrix editing disabled")
                                       )
                                ),
                                column(6,
                                       div(class = "btn-group", role = "group",
                                           actionButton(ns("reset_matrix"), 
                                                        "Reset to Original",
                                                        class = "btn-warning btn-sm",
                                                        icon = icon("undo")),
                                           actionButton(ns("save_edited_matrix"), 
                                                        "Save Edited Matrix",
                                                        class = "btn-success btn-sm",
                                                        icon = icon("floppy-disk"))
                                       )
                                )
                              ),
                              
                              # Matrix display
                              div(class = "matrix-editor",
                                  shinycssloaders::withSpinner(
                                    DT::dataTableOutput(ns("matrix_editor"))
                                  )
                              ),
                              
                              # Matrix validation
                              hr(),
                              h5(icon("check-circle"), "Matrix Validation:"),
                              verbatimTextOutput(ns("matrix_validation"))
                            ),
                            
                            conditionalPanel(
                              condition = paste0("output['", ns("computation_status"), "'] == null && input['", ns("matrix_upload"), "'] == null"),
                              div(class = "alert alert-warning",
                                  icon("exclamation-triangle"), 
                                  " Compute or upload a matrix to view/edit it here.")
                            )
                          )
                   )
                 )
        ),
        
        # Tab 3: Quality Control
        tabPanel("📊 Quality Control", 
                 value = "qc",
                 br(),
                 
                 conditionalPanel(
                   condition = paste0("output['", ns("matrix_validation"), "'] != null"),
                   
                   fluidRow(
                     # QC Controls
                     column(3,
                            shinydashboard::box(
                              title = "QC Options", 
                              status = "warning", 
                              solidHeader = TRUE,
                              width = 12,
                              
                              h5(icon("cog"), "Analysis Settings:"),
                              
                              shinyWidgets::pickerInput(
                                ns("qc_channels"),
                                "Channels to Analyze:",
                                choices = NULL,
                                multiple = TRUE,
                                options = list(
                                  `actions-box` = TRUE,
                                  `live-search` = TRUE,
                                  title = "Select channels..."
                                )
                              ),
                              
                              numericInput(ns("sample_size"), 
                                           "Sample Size for Plots:",
                                           value = 5000, 
                                           min = 1000, 
                                           max = 50000, 
                                           step = 1000),
                              
                              checkboxInput(ns("show_negative_events"), 
                                            "Highlight Negative Events", 
                                            value = TRUE),
                              
                              actionButton(ns("run_qc"), 
                                           "Run QC Analysis",
                                           class = "btn-success",
                                           icon = icon("play"),
                                           style = "width: 100%;")
                            ),
                            
                            # QC Metrics Summary
                            shinydashboard::box(
                              title = "QC Metrics", 
                              status = "info", 
                              solidHeader = TRUE,
                              width = 12,
                              
                              shinycssloaders::withSpinner(
                                DT::dataTableOutput(ns("qc_metrics_table"))
                              )
                            )
                     ),
                     
                     # QC Visualizations
                     column(9,
                            # Before/After Comparison Plots
                            shinydashboard::box(
                              title = "Before/After Compensation", 
                              status = "primary", 
                              solidHeader = TRUE,
                              width = 12,
                              
                              shinycssloaders::withSpinner(
                                plotOutput(ns("before_after_plots"), height = "400px")
                              )
                            ),
                            
                            # Pairwise Scatterplots
                            shinydashboard::box(
                              title = "Pairwise Channel Analysis", 
                              status = "success", 
                              solidHeader = TRUE,
                              width = 12,
                              
                              shinycssloaders::withSpinner(
                                plotOutput(ns("pairwise_plots"), height = "500px")
                              )
                            )
                     )
                   )
                 ),
                 
                 conditionalPanel(
                   condition = paste0("output['", ns("matrix_validation"), "'] == null"),
                   div(class = "alert alert-info",
                       icon("info-circle"), 
                       " Please complete matrix generation in the Compensation tab first.")
                 )
        ),
        
        # Tab 4: Export Results
        tabPanel("💾 Export", 
                 value = "export",
                 br(),
                 fluidRow(
                   # Export Options
                   column(6,
                          shinydashboard::box(
                            title = "Export Compensated Data", 
                            status = "success", 
                            solidHeader = TRUE,
                            width = 12,
                            
                            h5(icon("file"), "Compensated Experimental Files:"),
                            
                            div(class = "alert alert-info", style = "margin-bottom: 15px;",
                                icon("info-circle"), 
                                strong(" Export contents: "), 
                                "Only selected experimental files will be compensated and exported. ",
                                "Check the option below to also include compensated control files."),
                            
                            # Channel compatibility preview
                            conditionalPanel(
                              condition = paste0("output['", ns("matrix_validation"), "'] != null"),
                              h6(icon("search"), "Channel Compatibility Preview:"),
                              shinycssloaders::withSpinner(
                                verbatimTextOutput(ns("channel_compatibility_preview"))
                              ),
                              br()
                            ),
                            
                            # Status indicator for compensated files
                            conditionalPanel(
                              condition = paste0("output['", ns("matrix_validation"), "'] != null"),
                              div(id = ns("compensation_status"),
                                  shinycssloaders::withSpinner(
                                    verbatimTextOutput(ns("compensation_file_status"))
                                  )
                              )
                            ),
                            
                            checkboxInput(ns("add_suffix"), 
                                          "Add '_compensated' suffix to filenames", 
                                          value = TRUE),
                            
                            checkboxInput(ns("export_controls"), 
                                          "Also export compensated control files", 
                                          value = FALSE),
                            
                            conditionalPanel(
                              condition = paste0("output['", ns("compensation_file_status"), "'].indexOf('ready') > -1"),
                              downloadButton(ns("download_compensated_fcs"), 
                                             "Download Compensated Experimental Files",
                                             class = "btn-success btn-lg",
                                             icon = icon("download"),
                                             style = "width: 100%; margin-bottom: 15px;")
                            ),
                            
                            conditionalPanel(
                              condition = paste0("output['", ns("compensation_file_status"), "'].indexOf('ready') == -1 && output['", ns("matrix_validation"), "'] != null"),
                              div(class = "alert alert-info",
                                  icon("info-circle"), 
                                  " Compensated files will be generated automatically after matrix computation."),
                              br(),
                              actionButton(ns("generate_compensated"), 
                                          "Generate Compensated Experimental Files Now",
                                          class = "btn-info",
                                          icon = icon("cogs"),
                                          style = "width: 100%;")
                            ),
                            
                            hr(),
                            
                            h5(icon("table"), "Spillover Matrix:"),
                            
                            fluidRow(
                              column(6,
                                     downloadButton(ns("download_matrix_csv"), 
                                                    "Download as CSV",
                                                    class = "btn-info",
                                                    icon = icon("file-csv"))
                              ),
                              column(6,
                                     downloadButton(ns("download_matrix_xlsx"), 
                                                    "Download as Excel",
                                                    class = "btn-info",
                                                    icon = icon("file-excel"))
                              )
                            )
                          )
                   ),
                   
                   # Session Management
                   column(6,
                          shinydashboard::box(
                            title = "Session Management", 
                            status = "warning", 
                            solidHeader = TRUE,
                            width = 12,
                            
                            h5(icon("save"), "Save/Load Session:"),
                            
                            textInput(ns("session_name"), 
                                      "Session Name:",
                                      placeholder = "Enter session name..."),
                            
                            fluidRow(
                              column(6,
                                     downloadButton(ns("save_session"), 
                                                    "Save Session",
                                                    class = "btn-warning",
                                                    icon = icon("save"))
                              ),
                              column(6,
                                     fileInput(ns("load_session"), 
                                               "Load Session:",
                                               accept = c(".rds"))
                              )
                            ),
                            
                            hr(),
                            
                            h5(icon("clipboard-list"), "Session Summary:"),
                            verbatimTextOutput(ns("session_summary"))
                          )
                   )
                 )
        )
      )
    )
  )
}

# Enhanced fluorescence channel detection functions

#' Detect fluorescence channels using multiple pattern matching strategies
#' @param parameter_names Vector of channel names from FCS file
#' @param parameter_descriptions Vector of channel descriptions (optional)
#' @return Vector of detected fluorescence channel names
detectFluorescenceChannels <- function(parameter_names, parameter_descriptions = NULL) {
  
  # Remove any NA or empty parameter names
  valid_params <- parameter_names[!is.na(parameter_names) & parameter_names != ""]
  
  if (length(valid_params) == 0) {
    return(character(0))
  }
  
  # Strategy 1: Positive pattern matching for known fluorescence formats
  fluorescence_patterns <- c(
    # Classic flow cytometry formats
    "^FL[0-9]+-[AHW]$",                    # FL1-A, FL2-H, FL3-W
    "^FL[0-9]+$",                          # FL1, FL2, FL3
    
    # Laser wavelength + filter formats
    "^[0-9]{3}\\s*[A-Z]\\s*[0-9]{3}.*-[AHW]$",   # 405 D 525_50-A, 488 B 530/30-A
    "^[0-9]{3}[/_-][0-9]{2,3}.*-[AHW]$",         # 488/530-A, 405-525-A
    
    # FJComp and similar compensation formats
    "^[A-Za-z]*[Cc]omp.*-[AHW]$",               # FJComp-A, Comp-PE-A, BioComp-488-A
    "^[A-Za-z]*[Cc]omp-.*",                     # FJComp-405 D 525_50-A
    
    # Direct fluorophore names
    "^(FITC|PE|APC|PerCP|BV|AF|Alexa).*-[AHW]$",   # FITC-A, PE-H, BV421-A, AF647-W
    "^(Pacific|Brilliant|eFluor).*-[AHW]$",        # Pacific Blue-A, Brilliant Violet-H
    
    # Marker-fluorophore combinations
    "^CD[0-9]+.*-[AHW]$",                          # CD3-FITC-A, CD4-PE-H
    "^[A-Za-z0-9]+-(FITC|PE|APC|PerCP|BV|AF).*-[AHW]$",  # Marker-Fluor-A
    
    # Wavelength-based patterns
    "^[0-9]{3}.*nm.*-[AHW]$",                      # 488nm-A, 405nm Blue-H
    "^[BUV|BV|PE|APC][0-9]*.*-[AHW]$",            # BUV395-A, BV510-H, PE-Cy7-A
    
    # Generic patterns for Area/Height/Width measurements
    ".*[Bb]lue.*-[AHW]$",                          # Pacific Blue-A, etc.
    ".*[Rr]ed.*-[AHW]$",                           # Texas Red-A, etc.
    ".*[Gg]reen.*-[AHW]$",                         # FITC Green-A, etc.
    ".*[Vv]iolet.*-[AHW]$",                        # Brilliant Violet-A, etc.
    ".*[Yy]ellow.*-[AHW]$"                         # Yellow-A, etc.
  )
  
  # Apply positive pattern matching
  fluorescence_candidates <- character(0)
  
  for (pattern in fluorescence_patterns) {
    matches <- grep(pattern, valid_params, value = TRUE, ignore.case = TRUE)
    fluorescence_candidates <- c(fluorescence_candidates, matches)
  }
  
  # Remove duplicates
  fluorescence_candidates <- unique(fluorescence_candidates)
  
  # Strategy 2: Use descriptions if available to identify additional channels
  if (!is.null(parameter_descriptions) && length(parameter_descriptions) == length(parameter_names)) {
    
    # Look for fluorophore names in descriptions
    fluor_desc_patterns <- c(
      "FITC", "PE", "APC", "PerCP", "Cy[0-9]", "Pacific", "Brilliant", "Violet",
      "Blue", "Green", "Red", "Yellow", "Orange", "Alexa", "AF[0-9]", "BV[0-9]",
      "BUV[0-9]", "eFluor", "DyLight", "Texas Red", "Rhodamine"
    )
    
    for (pattern in fluor_desc_patterns) {
      desc_matches <- grep(pattern, parameter_descriptions, ignore.case = TRUE)
      if (length(desc_matches) > 0) {
        desc_channels <- parameter_names[desc_matches]
        desc_channels <- desc_channels[!is.na(desc_channels) & desc_channels != ""]
        fluorescence_candidates <- c(fluorescence_candidates, desc_channels)
      }
    }
    
    # Remove duplicates again
    fluorescence_candidates <- unique(fluorescence_candidates)
  }
  
  # Strategy 3: Negative filtering as fallback (exclude known non-fluorescence channels)
  non_fluorescence_patterns <- c(
    "^FSC", "^SSC",                            # Forward/Side scatter
    "^Time", "^time",                          # Time channel
    "Width$", "Height$", "Area$",              # Geometric parameters (if not fluorescence)
    "Event", "event",                          # Event numbering
    "Center", "Offset", "Residual",            # Acquisition parameters
    "Trigger", "trigger",                      # Trigger parameters
    "^[Pp]ulse",                              # Pulse parameters
    "Baseline", "baseline"                     # Baseline parameters
  )
  
  # If positive matching didn't find anything, use negative filtering
  if (length(fluorescence_candidates) == 0) {
    exclude_pattern <- paste(non_fluorescence_patterns, collapse = "|")
    fluorescence_candidates <- valid_params[!grepl(exclude_pattern, valid_params, ignore.case = TRUE)]
  } else {
    # Remove any non-fluorescence channels that might have been included
    exclude_pattern <- paste(non_fluorescence_patterns, collapse = "|")
    fluorescence_candidates <- fluorescence_candidates[!grepl(exclude_pattern, fluorescence_candidates, ignore.case = TRUE)]
  }
  
  # Strategy 4: Additional validation
  # Remove channels that are clearly technical parameters
  technical_keywords <- c("width", "height", "residual", "offset", "baseline", "trigger")
  
  # Only exclude if it's clearly a technical parameter (not part of a fluorophore name)
  for (keyword in technical_keywords) {
    pattern <- paste0("\\b", keyword, "\\b")  # Word boundary to avoid false positives
    fluorescence_candidates <- fluorescence_candidates[!grepl(pattern, fluorescence_candidates, ignore.case = TRUE)]
  }
  
  # Final cleanup - remove any remaining empty or NA values
  fluorescence_candidates <- fluorescence_candidates[!is.na(fluorescence_candidates) & fluorescence_candidates != ""]
  
  return(unique(fluorescence_candidates))
}

#' Extract base names from channel names using multiple strategies
#' @param channel_names Vector of channel names
#' @return Vector of base names
extractChannelBaseNames <- function(channel_names) {
  
  base_names <- sapply(channel_names, function(ch) {
    
    # Strategy 1: Standard flow cytometry format (FL1-A, FL2-H, etc.)
    if (grepl("^FL[0-9]+-[AHW]$", ch)) {
      return(gsub("-[AHW]$", "", ch))
    }
    
    # Strategy 2: Laser wavelength + filter format (405 D 525_50-A)
    if (grepl("^[0-9]{3}\\s*[A-Z]\\s*[0-9]{3}.*-[AHW]$", ch)) {
      return(gsub("-[AHW]$", "", ch))
    }
    
    # Strategy 3: FJComp format with complex names
    if (grepl("^[A-Za-z]*[Cc]omp.*-[AHW]$", ch)) {
      base <- gsub("-[AHW]$", "", ch)
      # Further clean FJComp prefix if it's generic
      if (grepl("^[A-Za-z]*[Cc]omp-", base)) {
        base <- gsub("^[A-Za-z]*[Cc]omp-", "", base)
      }
      return(base)
    }
    
    # Strategy 4: Fluorophore names (FITC-A, PE-H, BV421-A)
    if (grepl("^(FITC|PE|APC|PerCP|BV|AF|Alexa|Pacific|Brilliant|eFluor).*-[AHW]$", ch)) {
      return(gsub("-[AHW]$", "", ch))
    }
    
    # Strategy 5: Marker-fluorophore combinations (CD3-FITC-A)
    if (grepl("^[A-Za-z0-9]+-[A-Za-z0-9]+-[AHW]$", ch)) {
      return(gsub("-[AHW]$", "", ch))
    }
    
    # Strategy 6: Generic -A/-H/-W ending
    if (grepl("-[AHW]$", ch)) {
      return(gsub("-[AHW]$", "", ch))
    }
    
    # Strategy 7: Underscore or other separators
    if (grepl("_[AHW]$", ch)) {
      return(gsub("_[AHW]$", "", ch))
    }
    
    # Strategy 8: Space separated format
    if (grepl("\\s+[AHW]$", ch)) {
      return(gsub("\\s+[AHW]$", "", ch))
    }
    
    # Strategy 9: Numbers at the end (remove trailing numbers if they seem like indices)
    if (grepl("[0-9]+$", ch) && nchar(gsub("[0-9]+$", "", ch)) > 2) {
      return(gsub("[0-9]+$", "", ch))
    }
    
    # Default: return as-is if no pattern matches
    return(ch)
  })
  
  # Clean up base names
  base_names <- gsub("^\\s+|\\s+$", "", base_names)  # Trim whitespace
  base_names[base_names == ""] <- channel_names[base_names == ""]  # Restore original if empty
  
  return(base_names)
}

#' Enhanced function to group channels by fluorophore with better pattern recognition
#' @param fluor_channels Vector of fluorescence channel names
#' @param parameter_descriptions Vector of parameter descriptions
#' @param parameter_names Vector of all parameter names
#' @return Named list of channel groups
groupChannelsByFluorophore <- function(fluor_channels, parameter_descriptions = NULL, parameter_names = NULL) {
  
  if (length(fluor_channels) == 0) {
    return(list())
  }
  
  channel_groups <- list()
  
  # Create channel information data frame
  channel_info <- data.frame(
    channel = fluor_channels,
    description = if (!is.null(parameter_descriptions)) {
      parameter_descriptions[match(fluor_channels, parameter_names)]
    } else {
      rep("", length(fluor_channels))
    },
    stringsAsFactors = FALSE
  )
  
  # Clean up descriptions
  channel_info$description[is.na(channel_info$description)] <- ""
  channel_info$description[channel_info$description == channel_info$channel] <- ""
  
  # Strategy 1: Group by meaningful descriptions first
  if (any(channel_info$description != "")) {
    desc_groups <- split(channel_info$channel, channel_info$description)
    
    for (desc in names(desc_groups)) {
      if (desc != "" && length(desc_groups[[desc]]) > 0) {
        # Clean up description for group name
        clean_desc <- gsub("[^A-Za-z0-9 _-]", "", desc)
        clean_desc <- gsub("\\s+", " ", trimws(clean_desc))
        if (clean_desc != "") {
          channel_groups[[clean_desc]] <- desc_groups[[desc]]
        }
      }
    }
  }
  
  # Strategy 2: Group channels without descriptions by base name
  unassigned_channels <- channel_info$channel[channel_info$description == ""]
  
  if (length(unassigned_channels) > 0) {
    base_names <- extractChannelBaseNames(unassigned_channels)
    base_groups <- split(unassigned_channels, base_names)
    
    for (base in names(base_groups)) {
      if (base != "" && length(base_groups[[base]]) > 0) {
        # Avoid duplicate group names
        group_name <- base
        counter <- 1
        while (group_name %in% names(channel_groups)) {
          group_name <- paste0(base, "_", counter)
          counter <- counter + 1
        }
        channel_groups[[group_name]] <- base_groups[[base]]
      }
    }
  }
  
  # Strategy 3: Sort channels within each group by measurement type (Area > Height > Width)
  for (group_name in names(channel_groups)) {
    channels <- channel_groups[[group_name]]
    
    if (length(channels) > 1) {
      # Create sort priority: Area (1), Height (2), Width (3), Other (4)
      sort_priority <- sapply(channels, function(ch) {
        if (grepl("-A$|Area$", ch, ignore.case = TRUE)) return(1)
        if (grepl("-H$|Height$", ch, ignore.case = TRUE)) return(2)
        if (grepl("-W$|Width$", ch, ignore.case = TRUE)) return(3)
        return(4)
      })
      
      # Sort by priority, then alphabetically
      sorted_indices <- order(sort_priority, channels)
      channel_groups[[group_name]] <- channels[sorted_indices]
    }
  }
  
  return(channel_groups)
}

# Helper function to validate fluorescence channel assignments
validateChannelAssignments <- function(channel_assignments, available_channels) {
  
  validation_results <- list(
    valid = TRUE,
    errors = character(0),
    warnings = character(0)
  )
  
  # Check if assigned channels exist in data
  for (channel in names(channel_assignments)) {
    if (!(channel %in% available_channels)) {
      validation_results$errors <- c(
        validation_results$errors,
        paste("Channel", channel, "not found in data")
      )
      validation_results$valid <- FALSE
    }
  }
  
  # Check for duplicate file assignments
  assigned_files <- unlist(channel_assignments)
  assigned_files <- assigned_files[assigned_files != "" & !is.na(assigned_files)]
  
  if (length(assigned_files) != length(unique(assigned_files))) {
    duplicate_files <- names(table(assigned_files))[table(assigned_files) > 1]
    validation_results$warnings <- c(
      validation_results$warnings,
      paste("Files assigned to multiple channels:", paste(duplicate_files, collapse = ", "))
    )
  }
  
  return(validation_results)
}

#' Server function for the Advanced Spillover Compensation Module
#' @param id Module ID  
#' @param app_state Reactive values with global app state
#' @return List with module outputs
compensationModuleServer <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values for module state
    values <- reactiveValues(
      # File management
      uploaded_files = NULL,
      file_assignments = list(),      # Individual channel assignments (for spillover computation)
      group_assignments = list(),     # Fluorophore group assignments (for display)
      channel_groups = list(),        # Channel grouping information
      experimental_files = character(), # List of experimental files to be compensated
      validated_assignment = FALSE,
      
      # Matrix management
      original_matrix = NULL,
      current_matrix = NULL,
      matrix_source = NULL,  # "computed" or "uploaded"
      
      # Compensated data
      compensated_flowset = NULL,
      
      # QC results
      qc_results = NULL,
      
      # Workflow state
      workflow_step = 1,
      steps_completed = rep(FALSE, 5)
    )
    
    # Initialize workflow step tracking
    observe({
      # Update step indicators based on progress
      step_classes <- character(5)
      for (i in 1:5) {
        if (i < values$workflow_step) {
          step_classes[i] <- "completed"
        } else if (i == values$workflow_step) {
          step_classes[i] <- "active"
        } else {
          step_classes[i] <- ""
        }
      }
      
      # Update CSS classes (would need custom JS for full implementation)
      # For now, we'll track progress in the server logic
    })
    
    # File upload and validation ----
    
    observeEvent(input$fcs_files, {
      req(input$fcs_files)
      
      # Store file information
      values$uploaded_files <- input$fcs_files
      values$workflow_step <- 2
      
      # Update UI choices
      file_choices <- setNames(
        input$fcs_files$name,
        paste0(input$fcs_files$name, " (", 
               format(input$fcs_files$size, units = "MB", digits = 2), ")")
      )
      
      shinyWidgets::updatePickerInput(
        session, "unstained_file",
        choices = file_choices
      )
    })
    
    # Server-side rendering for file assignment UI
    output$file_assignment_ui <- renderUI({
      if (is.null(input$fcs_files)) {
        # Show message when no files are uploaded
        div(class = "alert alert-info",
            icon("upload"), " Please upload FCS files first.")
      } else {
        # Show file assignment interface when files are uploaded
        tagList(
          h5(icon("tag"), "Assign File Roles:"),
          
          # Unstained control selection
          shinyWidgets::pickerInput(
            ns("unstained_file"),
            "Unstained Control:",
            choices = setNames(
              input$fcs_files$name,
              paste0(input$fcs_files$name, " (", 
                     format(input$fcs_files$size, units = "MB", digits = 2), ")")
            ),
            options = list(
              `live-search` = TRUE,
              title = "Select unstained control file..."
            )
          ),
          
                                      # Single-stain control assignment
                            h5(icon("flask"), "Single-Stain Controls:"),
                            
                            # Dynamic UI for single-stain assignment
                            uiOutput(ns("single_stain_assignment_ui")),
                            
                            hr(),
                            
                            # Experimental files assignment
                            h5(icon("microscope"), "Experimental Files:"),
                            div(class = "alert alert-info", style = "margin-bottom: 10px;",
                                icon("info-circle"), 
                                " Select which files contain your experimental data to be compensated."),
                            
                            uiOutput(ns("experimental_files_ui")),
                            
                            br(),
                            actionButton(ns("validate_assignment"), 
                                         "Validate File Assignment",
                                         class = "btn-success",
                                         icon = icon("check-circle"))
        )
      }
    })
    
    # Render file validation output - ENHANCED VERSION
    output$file_validation <- renderText({
      req(input$fcs_files)
      
      tryCatch({
        # Quick validation of FCS files
        file_info <- data.frame(
          Name = input$fcs_files$name,
          Size = format(input$fcs_files$size, units = "MB", digits = 2),
          stringsAsFactors = FALSE
        )
        
        # Try to read first file to validate FCS format
        test_file <- read.FCS(input$fcs_files$datapath[1], transformation = FALSE)
        channels <- colnames(test_file)
        n_events <- nrow(test_file)
        
        # Get parameter descriptions for better information
        params <- parameters(test_file)
        parameter_names <- colnames(exprs(test_file))
        parameter_descriptions <- params$desc[match(parameter_names, params$name)]
        
        # Use enhanced fluorescence channel detection
        fluor_channels <- detectFluorescenceChannels(parameter_names, parameter_descriptions)
        
        # Show detection method used
        detection_method <- if (length(fluor_channels) > 0) {
          "Enhanced pattern recognition"
        } else {
          "No fluorescence channels detected"
        }
        
        # Show first few fluorescent channels for verification
        display_channels <- if (length(fluor_channels) > 0) {
          sample_channels <- fluor_channels[1:min(5, length(fluor_channels))]
          sample_with_desc <- sapply(sample_channels, function(ch) {
            desc <- parameter_descriptions[match(ch, parameter_names)]
            if (!is.na(desc) && desc != "" && desc != ch) {
              paste0(ch, " (", desc, ")")
            } else {
              ch
            }
          })
          if (length(fluor_channels) > 5) {
            paste(c(sample_with_desc, "..."), collapse = ", ")
          } else {
            paste(sample_with_desc, collapse = ", ")
          }
        } else {
          "None detected - check channel naming convention"
        }
        
        # Group information
        if (length(fluor_channels) > 0) {
          channel_groups <- groupChannelsByFluorophore(fluor_channels, parameter_descriptions, parameter_names)
          group_info <- if (length(channel_groups) > 0) {
            paste0("Organized into ", length(channel_groups), " fluorophore groups")
          } else {
            "Individual channels (no grouping detected)"
          }
        } else {
          group_info <- ""
        }
        
        paste0("✓ Successfully uploaded ", nrow(file_info), " FCS files\n",
               "✓ Files are valid FCS format\n",
               "✓ Example file: ", n_events, " events, ", length(channels), " total channels\n",
               "✓ Detection method: ", detection_method, "\n",
               "✓ Detected ", length(fluor_channels), " fluorescent channels\n",
               if (group_info != "") paste0("✓ ", group_info, "\n") else "",
               "✓ Sample channels: ", display_channels, "\n",
               "✓ Ready for file assignment")
        
      }, error = function(e) {
        paste("✗ Error validating files:", e$message)
      })
    })
    
    # File summary table with dynamic role assignment
    output$file_summary_table <- DT::renderDataTable({
      req(input$fcs_files)
      
      file_summary <- data.frame(
        Filename = input$fcs_files$name,
        `Size (MB)` = round(input$fcs_files$size / 1024^2, 2),
        `Assigned Role` = "Not assigned",
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      
      # Update assigned roles based on current assignments
      # Mark unstained control
      if (!is.null(input$unstained_file) && input$unstained_file != "") {
        unstained_idx <- which(file_summary$Filename == input$unstained_file)
        if (length(unstained_idx) > 0) {
          file_summary$`Assigned Role`[unstained_idx] <- "Unstained Control"
        }
      }
      
      # Mark single-stain controls from direct channel assignments
      if (!is.null(input$fcs_files)) {
        # Get fluorescent channels using enhanced detection
        tryCatch({
          test_file <- read.FCS(input$fcs_files$datapath[1], transformation = FALSE)
          params <- parameters(test_file)
          parameter_names <- colnames(exprs(test_file))
          parameter_descriptions <- params$desc[match(parameter_names, params$name)]
          fluor_channels <- detectFluorescenceChannels(parameter_names, parameter_descriptions)
          
          # Check assignments for each channel
          for (channel in fluor_channels) {
            control_file <- input[[paste0("control_", channel)]]
            if (!is.null(control_file) && control_file != "") {
              control_idx <- which(file_summary$Filename == control_file)
              if (length(control_idx) > 0) {
                current_role <- file_summary$`Assigned Role`[control_idx]
                if (current_role == "Not assigned") {
                  file_summary$`Assigned Role`[control_idx] <- paste("Single-stain:", channel)
                } else if (!grepl("Unstained", current_role)) {
                  # File is assigned to multiple channels
                  file_summary$`Assigned Role`[control_idx] <- paste(current_role, "+", channel)
                }
              }
            }
          }
        }, error = function(e) {
          # Silent error handling for file summary updates
        })
      }
      
      # Mark experimental files
      if (!is.null(values$experimental_files) && length(values$experimental_files) > 0) {
        experimental_indices <- which(file_summary$Filename %in% values$experimental_files)
        for (exp_idx in experimental_indices) {
          current_role <- file_summary$`Assigned Role`[exp_idx]
          if (current_role == "Not assigned") {
            file_summary$`Assigned Role`[exp_idx] <- "Experimental Data"
          } else {
            # File is assigned as both control and experimental (warning case)
            file_summary$`Assigned Role`[exp_idx] <- paste(current_role, "+ Experimental")
          }
        }
      }
      
      DT::datatable(
        file_summary,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          searching = FALSE,
          lengthChange = FALSE
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          "Assigned Role",
          backgroundColor = DT::styleEqual(
            c("Not assigned", "Unstained Control", "Experimental Data"), 
            c("#f8d7da", "#d1ecf1", "#d4edda")  # Light red, light blue, light green
          )
        )
    })
    
    # Dynamic single-stain assignment UI - ENHANCED VERSION
    output$single_stain_assignment_ui <- renderUI({
      req(input$fcs_files)
      
      # Get first file to extract available channels
      tryCatch({
        test_file <- read.FCS(input$fcs_files$datapath[1], transformation = FALSE)
        
        # Get parameter information
        params <- parameters(test_file)
        parameter_names <- colnames(exprs(test_file))
        parameter_descriptions <- params$desc[match(parameter_names, params$name)]
        
        # Use enhanced fluorescence channel detection
        fluor_channels <- detectFluorescenceChannels(parameter_names, parameter_descriptions)
        
        # Check if we found fluorescent channels
        if (length(fluor_channels) == 0) {
          return(div(class = "alert alert-warning",
                     icon("exclamation-triangle"), 
                     " No fluorescent channels detected using enhanced detection. Available channels: ",
                     paste(parameter_names, collapse = ", "),
                     br(), br(),
                     "This might indicate an unusual channel naming convention. Please check your FCS file format."))
        }
        
        # Group channels by fluorophore for better organization
        channel_groups <- groupChannelsByFluorophore(fluor_channels, parameter_descriptions, parameter_names)
        
        # Create file choices
        file_choices <- c("Not assigned" = "", setNames(input$fcs_files$name, input$fcs_files$name))
        
        # Create UI organized by channel groups or individual channels
        if (length(channel_groups) > 0 && any(lengths(channel_groups) > 1)) {
          # Show grouped interface
          group_ui <- lapply(names(channel_groups), function(group_name) {
            group_channels <- channel_groups[[group_name]]
            
            # Create a collapsible section for each group
            div(class = "panel panel-default", style = "margin-bottom: 10px;",
                div(class = "panel-heading", style = "background-color: #e8f4f8; padding: 8px;",
                    h6(style = "margin: 0; font-weight: bold; color: #2c3e50;",
                       icon("layer-group"), " ", group_name, 
                       " (", length(group_channels), " channels)")
                ),
                div(class = "panel-body", style = "padding: 10px;",
                    lapply(group_channels, function(channel) {
                      # Get channel description if available
                      channel_desc <- parameter_descriptions[match(channel, parameter_names)]
                      display_name <- if (!is.na(channel_desc) && channel_desc != "" && channel_desc != channel) {
                        paste0(channel, " (", channel_desc, ")")
                      } else {
                        channel
                      }
                      
                      div(style = "margin-bottom: 8px; padding: 6px; border: 1px solid #ddd; border-radius: 3px; background-color: #fafafa;",
                          fluidRow(
                            column(7, 
                                   div(style = "padding-top: 6px;",
                                       strong(style = "color: #34495e; font-size: 13px;", display_name)
                                   )
                            ),
                            column(5,
                                   selectInput(
                                     ns(paste0("control_", channel)),
                                     NULL,
                                     choices = file_choices,
                                     width = "100%"
                                   )
                            )
                          )
                      )
                    })
                )
            )
          })
          
          ui_content <- do.call(tagList, group_ui)
          
        } else {
          # Show simple individual channel interface
          channel_ui <- lapply(fluor_channels, function(channel) {
            # Get channel description if available
            channel_desc <- parameter_descriptions[match(channel, parameter_names)]
            display_name <- if (!is.na(channel_desc) && channel_desc != "" && channel_desc != channel) {
              paste0(channel, " (", channel_desc, ")")
            } else {
              channel
            }
            
            div(style = "margin-bottom: 10px; padding: 8px; border: 1px solid #ddd; border-radius: 4px; background-color: #f8f9fa;",
                fluidRow(
                  column(6, 
                         div(style = "padding-top: 8px;",
                             strong(style = "color: #2c3e50;", display_name)
                         )
                  ),
                  column(6,
                         selectInput(
                           ns(paste0("control_", channel)),
                           NULL,
                           choices = file_choices,
                           width = "100%"
                         )
                  )
                )
            )
          })
          
          ui_content <- do.call(tagList, channel_ui)
        }
        
        # Return the complete UI with enhanced instructions
        tagList(
          div(class = "alert alert-info", style = "margin-bottom: 15px;",
              icon("info-circle"), 
              strong(" Enhanced Channel Detection: "), 
              "Using advanced pattern recognition to identify fluorescence channels. ",
              "Assign one single-stain control file to each fluorescent channel below."),
          div(class = "alert alert-success", style = "margin-bottom: 15px;",
              icon("check-circle"), 
              strong("Detected ", length(fluor_channels), " fluorescent channels"), 
              if (length(channel_groups) > 0) {
                paste0(" grouped into ", length(channel_groups), " fluorophore groups")
              } else {
                ""
              }),
          ui_content
        )
        
      }, error = function(e) {
        div(class = "alert alert-danger",
            icon("exclamation-circle"), 
            strong("Error reading channel information: "), e$message,
            br(), br(),
            "This might indicate a problem with the FCS file format or an unsupported channel naming convention.")
      })
    })
    
    # Experimental files assignment UI
    output$experimental_files_ui <- renderUI({
      if (is.null(input$fcs_files)) {
        return(div(class = "alert alert-secondary",
                   icon("upload"), " Upload files first to select experimental data."))
      }
      
      # Create choices excluding control files that are already assigned
      all_files <- input$fcs_files$name
      
      # Get assigned control files
      assigned_controls <- character()
      if (!is.null(input$unstained_file) && input$unstained_file != "") {
        assigned_controls <- c(assigned_controls, input$unstained_file)
      }
      
      # Get single-stain control assignments
      tryCatch({
        test_file <- read.FCS(input$fcs_files$datapath[1], transformation = FALSE)
        params <- parameters(test_file)
        parameter_names <- colnames(exprs(test_file))
        parameter_descriptions <- params$desc[match(parameter_names, params$name)]
        fluor_channels <- detectFluorescenceChannels(parameter_names, parameter_descriptions)
        
        for (channel in fluor_channels) {
          control_file <- input[[paste0("control_", channel)]]
          if (!is.null(control_file) && control_file != "") {
            assigned_controls <- c(assigned_controls, control_file)
          }
        }
      }, error = function(e) {
        # Silent error handling
      })
      
      # Remove duplicates and get experimental file choices
      assigned_controls <- unique(assigned_controls)
      experimental_choices <- setdiff(all_files, assigned_controls)
      
      # Create the UI
      if (length(experimental_choices) == 0) {
        return(div(class = "alert alert-warning",
                   icon("exclamation-triangle"), 
                   " All uploaded files are assigned as controls. Upload additional experimental files or adjust control assignments."))
      }
      
      # Create file choices with size information
      exp_file_choices <- setNames(
        experimental_choices,
        paste0(experimental_choices, " (", 
               format(input$fcs_files$size[match(experimental_choices, input$fcs_files$name)], 
                      units = "MB", digits = 2), ")")
      )
      
      tagList(
        shinyWidgets::pickerInput(
          ns("experimental_files_selection"),
          "Select Experimental Files:",
          choices = exp_file_choices,
          multiple = TRUE,
          selected = if (length(values$experimental_files) > 0) {
            intersect(values$experimental_files, experimental_choices)
          } else {
            experimental_choices  # Default: select all non-control files
          },
          options = list(
            `actions-box` = TRUE,
            `live-search` = TRUE,
            `select-all-text` = "Select All Experimental",
            `deselect-all-text` = "Deselect All",
            title = "Choose experimental files to compensate..."
          )
        ),
        
        div(class = "alert alert-success", style = "margin-top: 10px;",
            icon("check-circle"), 
            strong("Available experimental files: "), length(experimental_choices),
            if (length(assigned_controls) > 0) {
              paste0(" (", length(assigned_controls), " control files excluded)")
            } else {
              ""
            }
        )
      )
    })
    
    # Update experimental files when selection changes
    observeEvent(input$experimental_files_selection, {
      if (!is.null(input$experimental_files_selection)) {
        values$experimental_files <- input$experimental_files_selection
        message("Experimental files updated: ", paste(values$experimental_files, collapse = ", "))
      }
    })
    
    # Validate file assignment - ENHANCED VERSION
    observeEvent(input$validate_assignment, {
      req(input$unstained_file)
      
      # Get fluorescent channels using enhanced detection
      test_file <- read.FCS(input$fcs_files$datapath[1], transformation = FALSE)
      params <- parameters(test_file)
      parameter_names <- colnames(exprs(test_file))
      parameter_descriptions <- params$desc[match(parameter_names, params$name)]
      
      # Use enhanced detection
      fluor_channels <- detectFluorescenceChannels(parameter_names, parameter_descriptions)
      
      if (length(fluor_channels) == 0) {
        showNotification("No fluorescent channels detected. Please check your FCS file format.", 
                         type = "error")
        return()
      }
      
      # Collect assignments directly from individual channel inputs
      assignments <- list()
      assignments$unstained <- input$unstained_file
      
      channel_assignments <- list()
      assigned_files <- c()
      
      for (channel in fluor_channels) {
        control_file <- input[[paste0("control_", channel)]]
        if (!is.null(control_file) && control_file != "") {
          channel_assignments[[channel]] <- control_file
          assignments[[channel]] <- control_file
          assigned_files <- c(assigned_files, control_file)
        }
      }
      
      # Enhanced validation
      validation_result <- validateChannelAssignments(channel_assignments, fluor_channels)
      
      if (!validation_result$valid) {
        error_msg <- paste("Validation errors:", paste(validation_result$errors, collapse = "; "))
        showNotification(error_msg, type = "error")
        return()
      }
      
      if (length(validation_result$warnings) > 0) {
        warning_msg <- paste("Validation warnings:", paste(validation_result$warnings, collapse = "; "))
        showNotification(warning_msg, type = "warning")
      }
      
      # Validation - need at least 2 assignments for meaningful spillover computation
      if (length(channel_assignments) < 2) {  
        showNotification("Please assign at least 2 single-stain control files for spillover computation", 
                         type = "error")
        return()
      }
      
      # Validation - need at least 1 experimental file
      if (is.null(values$experimental_files) || length(values$experimental_files) == 0) {
        showNotification("Please select at least 1 experimental file to be compensated", 
                         type = "error")
        return()
      }
      
      # Store assignments for spillover computation
      values$file_assignments <- assignments
      values$group_assignments <- channel_assignments
      values$channel_groups <- setNames(as.list(fluor_channels), fluor_channels)
      values$validated_assignment <- TRUE
      values$workflow_step <- 3
      
      # Create enhanced success message
      assigned_channels <- names(channel_assignments)
      total_channels <- length(channel_assignments)
      total_available <- length(fluor_channels)
      experimental_count <- length(values$experimental_files)
      
      success_msg <- paste0("✓ Successfully assigned ", total_channels, " of ", total_available, " detected fluorescent channels\n",
                           "✓ Control channels: ", paste(assigned_channels, collapse = ", "), "\n",
                           "✓ Experimental files: ", experimental_count, " selected\n",
                           "✓ Enhanced detection method used\n",
                           "✓ Ready to compute spillover matrix!")
      
      showNotification(success_msg, type = "message", duration = 6)
    })
    
    # Matrix computation and management ----
    
    # Compute spillover matrix - IMPROVED FOR INDIVIDUAL CHANNELS
    observeEvent(input$compute_matrix, {
      req(values$validated_assignment)
      
      withProgress(message = "Computing spillover matrix...", value = 0, {
        
        tryCatch({
          incProgress(0.2, detail = "Reading control files...")
          
          # Get only channels that have assigned control files
          assigned_channels <- setdiff(names(values$file_assignments), "unstained")
          
          # Remove any NA values that might cause logical errors
          assigned_channels <- assigned_channels[!is.na(assigned_channels)]
          assigned_channels <- assigned_channels[assigned_channels != ""]
          
          if (length(assigned_channels) < 2) {
            stop("At least 2 fluorescent channels with assigned controls are required")
          }
          
          # Read unstained control first to get available channels
          unstained_file <- values$file_assignments$unstained
          if (is.null(unstained_file) || is.na(unstained_file) || unstained_file == "") {
            stop("No unstained control file assigned")
          }
          
          unstained_idx <- which(input$fcs_files$name == unstained_file)
          if (length(unstained_idx) == 0) {
            stop(paste("Unstained control file not found:", unstained_file))
          }
          
          unstained_ff <- read.FCS(input$fcs_files$datapath[unstained_idx], transformation = FALSE)
          available_channels <- colnames(unstained_ff)
          
          # Remove any NA values from available channels
          available_channels <- available_channels[!is.na(available_channels)]
          
          # Filter assigned channels to only those actually present in the data
          valid_channels <- intersect(assigned_channels, available_channels)
          
          if (length(valid_channels) < 2) {
            # Try enhanced detection to provide better error message
            enhanced_channels <- detectFluorescenceChannels(available_channels)
            stop(paste("Only", length(valid_channels), "assigned channels found in data.",
                      "Available channels:", paste(available_channels, collapse = ", "),
                      "Enhanced detection found", length(enhanced_channels), "fluorescence channels:",
                      paste(enhanced_channels, collapse = ", ")))
          }
          
          incProgress(0.4, detail = "Reading single-stain controls...")
          
          # Read control files only for valid channels
          control_flowframes <- list()
          
          for (channel in valid_channels) {
            # Skip if channel is NA or empty
            if (is.na(channel) || channel == "") {
              warning("Skipping invalid channel (NA or empty)")
              next
            }
            
            control_file <- values$file_assignments[[channel]]
            if (is.null(control_file) || is.na(control_file) || control_file == "") {
              warning(paste("No control file assigned for channel", channel, "- skipping"))
              next
            }
            
            control_idx <- which(input$fcs_files$name == control_file)
            if (length(control_idx) == 0) {
              warning(paste("Control file", control_file, "not found for channel", channel, "- skipping"))
              next
            }
            
            control_ff <- read.FCS(input$fcs_files$datapath[control_idx], transformation = FALSE)
            
            # Verify the channel exists in the control file
            control_channels <- colnames(control_ff)
            if (is.null(control_channels) || !any(!is.na(control_channels))) {
              warning(paste("No valid channels found in control file", control_file, "- skipping"))
              next
            }
            
            if (!(channel %in% control_channels)) {
              warning(paste("Channel", channel, "not found in control file", control_file, "- skipping"))
              next
            }
            
            control_flowframes[[channel]] <- control_ff
          }
          
          # Add unstained control
          control_flowframes[["unstained"]] <- unstained_ff
          
          incProgress(0.6, detail = "Filtering channels and creating flowSet...")
          
          # Update valid channels list based on successful file reads
          final_channels <- setdiff(names(control_flowframes), "unstained")
          
          # Remove any NA values from final channels
          final_channels <- final_channels[!is.na(final_channels)]
          final_channels <- final_channels[final_channels != ""]
          
          if (length(final_channels) < 2) {
            stop(paste("Only", length(final_channels), "valid control files loaded. Need at least 2."))
          }
          
          # Keep scatter parameters and final channels only
          scatter_params <- available_channels[grepl("FSC|SSC|Time", available_channels, ignore.case = TRUE)]
          keep_params <- c(scatter_params, final_channels)
          
          # Remove any NA values from keep_params
          keep_params <- keep_params[!is.na(keep_params)]
          keep_params <- keep_params[keep_params != ""]
          
          # Filter all flowFrames to same parameter set
          filtered_frames <- list()
          for (name in names(control_flowframes)) {
            if (is.na(name) || name == "") {
              warning("Skipping invalid flowFrame name")
              next
            }
            
            ff <- control_flowframes[[name]]
            if (is.null(ff)) {
              warning(paste("Skipping null flowFrame for", name))
              next
            }
            
            ff_channels <- colnames(ff)
            if (is.null(ff_channels) || !any(!is.na(ff_channels))) {
              warning(paste("Skipping flowFrame with no valid channels for", name))
              next
            }
            
            common_params <- intersect(keep_params, ff_channels)
            common_params <- common_params[!is.na(common_params)]
            
            if (length(common_params) >= length(final_channels)) {  # Must have all fluorescent channels
              filtered_frames[[name]] <- ff[, common_params]
            } else {
              warning(paste("Skipping", name, "- missing required channels"))
            }
          }
          
          if (length(filtered_frames) < 3) {  # unstained + at least 2 stains
            stop("Not enough valid control files after filtering")
          }
          
          # Create flowSet
          control_set <- flowSet(filtered_frames)
          
          incProgress(0.8, detail = "Computing spillover matrix...")
          
          # Create pattern to match only final channels
          # Ensure final_channels are safe for regex
          safe_channels <- final_channels[!is.na(final_channels)]
          safe_channels <- safe_channels[safe_channels != ""]
          
          if (length(safe_channels) == 0) {
            stop("No valid channels remaining for pattern matching")
          }
          
          # Escape special regex characters in channel names
          escaped_channels <- gsub("([\\[\\]\\(\\)\\{\\}\\+\\*\\?\\^\\$\\|\\.])", "\\\\\\1", safe_channels)
          channel_pattern <- paste0("^(", paste(escaped_channels, collapse = "|"), ")$")
          
          # Compute spillover matrix
          spillover_matrix <- spillover(control_set, 
                                        unstained = "unstained",
                                        patt = channel_pattern,
                                        method = "median")
          
          incProgress(1.0, detail = "Finalizing...")
          
          # Store results
          values$original_matrix <- spillover_matrix
          values$current_matrix <- spillover_matrix
          values$matrix_source <- "computed"
          values$workflow_step <- 4
          
          # FIXED: Trigger initial matrix display
          matrix_render_trigger(matrix_render_trigger() + 1)
          
          success_msg <- paste0("✓ Spillover matrix computed successfully!\n",
                               "✓ Channels included: ", paste(safe_channels, collapse = ", "), "\n",
                               "✓ Matrix dimensions: ", nrow(spillover_matrix), "×", ncol(spillover_matrix))
          
          showNotification(success_msg, type = "message", duration = 6)
          
        }, error = function(e) {
          error_msg <- paste("Error computing spillover matrix:", e$message)
          showNotification(error_msg, type = "error", duration = 10)
          
          # Additional debugging information for troubleshooting
          cat("Debug info:\n")
          if (exists("assigned_channels") && !is.null(assigned_channels)) {
            cat("Assigned channels:", paste(assigned_channels[!is.na(assigned_channels)], collapse = ", "), "\n")
          }
          if (exists("valid_channels") && !is.null(valid_channels)) {
            cat("Valid channels:", paste(valid_channels[!is.na(valid_channels)], collapse = ", "), "\n")
          }
          if (exists("safe_channels") && !is.null(safe_channels)) {
            cat("Safe channels:", paste(safe_channels, collapse = ", "), "\n")
          }
        })
      })
    })
    
    # Handle pre-computed spillover matrix upload
    observeEvent(input$matrix_upload, {
      req(input$matrix_upload)
      
      tryCatch({
        # Read CSV or Excel file
        ext <- tools::file_ext(input$matrix_upload$name)
        if (ext == "csv") {
          spillover_matrix <- read.csv(input$matrix_upload$datapath, row.names = 1)
        } else if (ext %in% c("xlsx", "xls")) {
          spillover_matrix <- openxlsx::read.xlsx(input$matrix_upload$datapath, rowNames = TRUE)
        } else {
          stop("Unsupported file format. Please use CSV or Excel files.")
        }
        
        spillover_matrix <- as.matrix(spillover_matrix)
        
        # Basic validation of matrix structure
        if (nrow(spillover_matrix) != ncol(spillover_matrix)) {
          stop("Matrix must be square (same number of rows and columns)")
        }
        
        if (any(is.na(spillover_matrix))) {
          stop("Matrix contains missing values (NA)")
        }
        
        # Store the spillover matrix
        values$original_matrix <- spillover_matrix
        values$current_matrix <- spillover_matrix
        values$matrix_source <- "uploaded"
        values$workflow_step <- 4
        
        # FIXED: Trigger initial matrix display
        matrix_render_trigger(matrix_render_trigger() + 1)
        
        # Clear any existing compensated files since we have a new matrix
        values$compensated_flowset <- NULL
        
        showNotification(paste0("Spillover matrix uploaded successfully!\n",
                               "Dimensions: ", nrow(spillover_matrix), "×", ncol(spillover_matrix), "\n",
                               "Channels: ", paste(head(colnames(spillover_matrix), 3), collapse = ", "),
                               if (ncol(spillover_matrix) > 3) "..." else ""), 
                        type = "message", duration = 5)
        
      }, error = function(e) {
        showNotification(paste("Error loading spillover matrix:", e$message), type = "error")
      })
    })
    
    # Matrix computation status - SAFE VERSION
    output$computation_status <- renderText({
      tryCatch({
        if (!is.null(values$current_matrix)) {
          # Get fluorophore names safely
          fluorophores <- if (!is.null(values$group_assignments)) {
            names(values$group_assignments)
          } else if (!is.null(colnames(values$current_matrix))) {
            colnames(values$current_matrix)
          } else {
            c("Unknown")
          }
          
          # Remove NA values from fluorophores
          fluorophores <- fluorophores[!is.na(fluorophores)]
          fluorophores <- fluorophores[fluorophores != ""]
          
          if (length(fluorophores) == 0) {
            fluorophores <- c("No valid names")
          }
          
          # Get matrix dimensions safely
          matrix_rows <- if (!is.null(values$current_matrix)) nrow(values$current_matrix) else 0
          matrix_cols <- if (!is.null(values$current_matrix)) ncol(values$current_matrix) else 0
          
          # Get assignment count safely
          assignment_count <- if (!is.null(values$file_assignments)) {
            max(0, length(values$file_assignments) - 1)  # -1 for unstained
          } else {
            0
          }
          
          paste0("✓ Matrix computed successfully\n",
                 "✓ Dimensions: ", matrix_rows, "×", matrix_cols, "\n",
                 "✓ Fluorophores: ", paste(fluorophores, collapse = ", "), "\n",
                 "✓ Total channels compensated: ", assignment_count)
          
        } else if (!is.null(values$validated_assignment) && isTRUE(values$validated_assignment)) {
          # Get assigned groups safely
          assigned_groups <- if (!is.null(values$group_assignments)) {
            names(values$group_assignments)
          } else {
            c()
          }
          
          # Remove NA values from assigned groups
          assigned_groups <- assigned_groups[!is.na(assigned_groups)]
          assigned_groups <- assigned_groups[assigned_groups != ""]
          
          if (length(assigned_groups) == 0) {
            assigned_groups <- c("None")
          }
          
          paste0("Ready to compute matrix.\n",
                 "✓ Assigned fluorophores: ", paste(assigned_groups, collapse = ", "), "\n",
                 "Click 'Compute Spillover Matrix' above.")
        } else {
          "Please complete file assignment first."
        }
      }, error = function(e) {
        paste("Status error:", e$message)
      })
    })
    
    # Add a reactive value to control when to update the datatable
    matrix_render_trigger <- reactiveVal(0)
    
    # Matrix editor - FIXED: Prevent re-rendering during edits
    output$matrix_editor <- DT::renderDataTable({
      req(values$current_matrix)
      
      # ISOLATE the matrix data to prevent re-rendering during edits
      # Only re-render when explicitly triggered
      matrix_render_trigger()
      
      matrix_df <- isolate({
        as.data.frame(round(values$current_matrix, 4))
      })
      matrix_df <- cbind(Channel = rownames(matrix_df), matrix_df)
      
      # Create proper editable configuration
      editable_config <- if(isolate(input$enable_editing)) {
        list(
          target = "cell",
          disable = list(columns = c(0))  # Disable first column (Channel names)
        )
      } else {
        FALSE
      }
      
      DT::datatable(
        matrix_df,
        options = list(
          scrollX = TRUE,
          pageLength = 20,
          editable = editable_config,
          columnDefs = list(
            list(className = 'dt-center', targets = 0),
            list(className = 'dt-right', targets = 1:(ncol(matrix_df)-1))
          ),
          # Add additional options for better editing experience
          dom = 'Bfrtip',
          ordering = FALSE  # Disable sorting to prevent confusion during editing
        ),
        rownames = FALSE,
        caption = if(isolate(input$enable_editing)) {
          HTML("<strong>Matrix Editor:</strong> Double-click cells to edit values. Diagonal should be ~1.0, off-diagonal <0.5")
        } else {
          "Spillover Matrix (Enable editing checkbox above to modify values)"
        },
        editable = editable_config  # Also set at top level for compatibility
      ) %>%
        DT::formatRound(columns = 2:ncol(matrix_df), digits = 4) %>%
        DT::formatStyle(
          columns = 2:ncol(matrix_df),
          backgroundColor = DT::styleInterval(
            cuts = c(0.01, 0.05, 0.1, 0.2),
            values = c("#e8f5e8", "#fff3cd", "#ffeaa7", "#ff7675", "#d63031")
          )
        ) %>%
        DT::formatStyle(
          columns = 1,  # Channel column
          backgroundColor = "#f8f9fa",
          fontWeight = "bold"
        )
    })
    
    # Monitor editing state changes and trigger datatable updates when needed
    observeEvent(input$enable_editing, {
      message("Matrix editing mode changed to: ", input$enable_editing)
      # Force datatable re-render when editing mode changes
      matrix_render_trigger(matrix_render_trigger() + 1)
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    # Trigger datatable re-render when matrix is computed or uploaded
    observeEvent(values$matrix_source, {
      message("Matrix source changed to: ", values$matrix_source)
      # Force datatable re-render with new matrix
      matrix_render_trigger(matrix_render_trigger() + 1)
    }, ignoreNULL = TRUE, ignoreInit = FALSE)
    
    # Handle matrix edits - FIXED: Update matrix without causing datatable re-render
    observeEvent(input$matrix_editor_cell_edit, {
      message("Matrix edit event triggered!")
      req(input$enable_editing, values$current_matrix)
      
      tryCatch({
        info <- input$matrix_editor_cell_edit
        message("Edit info - Row: ", info$row, " Col: ", info$col, " Value: ", info$value)
        new_matrix <- values$current_matrix
        
        # DT uses 0-based column indexing, where column 0 is Channel, 1+ are matrix columns
        row_idx <- info$row + 1  # Convert from 0-based to 1-based
        col_idx <- info$col      # Column index in the datatable (0 = Channel, 1+ = matrix)
        new_value <- as.numeric(info$value)
        
        # Validate inputs
        if (is.na(new_value)) {
          showNotification("Invalid value - must be a number", type = "error", duration = 3)
          return()
        }
        
        if (col_idx == 0) {
          showNotification("Cannot edit channel names", type = "warning", duration = 2)
          return()
        }
        
        # Convert datatable column index to matrix column index
        matrix_col_idx <- col_idx  # col_idx 1 -> matrix column 1, etc.
        
        # Validate matrix bounds
        if (row_idx < 1 || row_idx > nrow(new_matrix) || 
            matrix_col_idx < 1 || matrix_col_idx > ncol(new_matrix)) {
          showNotification("Edit position out of matrix bounds", type = "error", duration = 3)
          return()
        }
        
        # Get channel names for validation feedback
        row_channel <- rownames(new_matrix)[row_idx]
        col_channel <- colnames(new_matrix)[matrix_col_idx]
        old_value <- new_matrix[row_idx, matrix_col_idx]
        
        # Validate edit makes sense
        if (new_value < 0) {
          showNotification("Warning: Negative spillover values are unusual", type = "warning", duration = 4)
        }
        
        if (row_idx == matrix_col_idx && abs(new_value - 1.0) > 0.2) {
          showNotification(paste("Warning: Diagonal element", row_channel, "should be close to 1.0"), 
                         type = "warning", duration = 5)
        }
        
        if (row_idx != matrix_col_idx && new_value > 0.5) {
          showNotification(paste("Warning: High spillover from", col_channel, "to", row_channel, "(>50%)"), 
                         type = "warning", duration = 5)
        }
        
        # FIXED: Apply edit using isolate to prevent reactive cascade
        isolate({
          new_matrix[row_idx, matrix_col_idx] <- new_value
          values$current_matrix <- new_matrix
        })
        
        # Clear compensated files since matrix changed
        values$compensated_flowset <- NULL
        
        # Show success feedback with details
        change_info <- paste0("Updated ", row_channel, " ← ", col_channel, 
                             ": ", round(old_value, 4), " → ", round(new_value, 4))
        showNotification(paste("Matrix updated:", change_info), type = "message", duration = 4)
        
        # Log the change for debugging
        message("Matrix edit: ", change_info)
        
      }, error = function(e) {
        showNotification(paste("Error editing matrix:", e$message), type = "error", duration = 5)
        message("Matrix edit error: ", e$message)
      })
    })
    
    # Reset matrix to original
    observeEvent(input$reset_matrix, {
      req(values$original_matrix)
      
      # Check if matrix has been modified
      if (!identical(values$current_matrix, values$original_matrix)) {
        values$current_matrix <- values$original_matrix
        
        # FIXED: Force datatable re-render to show reset values
        matrix_render_trigger(matrix_render_trigger() + 1)
        
        # Clear compensated files since matrix changed
        values$compensated_flowset <- NULL
        
        showNotification("Matrix reset to original values. Compensated files cleared.", 
                        type = "message", duration = 4)
        
        message("Matrix reset to original")
      } else {
        showNotification("Matrix is already at original values", type = "message", duration = 2)
      }
    })
    
    # Save edited matrix
    observeEvent(input$save_edited_matrix, {
      req(values$current_matrix)
      
      tryCatch({
        # Check if matrix has been modified
        if (is.null(values$original_matrix) || identical(values$current_matrix, values$original_matrix)) {
          showNotification("No changes to save - matrix is unchanged", type = "message", duration = 3)
          return()
        }
        
        # Save the current matrix as the new original
        values$original_matrix <- values$current_matrix
        
        # FIXED: Ensure visual consistency after save
        matrix_render_trigger(matrix_render_trigger() + 1)
        
        # Clear compensated files since matrix changed
        values$compensated_flowset <- NULL
        
        # Create a timestamp for the save
        timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        
        showNotification(paste("Matrix saved successfully at", timestamp, "- compensated files will be regenerated"), 
                        type = "message", duration = 5)
        
        # Log the save action
        message("Matrix saved as new original at ", timestamp)
        
        # Show validation of the saved matrix
        matrix_info <- paste0("Saved matrix: ", nrow(values$current_matrix), " channels, ",
                             "diagonal range: ", round(min(diag(values$current_matrix)), 3), "-",
                             round(max(diag(values$current_matrix)), 3),
                             ", max off-diagonal: ", round(max(values$current_matrix[row(values$current_matrix) != col(values$current_matrix)]), 3))
        
        message(matrix_info)
        
      }, error = function(e) {
        showNotification(paste("Error saving matrix:", e$message), type = "error", duration = 5)
        message("Matrix save error: ", e$message)
      })
    })
    
    # Matrix validation - SAFE LOGICAL OPERATIONS
    output$matrix_validation <- renderText({
      req(values$current_matrix)
      
      tryCatch({
        matrix <- values$current_matrix
        validation_msgs <- character()
        
        # Validate matrix structure
        if (is.null(matrix) || !is.matrix(matrix) || nrow(matrix) == 0 || ncol(matrix) == 0) {
          return("⚠ Invalid matrix structure")
        }
        
        # Check diagonal elements with NA handling
        diag_elements <- diag(matrix)
        if (is.null(diag_elements) || length(diag_elements) == 0) {
          validation_msgs <- c(validation_msgs, "⚠ Unable to extract diagonal elements")
        } else {
          # Remove NA values before checking
          valid_diag <- diag_elements[!is.na(diag_elements)]
          if (length(valid_diag) == 0) {
            validation_msgs <- c(validation_msgs, "⚠ All diagonal elements are NA")
          } else if (any(abs(valid_diag - 1) > 0.1, na.rm = TRUE)) {
            validation_msgs <- c(validation_msgs, "⚠ Some diagonal elements deviate significantly from 1.0")
          } else {
            validation_msgs <- c(validation_msgs, "✓ Diagonal elements are appropriate")
          }
        }
        
        # Check for negative values with NA handling
        if (is.null(matrix) || all(is.na(matrix))) {
          validation_msgs <- c(validation_msgs, "⚠ Matrix contains only NA values")
        } else {
          # Check for negative values, excluding NAs
          has_negative <- any(matrix < 0, na.rm = TRUE)
          if (is.na(has_negative)) {
            validation_msgs <- c(validation_msgs, "⚠ Unable to check for negative values")
          } else if (has_negative) {
            validation_msgs <- c(validation_msgs, "⚠ Matrix contains negative values")
          } else {
            validation_msgs <- c(validation_msgs, "✓ No negative spillover values")
          }
        }
        
        # Check matrix symmetry assumption with NA handling
        if (nrow(matrix) > 1 && ncol(matrix) > 1) {
          upper_tri_values <- matrix[upper.tri(matrix)]
          if (is.null(upper_tri_values) || length(upper_tri_values) == 0) {
            validation_msgs <- c(validation_msgs, "⚠ Unable to extract upper triangle values")
          } else {
            # Remove NA values before finding max
            valid_upper <- upper_tri_values[!is.na(upper_tri_values)]
            if (length(valid_upper) == 0) {
              validation_msgs <- c(validation_msgs, "⚠ All upper triangle values are NA")
            } else {
              off_diag_max <- max(valid_upper)
              if (is.na(off_diag_max)) {
                validation_msgs <- c(validation_msgs, "⚠ Unable to determine maximum spillover value")
              } else if (off_diag_max > 0.5) {
                validation_msgs <- c(validation_msgs, "⚠ High spillover values detected (>50%)")
              } else {
                validation_msgs <- c(validation_msgs, "✓ Spillover values are reasonable")
              }
            }
          }
        } else {
          validation_msgs <- c(validation_msgs, "⚠ Matrix too small for full validation")
        }
        
        # Return validation results
        if (length(validation_msgs) == 0) {
          return("⚠ No validation results generated")
        } else {
          return(paste(validation_msgs, collapse = "\n"))
        }
        
      }, error = function(e) {
        return(paste("⚠ Matrix validation error:", e$message))
      })
    })
    
    # QC Analysis ----
    
    # Update channel choices for QC
    observe({
      req(values$current_matrix)
      
      channels <- colnames(values$current_matrix)
      shinyWidgets::updatePickerInput(
        session, "qc_channels",
        choices = channels,
        selected = channels[1:min(4, length(channels))]
      )
    })
    
    # Run QC analysis
    observeEvent(input$run_qc, {
      req(values$current_matrix, input$qc_channels)
      
      withProgress(message = "Running QC analysis...", value = 0, {
        
        tryCatch({
          incProgress(0.3, detail = "Applying compensation...")
          
          # Apply compensation to control files
          qc_results <- performCompensationQC(
            file_assignments = values$file_assignments,
            file_paths = input$fcs_files$datapath,
            file_names = input$fcs_files$name,
            spillover_matrix = values$current_matrix,
            channels = input$qc_channels,
            sample_size = input$sample_size
          )
          
          incProgress(1.0, detail = "Finalizing QC results...")
          
          values$qc_results <- qc_results
          values$workflow_step <- 5
          
          showNotification("QC analysis completed!", type = "message")
          
        }, error = function(e) {
          showNotification(paste("QC analysis failed:", e$message), type = "error")
        })
      })
    })
    
    # QC Metrics Table
    output$qc_metrics_table <- DT::renderDataTable({
      req(values$qc_results)
      
      metrics <- values$qc_results$metrics
      
      DT::datatable(
        metrics,
        options = list(
          pageLength = 10,
          scrollX = TRUE
        ),
        rownames = FALSE
      ) %>%
        DT::formatRound(columns = c("Negative_Events_Pct", "Signal_Spread"), digits = 2) %>%
        DT::formatStyle(
          "Negative_Events_Pct",
          backgroundColor = DT::styleInterval(
            cuts = c(5, 10, 20),
            values = c("#e8f5e8", "#fff3cd", "#ffeaa7", "#ff7675")
          )
        )
    })
    
    # Before/After Plots
    output$before_after_plots <- renderPlot({
      req(values$qc_results)
      
      values$qc_results$before_after_plot
    })
    
    # Pairwise Plots
    output$pairwise_plots <- renderPlot({
      req(values$qc_results)
      
      values$qc_results$pairwise_plot
    })
    
    # Apply compensation to experimental files only - triggered when matrix is available
    observeEvent(values$current_matrix, {
      # This observer runs after matrix computation/upload to generate compensated files
      req(values$current_matrix, values$uploaded_files, values$experimental_files)
      
      # Only run if we don't already have compensated files
      if (!is.null(values$compensated_flowset)) {
        return()
      }
      
      tryCatch({
        # Apply compensation only to experimental files
        withProgress(message = "Generating compensated experimental files...", value = 0, {
          compensated_files <- list()
          comp_obj <- compensation(values$current_matrix)
          
          # Filter to only experimental files
          experimental_file_indices <- which(values$uploaded_files$name %in% values$experimental_files)
          
          if (length(experimental_file_indices) == 0) {
            showNotification("No experimental files selected for compensation", type = "warning")
            return()
          }
          
          # Track compatibility issues for detailed reporting
          compatibility_issues <- list()
          
          for (i in experimental_file_indices) {
            incProgress(1/length(experimental_file_indices), 
                        detail = paste("Processing experimental file:", values$uploaded_files$name[i]))
            
            # Read the FCS file
            ff <- read.FCS(values$uploaded_files$datapath[i], transformation = FALSE)
            
            # Enhanced channel compatibility checking with normalization
            file_channels <- colnames(ff)
            matrix_channels <- colnames(values$current_matrix)
            
            # Use enhanced channel matching
            channel_matches <- findChannelMatches(matrix_channels, file_channels)
            
            # Count successful matches
            successful_matches <- sum(sapply(channel_matches, function(m) m$found))
            high_confidence_matches <- sum(sapply(channel_matches, function(m) m$found && m$confidence >= 0.9))
            
            # Decide whether to proceed with compensation
            compensation_threshold <- 0.8  # Require 80% of channels to match
            
            if (successful_matches >= length(matrix_channels) * compensation_threshold) {
              tryCatch({
                # Create channel mapping for compensation
                channel_mapping <- character()
                for (matrix_ch in names(channel_matches)) {
                  match_info <- channel_matches[[matrix_ch]]
                  if (match_info$found) {
                    channel_mapping[matrix_ch] <- match_info$file_channel
                  }
                }
                
                # Apply compensation with channel mapping if needed
                if (all(matrix_channels %in% file_channels)) {
                  # Direct compensation - no mapping needed
                  ff_comp <- compensate(ff, comp_obj)
                  compensated_files[[values$uploaded_files$name[i]]] <- ff_comp
                  message("✓ Successfully compensated (exact): ", values$uploaded_files$name[i])
                } else if (length(channel_mapping) >= length(matrix_channels) * compensation_threshold) {
                  # Compensation with channel mapping
                  # Create a new compensation object with mapped channel names
                  mapped_matrix <- values$current_matrix
                  
                  # Rename columns in spillover matrix to match file channels
                  for (matrix_ch in names(channel_mapping)) {
                    file_ch <- channel_mapping[matrix_ch]
                    if (matrix_ch %in% colnames(mapped_matrix)) {
                      colnames(mapped_matrix)[colnames(mapped_matrix) == matrix_ch] <- file_ch
                    }
                    if (matrix_ch %in% rownames(mapped_matrix)) {
                      rownames(mapped_matrix)[rownames(mapped_matrix) == matrix_ch] <- file_ch
                    }
                  }
                  
                  # Apply compensation with mapped matrix
                  mapped_comp_obj <- compensation(mapped_matrix)
                  ff_comp <- compensate(ff, mapped_comp_obj)
                  compensated_files[[values$uploaded_files$name[i]]] <- ff_comp
                  
                  match_quality <- paste0(high_confidence_matches, "/", length(matrix_channels), " high confidence")
                  message("✓ Successfully compensated (mapped): ", values$uploaded_files$name[i], 
                         " - ", match_quality)
                } else {
                  stop("Insufficient channel matches for compensation")
                }
              }, error = function(e) {
                # Store detailed error info
                compatibility_issues[[values$uploaded_files$name[i]]] <- createCompatibilityIssue(
                  channel_matches, file_channels, matrix_channels, e$message
                )
                message("✗ Compensation failed for: ", values$uploaded_files$name[i], " - ", e$message)
              })
            } else {
              # Store compatibility issue details with enhanced matching info
              compatibility_issues[[values$uploaded_files$name[i]]] <- createCompatibilityIssue(
                channel_matches, file_channels, matrix_channels, "Insufficient channel matches"
              )
              
              message("✗ Channel compatibility failed: ", values$uploaded_files$name[i], 
                     " - ", successful_matches, "/", length(matrix_channels), " channels matched")
            }
          }
          
          # Show detailed compatibility report if there were issues
          if (length(compatibility_issues) > 0) {
            showChannelCompatibilityReport(compatibility_issues, values$uploaded_files$name)
          }
          
          # Store compensated flowset
          if (length(compensated_files) > 0) {
            values$compensated_flowset <- flowSet(compensated_files)
            message("Generated ", length(compensated_files), " compensated experimental files")
            showNotification(paste("Successfully compensated", length(compensated_files), 
                                  "experimental files"), type = "message")
          } else {
            values$compensated_flowset <- NULL
            showNotification("No experimental files could be compensated - check channel compatibility", 
                           type = "warning")
          }
        })
      }, error = function(e) {
        showNotification(paste("Error generating compensated files:", e$message), 
                        type = "error")
      })
    })
    
    # Manual compensation generation for experimental files
    observeEvent(input$generate_compensated, {
      req(values$current_matrix, values$uploaded_files, values$experimental_files)
      
      # Clear existing compensated files to force regeneration
      values$compensated_flowset <- NULL
      
      tryCatch({
        # Apply compensation only to experimental files
        withProgress(message = "Manually generating compensated experimental files...", value = 0, {
          compensated_files <- list()
          comp_obj <- compensation(values$current_matrix)
          
          # Filter to only experimental files
          experimental_file_indices <- which(values$uploaded_files$name %in% values$experimental_files)
          
          if (length(experimental_file_indices) == 0) {
            showNotification("No experimental files selected for compensation", type = "warning")
            return()
          }
          
          # Track compatibility issues for detailed reporting
          compatibility_issues <- list()
          
          for (i in experimental_file_indices) {
            incProgress(1/length(experimental_file_indices), 
                        detail = paste("Processing experimental file:", values$uploaded_files$name[i]))
            
            # Read the FCS file
            ff <- read.FCS(values$uploaded_files$datapath[i], transformation = FALSE)
            
            # Enhanced channel compatibility checking with normalization
            file_channels <- colnames(ff)
            matrix_channels <- colnames(values$current_matrix)
            
            # Use enhanced channel matching
            channel_matches <- findChannelMatches(matrix_channels, file_channels)
            
            # Count successful matches
            successful_matches <- sum(sapply(channel_matches, function(m) m$found))
            high_confidence_matches <- sum(sapply(channel_matches, function(m) m$found && m$confidence >= 0.9))
            
            # Decide whether to proceed with compensation
            compensation_threshold <- 0.8  # Require 80% of channels to match
            
            if (successful_matches >= length(matrix_channels) * compensation_threshold) {
              tryCatch({
                # Create channel mapping for compensation
                channel_mapping <- character()
                for (matrix_ch in names(channel_matches)) {
                  match_info <- channel_matches[[matrix_ch]]
                  if (match_info$found) {
                    channel_mapping[matrix_ch] <- match_info$file_channel
                  }
                }
                
                # Apply compensation with channel mapping if needed
                if (all(matrix_channels %in% file_channels)) {
                  # Direct compensation - no mapping needed
                  ff_comp <- compensate(ff, comp_obj)
                  compensated_files[[values$uploaded_files$name[i]]] <- ff_comp
                  message("✓ Successfully compensated (exact): ", values$uploaded_files$name[i])
                } else if (length(channel_mapping) >= length(matrix_channels) * compensation_threshold) {
                  # Compensation with channel mapping
                  # Create a new compensation object with mapped channel names
                  mapped_matrix <- values$current_matrix
                  
                  # Rename columns in spillover matrix to match file channels
                  for (matrix_ch in names(channel_mapping)) {
                    file_ch <- channel_mapping[matrix_ch]
                    if (matrix_ch %in% colnames(mapped_matrix)) {
                      colnames(mapped_matrix)[colnames(mapped_matrix) == matrix_ch] <- file_ch
                    }
                    if (matrix_ch %in% rownames(mapped_matrix)) {
                      rownames(mapped_matrix)[rownames(mapped_matrix) == matrix_ch] <- file_ch
                    }
                  }
                  
                  # Apply compensation with mapped matrix
                  mapped_comp_obj <- compensation(mapped_matrix)
                  ff_comp <- compensate(ff, mapped_comp_obj)
                  compensated_files[[values$uploaded_files$name[i]]] <- ff_comp
                  
                  match_quality <- paste0(high_confidence_matches, "/", length(matrix_channels), " high confidence")
                  message("✓ Successfully compensated (mapped): ", values$uploaded_files$name[i], 
                         " - ", match_quality)
                } else {
                  stop("Insufficient channel matches for compensation")
                }
              }, error = function(e) {
                # Store detailed error info
                compatibility_issues[[values$uploaded_files$name[i]]] <- createCompatibilityIssue(
                  channel_matches, file_channels, matrix_channels, e$message
                )
                message("✗ Compensation failed for: ", values$uploaded_files$name[i], " - ", e$message)
              })
            } else {
              # Store compatibility issue details with enhanced matching info
              compatibility_issues[[values$uploaded_files$name[i]]] <- createCompatibilityIssue(
                channel_matches, file_channels, matrix_channels, "Insufficient channel matches"
              )
              
              message("✗ Channel compatibility failed: ", values$uploaded_files$name[i], 
                     " - ", successful_matches, "/", length(matrix_channels), " channels matched")
            }
          }
          
          # Show detailed compatibility report if there were issues
          if (length(compatibility_issues) > 0) {
            showChannelCompatibilityReport(compatibility_issues, values$uploaded_files$name)
          }
          
          # Store compensated flowset
          if (length(compensated_files) > 0) {
            values$compensated_flowset <- flowSet(compensated_files)
            showNotification(paste("Successfully generated", length(compensated_files), 
                                  "compensated experimental files"), type = "message")
          } else {
            values$compensated_flowset <- NULL
            showNotification("No experimental files could be compensated - check channel compatibility", 
                           type = "warning")
          }
        })
      }, error = function(e) {
        showNotification(paste("Error generating compensated files:", e$message), 
                        type = "error")
      })
    })
    
    # Channel compatibility preview
    output$channel_compatibility_preview <- renderText({
      req(values$current_matrix, values$experimental_files)
      
      if (length(values$experimental_files) == 0) {
        return("No experimental files selected")
      }
      
      tryCatch({
        matrix_channels <- colnames(values$current_matrix)
        preview_lines <- character()
        
        # Check a sample of experimental files
        sample_files <- head(values$experimental_files, 3)
        
        for (exp_file in sample_files) {
          file_idx <- which(values$uploaded_files$name == exp_file)
          if (length(file_idx) > 0) {
            ff <- read.FCS(values$uploaded_files$datapath[file_idx], transformation = FALSE)
            file_channels <- colnames(ff)
            
            # Use enhanced channel matching
            channel_matches <- findChannelMatches(matrix_channels, file_channels)
            successful_matches <- sum(sapply(channel_matches, function(m) m$found))
            exact_matches <- sum(sapply(channel_matches, function(m) m$found && m$match_type == "exact_normalized"))
            
            match_percentage <- round((successful_matches / length(matrix_channels)) * 100)
            
            if (successful_matches == length(matrix_channels)) {
              if (exact_matches == length(matrix_channels)) {
                preview_lines <- c(preview_lines, paste0("✅ ", exp_file, " - Perfect match (100%)"))
              } else {
                preview_lines <- c(preview_lines, paste0("✅ ", exp_file, " - All channels mapped (", match_percentage, "%)"))
              }
            } else if (successful_matches >= length(matrix_channels) * 0.8) {
              preview_lines <- c(preview_lines, paste0("⚠️ ", exp_file, " - Partial match (", match_percentage, "%) - May work"))
            } else {
              preview_lines <- c(preview_lines, paste0("❌ ", exp_file, " - Poor match (", match_percentage, "%) - Will fail"))
            }
          }
        }
        
        if (length(values$experimental_files) > 3) {
          preview_lines <- c(preview_lines, paste0("... and ", length(values$experimental_files) - 3, " more files"))
        }
        
        preview_lines <- c(preview_lines, "", paste0("Matrix channels: ", paste(matrix_channels, collapse = ", ")))
        
        return(paste(preview_lines, collapse = "\n"))
        
      }, error = function(e) {
        return(paste("Error checking compatibility:", e$message))
      })
    })
    
    # Compensation file status
    output$compensation_file_status <- renderText({
      # Make this reactive to all relevant changes
      matrix_available <- !is.null(values$current_matrix)
      files_available <- !is.null(values$compensated_flowset)
      matrix_source <- values$matrix_source
      experimental_files_count <- length(values$experimental_files)
      
      # Check if matrix is available
      if (!matrix_available) {
        return("No spillover matrix available")
      }
      
      # Check if experimental files are selected
      if (experimental_files_count == 0) {
        return("⚠ No experimental files selected for compensation\nPlease select experimental files in the Upload & Import tab.")
      }
      
      # Check if compensated files are ready
      if (files_available) {
        n_files <- length(values$compensated_flowset)
        file_names <- sampleNames(values$compensated_flowset)
        
        status_text <- paste0("✓ Compensated experimental files ready for download\n",
                             "✓ Experimental files processed: ", n_files, " of ", experimental_files_count, "\n",
                             "✓ Sample files: ", paste(head(file_names, 3), collapse = ", "),
                             if (n_files > 3) "..." else "", "\n",
                             "✓ Matrix source: ", ifelse(matrix_source == "computed", "Computed", "Uploaded"), "\n",
                             "✓ Matrix applied: ", nrow(values$current_matrix), " channels\n",
                             "✓ Ready to export compensated experimental data")
        return(status_text)
      }
      
      # Check if matrix computation is in progress or matrix exists but no compensated files
      if (!is.null(values$current_matrix) && is.null(values$compensated_flowset)) {
        return(paste0("⏳ Generating compensated experimental files from spillover matrix...\n",
                     "Selected experimental files: ", experimental_files_count, "\n",
                     "This process runs automatically after matrix computation."))
      }
      
      # Default status
      return("⚠ Compute spillover matrix and select experimental files to generate compensated data")
    })
    
    # Export functionality ----
    
    # Download compensated FCS files
    output$download_compensated_fcs <- downloadHandler(
      filename = function() {
        paste0("compensated_fcs_files_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
      },
      content = function(file) {
        req(values$compensated_flowset)
        
        tryCatch({
          # Create temporary directory for FCS files
          temp_dir <- tempdir()
          fcs_dir <- file.path(temp_dir, "compensated_fcs")
          if (dir.exists(fcs_dir)) unlink(fcs_dir, recursive = TRUE)
          dir.create(fcs_dir, recursive = TRUE)
          
          # Get sample names
          sample_names <- sampleNames(values$compensated_flowset)
          files_written <- 0
          
          # Write each compensated FCS file
          for (sample_name in sample_names) {
            # Create safe filename
            safe_name <- gsub("[^A-Za-z0-9._-]", "_", sample_name)
            
            # Add suffix if requested
            if (input$add_suffix) {
              # Remove .fcs extension if present, add _compensated, then add .fcs back
              safe_name <- gsub("\\.fcs$", "", safe_name, ignore.case = TRUE)
              safe_name <- paste0(safe_name, "_compensated.fcs")
            } else if (!grepl("\\.fcs$", safe_name, ignore.case = TRUE)) {
              # Add .fcs extension if not present
              safe_name <- paste0(safe_name, ".fcs")
            }
            
            output_path <- file.path(fcs_dir, safe_name)
            
            # Write the compensated FCS file
            ff_comp <- values$compensated_flowset[[sample_name]]
            write.FCS(ff_comp, filename = output_path)
            files_written <- files_written + 1
          }
          
          # Include control files if requested
          if (input$export_controls && !is.null(values$file_assignments)) {
            # Apply compensation to control files as well
            for (assignment_name in names(values$file_assignments)) {
              control_file_name <- values$file_assignments[[assignment_name]]
              
              # Find the control file in uploaded files
              control_idx <- which(values$uploaded_files$name == control_file_name)
              if (length(control_idx) > 0) {
                # Read and compensate control file
                ff_control <- read.FCS(values$uploaded_files$datapath[control_idx], 
                                     transformation = FALSE)
                
                # Check if it has required channels
                if (all(colnames(values$current_matrix) %in% colnames(ff_control))) {
                  ff_control_comp <- compensate(ff_control, compensation(values$current_matrix))
                  
                  # Create safe filename for control
                  control_safe_name <- gsub("[^A-Za-z0-9._-]", "_", control_file_name)
                  control_safe_name <- gsub("\\.fcs$", "", control_safe_name, ignore.case = TRUE)
                  control_safe_name <- paste0(control_safe_name, "_compensated_", assignment_name, ".fcs")
                  
                  control_output_path <- file.path(fcs_dir, control_safe_name)
                  write.FCS(ff_control_comp, filename = control_output_path)
                  files_written <- files_written + 1
                }
              }
            }
          }
          
          # Create ZIP file
          if (files_written > 0) {
            fcs_files <- list.files(fcs_dir, pattern = "\\.fcs$", full.names = TRUE)
            zip(file, files = fcs_files, flags = "-r9X", extras = "-j")
            
            showNotification(paste("Successfully exported", files_written, "compensated FCS files"), 
                           type = "message")
          } else {
            # Create an empty file with error message
            writeLines("No compensated FCS files could be exported", file)
            showNotification("No compensated FCS files could be exported", type = "error")
          }
          
          # Clean up temporary directory
          unlink(fcs_dir, recursive = TRUE)
          
        }, error = function(e) {
          # Create error file
          writeLines(paste("Export error:", e$message), file)
          showNotification(paste("Error exporting compensated FCS files:", e$message), 
                          type = "error")
        })
      }
    )
    
    # Session summary
    output$session_summary <- renderText({
      summary_lines <- character()
      
      if (!is.null(values$uploaded_files)) {
        summary_lines <- c(summary_lines, 
                           paste("Files uploaded:", nrow(values$uploaded_files)))
      }
      
      if (values$validated_assignment) {
        summary_lines <- c(summary_lines, 
                           paste("Controls assigned:", length(values$file_assignments)))
      }
      
      if (!is.null(values$current_matrix)) {
        summary_lines <- c(summary_lines, 
                           paste("Matrix computed:", nrow(values$current_matrix), "channels"))
      }
      
      if (!is.null(values$compensated_flowset)) {
        summary_lines <- c(summary_lines, 
                           paste("Compensated files ready:", length(values$compensated_flowset), "files"))
      }
      
      if (!is.null(values$qc_results)) {
        summary_lines <- c(summary_lines, "QC analysis completed")
      }
      
      if (length(summary_lines) == 0) {
        summary_lines <- "No analysis completed yet"
      }
      
      paste(summary_lines, collapse = "\n")
    })
    
    # Download handlers
    output$download_matrix_csv <- downloadHandler(
      filename = function() {
        paste0("spillover_matrix_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        req(values$current_matrix)
        write.csv(values$current_matrix, file, row.names = TRUE)
      }
    )
    
    output$download_matrix_xlsx <- downloadHandler(
      filename = function() {
        paste0("spillover_matrix_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
      },
      content = function(file) {
        req(values$current_matrix)
        openxlsx::write.xlsx(as.data.frame(values$current_matrix), file, rowNames = TRUE)
      }
    )
    
    # Return reactive values for use by other modules
    return(list(
      spillover_matrix = reactive(values$current_matrix),
      compensated_data = reactive(values$compensated_flowset),
      workflow_complete = reactive(values$workflow_step >= 4)
    ))
  })
}

# Helper function for QC analysis
performCompensationQC <- function(file_assignments, file_paths, file_names, spillover_matrix, channels, sample_size = 5000) {
  
  results <- list()
  metrics_list <- list()
  before_plots <- list()
  after_plots <- list()
  
  for (channel in names(file_assignments)) {
    if (channel == "unstained") next
    
    # Read control file
    control_file <- file_assignments[[channel]]
    file_idx <- which(file_names == control_file)
    ff <- read.FCS(file_paths[file_idx], transformation = FALSE)
    
    # Sample data for plotting
    if (nrow(ff) > sample_size) {
      sample_idx <- sample(nrow(ff), sample_size)
      ff_sample <- ff[sample_idx, ]
    } else {
      ff_sample <- ff
    }
    
    # Apply compensation
    comp_obj <- compensation(spillover_matrix)
    ff_comp <- compensate(ff_sample, comp_obj)
    
    # Calculate metrics
    original_data <- exprs(ff_sample)[, channels]
    compensated_data <- exprs(ff_comp)[, channels]
    
    # Negative events percentage
    neg_pct_before <- apply(original_data, 2, function(x) sum(x < 0) / length(x) * 100)
    neg_pct_after <- apply(compensated_data, 2, function(x) sum(x < 0) / length(x) * 100)
    
    # Signal spread (coefficient of variation)
    cv_before <- apply(original_data, 2, function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE))
    cv_after <- apply(compensated_data, 2, function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE))
    
    # Store metrics
    for (ch in channels) {
      metrics_list[[paste(channel, ch, "before", sep = "_")]] <- data.frame(
        File = control_file,
        Target_Channel = channel,
        Measured_Channel = ch,
        Compensation = "Before",
        Negative_Events_Pct = neg_pct_before[ch],
        Signal_Spread = cv_before[ch]
      )
      
      metrics_list[[paste(channel, ch, "after", sep = "_")]] <- data.frame(
        File = control_file,
        Target_Channel = channel,
        Measured_Channel = ch,
        Compensation = "After",
        Negative_Events_Pct = neg_pct_after[ch],
        Signal_Spread = cv_after[ch]
      )
    }
  }
  
  # Combine metrics
  metrics_df <- do.call(rbind, metrics_list)
  
  # Create before/after comparison plot
  before_after_plot <- createBeforeAfterPlot(file_assignments, file_paths, file_names, spillover_matrix, channels[1:2])
  
  # Create pairwise plot
  pairwise_plot <- createPairwisePlot(file_assignments, file_paths, file_names, spillover_matrix, channels)
  
  return(list(
    metrics = metrics_df,
    before_after_plot = before_after_plot,
    pairwise_plot = pairwise_plot
  ))
}

# Helper function to create before/after plots
createBeforeAfterPlot <- function(file_assignments, file_paths, file_names, spillover_matrix, channels) {
  
  if (length(channels) < 2) return(ggplot() + labs(title = "Need at least 2 channels"))
  
  # Get first control file (non-unstained)
  control_channels <- setdiff(names(file_assignments), "unstained")
  if (length(control_channels) == 0) return(ggplot() + labs(title = "No control files"))
  
  control_file <- file_assignments[[control_channels[1]]]
  file_idx <- which(file_names == control_file)
  ff <- read.FCS(file_paths[file_idx], transformation = FALSE)
  
  # Sample data
  if (nrow(ff) > 5000) {
    ff <- ff[sample(nrow(ff), 5000), ]
  }
  
  # Apply compensation
  comp_obj <- compensation(spillover_matrix)
  ff_comp <- compensate(ff, comp_obj)
  
  # Extract data
  before_data <- data.frame(
    x = exprs(ff)[, channels[1]],
    y = exprs(ff)[, channels[2]],
    Type = "Before"
  )
  
  after_data <- data.frame(
    x = exprs(ff_comp)[, channels[1]],
    y = exprs(ff_comp)[, channels[2]],
    Type = "After"
  )
  
  plot_data <- rbind(before_data, after_data)
  
  ggplot(plot_data, aes(x = x, y = y, color = after_stat(ndensity))) +
    geom_pointdensity(alpha = 0.6, size = 0.5) +
    facet_wrap(~Type, scales = "free") +
    scale_color_viridis_c() +
    labs(
      title = paste("Before/After Compensation:", control_channels[1], "Control"),
      x = channels[1],
      y = channels[2],
      color = "Density"
    ) +
    theme_minimal()
}

# Helper function to create pairwise plots
createPairwisePlot <- function(file_assignments, file_paths, file_names, spillover_matrix, channels) {
  
  if (length(channels) < 2) return(ggplot() + labs(title = "Need at least 2 channels"))
  
  # Use first few channels for pairwise analysis
  n_channels <- min(4, length(channels))
  selected_channels <- channels[1:n_channels]
  
  # Get unstained control
  unstained_file <- file_assignments$unstained
  file_idx <- which(file_names == unstained_file)
  ff <- read.FCS(file_paths[file_idx], transformation = FALSE)
  
  # Sample data
  if (nrow(ff) > 3000) {
    ff <- ff[sample(nrow(ff), 3000), ]
  }
  
  # Apply compensation
  comp_obj <- compensation(spillover_matrix)
  ff_comp <- compensate(ff, comp_obj)
  
  # Create pairwise plots
  plot_list <- list()
  
  for (i in 1:(n_channels-1)) {
    for (j in (i+1):n_channels) {
      plot_data <- data.frame(
        x = exprs(ff_comp)[, selected_channels[i]],
        y = exprs(ff_comp)[, selected_channels[j]]
      )
      
      p <- ggplot(plot_data, aes(x = x, y = y, color = after_stat(ndensity))) +
        geom_pointdensity(alpha = 0.6, size = 0.3) +
        scale_color_viridis_c() +
        labs(
          x = selected_channels[i],
          y = selected_channels[j]
        ) +
        theme_minimal() +
        theme(legend.position = "none")
      
      plot_list[[paste(i, j, sep = "_")]] <- p
    }
  }
  
  # Arrange plots
  if (length(plot_list) > 0) {
    gridExtra::grid.arrange(grobs = plot_list, ncol = 2)
  } else {
    ggplot() + labs(title = "No pairwise combinations available")
  }
}

# Enhanced channel normalization for flow cytometry naming conventions
normalizeChannelName <- function(channel_name) {
  if (is.na(channel_name) || channel_name == "") return("")
  
  # Convert to character and clean
  ch <- as.character(channel_name)
  
  # Remove common prefixes (case-insensitive)
  common_prefixes <- c(
    "FJComp-", "fjcomp-", "FJCOMP-",           # FlowJo compensation
    "Comp-", "comp-", "COMP-",                 # Generic compensation
    "BioComp-", "biocomp-", "BIOCOMP-",       # Bio-Rad compensation  
    "CytoComp-", "cytocomp-", "CYTOCOMP-",    # Cytometer-specific
    "SpillComp-", "spillcomp-", "SPILLCOMP-", # Spillover compensation
    "FC-", "fc-", "FC_", "fc_",               # Flow cytometry prefixes
    "Ch-", "ch-", "CH-", "Channel-", "channel-", "CHANNEL-"  # Channel prefixes
  )
  
  for (prefix in common_prefixes) {
    if (startsWith(ch, prefix)) {
      ch <- substring(ch, nchar(prefix) + 1)
      break
    }
  }
  
  # Normalize separators and spacing
  ch <- gsub("\\s+", " ", ch)        # Multiple spaces to single space
  ch <- gsub("_+", "_", ch)          # Multiple underscores to single
  ch <- gsub("-+", "-", ch)          # Multiple hyphens to single
  ch <- trimws(ch)                   # Remove leading/trailing whitespace
  
  # Normalize wavelength formats: 450_50, 450/50, 450-50 → 450_50
  ch <- gsub("([0-9]{3,4})[/-]([0-9]{2,3})", "\\1_\\2", ch)
  
  # Normalize measurement suffixes: ensure consistent format
  # Convert various suffix formats to standard -A, -H, -W
  ch <- gsub("[-_\\s]*(Area|area|AREA)\\s*$", "-A", ch)
  ch <- gsub("[-_\\s]*(Height|height|HEIGHT)\\s*$", "-H", ch)  
  ch <- gsub("[-_\\s]*(Width|width|WIDTH)\\s*$", "-W", ch)
  
  # Standardize common measurement suffixes
  ch <- gsub("[-_\\s]*A\\s*$", "-A", ch)
  ch <- gsub("[-_\\s]*H\\s*$", "-H", ch)
  ch <- gsub("[-_\\s]*W\\s*$", "-W", ch)
  
  # Normalize fluorophore names (case-insensitive)
  fluor_mappings <- list(
    c("fitc", "FITC"),
    c("pe", "PE"), 
    c("apc", "APC"),
    c("percp", "PerCP", "PERCP"),
    c("cy5", "Cy5", "CY5"),
    c("cy7", "Cy7", "CY7"),
    c("texas\\s*red", "Texas Red", "TEXAS RED", "TexasRed"),
    c("pacific\\s*blue", "Pacific Blue", "PACIFIC BLUE", "PacificBlue"),
    c("brilliant\\s*violet", "Brilliant Violet", "BRILLIANT VIOLET", "BrilliantViolet")
  )
  
  for (mapping in fluor_mappings) {
    standard_name <- mapping[1]
    for (variant in mapping[-1]) {
      ch <- gsub(paste0("\\b", variant, "\\b"), standard_name, ch, ignore.case = TRUE)
    }
  }
  
  return(ch)
}

# Enhanced channel matching with normalization
findChannelMatches <- function(matrix_channels, file_channels) {
  
  # Normalize all channel names
  norm_matrix <- sapply(matrix_channels, normalizeChannelName)
  norm_file <- sapply(file_channels, normalizeChannelName)
  
  # Create mapping results
  matches <- list()
  
  for (i in seq_along(matrix_channels)) {
    matrix_ch <- matrix_channels[i]
    norm_matrix_ch <- norm_matrix[i]
    
    # First try exact match on normalized names
    exact_match_idx <- which(norm_file == norm_matrix_ch)
    
    if (length(exact_match_idx) > 0) {
      matches[[matrix_ch]] <- list(
        found = TRUE,
        file_channel = file_channels[exact_match_idx[1]],
        match_type = "exact_normalized",
        confidence = 1.0
      )
    } else {
      # Try fuzzy matching on normalized names
      fuzzy_scores <- sapply(norm_file, function(fc) {
        # Calculate similarity score
        if (nchar(norm_matrix_ch) == 0 || nchar(fc) == 0) return(0)
        
        # Exact substring match gets high score
        if (grepl(norm_matrix_ch, fc, fixed = TRUE) || grepl(fc, norm_matrix_ch, fixed = TRUE)) {
          return(0.9)
        }
        
        # String distance-based similarity
        max_len <- max(nchar(norm_matrix_ch), nchar(fc))
        if (max_len == 0) return(0)
        
        distance <- adist(norm_matrix_ch, fc, ignore.case = TRUE)[1]
        similarity <- 1 - (distance / max_len)
        
        return(similarity)
      })
      
      best_match_idx <- which.max(fuzzy_scores)
      best_score <- fuzzy_scores[best_match_idx]
      
      if (best_score > 0.7) {  # Threshold for fuzzy matching
        matches[[matrix_ch]] <- list(
          found = TRUE,
          file_channel = file_channels[best_match_idx],
          match_type = "fuzzy_normalized", 
          confidence = best_score
        )
      } else {
        matches[[matrix_ch]] <- list(
          found = FALSE,
          file_channel = NA,
          match_type = "no_match",
          confidence = 0.0
        )
      }
    }
  }
  
  return(matches)
}

# Helper function to create compatibility issue details
createCompatibilityIssue <- function(channel_matches, file_channels, matrix_channels, error_message = "") {
  
  # Extract match information
  matched_channels <- character()
  fuzzy_matches <- character()
  unmatched_channels <- character()
  
  for (matrix_ch in names(channel_matches)) {
    match_info <- channel_matches[[matrix_ch]]
    
    if (match_info$found) {
      if (match_info$match_type == "exact_normalized") {
        matched_channels <- c(matched_channels, paste0(matrix_ch, " ✓ ", match_info$file_channel))
      } else {
        confidence_pct <- round(match_info$confidence * 100)
        fuzzy_matches <- c(fuzzy_matches, paste0(matrix_ch, " → ", match_info$file_channel, " (", confidence_pct, "%)"))
      }
    } else {
      unmatched_channels <- c(unmatched_channels, matrix_ch)
    }
  }
  
  return(list(
    missing = unmatched_channels,
    file_channels = file_channels,
    matrix_channels = matrix_channels,
    matched_channels = matched_channels,
    fuzzy_matches = fuzzy_matches,
    unmatched = unmatched_channels,
    error_message = error_message
  ))
}

# Helper function to show detailed channel compatibility report
showChannelCompatibilityReport <- function(compatibility_issues, all_file_names) {
  
  report_lines <- character()
  
  # Header
  report_lines <- c(report_lines, 
                   "🔍 CHANNEL COMPATIBILITY ANALYSIS",
                   "=====================================")
  
  # Summary
  n_issues <- length(compatibility_issues)
  n_total <- length(all_file_names)
  n_success <- n_total - n_issues
  
  report_lines <- c(report_lines,
                   paste0("📊 SUMMARY: ", n_success, " files compensated successfully, ", n_issues, " files failed"),
                   "")
  
  # Detailed breakdown for each problematic file
  for (file_name in names(compatibility_issues)) {
    issue <- compatibility_issues[[file_name]]
    
    report_lines <- c(report_lines,
                     paste0("❌ FILE: ", file_name))
    
    # Show error message if any
    if (!is.null(issue$error_message) && issue$error_message != "") {
      report_lines <- c(report_lines,
                       paste0("   Error: ", issue$error_message))
    }
    
    # Show successful matches
    if (!is.null(issue$matched_channels) && length(issue$matched_channels) > 0) {
      report_lines <- c(report_lines,
                       "   ✅ Successful matches:")
      for (match in issue$matched_channels) {
        report_lines <- c(report_lines, paste0("      ", match))
      }
    }
    
    # Show fuzzy matches if any
    if (length(issue$fuzzy_matches) > 0) {
      report_lines <- c(report_lines,
                       "   🔍 Fuzzy matches found:")
      for (match in issue$fuzzy_matches) {
        report_lines <- c(report_lines, paste0("      ", match))
      }
    }
    
    # Show completely unmatched channels
    if (length(issue$unmatched) > 0) {
      report_lines <- c(report_lines,
                       paste0("   ⚠️ No matches found for: ", paste(issue$unmatched, collapse = ", ")))
    }
    
    # Show first few channels in the file for reference
    n_show <- min(8, length(issue$file_channels))
    sample_channels <- paste(issue$file_channels[1:n_show], collapse = ", ")
    if (length(issue$file_channels) > n_show) {
      sample_channels <- paste0(sample_channels, "...")
    }
    
    report_lines <- c(report_lines,
                     paste0("   📋 File channels: ", sample_channels),
                     "")
  }
  
  # Show matrix channels for reference
  if (n_issues > 0) {
    first_issue <- compatibility_issues[[1]]
    report_lines <- c(report_lines,
                     "🎯 REQUIRED CHANNELS (from spillover matrix):",
                     paste0("   ", paste(first_issue$matrix_channels, collapse = ", ")),
                     "")
  }
  
  # Recommendations
  report_lines <- c(report_lines,
                   "💡 RECOMMENDATIONS:",
                   "   1. ✅ Auto-mapping activated: FJComp-, Comp- and other common prefixes are handled",
                   "   2. ✅ Fuzzy matching enabled: Similar channel names are automatically detected",
                   "   3. 🔧 For remaining issues: Check if experimental files use the same acquisition panel",
                   "   4. 🔄 Alternative: Recompute spillover matrix using files with matching channel names",
                   "   5. ⚙️ Threshold: 80% of channels must match for automatic compensation",
                   "")
  
  # Show the report as a notification
  report_text <- paste(report_lines, collapse = "\n")
  
  # Split into chunks for multiple notifications (due to character limits)
  max_chars <- 2000
  if (nchar(report_text) > max_chars) {
    # Show summary notification first
    summary_text <- paste(report_lines[1:6], collapse = "\n")
    showNotification(summary_text, type = "error", duration = 15)
    
    # Show detailed issues in console/log
    cat("\n", report_text, "\n")
    message("Channel compatibility report written to console - check R console for full details")
  } else {
    showNotification(report_text, type = "error", duration = 15)
  }
  
  # Also log to console for easy copying
  cat("\n=== CHANNEL COMPATIBILITY REPORT ===\n")
  cat(report_text)
  cat("\n===================================\n\n")
}