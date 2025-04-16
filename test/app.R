library(shiny)
library(flowCore)
library(ggplot2)
library(Rtsne)
library(uwot)
library(data.table)
library(plotly)
library(shinycssloaders)
library(dplyr)
library(tidyr)
library(DT)
library(readr)
library(openxlsx)
library(shinythemes)
library(shinyWidgets)
library(cluster)
# libraries for clustering
library(dbscan)
library(FlowSOM)
library(Rphenograph)
library(igraph)
# install.packages("devtools")
# devtools::install_github("JinmiaoChenLab/Rphenograph")
# install.packages("FlowSOM")


options(shiny.maxRequestSize = 250*1024^2)

asinhTransform <- function(x, cofactor = 5) {
  asinh(x / cofactor)
}

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Flow Cytometry Analysis Tool"),
  
  tabsetPanel(
    id = "main_tabs",
    tabPanel("Raw Data Analysis", 
             sidebarLayout(
               sidebarPanel(
                 fileInput("fcsFile", "Upload FCS/CSV/TSV File", accept = c(".fcs", ".csv", ".tsv")),
                 uiOutput("markerSelectUI"),
                 checkboxInput("transform", "Apply arcsinh transformation", value = TRUE),
                 numericInput("cofactor", "Transformation cofactor", value = 5, min = 1, max = 10),
                 numericInput("nEvents", "Number of events to sample", value = 5000, min = 100, step = 1000),
                 sliderInput("perplexity", "t-SNE perplexity", min = 5, max = 50, value = 30),
                 sliderInput("n_neighbors", "UMAP n_neighbors", min = 2, max = 100, value = 15),
                 checkboxGroupInput("methods", "Select Dimensionality Reduction Methods", choices = c("t-SNE", "UMAP"), selected = c("t-SNE", "UMAP")),
                 selectInput("colorBy", "Color points by:", choices = c("None")),
                 sliderInput("plotHeight", "Plot height (px)", min = 300, max = 1200, value = 600, step = 50),
                 sliderInput("plotWidth", "Plot width (px)", min = 300, max = 1200, value = 600, step = 50),
                 
                 # New clustering controls
                 hr(),
                 h4("Clustering Options"),
                 checkboxInput("showClusteringOptions", "Show Clustering Tools", FALSE),
                 conditionalPanel(
                   condition = "input.showClusteringOptions == true",
                   selectInput("clusterMethod", "Clustering Method", 
                               choices = c("K-means", "DBSCAN")), # Add FlowSOM, PhenoGraph when libraries available
                   conditionalPanel(
                     condition = "input.clusterMethod == 'K-means'",
                     numericInput("numClusters", "Number of Clusters", value = 5, min = 2, max = 20)
                   ),
                   conditionalPanel(
                     condition = "input.clusterMethod == 'DBSCAN'",
                     numericInput("dbscan_eps", "DBSCAN epsilon", value = 0.5, min = 0.01, step = 0.1),
                     numericInput("dbscan_minPts", "DBSCAN minPts", value = 5, min = 1, step = 1)
                   ),
                   checkboxInput("showClusterProfiles", "Show Cluster Profiles", TRUE),
                   actionButton("runClustering", "Run Clustering", class = "btn-info")
                 ),
                 
                 actionButton("run", "Run Analysis", class = "btn-primary")
               ),
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("t-SNE", withSpinner(plotlyOutput("tsnePlot", height = "600px"))),
                   tabPanel("UMAP", withSpinner(plotlyOutput("umapPlot", height = "600px"))),
                   tabPanel("Data Info", verbatimTextOutput("fcsInfo")),
                   # New tabs for cluster analysis
                   tabPanel("Cluster Analysis", 
                            conditionalPanel(
                              condition = "input.showClusteringOptions == true",
                              tabsetPanel(
                                tabPanel("Cluster Visualization", plotlyOutput("clusterPlot")),
                                tabPanel("Cluster Profiles", withSpinner(plotOutput("clusterHeatmap"))),
                                tabPanel("Cluster Statistics", DT::dataTableOutput("clusterStats"))
                              )
                            )
                   )
                 )
               )
             )
    ),
    
    tabPanel("Cleaned Data Analysis",
             sidebarLayout(
               sidebarPanel(
                 fileInput("cleanedFile", "Upload CSV/TSV/Excel File", accept = c(".csv", ".tsv", ".xlsx")),
                 uiOutput("analysis_type_ui"),
                 uiOutput("marker_ui"),
                 uiOutput("treatment_ui"),
                 selectInput("dimred_method", "Dimensionality Reduction Method", choices = c("t-SNE", "UMAP")),
                 numericInput("perplexity_cleaned", "t-SNE: Perplexity", value = 5, min = 2, max = 50),
                 numericInput("neighbors", "UMAP: n_neighbors", value = 5, min = 2, max = 100),
                 numericInput("min_dist", "UMAP: min_dist", value = 0.1, min = 0, max = 1, step = 0.05),
                 numericInput("n_clusters", "Number of Clusters (k-means)", value = 3, min = 1),
                 sliderInput("plot_width", "Plot Width (px)", min = 300, max = 1200, value = 600, step = 50),
                 sliderInput("plot_height", "Plot Height (px)", min = 300, max = 1200, value = 600, step = 50),
                 actionButton("run_cleaned", "Run Analysis", class = "btn-primary")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Data Preview", DTOutput("preview")),
                   tabPanel("Structure Detection", verbatimTextOutput("structure")),
                   tabPanel("Results Plot", plotlyOutput("plot")),
                   tabPanel("t-SNE / UMAP", plotlyOutput("dimred_plot", width = "auto", height = "auto")),
                   tabPanel("Summary Table", DTOutput("summary_table"))
                 )
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  # --- COMMON FUNCTIONS ---
  
  
  # --- RAW DATA ANALYSIS (from first app) ---
  
  rawFCS <- reactive({
    req(input$fcsFile)
    ext <- tools::file_ext(input$fcsFile$name)
    switch(ext,
           "fcs" = read.FCS(input$fcsFile$datapath, transformation = FALSE),
           "csv" = fread(input$fcsFile$datapath),
           "tsv" = fread(input$fcsFile$datapath, sep = "\t"),
           stop("Unsupported file format")
    )
  })
  
  output$fcsInfo <- renderPrint({
    req(input$fcsFile)
    fcs <- rawFCS()
    cat("File name: ", input$fcsFile$name, "\n")
    cat("Dimensions: ", paste(dim(fcs), collapse = " x "), "\n")
    if (inherits(fcs, "flowFrame")) {
      cat("Parameters:\n")
      print(head(parameters(fcs)[, c("name", "desc")], 20))
    } else {
      cat("Column names:\n")
      print(colnames(fcs))
    }
  })
  
  output$markerSelectUI <- renderUI({
    req(input$fcsFile)
    data <- rawFCS()
    if (inherits(data, "flowFrame")) {
      exprs_data <- exprs(data)
      params <- parameters(data)
      choices <- setNames(colnames(exprs_data), paste0(colnames(exprs_data), " - ", params$desc))
    } else {
      exprs_data <- data
      choices <- colnames(exprs_data)
    }
    
    selectInput("selectedMarkers", "Select Markers/Channels", choices = choices, selected = choices[1:min(5, length(choices))], multiple = TRUE)
  })
  
  observe({
    req(input$selectedMarkers)
    markers <- input$selectedMarkers
    updateSelectInput(session, "colorBy", choices = c("None", "Cluster", markers))
  })
  
  processedData <- reactiveVal(NULL)
  
  observeEvent(input$run, {
    req(input$selectedMarkers)
    
    withProgress(message = 'Processing data...', value = 0, {
      data <- rawFCS()
      if (inherits(data, "flowFrame")) {
        exprs_data <- exprs(data)[, input$selectedMarkers, drop = FALSE]
      } else {
        exprs_data <- data[, input$selectedMarkers, drop = FALSE]
      }
      
      set.seed(123)
      sampled_indices <- sample(nrow(exprs_data), min(input$nEvents, nrow(exprs_data)))
      sampled_data <- exprs_data[sampled_indices, ]
      
      incProgress(0.2, detail = "Transforming data...")
      if (input$transform) {
        sampled_data <- apply(sampled_data, 2, asinhTransform, cofactor = input$cofactor)
      }
      
      scaled_data <- scale(sampled_data)
      
      results <- list(raw_data = sampled_data, scaled_data = scaled_data)
      
      if ("t-SNE" %in% input$methods) {
        incProgress(0.4, detail = "Running t-SNE...")
        tsne_result <- Rtsne(scaled_data, dims = 2, perplexity = input$perplexity, verbose = FALSE)
        results$tsne <- data.frame(tsne1 = tsne_result$Y[,1], tsne2 = tsne_result$Y[,2])
      }
      
      if ("UMAP" %in% input$methods) {
        incProgress(0.7, detail = "Running UMAP...")
        umap_result <- umap(scaled_data, n_neighbors = input$n_neighbors)
        results$umap <- data.frame(umap1 = umap_result[,1], umap2 = umap_result[,2])
      }
      
      plot_data <- as.data.frame(sampled_data)
      colnames(plot_data) <- input$selectedMarkers
      if (!is.null(results$tsne)) {
        plot_data$tsne1 <- results$tsne$tsne1
        plot_data$tsne2 <- results$tsne$tsne2
      }
      if (!is.null(results$umap)) {
        plot_data$umap1 <- results$umap$umap1
        plot_data$umap2 <- results$umap$umap2
      }
      
      results$plot_data <- plot_data
      processedData(results)
      
      # Reset clustering and gates when new data is loaded
      clustering_results(NULL)
    })
  })
  
  # --- CLUSTERING FUNCTIONALITY ---
  
  clustering_results <- reactiveVal(NULL)
  
  observeEvent(input$runClustering, {
    req(processedData(), input$selectedMarkers)
    
    withProgress(message = 'Clustering cells...', value = 0, {
      plot_data <- processedData()$plot_data
      # Extract just the marker columns for clustering
      marker_data <- plot_data[, input$selectedMarkers, drop = FALSE]
      
      # Run the selected clustering algorithm
      if (input$clusterMethod == "K-means") {
        set.seed(123)  # For reproducibility
        incProgress(0.3, detail = "Running K-means clustering...")
        km <- kmeans(marker_data, centers = input$numClusters, nstart = 10)
        cluster_ids <- km$cluster
        
        # Store cluster centers for intensity profiles
        centers <- km$centers
        colnames(centers) <- input$selectedMarkers
        
        # Create results object
        results <- list(
          cluster_ids = cluster_ids,
          centers = centers,
          method = "K-means"
        )
        
        clustering_results(results)
      }
      else if (input$clusterMethod == "DBSCAN") {
        incProgress(0.3, detail = "Running DBSCAN clustering...")
        # Scale data for DBSCAN
        scaled_markers <- scale(marker_data)
        
        # Run DBSCAN
        dbscan_result <- dbscan::dbscan(scaled_markers, eps = input$dbscan_eps, minPts = input$dbscan_minPts)
        cluster_ids <- dbscan_result$cluster
        
        # Calculate cluster centers (mean of each cluster)
        unique_clusters <- sort(unique(cluster_ids))
        centers <- matrix(NA, nrow = length(unique_clusters), ncol = ncol(marker_data))
        
        for (i in seq_along(unique_clusters)) {
          cluster_idx <- which(cluster_ids == unique_clusters[i])
          if (length(cluster_idx) > 0) {
            centers[i,] <- colMeans(marker_data[cluster_idx, , drop = FALSE])
          }
        }
        
        colnames(centers) <- input$selectedMarkers
        rownames(centers) <- paste("Cluster", unique_clusters)
        
        # Create results object
        results <- list(
          cluster_ids = cluster_ids,
          centers = centers,
          method = "DBSCAN"
        )
        
        clustering_results(results)
      }
      # Add other clustering methods here
    })
  })
  
  # Update plot outputs to show clusters
  observe({
    req(clustering_results())
    updateSelectInput(session, "colorBy", choices = c("None", "Cluster", input$selectedMarkers), selected = "Cluster")
  })
  
  output$tsnePlot <- renderPlotly({
    req(processedData(), "t-SNE" %in% input$methods)
    plot_data <- processedData()$plot_data
    req("tsne1" %in% colnames(plot_data))
    
    # Create a copy of plot_data to avoid modifying the reactive value
    plot_data_copy <- plot_data
    
    # If clustering results exist, add them to the plot data
    if (!is.null(clustering_results()) && input$colorBy == "Cluster") {
      plot_data_copy$Cluster <- as.factor(clustering_results()$cluster_ids)
      
      p <- ggplot(plot_data_copy, aes(x = tsne1, y = tsne2, color = Cluster, 
                                      text = paste("Cluster:", Cluster))) +
        geom_point(alpha = 0.7) +
        scale_color_viridis_d() +
        labs(color = "Cluster")
    } else if (!is.null(input$colorBy) && input$colorBy != "None" && input$colorBy != "Cluster") {
      p <- ggplot(plot_data_copy, aes(x = tsne1, y = tsne2)) +
        geom_point(aes(color = .data[[input$colorBy]], 
                       text = paste("Value:", .data[[input$colorBy]])), alpha = 0.7) +
        scale_color_viridis_c() +
        labs(color = input$colorBy)
    } else {
      p <- ggplot(plot_data_copy, aes(x = tsne1, y = tsne2)) +
        geom_point(aes(text = paste("Index:", rownames(plot_data_copy))), alpha = 0.7)
    }
    
    
    p <- p + labs(title = "t-SNE Projection", x = "t-SNE 1", y = "t-SNE 2") +
      theme_minimal(base_size = 16) +
      theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))
    
    ggplotly(p, tooltip = "text") %>% layout(height = input$plotHeight, width = input$plotWidth)
  })
  
  output$umapPlot <- renderPlotly({
    req(processedData(), "UMAP" %in% input$methods)
    plot_data <- processedData()$plot_data
    req("umap1" %in% colnames(plot_data))
    
    # Create a copy of plot_data to avoid modifying the reactive value
    plot_data_copy <- plot_data
    
    # If clustering results exist, add them to the plot data
    if (!is.null(clustering_results()) && input$colorBy == "Cluster") {
      plot_data_copy$Cluster <- as.factor(clustering_results()$cluster_ids)
      
      p <- ggplot(plot_data_copy, aes(x = umap1, y = umap2, color = Cluster, 
                                      text = paste("Cluster:", Cluster))) +
        geom_point(alpha = 0.7) +
        scale_color_viridis_d() +
        labs(color = "Cluster")
    } else if (!is.null(input$colorBy) && input$colorBy != "None" && input$colorBy != "Cluster") {
      p <- ggplot(plot_data_copy, aes(x = umap1, y = umap2)) +
        geom_point(aes(color = .data[[input$colorBy]], 
                       text = paste("Value:", .data[[input$colorBy]])), alpha = 0.7) +
        scale_color_viridis_c() +
        labs(color = input$colorBy)
    } else {
      p <- ggplot(plot_data_copy, aes(x = umap1, y = umap2)) +
        geom_point(aes(text = paste("Index:", rownames(plot_data_copy))), alpha = 0.7)
    }
    

    p <- p + labs(title = "UMAP Projection", x = "UMAP 1", y = "UMAP 2") +
      theme_minimal(base_size = 16) +
      theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))
    
    ggplotly(p, tooltip = "text") %>% layout(height = input$plotHeight, width = input$plotWidth)
  })
  
  # Cluster visualization in dedicated tab
  output$clusterPlot <- renderPlotly({
    req(processedData(), clustering_results())
    
    # Check what dimension reduction method is available
    plot_data <- processedData()$plot_data
    
    # Choose appropriate dimensions
    if ("tsne1" %in% colnames(plot_data)) {
      dim1 <- "tsne1"
      dim2 <- "tsne2"
      dim_labels <- c("t-SNE 1", "t-SNE 2")
    } else if ("umap1" %in% colnames(plot_data)) {
      dim1 <- "umap1"
      dim2 <- "umap2"
      dim_labels <- c("UMAP 1", "UMAP 2")
    } else {
      # Fallback to first two markers if no dimension reduction available
      dim1 <- input$selectedMarkers[1]
      dim2 <- input$selectedMarkers[2]
      dim_labels <- input$selectedMarkers[1:2]
    }
    
    # Add cluster information
    plot_data$Cluster <- as.factor(clustering_results()$cluster_ids)
    
    # Create plot
    p <- plot_ly(plot_data, 
                 x = ~.data[[dim1]], 
                 y = ~.data[[dim2]],
                 color = ~Cluster, 
                 type = "scatter", 
                 mode = "markers",
                 marker = list(size = 8, opacity = 0.7),
                 text = ~paste("Cluster:", Cluster, 
                               "<br>", dim_labels[1], ":", .data[[dim1]], 
                               "<br>", dim_labels[2], ":", .data[[dim2]])) %>%
      layout(title = paste("Clusters from", clustering_results()$method),
             xaxis = list(title = dim_labels[1]),
             yaxis = list(title = dim_labels[2]),
             height = input$plotHeight,
             width = input$plotWidth)
    
    p
  })
  
  # Cluster heatmap
  output$clusterHeatmap <- renderPlot({
    req(clustering_results(), input$showClusterProfiles)
    
    # Get cluster centers (mean intensities for each marker per cluster)
    centers <- clustering_results()$centers
    
    # Create heatmap
    heatmap_data <- as.matrix(centers)
    
    # Scale for better visualization
    heatmap_data_scaled <- t(scale(t(heatmap_data)))
    
    # Generate a color palette
    col_palette <- colorRampPalette(c("navy", "white", "firebrick3"))(100)
    
    # Plot heatmap
    heatmap(heatmap_data_scaled, 
            Colv = NA,  # Don't cluster columns (markers)
            col = col_palette,
            main = paste("Cluster Intensity Profiles from", clustering_results()$method),
            xlab = "Markers", ylab = "Clusters",
            margins = c(10, 8))
  })
  
  # Cluster statistics
  output$clusterStats <- DT::renderDataTable({
    req(clustering_results(), processedData())
    
    plot_data <- processedData()$plot_data
    plot_data$Cluster <- as.factor(clustering_results()$cluster_ids)
    
    # Calculate basic statistics for each cluster
    cluster_stats <- plot_data %>%
      group_by(Cluster) %>%
      summarize(
        Count = n(),
        Percentage = n() / nrow(plot_data) * 100,
        across(all_of(input$selectedMarkers), 
               list(Mean = ~mean(., na.rm = TRUE), 
                    Median = ~median(., na.rm = TRUE),
                    SD = ~sd(., na.rm = TRUE)))
      )
    
    DT::datatable(cluster_stats, 
                  options = list(scrollX = TRUE, pageLength = 5),
                  caption = paste("Cluster statistics from", clustering_results()$method))
  })
  
  # --- CLEANED DATA ANALYSIS (from second app) ---
  
  cleaned_data <- reactive({
    req(input$cleanedFile)
    ext <- tools::file_ext(input$cleanedFile$name)
    switch(ext,
           csv = read_csv(input$cleanedFile$datapath),
           tsv = read_tsv(input$cleanedFile$datapath),
           xlsx = read.xlsx(input$cleanedFile$datapath),
           validate("Unsupported file format"))
  })
  
  markers_cleaned <- reactive({
    req(cleaned_data())
    grep("^[48]", colnames(cleaned_data()), value = TRUE)
  })
  
  output$preview <- renderDT({
    req(cleaned_data())
    datatable(head(cleaned_data(), 10), options = list(scrollX = TRUE))
  })
  
  output$structure <- renderPrint({
    req(cleaned_data())
    list(
      Columns = colnames(cleaned_data()),
      CD4 = grep("^4", colnames(cleaned_data()), value = TRUE),
      CD8 = grep("^8", colnames(cleaned_data()), value = TRUE)
    )
  })
  
  output$analysis_type_ui <- renderUI({
    req(input$cleanedFile)
    selectInput("analysis_type", "Select Analysis Type", 
                choices = c("Marker Comparison", "Treatment Comparison", 
                            "Fold Change", "Summary Statistics"))
  })
  
  output$marker_ui <- renderUI({
    req(markers_cleaned())
    pickerInput("selected_markers", "Select Markers", choices = markers_cleaned(), 
                multiple = TRUE, selected = markers_cleaned()[1], options = list(`actions-box` = TRUE))
  })
  
  output$treatment_ui <- renderUI({
    req(cleaned_data())
    selectInput("selected_treatment", "Select Treatment Column", 
                choices = names(cleaned_data())[sapply(cleaned_data(), is.character)])
  })
  
  output$plot <- renderPlotly({
    req(input$run_cleaned, cleaned_data(), input$selected_markers, input$selected_treatment)
    df <- cleaned_data()
    df_long <- pivot_longer(df, cols = all_of(input$selected_markers),
                            names_to = "Marker", values_to = "Value")
    
    p <- ggplot(df_long, aes_string(x = input$selected_treatment, y = "Value", color = "Marker")) +
      geom_point(position = position_jitter(width = 0.2), alpha = 0.8, size = 3.5) +
      theme_minimal(base_size = 16) +
      theme(axis.title = element_text(size = 18),
            axis.text  = element_text(size = 15),
            legend.title = element_text(size = 16),
            legend.text  = element_text(size = 14))
    
    ggplotly(p)
  })
  
  output$dimred_plot <- renderPlotly({
    req(input$run_cleaned, cleaned_data(), input$selected_markers, input$selected_treatment)
    df <- cleaned_data()
    df_sel <- df[, input$selected_markers, drop = FALSE] %>%
      mutate(across(everything(), as.numeric)) %>%
      na.omit()
    
    if (nrow(df_sel) < 3) {
      showNotification("Not enough data for dimensionality reduction.", type = "error")
      return(NULL)
    }
    
    treatment <- df[as.numeric(rownames(df_sel)), input$selected_treatment]
    sample_ids <- if ("Sample" %in% colnames(df)) df[as.numeric(rownames(df_sel)), "Sample"] else paste("Sample", seq_len(nrow(df_sel)))
    
    set.seed(42)  # Ensure reproducibility
    dimred <- switch(input$dimred_method,
                     "t-SNE" = Rtsne(df_sel, perplexity = input$perplexity_cleaned, verbose = FALSE, check_duplicates = FALSE)$Y,
                     "UMAP" = umap(df_sel, n_neighbors = input$neighbors, min_dist = input$min_dist))
    
    cluster_labels <- as.factor(kmeans(df_sel, centers = input$n_clusters, nstart = 10)$cluster)
    
    dimred_df <- data.frame(Dim1 = dimred[,1], Dim2 = dimred[,2], 
                            Treatment = treatment, 
                            Cluster = cluster_labels,
                            Sample = sample_ids)
    
    plot_ly(dimred_df, x = ~Dim1, y = ~Dim2, color = ~Treatment,
            text = ~paste("Sample:", Sample, "<br>Cluster:", Cluster),
            colors = "Set1",
            type = 'scatter', mode = 'markers',
            marker = list(size = 12, line = list(width = 1, color = '#333')),
            width = input$plot_width, height = input$plot_height) %>%
      layout(title = list(text = paste(input$dimred_method, "Projection"), font = list(size = 20)),
             xaxis = list(title = "Dim 1", titlefont = list(size = 18), tickfont = list(size = 15),
                          scaleanchor = "y", scaleratio = 1),
             yaxis = list(title = "Dim 2", titlefont = list(size = 18), tickfont = list(size = 15)),
             margin = list(l = 50, r = 50, b = 50, t = 60))
    
  })
  
  output$summary_table <- renderDT({
    req(input$run_cleaned, cleaned_data(), input$selected_markers, input$selected_treatment)
    df <- cleaned_data()
    df_long <- pivot_longer(df, cols = all_of(input$selected_markers),
                            names_to = "Marker", values_to = "Value")
    
    summary_df <- df_long %>%
      group_by(across(all_of(input$selected_treatment)), Marker) %>%
      summarise(Mean = mean(Value, na.rm = TRUE),
                SD = sd(Value, na.rm = TRUE), .groups = "drop")
    
    datatable(summary_df)
  })
}


shinyApp(ui, server)
