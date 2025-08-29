**CytoGateR** is an R Shiny application designed for comprehensive flow cytometry data analysis. It provides an intuitive interface for everything from basic compensation to advanced clustering analysis, making flow cytometry accessible to researchers at all levels.

## Key Features

### Core Analysis Modules
- **Interactive Data Visualization**: Multiple plot types optimized for flow cytometry
- **Spillover Compensation**: Automated and manual compensation matrix generation
- **Hierarchical Gating**: Interactive population definition with real-time statistics
- **Batch Processing**: Simultaneous analysis of multiple samples
- **Advanced Clustering**: K-means, DBSCAN, FlowSOM, and PhenoGraph algorithms
- **Dimensionality Reduction**: t-SNE and UMAP implementations

## Technology Stack

### Core Flow Cytometry (Bioconductor)
- **Data Handling**: `flowCore`, `flowWorkspace` for FCS file processing and workspace management
- **Compensation & QC**: `flowStats`, `flowAI`, `flowDensity` for spillover correction and quality control
- **Gating & Analysis**: `openCyto`, `flowClust`, `flowMatch` for automated gating and population matching
- **Visualization**: `ggcyto` for ggplot2-based flow cytometry plotting

### Web Interface (Shiny Ecosystem)
- **Core Framework**: `shiny` with `shinydashboard` for structured layouts
- **Enhanced UI**: `shinyjs`, `shinycssloaders`, `shinythemes`, `shinyWidgets` for modern interface components
- **Interactive Tables**: `DT` for dynamic data tables with sorting and filtering

### Data Visualization & Plotting
- **Static Plots**: `ggplot2` with extensions (`ggpointdensity`, `gridExtra`, `scattermore`)
- **Interactive Plots**: `plotly` for dynamic, zoomable visualizations
- **Layout & Arrangement**: `gridExtra` for multi-panel figures

### Data Processing & Manipulation
- **Core Data**: `dplyr`, `tidyr`, `data.table` for efficient data transformation
- **File I/O**: `readr`, `openxlsx`, `tools` for reading various file formats
- **Statistical Analysis**: `MASS`, `cluster` for statistical modeling and clustering

### Advanced Analytics
- **Dimensionality Reduction**: `Rtsne` (t-SNE), `uwot` (UMAP) for visualization of high-dimensional data
- **Clustering Algorithms**: 
  - `FlowSOM` for self-organizing maps
  - `Rphenograph` for graph-based clustering
  - `dbscan` for density-based clustering
  - `igraph` for network analysis and visualization
  - `cluster` for traditional clustering methods

## Project Structure

```
FlowCyto Project/
├── FlowCytoApp/                    # Main Shiny application
│   ├── app.R                       # Application entry point
│   ├── global.R                    # Global configurations and libraries
│   ├── manifest.json               # Application metadata
│   ├── FlowCytoApp.Rproj          # RStudio project file
│   ├── R/
│   │   ├── modules/                # Modular Shiny components
│   │   │   ├── raw_data_module.R           # Raw data upload and preprocessing
│   │   │   ├── compensation_module.R       # Spillover compensation
│   │   │   ├── gating_module.R             # Interactive population gating
│   │   │   ├── batch_analysis_module.R     # Multi-sample processing
│   │   │   ├── processed_data_module.R     # Advanced analysis and visualization
│   │   │   ├── settings_module.R           # Global configuration settings
│   │   │   ├── cell_identification_module.R # Cell population identification
│   │   │   ├── clustering_module.R         # Clustering algorithms
│   │   │   └── visualization_module.R      # Advanced plotting functions
│   │   └── utils/                  # Utility functions
│   │       ├── clustering.R                # K-means, DBSCAN, FlowSOM, PhenoGraph
│   │       ├── gating_helpers.R            # Gate drawing and validation
│   │       ├── preprocessing.R             # Data transformation and QC
│   │       ├── plotting.R                  # Plotting utility functions
│   │       ├── helpers.R                   # General helper functions
│   │       ├── gating_plots.R              # Gating-specific visualizations
│   │       └── advanced_gating_helpers.R   # Advanced gating operations
│   ├── www/                        # Web assets
│   │   ├── custom.css              # Custom styling
│   │   └── custom.js               # JavaScript functions
└── README.md                       # This file
```

## Quick Start

### Prerequisites
- R (version 4.0 or higher)
- RStudio (recommended)
- Modern web browser (Chrome, Firefox, Safari, Edge)

### Installation

1. **Clone the repository**:
```bash
git clone https://github.com/Helmy-Lab/FlowCytoAppProject.git
cd FlowCytoAppProject
```

2. **Install required R packages**:
```r
# Install Bioconductor packages
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install(c("flowCore", "flowStats", "openCyto", "flowAI", 
                       "flowDensity", "flowClust", "flowWorkspace", 
                       "ggcyto", "flowMatch"))

# Install CRAN packages
install.packages(c(
  # Core Shiny packages
  "shiny", "shinydashboard", "shinyjs", "shinycssloaders",
  "shinythemes", "shinyWidgets",
  
  # Data manipulation and visualization
  "ggplot2", "plotly", "dplyr", "tidyr", "data.table",
  "DT", "readr", "openxlsx",
  
  # Dimensionality reduction and clustering
  "Rtsne", "uwot", "cluster", "dbscan", "FlowSOM", 
  "Rphenograph", "igraph",
  
  # Statistical analysis and utilities
  "MASS", "tools", "gridExtra", "ggpointdensity", "scattermore"
))
```

3. **Launch the application**:
```r
# Set working directory to FlowCytoApp
setwd("FlowCytoApp")

# Run the app
shiny::runApp()
```
## Documentation

Comprehensive documentation is available at [helmy-lab.github.io/CytoGateR/](https://helmy-lab.github.io/CytoGateR/):

### Quick Navigation
- **[Getting Started](https://helmy-lab.github.io/CytoGateR/getting-started.html)** - Set up CytoGateR and learn the basics
- **[Settings](https://helmy-lab.github.io/CytoGateR/settings.html)** - Configure global plot preferences
- **[Spillover Compensation](https://helmy-lab.github.io/CytoGateR/compensation.html)** - Remove fluorophore spillover
- **[Interactive Gating](https://helmy-lab.github.io/CytoGateR/gating.html)** - Define cell populations
- **[Raw Data Analysis](https://helmy-lab.github.io/CytoGateR/raw-data.html)** - Process and analyze raw flow cytometry data
- **[Batch Analysis](https://helmy-lab.github.io/CytoGateR/batch-analysis.html)** - Process multiple samples simultaneously
- **[Processed Data](https://helmy-lab.github.io/CytoGateR/processed-data.html)** - Advanced visualization and analysis
- **[Troubleshooting](https://helmy-lab.github.io/CytoGateR/troubleshooting.html)** - Common issues and solutions
- **[FAQ](https://helmy-lab.github.io/CytoGateR/faq.html)** - Frequently asked questions

