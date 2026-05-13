---
layout: default
title: Raw Data Analysis
nav_order: 6
has_children: true
---

<link rel="stylesheet" href="custom.css">

# Raw Data Analysis
{: .no_toc }

## Table of contents
{: .no_toc .text-delta }

1. TOC
{:toc}

---

## Overview

The Raw Data module serves as your starting point for flow cytometry analysis, providing essential preprocessing and initial visualization capabilities.

## File Upload

**Upload Data Tab:**
Click on the **grey tab** to upload a raw file with an **.fcs, .tsv or .csv extension**.

**Supported Formats:**
- **.fcs**: Flow Cytometry Standard files (recommended)
- **.csv/.tsv**: Comma/tab-separated values with proper headers

**Upload Process:**
1. Click the **grey upload tab**
2. Select your file using the file browser
3. Wait for upload confirmation
4. Verify file contents in the Data Preview tab
5. Select Markers/Channels that you would like included in the analysis

<img width="592" height="410" alt="image" src="https://github.com/user-attachments/assets/94a5262a-efc0-44b3-97ee-f279d383b972" />

<img width="596" height="381" alt="image" src="https://github.com/user-attachments/assets/1d2410e8-574d-41bf-80da-9657129e9032" />

## Data Transformation

**Arcsinh Transformation:**
Flow cytometry data often requires transformation to handle the wide dynamic range and improve visualization.

**Configuration:**
- **Enable transformation**: Click on the box to apply the arcsinh transformation
- **Cofactors**: To edit the transformation cofactors and number of events to analyze, **manually type in the desired number** or **increase/decrease the amount by clicking on the up or down arrow** beside the listed number
- **Events to analyze**: Set sampling size for performance optimization

**When to adjust cofactors:**
- **High cofactors (500-1000)**: For dim markers or autofluorescence
- **Low cofactors (50-150)**: For bright, well-separated populations

<img width="591" height="387" alt="image" src="https://github.com/user-attachments/assets/fee1dcbe-2c54-487f-bbc7-184ff88a7380" />

## Quality Control and Gating

**Debris Removal:**
**Debris and dead cell gating can be enabled by selecting the checkbox** next to the option.

<img width="593" height="304" alt="image" src="https://github.com/user-attachments/assets/60732000-289f-495e-a736-f86476ed5b5a" />

## Dimensionality Reduction

**Method Selection:**
Select the dimensionality reduction method(s) to apply to the raw data. **To apply multiple methods, check the corresponding boxes** for each desired option.

**Available Methods:**

### t-SNE (t-Distributed Stochastic Neighbor Embedding)
- **Best for**: Revealing local population structure and rare cell types
- **Perplexity**: This can be adjusted by **typing in the preferred perplexity**
- **Barnes-Hut approximation**: Can be **added or removed by clicking on the box** - this speeds up t-SNE on large datasets
- **Barnes-Hut theta**: Can be adjusted between 0-1 by **dragging the pointers on the scale** towards the desired number. This tuning parameter allows for better accuracy-speed trade-off in the Barnes-Hut approximation in t-SNE. **Increased number = better approximation** during t-SNE processing
- **Max iterations**: The maximum number of iterations can be adjusted either by **entering a desired value manually** or by **using the up/down arrows** to increase or decrease the value

### UMAP (Uniform Manifold Approximation and Projection)
- **Best for**: Preserving both local and global structure
- **N-neighbors**: Can be adjusted either by **entering a desired value manually** or by **using the up/down arrows** to increase or decrease the value
- **Faster than t-SNE** for large datasets

### PCA (Principal Component Analysis)
- **Best for**: Linear dimensionality reduction and noise reduction
- **Number of components**: Can also be adjusted either by **entering a desired value manually** or by **using the up/down arrows** to increase or decrease the value
- **Linear method**: Preserves linear relationships

## Interactive Heatmap Controls

**What are Interactive Heatmap Controls?**
The Interactive Heatmap Controls panel provides a dynamic interface for customizing marker-specific visualizations and heatmap displays. This powerful feature allows you to focus on individual markers, adjust visualization parameters, and rename technical channel names to meaningful biological markers.

### Select Marker
- **Purpose**: Choose which specific marker to visualize and analyze
- **Options**: Dropdown list of all available fluorescence channels
- **Usage**: Select markers like CD3, CD4, CD8, etc. to focus analysis on specific cell surface proteins
- **Tip**: Start with key markers for your cell populations of interest

<img width="445" height="295" alt="image" src="https://github.com/user-attachments/assets/a5285c9d-747a-499f-80b9-7754da53f021" />

### Dimensionality Method
- **Purpose**: Choose the dimensionality reduction technique for visualization
- **Available options** (Has to have been made during the analysis):
  - **t-SNE**: Best for revealing local population structure
  - **UMAP**: Balanced local and global structure preservation  
  - **PCA**: Linear reduction, good for overview
  - **MDS**: Classical multidimensional scaling
- **Impact**: Changes how your data points are positioned in 2D space

<img width="425" height="158" alt="image" src="https://github.com/user-attachments/assets/c517ef44-1cd3-4fb7-9773-53340cb161df" />

### Visualization Method
- **Purpose**: Select the type of plot for displaying your data
- **Available options**:
  - **Scatter Plot**: Individual data points, best for detailed analysis
  - **Density Plot**: Shows cell density distributions
  - **Contour Plot**: Population boundaries and structure
  - **Hexagon Plot**: Binned visualization for large datasets
- **Recommendation**: Use Scatter Plot for marker analysis, Hexagon for large datasets

<img width="431" height="193" alt="image" src="https://github.com/user-attachments/assets/b86f201e-8cdc-47fe-8cd4-0e5b591d4004" />

### Color Palette
- **Purpose**: Choose color schemes for data visualization
- **Available palettes**:
  - **Plasma**: Purple to pink to yellow gradient
  - **Viridis**: Blue to green to yellow (colorblind-friendly)
  - **Magma**: Black to purple to white
  - **Inferno**: Black to red to yellow
- **Scientific benefit**: Perceptually uniform palettes ensure accurate data interpretation

<img width="428" height="185" alt="image" src="https://github.com/user-attachments/assets/61970caa-fa90-4349-aceb-d60035a51502" />

## Marker Renaming Feature

**What is Marker Renaming?**
Convert technical channel names (FL1-A, FL2-A) to meaningful biological marker names (CD3, CD4) for easier interpretation and publication-ready figures.

**How to Use:**
1. **Click "Rename Markers"** button to open the renaming interface
2. **View Current Mappings** to see existing channel-to-marker assignments
3. **Edit mappings** to update channel names to biological markers
4. **Save changes** to apply throughout your analysis

**Benefits of Marker Renaming:**
- **Clearer interpretation**: Biological names are more meaningful than technical channels
- **Publication ready**: Professional figures with proper marker labels
- **Consistent analysis**: Same marker names across all visualizations
- **Collaboration**: Easier sharing with colleagues who understand biological markers

<img width="928" height="624" alt="image" src="https://github.com/user-attachments/assets/cbceb6f9-9cda-4441-8d45-897cb70c4a3e" />

## Fast Grid View

**What is Fast Grid View?**
An optimized visualization mode that displays multiple markers simultaneously in a grid layout, allowing rapid comparison across your marker panel.

**When to use:**
- **Quick overview** of all markers in your panel
- **Population screening** to identify key differentiating markers
- **Quality control** to spot potential issues across markers
- **Comparison analysis** between different experimental conditions

<img width="9600" height="3600" alt="00_COMBINED_All_Markers-min" src="https://github.com/user-attachments/assets/d1bf5f4b-feb7-4b55-8158-86817cad67f8" />

## Clustering Analysis

**Enable Clustering:**
**By clicking on 'enable clustering'** more clustering options will become available.

**Clustering Methods:**
**By clicking on the dropdown list**, there are options to choose the type of clustering methods to apply to the dataset.

### K-means
- **Number of clusters**: **Can be manually put in** - allows the number of clusters to be predefined
- **Best for**: Well-separated, spherical populations
- **Tip**: Start with expected number of major populations

### DBSCAN
- **Epsilon and Min pts**: **Can be set individually to the desired number manually**
- **Epsilon**: Controls density sensitivity 
- **Min pts**: Min number of points required within the radius to be considered a cluster 
- **Best for**: Irregularly shaped populations
- **Handles noise**: Automatically identifies outliers

### FlowSOM
- **SOM Grid X dimension**: **Can input the desired number of nodes horizontally by entering a desired value manually** or by **using the up/down arrows** to increase or decrease the value
- **SOM Grid Y dimension**: **Can input the desired number of nodes vertically by entering a desired value manually** or by **using the up/down arrows** to increase or decrease the value
- **Metacluster**: **Can input the number of meta clusters by entering a desired value manually** or by **using the up/down arrows** to increase or decrease the value. **Controls second level of clustering**
- **Training iterations**: **Define the numbers of how many times the SOM training algorithm will update** - by **entering a desired value manually** or by **using the up/down arrows** to increase or decrease the value
- **Best for**: Flow cytometry-specific hierarchical analysis

---

## Workflow Tips

### Data Preparation
{: .label .label-green }

- **File quality**: Ensure your FCS files are not corrupted and contain proper channel information
- **Channel selection**: Choose relevant fluorescence markers for your analysis
- **Sample size**: Consider using data sampling for large files to improve processing speed

### Transformation Guidelines
{: .label .label-blue }

- **Always transform**: Flow cytometry data typically requires transformation for proper visualization
- **Adjust cofactors**: Lower values for bright markers, higher values for dim markers
- **Check distributions**: Verify that transformed data shows proper population separation

### Dimensionality Reduction Tips
{: .label .label-yellow }

- **t-SNE for discovery**: Use when exploring unknown population structures
- **UMAP for speed**: Faster processing while maintaining good structure preservation
- **PCA for linearity**: Best when you need to preserve linear relationships
- **Multiple methods**: Try different approaches to find the best visualization for your data

### Clustering Best Practices
{: .label .label-red }

- **Start simple**: Begin with K-means if you know expected population numbers
- **FlowSOM for flow**: Specifically designed for flow cytometry data analysis
- **DBSCAN for flexibility**: Handles irregular shapes and varying densities
- **Validate results**: Compare clustering results with known biological populations