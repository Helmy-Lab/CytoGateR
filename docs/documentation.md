---
layout: default
title: CytoVerse Documentation  
---

<style>
/* Auto-fix all images in the documentation */
img {
  max-width: 100% !important;
  height: auto !important;
  display: block;
  margin: 20px auto;
  border: 1px solid #e1e4e8;
  border-radius: 6px;
  box-shadow: 0 2px 4px rgba(0,0,0,0.1);
}

@media (max-width: 768px) {
  img {
    margin: 15px auto;
    max-width: 95% !important;
  }
}
</style>

# CytoVerse Documentation

**An Interactive Flow Cytometry Analysis Platform**

---

## Table of Contents

1. [Introduction](#introduction)
2. [Getting Started](#getting-started)
3. [Settings](#settings)
4. [Spillover Compensation](#spillover-compensation)
5. [Interactive Gating](#interactive-gating)
6. [Raw Data Analysis](#raw-data-analysis)
7. [Batch Analysis](#batch-analysis)
8. [Processed Data](#processed-data)
9. [Troubleshooting](#troubleshooting)
10. [FAQ](#faq)

---

## Introduction

CytoVerse is a powerful, web-based flow cytometry analysis platform designed to streamline your cytometry workflow from raw data processing to publication-ready results. Whether you're analyzing single samples or processing large batch experiments, CytoVerse provides the tools you need for flow cytometry analysis in an interactive platform.

### Key Features

- **📊 Comprehensive Analysis Pipeline**: From spillover compensation to advanced clustering
- **🔧 Interactive Gating**: Intuitive hierarchical gating with real-time statistics
- **📈 Advanced Visualization**: Multiple plot types optimized for flow cytometry data
- **⚡ Batch Processing**: Analyze dozens of samples simultaneously with standardized parameters
- **🎨 Customizable Plots**: Publication-ready figures with extensive customization options
- **💾 Template System**: Save and reuse analysis workflows for consistent results

### Typical Workflow

1. **Upload & Preprocess**: Load raw FCS files and apply quality control
2. **Compensation**: Remove fluorophore spillover using control files
3. **Gating**: Define cell populations through hierarchical gating
4. **Analysis**: Apply dimensionality reduction and clustering algorithms
5. **Export**: Generate publication-ready results and statistics

---

## Getting Started

### System Requirements

CytoVerse runs in your web browser and supports:
- **Browsers**: Chrome, Firefox, Safari, Edge (latest versions)
- **File Formats**: .fcs, .csv, .tsv, .xlsx
- **File Size**: Up to 250MB per upload session

### Quick Start Guide

1. **Access CytoVerse** through your web browser
2. **Start with Settings** to configure global plot preferences
3. **Upload your data** in the Raw Data tab
4. **Apply compensation** if using multi-color panels
5. **Gate your populations** using Interactive Gating
6. **Analyze and export** your results

---

## Settings

### Global Plot Settings


<img width="852" height="898" alt="image" src="https://github.com/user-attachments/assets/a76122e9-750e-4ce9-9c3d-df679f219a53" />


This allows the plots to be visually adjusted. The width and height of the plot can be changed accordingly by dragging the pointer along the scale. Similarly, by dragging on the pointer on the scale, moving left or right the font size as well as the sizes of the points on the graph can be changed. Finally, the color palette can be customized by selecting the color circle next to the palette selector. The tool offers a variety of options, including colorblindfriendly palettes, as well as categorical, sequential, and diverging schemes to enhance data interpretation.

**Plot Dimensions:**
- **Width/Height**: Drag the pointer along the scale to adjust plot size (default: 600x600 pixels)
- **Optimal for presentations**: 800x800 pixels
- **Optimal for publications**: 600x600 pixels (fits journal requirements)

**Typography:**
- **Font Size**: Drag the pointer on the scale left or right to adjust text size for better readability (range: 8-24px)
- **Point Size**: Drag the pointer to control data point visibility (range: 1-12px)
- **Recommended for dense data**: Smaller point sizes (2-4px)
- **Recommended for presentations**: Larger point sizes (6-8px)

**Color Palettes:**
Customize by selecting the **color circle next to the palette selector**. Choose from scientifically-optimized color schemes:
- **Viridis/Plasma/Magma**: Perceptually uniform, colorblind-friendly
- **Sequential palettes**: Best for density plots and continuous data
- **Categorical palettes**: Ideal for discrete populations
- **Diverging palettes**: Perfect for fold-change and comparison data

---

## Spillover Compensation

### Overview

**What is spillover compensation?**
Spillover occurs when fluorophores emit light that bleeds into adjacent detection channels, creating false positive signals. Compensation mathematically corrects this by subtracting the spillover signal from each channel based on control measurements.

### Step 1: Upload Files

1. Click **"Browse FCS Files"** to upload:
   - **Unstained control** (required): Cells with no fluorescent staining - establishes baseline autofluorescence
   - **Single-stain controls** for each fluorophore (minimum 2 required): Cells stained with only one fluorophore each - measures spillover from that specific fluorophore into other channels
   - **Experimental samples**: Your actual samples with multiple fluorophores


<img width="1240" height="1162" alt="image" src="https://github.com/user-attachments/assets/d8d15ee7-5e39-484d-a59f-10ef0a927e31" />


2. **Auto-detected channel formats:**
   - FL formats: FL1-A, FL2-H, FL3-W
   - Laser wavelength: 405 D 525_50-A, 488/530-A
   - FJComp formats: FJ-Comp-A, Comp-PE-A
   - Fluorophore names: FITC-A, PE-H, BV421-A
   - Marker combinations: CD3-FITC-A

### Step 2: Assign File Roles

1. **Select unstained control** from uploaded files
2. **Assign single-stain controls** to each detected channel
3. Click **"Validate File Assignment"** to save assignments


<img width="1221" height="1213" alt="image" src="https://github.com/user-attachments/assets/14089d2c-f4a4-49a6-955e-0498632863ec" />


<img width="1212" height="360" alt="image" src="https://github.com/user-attachments/assets/57c6eb54-eb38-45f3-8138-16a9bbdd2b13" />


<img width="1244" height="742" alt="image" src="https://github.com/user-attachments/assets/11496d30-8278-48dc-8a71-0f62b1eebb05" />


### Step 3: Generate Compensation Matrix

**What is a compensation matrix?**
A mathematical matrix that defines how much signal from each fluorophore spills into other channels. The system uses this to subtract spillover signals from your experimental data.

**Option A: Auto-compute**
- Click **"Compute Spillover Matrix"**
- System automatically calculates using **median method** (uses median fluorescence values from single-stain controls to be robust against outliers)

**Option B: Import existing matrix**
- Upload .csv or Excel (.xlsx) file with pre-calculated compensation values
- Matrix must be square with fluorophores as row/column names
- Useful when you have standardized compensation from instrument software


<img width="2491" height="956" alt="image" src="https://github.com/user-attachments/assets/b6298e6b-651a-4180-a9d9-6bafad77266d" />


### Step 4: Review & Edit Matrix

- **Color coding:**
  - 🟢 Green: Low spillover (0-1%)
  - 🟡 Yellow: Moderate (1-5%)
  - 🟠 Orange: High (5-10%)
  - 🔴 Red: Very high (>10%)
- Enable editing to modify values manually
- Click **"Reset to Original"** to undo changes


<img width="566" height="450" alt="image" src="https://github.com/user-attachments/assets/7431a498-d970-477b-8d84-75463e0f978a" />


### Step 5: Quality Control

1. **Select channels** to analyze (subset of compensated channels)
2. **Set sample size** (1,000-50,000 events) - smaller samples = faster processing
3. Click **"Run QC Analysis"**

**Key QC metrics explained:**
- **Negative events percentage**: Should be <5% after compensation. High percentages indicate over-compensation or poor controls
- **Signal spread (CV)**: Coefficient of variation measures how tight your positive population is. Lower CV = better separation
- **Before/after comparison**: Visual check that compensation improved channel separation without creating artifacts

**What good compensation looks like:**
- Positive populations shift toward axes (less spillover)
- Negative populations stay near zero
- No new negative populations created

### Step 6: Export Results

- **Compensated FCS files**: Download with "_compensated" suffix
- **Spillover matrix**: Export as CSV or Excel
- **Session data**: Save complete workflow for later use

<img width="1222" height="865" alt="image" src="https://github.com/user-attachments/assets/2f639b97-cee0-4d19-95b4-d6e010694416" />

---

## Interactive Gating

### Overview

**What is gating?**
Gating is the process of selecting specific cell populations based on their fluorescence or scatter properties. You draw boundaries (gates) around populations of interest to separate them from other cells for analysis.

**Why use hierarchical gating?**
Start with broad populations (e.g., live cells) and progressively refine to specific subsets (e.g., CD4+ T cells). This ensures accurate population identification and reduces background interference.

### Step 1: Setup Data Source

**Choose one:**
- Use preprocessed data from Raw Data tab
- Load new FCS files directly
- 
<img width="600" height="347" alt="image" src="https://github.com/user-attachments/assets/81e703fa-a801-4cdb-96fc-6549d75494ee" />

**Select parameters:**
- Sample from dropdown
- X and Y axis channels for 2D plots

<img width="563" height="328" alt="image" src="https://github.com/user-attachments/assets/65421dbe-251e-407a-a16c-373c36e51932" />

### Step 2: Create Gates

1. **Enter population name**
2. **Select gate type**
3. Click **"Start Gating"**
4. **Draw gate:**
   - **Static mode**: Click points or use brush selection
   - **Interactive mode**: Use plotly toolbar (box/lasso select)
5. Click **"Save Gate"** when complete

   
<img width="575" height="619" alt="image" src="https://github.com/user-attachments/assets/fdf0a0be-e060-4347-8412-ef4ca38e480c" />


<img width="1380" height="913" alt="image" src="https://github.com/user-attachments/assets/8356fabf-9cff-4b2c-908c-05c467a82e0a" />


<img width="1358" height="890" alt="image" src="https://github.com/user-attachments/assets/8fb38931-9f9e-44ae-b96c-c2c77d19d29d" />


### Step 3: Visualization Options

**Plot modes explained:**
- **Static mode**: Shows all events simultaneously, better performance for large datasets (>10,000 events). Recommended for most gating tasks
- **Interactive mode**: Limited to 10,000 randomly sampled events but provides full zoom, pan, and selection tools. Use when you need precise navigation

**Plot types and when to use them:**
- **Scatter plot**: Basic dot plot showing individual events. Best for small datasets or when you need to see every single event
- **Density hotspots**: Points colored by local cell density (red = high density, blue = low). Excellent for identifying main populations and rare events
- **Hotspots + contours**: Combines density coloring with population boundary lines. Ideal for complex samples with multiple overlapping populations
- **Hexagon plot**: Divides plot into hexagonal bins showing event counts. Essential for very large datasets (>100,000 events) where individual points would overlap

### Step 4: Hierarchical Gating

**How hierarchy works:**
- **Start with "root"** (all events in your sample)
- **Select parent population** for sub-gating (e.g., gate lymphocytes from root, then CD4+ cells from lymphocytes)
- **View hierarchy tree** with real-time counts showing how populations relate to each other
- **Edit/delete gates** - changes automatically update all downstream populations

<img width="589" height="159" alt="image" src="https://github.com/user-attachments/assets/641771f8-4b90-4317-9328-6574a10753c8" />

<img width="595" height="321" alt="image" src="https://github.com/user-attachments/assets/48627011-142f-4b44-a837-503cf364c72f" />

**Understanding population statistics:**
- **Event count**: Absolute number of events in this population
- **% of parent**: What percentage of the immediate parent population this represents
- **% of total**: What percentage of all original events this represents
- **Hierarchy depth**: Shows how many gating steps deep you are (root → lymphocytes → CD4+ T cells = 3 levels)

### Step 5: Population Analysis

**Real-time statistics:**
- Event counts per population (updates as you modify gates)
- Percentage of parent and total events
- Hierarchical organization showing parent-child relationships

**Data extraction explained:**
- **Select specific populations**: Choose which gated populations to export for further analysis
- **Extract as flowSet objects**: FlowSet is a standard R/Bioconductor data structure containing flow cytometry data that can be used in downstream analysis software
- **Apply all gates**: Ensures your extracted data includes all gating decisions in the hierarchy


<img width="593" height="887" alt="image" src="https://github.com/user-attachments/assets/e4f758af-6b52-4fc7-89fd-18e148dfeb4a" />


### Step 6: Export & Templates

**Export options:**
- **Gated FCS files**: Individual populations as new files
- **Gate definitions**: Coordinates and parameters (CSV)
- **Population statistics**: Counts and percentages

  
<img width="565" height="285" alt="image" src="https://github.com/user-attachments/assets/b2c7c72a-fa48-4f6a-843c-7d768f7a7ef2" />


**Template management:**
- Save gating strategies for reuse
- Load templates for standardized workflows

---

## Raw Data Analysis

The Raw Data module serves as your starting point for flow cytometry analysis, providing essential preprocessing and initial visualization capabilities.

### File Upload

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


### Data Transformation

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


### Quality Control and Gating

**Debris Removal:**
**Debris and dead cell gating can be enabled by selecting the checkbox** next to the option.

<img width="593" height="304" alt="image" src="https://github.com/user-attachments/assets/60732000-289f-495e-a736-f86476ed5b5a" />

### Dimensionality Reduction

**Method Selection:**
Select the dimensionality reduction method(s) to apply to the raw data. **To apply multiple methods, check the corresponding boxes** for each desired option.

**Available Methods:**

**t-SNE (t-Distributed Stochastic Neighbor Embedding):**
- **Best for**: Revealing local population structure and rare cell types
- **Perplexity**: This can be adjusted by **typing in the preferred perplexity**
- **Barnes-Hut approximation**: Can be **added or removed by clicking on the box** - this speeds up t-SNE on large datasets
- **Barnes-Hut theta**: Can be adjusted between 0-1 by **dragging the pointers on the scale** towards the desired number. This tuning parameter allows for better accuracy-speed trade-off in the Barnes-Hut approximation in t-SNE. **Increased number = better approximation** during t-SNE processing
- **Max iterations**: The maximum number of iterations can be adjusted either by **entering a desired value manually** or by **using the up/down arrows** to increase or decrease the value

**UMAP (Uniform Manifold Approximation and Projection):**
- **Best for**: Preserving both local and global structure
- **N-neighbors**: Can be adjusted either by **entering a desired value manually** or by **using the up/down arrows** to increase or decrease the value
- **Faster than t-SNE** for large datasets

**PCA (Principal Component Analysis):**
- **Best for**: Linear dimensionality reduction and noise reduction
- **Number of components**: Can also be adjusted either by **entering a desired value manually** or by **using the up/down arrows** to increase or decrease the value
- **Linear method**: Preserves linear relationships

### Marker Heatmaps
What are marker heatmaps?
Marker heatmaps visualize the intensity profiles of fluorescence markers overlaid onto t-SNE or UMAP plots produced from our dimensionality reduction feature. They provide an insightful overview of how each marker is expressed in across the sample, making it easy to characterize and compare potential cell populations and clusters.

**Select Marker:**
- **Purpose**: Choose which specific marker to visualize and analyze
- **Options**: Dropdown list of all available fluorescence channels
- **Usage**: Select markers like CD3, CD4, CD8, etc. to focus analysis on specific cell surface proteins
- **Tip**: Start with key markers for your cell populations of interest

<img width="445" height="295" alt="image" src="https://github.com/user-attachments/assets/a5285c9d-747a-499f-80b9-7754da53f021" />


**Dimensionality Method:**
- **Purpose**: Choose the dimensionality reduction technique for visualization
- **Available options (Has to have been made during the analysis)**:
  - **t-SNE**: Best for revealing local population structure
  - **UMAP**: Balanced local and global structure preservation  
  - **PCA**: Linear reduction, good for overview
  - **MDS**: Classical multidimensional scaling
- **Impact**: Changes how your data points are positioned in 2D space

<img width="425" height="158" alt="image" src="https://github.com/user-attachments/assets/c517ef44-1cd3-4fb7-9773-53340cb161df" />


**Visualization Method:**
- **Purpose**: Select the type of plot for displaying your data
- **Available options**:
  - **Scatter Plot**: Individual data points, best for detailed analysis
  - **Density Plot**: Shows cell density distributions
  - **Contour Plot**: Population boundaries and structure
  - **Hexagon Plot**: Binned visualization for large datasets
- **Recommendation**: Use Scatter Plot for marker analysis, Hexagon for large datasets

<img width="431" height="193" alt="image" src="https://github.com/user-attachments/assets/b86f201e-8cdc-47fe-8cd4-0e5b591d4004" />


**Color Palette:**
- **Purpose**: Choose color schemes for data visualization
- **Available palettes**:
  - **Plasma**: Purple to pink to yellow gradient
  - **Viridis**: Blue to green to yellow (colorblind-friendly)
  - **Magma**: Black to purple to white
  - **Inferno**: Black to red to yellow
- **Scientific benefit**: Perceptually uniform palettes ensure accurate data interpretation

<img width="428" height="185" alt="image" src="https://github.com/user-attachments/assets/61970caa-fa90-4349-aceb-d60035a51502" />


### Marker Renaming Feature

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


### Fast Grid View

**What is Fast Grid View?**
An optimized visualization mode that displays multiple markers simultaneously in a grid layout, allowing rapid comparison across your marker panel.

**When to use:**
- **Quick overview** of all markers in your panel
- **Population screening** to identify key differentiating markers
- **Quality control** to spot potential issues across markers
- **Comparison analysis** between different experimental conditions

<img width="9600" height="3600" alt="00_COMBINED_All_Markers-min" src="https://github.com/user-attachments/assets/d1bf5f4b-feb7-4b55-8158-86817cad67f8" />


### Clustering Analysis

**Enable Clustering:**
**By clicking on 'enable clustering'** more clustering options will become available.

**Clustering Methods:**
**By clicking on the dropdown list**, there are options to choose the type of clustering methods to apply to the dataset.

**K-means:**
- **Number of clusters**: **Can be manually put in** - allows the number of clusters to be predefined
- **Best for**: Well-separated, spherical populations
- **Tip**: Start with expected number of major populations

**DBSCAN:**
- **Epsilon and Min pts**: **Can be set individually to the desired number manually**
- **Epsilon**: Controls density sensitivity 
- **Min pts**: Min number of points required within the radius to be considered a cluster 
- **Best for**: Irregularly shaped populations
- **Handles noise**: Automatically identifies outliers

**FlowSOM:**
- **SOM Grid X dimension**: **Can input the desired number of nodes horizontally by entering a desired value manually** or by **using the up/down arrows** to increase or decrease the value
- **SOM Grid Y dimension**: **Can input the desired number of nodes vertically by entering a desired value manually** or by **using the up/down arrows** to increase or decrease the value
- **Metacluster**: **Can input the number of meta clusters by entering a desired value manually** or by **using the up/down arrows** to increase or decrease the value. **Controls second level of clustering**
- **Training iterations**: **Define the numbers of how many times the SOM training algorithm will update** - by **entering a desired value manually** or by **using the up/down arrows** to increase or decrease the value
- **Best for**: Flow cytometry-specific hierarchical analysis

---

## Batch Analysis

### Overview

**What is batch analysis?**
Batch analysis allows you to process multiple flow cytometry samples simultaneously with standardized parameters. This ensures consistent analysis across experimental groups and enables statistical comparisons between samples and conditions.

**Why use batch analysis?**
- **Consistency**: Apply identical processing parameters across all samples
- **Efficiency**: Process dozens of samples at once instead of one by one
- **Comparison**: Generate standardized results for statistical analysis between groups
- **Reproducibility**: Save processing templates for future experiments

### Step 1: Sample Management

1. **Upload multiple files**:
   - Click **"Browse Files"** to select multiple .fcs, .csv, or .tsv files
   - Files can be from different experimental groups or conditions
   - Each file becomes a separate sample in your batch

2. **Automatic grouping** (optional):
   - **Filename pattern grouping**: Files are automatically assigned to groups based on naming patterns
   - **Control pattern**: Enter text pattern that identifies control samples (e.g., "ctrl", "control")
   - **Treated pattern**: Enter text pattern that identifies treated samples (e.g., "treat", "stim")
   - **Manual grouping**: Assign groups manually if automatic detection doesn't work

3. **Sample list management**:
   - View all uploaded samples with their assigned groups
   - **Remove individual samples** or **clear entire batch**
   - Check sample details before processing


<img width="1804" height="421" alt="image" src="https://github.com/user-attachments/assets/1f4b94b6-12b7-44b4-a3a9-41d3d654a0b2" />


### Step 2: Configure Batch Processing Parameters

**Preprocessing options:**
- **Data transformation**: Choose arcsinh transformation with configurable cofactors
- **Channel selection**: Select which fluorescence channels to include in analysis
- **Event sampling**: Set number of events to analyze per sample (for performance)


<img width="599" height="565" alt="image" src="https://github.com/user-attachments/assets/96e3a29e-3e76-4ee5-87e9-ad564c8e4cc2" />


**Debris Removal:**
**Debris and dead cell gating can be enabled by selecting the checkbox** next to the option.


<img width="589" height="345" alt="image" src="https://github.com/user-attachments/assets/3dd963d6-4573-4178-a0db-39b317a095f2" />


### Step 3: Dimensionality Reduction

**Choose reduction method** for visualization:


<img width="586" height="651" alt="image" src="https://github.com/user-attachments/assets/ae4e3740-1885-4ae8-9618-bffddc2a06ed" />


**t-SNE parameters:**
- **Perplexity**: Controls local vs. global structure balance (default: 30, range: 5-50)
- **Barnes-Hut approximation**: Speeds up processing for large datasets
- **Barnes-Hut theta**: Accuracy vs. speed trade-off (0-1, higher = faster but less accurate)
- **Max iterations**: Number of optimization steps (default: 1000)

<img width="559" height="412" alt="image" src="https://github.com/user-attachments/assets/b5ffb4fd-3783-47f8-ba6f-fd9cbb57fe66" />

**UMAP parameters:**
- **N-neighbors**: Controls local neighborhood size (default: 15, affects local vs. global structure)
- **Min distance**: Minimum distance between points in embedding
- **Metric**: Distance metric for high-dimensional space
  
<img width="563" height="176" alt="image" src="https://github.com/user-attachments/assets/d689f0e2-0470-4885-9d08-cf1fd8ad2fb9" />

**PCA parameters:**
- **Number of components**: How many principal components to calculate (default: 2 for visualization)
- 
<img width="564" height="161" alt="image" src="https://github.com/user-attachments/assets/6a920fd1-792c-4723-9bfd-99013a761cf7" />

### Step 4: Clustering Analysis

**Enable clustering** to identify cell populations automatically:

**Clustering methods:**

**K-means:**
- **Number of clusters**: Pre-define how many populations to find
- **Best for**: Well-separated, roughly spherical populations
- **When to use**: When you know approximately how many populations to expect

**DBSCAN:**
- **Epsilon**: Controls density sensitivity (neighborhood size)
- **MinPts**: Minimum points required to form a cluster core
- **Best for**: Populations with varying densities and shapes
- **When to use**: When populations have irregular shapes or unknown numbers

**FlowSOM:**
- **SOM grid X/Y dimensions**: Size of self-organizing map grid
- **Metaclusters**: Number of final population groups (second-level clustering)
- **Training iterations**: How many times the algorithm updates
- **Best for**: Flow cytometry-specific analysis with hierarchical population structure

**Phenograph:**
- **k (nearest neighbors)**: Number of neighbors for graph construction
- **Best for**: Identifying rare populations and complex population structures
- **When to use**: When you want to detect subtle population differences

### Step 5: Population Identification

**Automatic population naming** based on marker expression:

1. **Enable population identification** to automatically name clusters
2. **Expression thresholds**:
   - **High expression threshold**: Level considered "positive" for a marker (default: 0.5)
   - **Low expression threshold**: Level considered "negative" for a marker (default: -0.5)
3. **Confidence threshold**: Minimum confidence required for population assignment (default: 30%)

<img width="563" height="654" alt="image" src="https://github.com/user-attachments/assets/6cb3cb4c-8c21-44d8-af2e-085d8b5f2e01" />

**How it works:**
- System analyzes mean marker expression for each cluster
- Compares to thresholds to determine positive/negative markers
- Generates population names like "CD4+CD8-" or "B cells"
- Assigns confidence scores based on separation from thresholds

### Step 6: Visualization and Results

**Plot options:**
- **Show cluster labels**: Display cluster numbers on plots
- **Show population labels**: Display identified population names
- **Color by**: Choose to color by cluster ID, population, or sample group
- **Point density visualization**: Show cell density within populations
  
<img width="1803" height="1055" alt="image" src="https://github.com/user-attachments/assets/b61c3f4a-8605-4379-8a60-45fa593e1021" />

<img width="1814" height="1056" alt="image" src="https://github.com/user-attachments/assets/eb97ec89-c027-4585-b7b3-193dfc7ddc50" />

**Cluster management:**
- **Merge clusters**: Combine similar clusters manually if needed
- **Edit population names**: Modify automatic population assignments
- **View cluster statistics**: See event counts and percentages per cluster/population

### Step 7: Export and Templates

**Export options:**
- **Visualization plots**: Export plots as PDF or PNG

**Template management:**
- **Save processing template**: Save all parameters for reuse on future experiments
- **Load template**: Apply saved parameter sets to new batches
- **Template library**: Organize templates by experiment type or research question

---

## Processed Data

This tab provides a range of visualizations for the processed data. The sidebar includes adjustable parameters that allow users to customize and generate the desired plot. Upon uploading a file in .fcs, .csv, or .xlsx format, the data can be explored using the selected dimensionality reduction method. By clicking on the drop down menu, dimensionality reduction methods such as tSNE, UMAP, PCA or MDS can be chosen. Based on the dimensionality reduction technique chosen, additional parameters can be set. For example, if tSNE is selected as the dimensionality reduction method, the perplexity parameter and number of clusters can be adjusted accordingly. The plot width and length can also be adjusted by dragging the pointer along the scale. 

### Side Panel Controls

**File Upload and Exploration:**
Upon uploading a file in **.fcs, .csv, or .xlsx format**, the data can be explored using the selected dimensionality reduction method.

**Dimensionality Reduction Selection:**
By clicking on the **dropdown menu**, dimensionality reduction methods can be chosen:
- **t-SNE**: Best for revealing local structure and rare populations
- **UMAP**: Balances local and global structure preservation
- **PCA**: Linear method for noise reduction and overview
- **MDS**: Classical multidimensional scaling for distance preservation

**Method-Specific Parameters:**
Based on the dimensionality reduction technique chosen, additional parameters can be set. For example, if **t-SNE is selected** as the dimensionality reduction method, the **perplexity parameter and number of clusters** can be adjusted accordingly.

**Plot Customization:**
- **Plot dimensions**: The plot width and length can be adjusted by **dragging the pointer along the scale**
- **Visual parameters**: Modify point size, colors, and other aesthetic elements
- **Real-time updates**: Parameters apply immediately to visualization

### Data Preview

The Data Preview tab displays your uploaded dataset with the parameters that were chosen and applied to the dataset. This allows you to:
- **Verify data integrity**: Ensure your data loaded correctly
- **Check transformations**: See the effects of applied preprocessing steps
- **Review parameters**: Confirm that chosen parameters are appropriate for your data
- **Quality assessment**: Identify any issues before proceeding with analysis

---

## Troubleshooting

### Compensation Issues

**Diagonal matrix values ≠ 1.0:**
- **Cause**: Poor single-stain control quality
- **Solution**: Check that controls have bright, positive populations

**Negative spillover values:**
- **Cause**: Incorrect file assignment or poor controls
- **Solution**: Verify file assignments and control quality

**Spillover levels >50%:**
- **Cause**: Spectral overlap too high for reliable compensation
- **Solution**: Consider different fluorophore combinations

**High negative events after compensation:**
- **Cause**: Over-compensation
- **Solution**: Reduce spillover values manually in matrix editor

### Gating Issues

**Slow performance:**
- **Solution**: Use Static mode for datasets >10,000 events

**Population counts not updating:**
- **Solution**: Click "Refresh display" to recalculate hierarchy

**Can't see populations clearly:**
- **Solution**: Try density hotspots or hexagon plots for better visualization

**Gates not saving:**
- **Solution**: Ensure you click "Save Gate" after drawing each gate

**Sub-gates appearing empty:**
- **Solution**: Check that parent population selection is correct

### File Format Issues

**Channels not detected:**
- **Cause**: Non-standard channel naming
- **Solution**: Ensure FCS files use standard naming conventions (FL1-A, FITC-A, etc.)

**Import failures:**
- **Cause**: Corrupted files or invalid format
- **Solution**: Verify FCS files contain actual flow cytometry data

### Performance Issues

**Slow processing:**
- **Solution**: Reduce number of events analyzed or use sampling
- **Solution**: Enable Barnes-Hut approximation for t-SNE

**Browser freezing:**
- **Solution**: Use smaller datasets or increase browser memory allocation
- **Solution**: 
- **Solution**: Close other browser tabs and applications

---

## FAQ

### General Questions

**Q: What file formats does CytoVerse support?**
A: CytoVerse supports .fcs (Flow Cytometry Standard), .csv, .tsv, and .xlsx files. FCS files are recommended for best compatibility across all features.

**Q: What's the maximum file size I can upload?**
A: The current limit is 250MB per upload session. For larger datasets, consider using the batch analysis features or data sampling.

### Technical Questions

**Q: How does the automatic population identification work?**
A: The system analyzes mean marker expression for each cluster and compares values to user-defined thresholds. Populations are named based on positive/negative marker combinations with confidence scores.
However, we are currently working on a way to allow users to specify their own populations to allow for a wider coverage of markers.

**Q: Can I export my gating strategy for use in other software?**
A: Yes, you can export gate definitions as CSV files and population statistics for analysis in other platforms like FlowJo or FCS Express.

**Q: How do I choose between t-SNE and UMAP for dimensionality reduction?**
A: Use t-SNE for discovering local structure and rare populations. Use UMAP when you need to preserve both local and global relationships, or for faster processing of large datasets.

### Workflow Questions

**Q: Do I need to compensate my data before gating?**
A: Yes, compensation should always be performed before gating when using multi-color panels. This removes fluorophore spillover that could affect population identification.

**Q: Can I apply the same gating strategy to multiple samples?**
A: Yes, use the template system in Interactive Gating to save gating strategies and apply them to new samples. For automated processing, use the Batch Analysis module.

**Q: How do I know if my compensation is working correctly?**
A: Check the QC metrics: negative event percentage should be <5%, and you should see improved separation between positive and negative populations in before/after plots.

---
