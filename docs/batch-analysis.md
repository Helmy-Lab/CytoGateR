---
layout: default
title: Batch Analysis
nav_order: 7
---

<link rel="stylesheet" href="custom.css">

# Batch Analysis
{: .no_toc }

## Table of contents
{: .no_toc .text-delta }

1. TOC
{:toc}

---

## Overview

**What is batch analysis?**
Batch analysis allows you to process multiple flow cytometry samples simultaneously with standardized parameters. This ensures consistent analysis across experimental groups and enables statistical comparisons between samples and conditions.

**Why use batch analysis?**
- **Consistency**: Apply identical processing parameters across all samples
- **Efficiency**: Process dozens of samples at once instead of one by one
- **Comparison**: Generate standardized results for statistical analysis between groups
- **Reproducibility**: Save processing templates for future experiments

## Step 1: Sample Management

### Upload Multiple Files

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

## Step 2: Configure Batch Processing Parameters

### Preprocessing Options

**Data transformation**: Choose arcsinh transformation with configurable cofactors
**Channel selection**: Select which fluorescence channels to include in analysis
**Event sampling**: Set number of events to analyze per sample (for performance)

<img width="599" height="565" alt="image" src="https://github.com/user-attachments/assets/96e3a29e-3e76-4ee5-87e9-ad564c8e4cc2" />

### Quality Control Gating

**Debris Removal:**
**Debris and dead cell gating can be enabled by selecting the checkbox** next to the option.

<img width="589" height="345" alt="image" src="https://github.com/user-attachments/assets/3dd963d6-4573-4178-a0db-39b317a095f2" />

## Step 3: Dimensionality Reduction

**Choose reduction method** for visualization:

<img width="586" height="651" alt="image" src="https://github.com/user-attachments/assets/ae4e3740-1885-4ae8-9618-bffddc2a06ed" />

### t-SNE Parameters

- **Perplexity**: Controls local vs. global structure balance (default: 30, range: 5-50)
- **Barnes-Hut approximation**: Speeds up processing for large datasets
- **Barnes-Hut theta**: Accuracy vs. speed trade-off (0-1, higher = faster but less accurate)
- **Max iterations**: Number of optimization steps (default: 1000)

<img width="559" height="412" alt="image" src="https://github.com/user-attachments/assets/b5ffb4fd-3783-47f8-ba6f-fd9cbb57fe66" />

### UMAP Parameters

- **N-neighbors**: Controls local neighborhood size (default: 15, affects local vs. global structure)
- **Min distance**: Minimum distance between points in embedding
- **Metric**: Distance metric for high-dimensional space

<img width="563" height="176" alt="image" src="https://github.com/user-attachments/assets/d689f0e2-0470-4885-9d08-cf1fd8ad2fb9" />

### PCA Parameters

- **Number of components**: How many principal components to calculate (default: 2 for visualization)

<img width="564" height="161" alt="image" src="https://github.com/user-attachments/assets/6a920fd1-792c-4723-9bfd-99013a761cf7" />

## Step 4: Clustering Analysis

**Enable clustering** to identify cell populations automatically:

### Clustering Methods

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

## Step 5: Population Identification

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

## Step 6: Visualization and Results

### Plot Options

- **Show cluster labels**: Display cluster numbers on plots
- **Show population labels**: Display identified population names
- **Color by**: Choose to color by cluster ID, population, or sample group
- **Point density visualization**: Show cell density within populations

<img width="1803" height="1055" alt="image" src="https://github.com/user-attachments/assets/b61c3f4a-8605-4379-8a60-45fa593e1021" />

<img width="1814" height="1056" alt="image" src="https://github.com/user-attachments/assets/eb97ec89-c027-4585-b7b3-193dfc7ddc50" />

### Cluster Management

- **Merge clusters**: Combine similar clusters manually if needed
- **Edit population names**: Modify automatic population assignments
- **View cluster statistics**: See event counts and percentages per cluster/population

## Step 7: Comparative Analysis

### Group Comparisons

- **Population frequencies**: Compare population percentages between experimental groups
- **Statistical testing**: Automatic statistical comparisons (t-tests, ANOVA)
- **Fold change analysis**: Calculate population changes between conditions
- **Visualization**: Generate comparison plots and summary tables

### Batch-wide Statistics

- **Consistency metrics**: Measure how similar samples are within groups
- **Quality control metrics**: Identify outlier samples
- **Population correlation**: See which populations change together across samples

## Step 8: Export and Templates

### Export Options

- **Processed sample data**: Download all samples with cluster assignments
- **Population statistics**: Export population counts and percentages for all samples
- **Batch summary**: Combined statistics across all samples and groups
- **Cluster definitions**: Save cluster centers and parameters
- **Visualization plots**: Export plots as PDF or PNG

### Template Management

- **Save processing template**: Save all parameters for reuse on future experiments
- **Load template**: Apply saved parameter sets to new batches
- **Template library**: Organize templates by experiment type or research question

---

## Batch Analysis Best Practices

### Experimental Design
{: .label .label-green }

- **Balanced groups**: Ensure adequate sample sizes in each experimental group
- **Consistent processing**: Use identical sample preparation protocols across batches
- **Control samples**: Include appropriate negative and positive controls
- **Randomization**: Randomize sample processing order to minimize batch effects

### Parameter Selection
{: .label .label-blue }

- **Conservative settings**: Start with standard parameters that work across most samples
- **Consistent transformation**: Use the same cofactors for all samples in the batch
- **Appropriate sampling**: Balance analysis depth with computational efficiency
- **Template usage**: Save successful parameter sets for future experiments

### Quality Control
{: .label .label-yellow }

- **Monitor processing**: Check that all samples complete analysis successfully
- **Review statistics**: Examine population frequencies for biological plausibility
- **Identify outliers**: Flag samples with unusual patterns for manual review
- **Batch effects**: Look for systematic differences between processing batches

### Statistical Considerations
{: .label .label-red }

- **Multiple testing**: Apply appropriate corrections for multiple comparisons
- **Effect sizes**: Consider biological significance, not just statistical significance
- **Replication**: Ensure adequate biological replicates for meaningful conclusions
- **Validation**: Confirm key findings with independent validation experiments

## Troubleshooting

### Processing Issues

**Samples failing to process:**
- Check file format and integrity
- Verify sufficient memory/processing power
- Reduce event sampling for large files
- Check for corrupted or truncated files

**Inconsistent results across samples:**
- Verify identical preprocessing parameters
- Check for batch effects in sample preparation
- Ensure consistent marker panel across samples
- Review outlier samples for technical issues

### Performance Optimization

**Slow processing:**
- Reduce number of events per sample
- Use fewer dimensionality reduction iterations
- Consider processing samples in smaller batches
- Enable sampling for initial parameter optimization

**Memory issues:**
- Process smaller batches of samples
- Reduce event sampling size
- Close other applications to free memory
- Consider processing on more powerful hardware

### Biological Interpretation

**Unexpected population patterns:**
- Verify marker panel is appropriate for sample types
- Check for technical artifacts in sample preparation
- Compare with single-sample analysis for validation
- Review literature for expected population frequencies

**Poor population separation:**
- Adjust clustering parameters for your data type
- Try different clustering algorithms
- Increase number of markers for better resolution
- Check data transformation settings