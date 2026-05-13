---
layout: default
title: Processed Data
nav_order: 8
---

<link rel="stylesheet" href="custom.css">

# Processed Data
{: .no_toc }

## Table of contents
{: .no_toc .text-delta }

1. TOC
{:toc}

---

## Overview

This tab provides a range of visualizations for the processed data. The sidebar includes adjustable parameters that allow users to customize and generate the desired plot. Upon uploading a file in .fcs, .csv, or .xlsx format, the data can be explored using the selected dimensionality reduction method. By clicking on the dropdown menu, dimensionality reduction methods such as t-SNE, UMAP, PCA or MDS can be chosen. Based on the dimensionality reduction technique chosen, additional parameters can be set. For example, if t-SNE is selected as the dimensionality reduction method, the perplexity parameter and number of clusters can be adjusted accordingly. The plot width and length can also be adjusted by dragging the pointer along the scale.

## Side Panel Controls

### File Upload and Exploration

Upon uploading a file in **.fcs, .csv, or .xlsx format**, the data can be explored using the selected dimensionality reduction method.

### Dimensionality Reduction Selection

By clicking on the **dropdown menu**, dimensionality reduction methods can be chosen:
- **t-SNE**: Best for revealing local structure and rare populations
- **UMAP**: Balances local and global structure preservation
- **PCA**: Linear method for noise reduction and overview
- **MDS**: Classical multidimensional scaling for distance preservation

### Method-Specific Parameters

Based on the dimensionality reduction technique chosen, additional parameters can be set. For example, if **t-SNE is selected** as the dimensionality reduction method, the **perplexity parameter and number of clusters** can be adjusted accordingly.

#### t-SNE Parameters
- **Perplexity**: Controls the balance between local and global structure
  - Low values (5-15): Focus on local structure, may create fragmented clusters
  - High values (30-50): Preserve global structure, may merge distinct populations
- **Barnes-Hut approximation**: Computational optimization for large datasets
- **Max iterations**: Number of optimization steps (typically 1000-5000)

#### UMAP Parameters
- **N-neighbors**: Controls local neighborhood size
  - Small values: Focus on local structure
  - Large values: Preserve global relationships
- **Min distance**: Minimum distance between points in low-dimensional space
- **Metric**: Distance function used in high-dimensional space

#### PCA Parameters
- **Number of components**: How many principal components to calculate
  - 2 components: Standard 2D visualization
  - 3+ components: For advanced analysis or 3D visualization

#### MDS Parameters
- **Metric**: Whether to use metric or non-metric MDS
- **Dimensions**: Number of output dimensions (typically 2 or 3)

### Plot Customization

- **Plot dimensions**: The plot width and length can be adjusted by **dragging the pointer along the scale**
- **Visual parameters**: Modify point size, colors, and other aesthetic elements
- **Real-time updates**: Parameters apply immediately to visualization

#### Visualization Options
- **Point size**: Adjust for data density and visibility
- **Transparency**: Control point opacity for overlapping data
- **Color schemes**: Choose from scientific color palettes
- **Axis labels**: Customize dimension labels and scaling

## Data Preview

The Data Preview tab displays your uploaded dataset with the parameters that were chosen and applied to the dataset. This allows you to:
- **Verify data integrity**: Ensure your data loaded correctly
- **Check transformations**: See the effects of applied preprocessing steps
- **Review parameters**: Confirm that chosen parameters are appropriate for your data
- **Quality assessment**: Identify any issues before proceeding with analysis

### Preview Features

**Data table display:**
- First 100 rows of your dataset
- All columns with proper headers
- Data type verification (numeric vs. categorical)
- Missing value identification

**Summary statistics:**
- Sample size (number of events)
- Number of markers/channels
- Data ranges and distributions
- Quality metrics

## Analysis Workflow

### Step 1: Data Upload
1. Select your processed data file (.fcs, .csv, or .xlsx)
2. Verify successful upload in Data Preview
3. Check data structure and quality

### Step 2: Method Selection
1. Choose appropriate dimensionality reduction method
2. Consider your analysis goals:
   - **Exploration**: Use t-SNE for discovering new patterns
   - **Speed**: Use UMAP for large datasets
   - **Linearity**: Use PCA when linear relationships matter
   - **Distance preservation**: Use MDS for metric relationships

### Step 3: Parameter Optimization
1. Start with default parameters
2. Adjust based on data characteristics:
   - **Large datasets**: Enable approximations, reduce iterations
   - **Small datasets**: Use exact methods, increase iterations
   - **Complex structure**: Adjust perplexity/neighbors accordingly

### Step 4: Visualization
1. Generate initial plot with chosen parameters
2. Adjust plot aesthetics for clarity
3. Export high-quality figures for publication

---

## Processed Data Best Practices

### Method Selection Guidelines
{: .label .label-green }

- **t-SNE for discovery**: Best when exploring unknown population structures
- **UMAP for balance**: Good compromise between local and global structure
- **PCA for overview**: Ideal for initial data exploration and linear relationships
- **MDS for distances**: When preserving original distances is important

### Parameter Optimization
{: .label .label-blue }

- **Start conservative**: Begin with default parameters before fine-tuning
- **Iterative approach**: Make small parameter changes and observe effects
- **Biological validation**: Ensure results make biological sense
- **Multiple runs**: Try different random seeds for consistency

### Visualization Best Practices
{: .label .label-yellow }

- **Appropriate scaling**: Ensure plots are readable at intended size
- **Color choices**: Use colorblind-friendly palettes for accessibility
- **Point density**: Adjust transparency and size for data density
- **Export quality**: Use high resolution for publication figures

### Common Pitfalls
{: .label .label-red }

- **Over-interpretation**: Remember that dimensionality reduction can create artificial patterns
- **Parameter dependence**: Results can vary significantly with parameter choices
- **Batch effects**: Consider technical variation when comparing across experiments
- **Computational limits**: Large datasets may require approximations that affect results

## Troubleshooting

### File Upload Issues

**File not recognized:**
- Verify file format (.fcs, .csv, .xlsx)
- Check file size limits (250MB maximum)
- Ensure file is not corrupted or truncated

**Missing data columns:**
- Verify column headers are present
- Check for special characters in column names
- Ensure numeric data is properly formatted

### Visualization Problems

**Poor separation of populations:**
- Try different dimensionality reduction methods
- Adjust method-specific parameters
- Check data quality and preprocessing
- Consider data transformation needs

**Slow performance:**
- Reduce dataset size through sampling
- Enable approximation algorithms
- Use fewer iterations for initial exploration
- Consider processing in batches

### Interpretation Challenges

**Unexpected patterns:**
- Verify preprocessing steps were appropriate
- Check for batch effects or technical artifacts
- Compare with known biological controls
- Validate with alternative analysis methods

**Inconsistent results:**
- Use fixed random seeds for reproducibility
- Try multiple parameter settings
- Validate with independent datasets
- Consider biological and technical replicates

---

## Advanced Features

### Custom Analysis Pipelines
{: .label .label-purple }

- **Scripted workflows**: Save parameter combinations for reproducible analysis
- **Batch processing**: Apply same parameters to multiple datasets
- **Quality metrics**: Automated assessment of dimensionality reduction quality
- **Export options**: Multiple formats for downstream analysis

### Integration with Other Modules
{: .label .label-green }

- **Raw data import**: Seamlessly work with preprocessed data from Raw Data module
- **Gating integration**: Import gated populations for focused analysis
- **Batch analysis**: Compare processed results across experimental groups
- **Export compatibility**: Generate data for external analysis tools

### Publication-Ready Outputs
{: .label .label-blue }

- **High-resolution exports**: Vector graphics for scalable figures
- **Customizable aesthetics**: Publication-standard formatting options
- **Statistical overlays**: Add statistical information to visualizations
- **Multi-panel figures**: Combine multiple analyses in single figure