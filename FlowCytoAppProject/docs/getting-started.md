---
layout: default
title: Getting Started
nav_order: 2
---

<link rel="stylesheet" href="custom.css">

# Getting Started
{: .no_toc }

## Table of contents
{: .no_toc .text-delta }

1. TOC
{:toc}

---

## System Requirements

CytoGateR runs in your web browser and supports:
- **Browsers**: Chrome, Firefox, Safari, Edge (latest versions)
- **File Formats**: .fcs, .csv, .tsv, .xlsx
- **File Size**: Up to 250MB per upload session

### Recommended Browser Settings

For optimal performance:
- **Enable JavaScript**: Required for all interactive features
- **Allow pop-ups**: Needed for file downloads and exports
- **Sufficient RAM**: 8GB+ recommended for large datasets
- **Stable internet**: Consistent connection for file uploads

## Quick Start Guide

### Step 1: Access CytoGateR
**Access CytoGateR** through your web browser at the provided URL. No downloads or installations required.

### Step 2: Configure Settings
**Start with Settings** to configure global plot preferences:
- Set appropriate plot dimensions for your screen
- Choose font sizes for readability
- Select colorblind-friendly palettes
- Adjust point sizes for your data density

### Step 3: Upload Your Data
**Upload your data** in the Raw Data tab:
- Drag and drop or browse for your FCS files
- Verify file contents and marker information
- Select relevant channels for analysis
- Apply appropriate data transformations

### Step 4: Apply Compensation (if needed)
**Apply compensation** if using multi-color panels:
- Upload single-stain and unstained controls
- Generate or import compensation matrix
- Review matrix quality and edit if necessary
- Export compensated files for downstream analysis

### Step 5: Gate Your Populations
**Gate your populations** using Interactive Gating:
- Start with broad populations (live cells, singlets)
- Use hierarchical gating strategy
- Apply appropriate visualization methods
- Save gating templates for reuse

### Step 6: Analyze and Export
**Analyze and export** your results:
- Apply clustering algorithms if desired
- Generate publication-quality figures
- Export population statistics
- Save analysis parameters for reproducibility

## Understanding the Interface

### Navigation Layout

**Main Navigation Tabs:**
- **Settings**: Global plot and display preferences
- **Raw Data**: Initial data upload and preprocessing
- **Spillover Compensation**: Multi-color panel compensation
- **Interactive Gating**: Manual population definition
- **Processed Data**: Advanced visualization and analysis
- **Batch Analysis**: Multi-sample processing

### Data Flow Between Modules

**Module Integration:**
- Settings apply globally across all modules
- Raw Data preprocessing feeds into all analysis modules
- Compensation results can be used in gating and batch analysis
- Gated populations can be exported for external analysis

## First Analysis Walkthrough

### Scenario: Basic T Cell Analysis

**Objective**: Identify and quantify CD4+ and CD8+ T cell populations

**Required files**:
- Sample FCS file with CD3, CD4, CD8 markers
- Single-stain controls (if compensation needed)
- Unstained control (if compensation needed)

### Step-by-Step Analysis

#### 1. Data Upload
- Check that CD3, CD4, CD8 appear in marker list
- Verify event count is sufficient (>10,000 events)
- Apply arcsinh transformation with standard cofactors

#### 2. Compensation (if multi-color)
- Upload single-stain controls for each fluorophore
- Generate compensation matrix automatically
- Check QC metrics: <5% negative events after compensation

#### 3. Initial Gating
- Start with Forward/Side scatter to exclude debris
- Gate live cells using viability marker (if available)
- Identify lymphocyte population based on scatter properties

#### 4. T Cell Identification
- Gate CD3+ cells from lymphocyte population
- Create CD4 vs CD8 plot on CD3+ population
- Define quadrants: CD4+CD8-, CD4-CD8+, Double Positive, Double Negative

#### 5. Population Analysis
- Review population percentages and event counts
- Verify results make biological sense
- Export gated populations and statistics

### Expected Results

**Typical T cell percentages in blood:**
- CD4+ T cells: 40-60% of T cells
- CD8+ T cells: 20-40% of T cells
- Double positive: <5% (rare in periphery)
- Double negative: 5-15%

## Best Practices for New Users

### Data Preparation
{: .label .label-green }

- **Quality control**: Ensure samples are fresh and properly handled
- **Consistent protocols**: Use identical sample preparation across experiments
- **Appropriate controls**: Include single-stain, unstained, and biological controls
- **Documentation**: Keep detailed records of experimental conditions

### Analysis Strategy
{: .label .label-blue }

- **Start simple**: Begin with basic 2D gating before advanced clustering
- **Validate results**: Compare new methods with established approaches
- **Use controls**: Leverage negative and positive controls for validation
- **Iterate parameters**: Try multiple settings to ensure robust results

### Common Beginner Mistakes
{: .label .label-red }

- **Skipping compensation**: Always compensate multi-color data before analysis
- **Over-gating**: Drawing too many restrictive gates reduces population sizes
- **Ignoring controls**: Controls are essential for proper gate placement
- **Poor file organization**: Keep track of which files correspond to which conditions

### Learning Resources
{: .label .label-purple }

- **Documentation**: Read through relevant module documentation
- **Practice datasets**: Start with simple, well-characterized samples
- **Literature**: Review flow cytometry analysis papers for best practices
- **Community**: Connect with other researchers for advice and troubleshooting

---

## Next Steps

### After Your First Analysis

1. **Explore advanced features**: Try clustering algorithms and batch analysis
2. **Optimize parameters**: Fine-tune settings for your specific data types
3. **Create templates**: Save successful analysis workflows for reuse
4. **Validate findings**: Confirm results with independent methods or samples

### Building Expertise

**Progressive skill development:**
1. **Basic gating**: Master manual population identification
2. **Compensation**: Understand spillover correction principles
3. **Advanced analysis**: Explore clustering and dimensionality reduction
4. **Batch processing**: Scale up to multi-sample experiments
5. **Integration**: Combine CytoGateR with other analysis tools

### Getting Help

**When you need assistance:**
- Check the **Troubleshooting** section for common issues
- Review the **FAQ** for frequently asked questions
- Consult module-specific documentation for detailed guidance
- Contact support for technical issues or feature requests

---

## Welcome to CytoGateR!

You're now ready to begin your flow cytometry analysis journey with CytoGateR. Start with the Settings module to configure your preferences, then move on to the Raw Data module to upload your first dataset.

Remember: **Start simple, validate results, and don't hesitate to experiment** with different approaches to find what works best for your data!