---
layout: default
title: Marker Heatmaps
parent: Raw Data Analysis
nav_order: 1
---

<link rel="stylesheet" href="custom.css">

# Marker Heatmaps
{: .no_toc }

## Table of contents
{: .no_toc .text-delta }

1. TOC
{:toc}

---

## What are marker heatmaps?

Marker heatmaps visualize the intensity profiles of fluorescence markers across different cell clusters. They provide a comprehensive overview of how each marker is expressed in each identified population, making it easy to characterize and compare cell clusters.

## When to use marker heatmaps

- **After clustering analysis** to understand what makes each cluster unique
- **For population characterization** to identify cell types based on marker expression patterns
- **For validation** to confirm that clustering results make biological sense
- **For publication figures** showing clear population signatures

## Accessing Marker Heatmaps

1. **Enable clustering analysis** in the Raw Data tab sidebar
2. **Run your clustering analysis** (K-means, DBSCAN, or FlowSOM)
3. **Navigate to the "Cluster Profiles" tab** in the Cluster Analysis Results section
4. **View the heatmap** showing "Cluster Intensity Profiles"

## Understanding the Heatmap

### Visual Elements

**Rows**: Represent different cell clusters identified by your clustering algorithm

**Columns**: Represent different fluorescence markers from your panel

**Colors**: Show expression intensity levels
- **Dark blue/navy**: Low expression (negative population)
- **White**: Moderate/baseline expression
- **Red/firebrick**: High expression (positive population)

### Key Features

- **Z-score normalization**: Expression values are standardized across markers for fair comparison
- **Cluster labels**: Rows show cluster numbers and identified population names (if population identification is enabled)
- **Marker names**: Column headers show the fluorescence channels/markers
- **Expression patterns**: Visual patterns help identify population signatures

## Interpreting Results

### Population Signatures

**Bright red patterns**: Indicate markers that are highly expressed in that cluster

**Dark blue patterns**: Show markers that are low/negative in that cluster

**Mixed patterns**: Reveal complex phenotypes with multiple positive/negative markers

### Biological Interpretation

**T cell populations**: Look for CD3+ patterns with varying CD4/CD8 expression

**B cell populations**: Identify CD19+ or CD20+ signatures

**Monocyte populations**: Find CD14+ or CD16+ expression patterns

**NK cells**: Look for CD56+ CD3- patterns

### Quality Assessment

**Clear separation**: Good clustering shows distinct expression patterns between clusters

**Biological relevance**: Expression patterns should match known cell type signatures

**Consistency**: Similar clusters should have similar heatmap patterns

## Advanced Features

### Population Identification Integration

When population identification is enabled, cluster rows are labeled with predicted cell types (e.g., "Cluster 1 (CD4+ T cells)"), making interpretation easier.

### Customization Options

- **Font size**: Adjusted through global plot settings
- **Color scaling**: Automatically optimized for your data range
- **Cluster ordering**: Ordered by similarity for easier interpretation

### Export and Analysis

- **Right-click to save** the heatmap as an image file
- **Use for publications**: High-quality figures suitable for papers
- **Compare with literature**: Validate your population signatures against known phenotypes

---

## Marker Heatmap Best Practices

### Data Preparation
{: .label .label-green }

- **Quality clustering**: Ensure your clustering analysis produced meaningful, well-separated populations
- **Appropriate markers**: Include relevant lineage and functional markers in your panel
- **Sufficient events**: Have enough cells per cluster for reliable mean calculations

### Interpretation Guidelines
{: .label .label-blue }

- **Know your markers**: Understand the biological significance of each marker in your panel
- **Literature comparison**: Compare patterns with published cell type signatures
- **Positive/negative thresholds**: Use biological knowledge to interpret expression levels
- **Context matters**: Consider the tissue source and experimental conditions

### Validation Strategies
{: .label .label-yellow }

- **Manual gating**: Compare heatmap results with traditional gating approaches
- **Control samples**: Use known cell type controls to validate population assignments
- **Functional assays**: Confirm population identity with functional experiments
- **Multiple clustering methods**: Try different algorithms to ensure robust results

### Common Pitfalls
{: .label .label-red }

- **Over-interpretation**: Don't assign cell types based on single markers
- **Technical artifacts**: Be aware that some patterns may reflect technical rather than biological variation
- **Batch effects**: Consider sample preparation differences when comparing across experiments
- **Rare populations**: Small clusters may have unreliable mean expression values

## Troubleshooting

### No heatmap displayed
- Ensure clustering analysis is enabled and has been run
- Check that you have multiple fluorescence markers in your data
- Verify that clustering produced multiple clusters

### Unclear patterns
- Try different clustering methods (K-means vs. FlowSOM vs. DBSCAN)
- Adjust cluster numbers to get more distinct populations
- Consider data transformation settings

### Biological validation
- Compare expression patterns with known cell type signatures
- Check that positive/negative patterns make biological sense
- Validate results with traditional gating approaches

---

## Example Interpretations

### T Cell Populations
{: .label .label-purple }

- **CD4+ T helpers**: CD3+ CD4+ CD8- pattern
- **CD8+ cytotoxic**: CD3+ CD4- CD8+ pattern
- **Regulatory T cells**: CD3+ CD4+ Foxp3+ pattern
- **Memory T cells**: CD3+ CD45RO+ CD45RA- pattern

### Myeloid Populations
{: .label .label-green }

- **Classical monocytes**: CD14+ CD16- pattern
- **Non-classical monocytes**: CD14+ CD16+ pattern
- **Dendritic cells**: CD11c+ HLA-DR+ pattern
- **Neutrophils**: CD15+ CD16+ CD14- pattern

### B Cell Populations
{: .label .label-blue }

- **Naive B cells**: CD19+ CD27- IgD+ pattern
- **Memory B cells**: CD19+ CD27+ IgD- pattern
- **Plasma cells**: CD19+ CD38+ CD27+ pattern
- **Transitional B cells**: CD19+ CD24+ CD38+ pattern