---
layout: default
title: Interactive Gating
nav_order: 5
---

<link rel="stylesheet" href="custom.css">

# Interactive Gating
{: .no_toc }

## Table of contents
{: .no_toc .text-delta }

1. TOC
{:toc}

---

## Overview

**What is gating?**
Gating is the process of selecting specific cell populations based on their fluorescence or scatter properties. You draw boundaries (gates) around populations of interest to separate them from other cells for analysis.

**Why use hierarchical gating?**
Start with broad populations (e.g., live cells) and progressively refine to specific subsets (e.g., CD4+ T cells). This ensures accurate population identification and reduces background interference.

## Step 1: Setup Data Source

**Choose one:**
- Use preprocessed data from Raw Data tab
- Load new FCS files directly

<img width="600" height="347" alt="image" src="https://github.com/user-attachments/assets/81e703fa-a801-4cdb-96fc-6549d75494ee" />

**Select parameters:**
- Sample from dropdown
- X and Y axis channels for 2D plots

<img width="563" height="328" alt="image" src="https://github.com/user-attachments/assets/65421dbe-251e-407a-a16c-373c36e51932" />

## Step 2: Create Gates

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

## Step 3: Visualization Options

**Plot modes explained:**
- **Static mode**: Shows all events simultaneously, better performance for large datasets (>10,000 events). Recommended for most gating tasks
- **Interactive mode**: Limited to 10,000 randomly sampled events but provides full zoom, pan, and selection tools. Use when you need precise navigation

**Plot types and when to use them:**
- **Scatter plot**: Basic dot plot showing individual events. Best for small datasets or when you need to see every single event
- **Density hotspots**: Points colored by local cell density (red = high density, blue = low). Excellent for identifying main populations and rare events
- **Hotspots + contours**: Combines density coloring with population boundary lines. Ideal for complex samples with multiple overlapping populations
- **Hexagon plot**: Divides plot into hexagonal bins showing event counts. Essential for very large datasets (>100,000 events) where individual points would overlap

## Step 4: Hierarchical Gating

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

## Step 5: Population Analysis

**Real-time statistics:**
- Event counts per population (updates as you modify gates)
- Percentage of parent and total events
- Hierarchical organization showing parent-child relationships

**Data extraction explained:**
- **Select specific populations**: Choose which gated populations to export for further analysis
- **Extract as flowSet objects**: FlowSet is a standard R/Bioconductor data structure containing flow cytometry data that can be used in downstream analysis software
- **Apply all gates**: Ensures your extracted data includes all gating decisions in the hierarchy

<img width="593" height="887" alt="image" src="https://github.com/user-attachments/assets/e4f758af-6b52-4fc7-89fd-18e148dfeb4a" />

## Step 6: Export & Templates

**Export options:**
- **Gated FCS files**: Individual populations as new files
- **Gate definitions**: Coordinates and parameters (CSV)
- **Population statistics**: Counts and percentages

<img width="565" height="285" alt="image" src="https://github.com/user-attachments/assets/b2c7c72a-fa48-4f6a-843c-7d768f7a7ef2" />

**Template management:**
- Save gating strategies for reuse
- Load templates for standardized workflows

---

## Gating Best Practices

### Sequential Gating Strategy
{: .label .label-green }

1. **Start broad**: Begin with FSC/SSC to exclude debris and doublets
2. **Viability gating**: Use live/dead markers if available
3. **Major lineages**: Gate on CD45+ for leukocytes
4. **Specific populations**: Progressively narrow to your populations of interest
5. **Functional markers**: Finally, gate on activation or functional markers

### Gate Drawing Tips
{: .label .label-blue }

- **Use appropriate gate types**: Polygonal for irregular populations, rectangular for simple separations
- **Leave buffer space**: Don't draw gates too tightly around populations
- **Check all quadrants**: Ensure your gates capture the intended populations
- **Validate with controls**: Use negative controls to set proper gate boundaries

### Common Gating Mistakes
{: .label .label-red }

- **Over-gating**: Drawing too many restrictive gates can artificially reduce population sizes
- **Under-gating**: Too loose gates include unwanted events
- **Ignoring hierarchy**: Skipping important intermediate populations can affect downstream analysis
- **Static gates**: Not adjusting gates for biological variation between samples

### Performance Optimization
{: .label .label-yellow }

- **Use Static mode**: Better performance for large datasets
- **Sample your data**: Use subset of events for initial gate design
- **Refresh regularly**: Update population counts after making changes
- **Save frequently**: Use templates to preserve your gating strategies