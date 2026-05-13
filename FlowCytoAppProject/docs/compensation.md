---
layout: default
title: Spillover Compensation
nav_order: 4
---

<link rel="stylesheet" href="custom.css">

# Spillover Compensation
{: .no_toc }

## Table of contents
{: .no_toc .text-delta }

1. TOC
{:toc}

---

## Overview

**What is spillover compensation?**
Spillover occurs when fluorophores emit light that bleeds into adjacent detection channels, creating false positive signals. Compensation mathematically corrects this by subtracting the spillover signal from each channel based on control measurements.

## Step 1: Upload Files

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

## Step 2: Assign File Roles

1. **Select unstained control** from uploaded files
2. **Assign single-stain controls** to each detected channel
3. Click **"Validate File Assignment"** to save assignments

<img width="1221" height="1213" alt="image" src="https://github.com/user-attachments/assets/14089d2c-f4a4-49a6-955e-0498632863ec" />

<img width="1212" height="360" alt="image" src="https://github.com/user-attachments/assets/57c6eb54-eb38-45f3-8138-16a9bbdd2b13" />

<img width="1244" height="742" alt="image" src="https://github.com/user-attachments/assets/11496d30-8278-48dc-8a71-0f62b1eebb05" />

## Step 3: Generate Compensation Matrix

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

## Step 4: Review & Edit Matrix

- **Color coding:**
  - 🟢 Green: Low spillover (0-1%)
  - 🟡 Yellow: Moderate (1-5%)
  - 🟠 Orange: High (5-10%)
  - 🔴 Red: Very high (>10%)
- Enable editing to modify values manually
- Click **"Reset to Original"** to undo changes

<img width="566" height="450" alt="image" src="https://github.com/user-attachments/assets/7431a498-d970-477b-8d84-75463e0f978a" />

## Step 5: Quality Control

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

## Step 6: Export Results

- **Compensated FCS files**: Download with "_compensated" suffix
- **Spillover matrix**: Export as CSV or Excel
- **Session data**: Save complete workflow for later use

<img width="1222" height="865" alt="image" src="https://github.com/user-attachments/assets/2f639b97-cee0-4d19-95b4-d6e010694416" />

---

## Tips for Successful Compensation

### Control Quality
{: .label .label-green }

- **Bright positive populations**: Single-stain controls should have clear positive and negative populations
- **Same cell type**: Use the same cell preparation for all controls and samples
- **Fresh samples**: Avoid using old or degraded control samples

### Common Issues
{: .label .label-yellow }

- **High spillover values (>50%)**: Consider different fluorophore combinations with less spectral overlap
- **Negative populations after compensation**: Usually indicates over-compensation - reduce spillover values manually
- **Poor separation**: May need to adjust PMT voltages or use different fluorophores

### Best Practices
{: .label .label-blue }

- **Plan your panel**: Choose fluorophores with minimal spectral overlap
- **Titrate antibodies**: Use optimal concentrations to maximize signal-to-noise
- **Save your matrix**: Export compensation matrices for consistent analysis across experiments