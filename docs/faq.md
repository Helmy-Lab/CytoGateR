---
layout: default
title: FAQ
nav_order: 10
---

<link rel="stylesheet" href="custom.css">

# Frequently Asked Questions
{: .no_toc }

## Table of contents
{: .no_toc .text-delta }

1. TOC
{:toc}

---

## General Questions

### What file formats does CytoVerse support?
{: .label .label-blue }

**A:** CytoVerse supports .fcs (Flow Cytometry Standard), .csv, .tsv, and .xlsx files. FCS files are recommended for best compatibility across all features.

### What's the maximum file size I can upload?
{: .label .label-blue }

**A:** The current limit is 250MB per upload session. For larger datasets, consider using the batch analysis features or data sampling.

### Can I use CytoVerse for mass cytometry (CyTOF) data?
{: .label .label-blue }

**A:** Yes, CytoVerse works with mass cytometry data stored in FCS format. You may need to adjust transformation parameters for optimal visualization.

### Is CytoVerse free to use?
{: .label .label-green }

**A:** Yes, CytoVerse is freely available for academic and research use. Check with the Helmy Lab for any licensing requirements for commercial applications.

### Do I need to install any software?
{: .label .label-green }

**A:** No, CytoVerse runs entirely in your web browser. No downloads or installations are required.

### Can I use CytoVerse offline?
{: .label .label-yellow }

**A:** No, CytoVerse requires an internet connection to function. All processing occurs on web servers.

## Technical Questions

### How does the automatic population identification work?
{: .label .label-blue }

**A:** The system analyzes mean marker expression for each cluster and compares values to user-defined thresholds. Populations are named based on positive/negative marker combinations with confidence scores. However, we are currently working on a way to allow users to specify their own populations to allow for a wider coverage of markers.

### Can I export my gating strategy for use in other software?
{: .label .label-blue }

**A:** Yes, you can export gate definitions as CSV files and population statistics for analysis in other platforms like FlowJo or FCS Express.

### How do I choose between t-SNE and UMAP for dimensionality reduction?
{: .label .label-blue }

**A:** Use t-SNE for discovering local structure and rare populations. Use UMAP when you need to preserve both local and global relationships, or for faster processing of large datasets.

### What clustering algorithm should I use?
{: .label .label-yellow }

**A:** 
- **K-means**: When you know the approximate number of expected populations
- **FlowSOM**: Specifically designed for flow cytometry data with hierarchical structure
- **DBSCAN**: For populations with irregular shapes or unknown numbers
- **Phenograph**: For identifying rare populations and subtle differences

### Can I analyze spectral flow cytometry data?
{: .label .label-blue }

**A:** Yes, CytoVerse can handle spectral flow cytometry data. Ensure your data is properly unmixed and exported in FCS format from your acquisition software.

### How many markers can I analyze simultaneously?
{: .label .label-green }

**A:** There's no strict limit on marker numbers. However, performance may decrease with very large panels (>40 markers). Consider focusing on relevant markers for your analysis.

## Workflow Questions

### Do I need to compensate my data before gating?
{: .label .label-red }

**A:** Yes, compensation should always be performed before gating when using multi-color panels. This removes fluorophore spillover that could affect population identification.

### Can I apply the same gating strategy to multiple samples?
{: .label .label-blue }

**A:** Yes, use the template system in Interactive Gating to save gating strategies and apply them to new samples. For automated processing, use the Batch Analysis module.

### How do I know if my compensation is working correctly?
{: .label .label-blue }

**A:** Check the QC metrics: negative event percentage should be <5%, and you should see improved separation between positive and negative populations in before/after plots.

### Should I gate before or after clustering?
{: .label .label-yellow }

**A:** Both approaches are valid:
- **Gate first**: Better for focused analysis of known populations
- **Cluster first**: Better for discovery of new or unexpected populations

### Can I combine results from different modules?
{: .label .label-green }

**A:** Yes, data flows between modules. For example, you can use Raw Data preprocessing, apply Interactive Gating, and then perform Batch Analysis on gated populations.

## Data Analysis Questions

### How do I validate my clustering results?
{: .label .label-yellow }

**A:** 
- Compare with manual gating of the same populations
- Check that expression patterns make biological sense
- Use known controls to verify population assignments
- Try multiple clustering methods for consistency

### What should I do if populations don't separate well?
{: .label .label-yellow }

**A:**
- Check compensation quality first
- Adjust transformation cofactors
- Try different dimensionality reduction methods
- Verify your marker panel is appropriate for the populations of interest

### How many events do I need for reliable analysis?
{: .label .label-blue }

**A:** This depends on your analysis goals:
- **Basic gating**: 1,000-10,000 events per population
- **Clustering analysis**: 10,000-100,000 total events
- **Rare population analysis**: 100,000+ events
- **Statistical comparisons**: Adequate events in smallest population of interest

### Can I analyze time-course experiments?
{: .label .label-green }

**A:** Yes, use Batch Analysis to process multiple time points with consistent parameters. You can then compare population frequencies over time.

## Export and Publication Questions

### What format should I use for publication figures?
{: .label .label-blue }

**A:** 
- **PNG**: Good for web and presentations
- **PDF**: Vector format, best for publications
- **High resolution**: Use 300 DPI minimum for print publications

### Can I get publication-quality statistics?
{: .label .label-blue }

**A:** CytoVerse provides population counts and percentages. For advanced statistics (p-values, effect sizes), export data and use statistical software like R, GraphPad Prism, or SPSS.

### How do I cite CytoVerse in my paper?
{: .label .label-green }

**A:** Include the software name, version (if available), and the Helmy Lab as developers. Check with the lab for specific citation requirements and any associated publications.

### Can I include my analysis parameters in publications?
{: .label .label-blue }

**A:** Yes, you should include key parameters for reproducibility:
- Transformation cofactors
- Dimensionality reduction parameters
- Clustering algorithm and settings
- Population identification thresholds

## Troubleshooting Questions

### Why is my analysis running slowly?
{: .label .label-yellow }

**A:** 
- Large file sizes can slow processing
- Try data sampling to reduce computational load
- Close other browser tabs to free memory
- Use Static mode for gating instead of Interactive mode

### My gates aren't saving properly. What should I do?
{: .label .label-red }

**A:**
- Ensure you click "Save Gate" after drawing
- Check that you've entered a population name
- Try refreshing the page and re-drawing
- Clear your browser cache if problems persist

### The compensation matrix looks wrong. How do I fix it?
{: .label .label-red }

**A:**
- Check single-stain control quality
- Verify file assignments are correct
- Ensure controls have clear positive/negative populations
- Try manual matrix editing for fine adjustments

## Advanced Usage Questions

### Can I write scripts to automate CytoVerse analysis?
{: .label .label-yellow }

**A:** Currently, CytoVerse is designed for interactive use through the web interface. Scripting capabilities may be added in future versions.

### How do I handle batch effects in my data?
{: .label .label-yellow }

**A:**
- Use consistent protocols across batches
- Include control samples in each batch
- Monitor for systematic differences between processing days
- Consider normalizing to control samples

### Can I analyze paired samples (before/after treatment)?
{: .label .label-green }

**A:** Yes, use Batch Analysis with appropriate grouping to compare paired samples. Export population statistics for paired statistical testing in external software.

### How do I optimize parameters for my specific experiment type?
{: .label .label-blue }

**A:**
- Start with default parameters
- Use a representative subset of your data for optimization
- Save successful parameter combinations as templates
- Document your optimization process for reproducibility

---

## Still Have Questions?

### Getting Additional Help
{: .label .label-green }

If your question isn't answered here:

1. **Check the documentation**: Review the relevant module documentation
2. **Try the troubleshooting guide**: Many common issues have specific solutions
3. **Contact the Helmy Lab**: Reach out for technical support or feature requests
4. **Join the community**: Connect with other CytoVerse users for tips and advice

### Feature Requests
{: .label .label-purple }

CytoVerse is actively developed. If you have suggestions for new features or improvements:

- Contact the development team with detailed descriptions
- Provide use cases for requested features
- Share example data if relevant to your request

### Reporting Bugs
{: .label .label-red }

When reporting issues:

- Include browser type and version
- Describe steps to reproduce the problem
- Provide error messages if any
- Include screenshots when helpful