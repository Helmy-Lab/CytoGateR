---
layout: default
title: Troubleshooting
nav_order: 9
---

<link rel="stylesheet" href="custom.css">

# Troubleshooting
{: .no_toc }

## Table of contents
{: .no_toc .text-delta }

1. TOC
{:toc}

---

## Compensation Issues

### Diagonal Matrix Values ≠ 1.0
{: .label .label-red }

**Cause**: Poor single-stain control quality

**Solution**: 
- Check that controls have bright, positive populations
- Verify single-stain controls contain only one fluorophore
- Ensure controls use the same cell type as experimental samples
- Check PMT voltages are optimized for each channel

### Negative Spillover Values
{: .label .label-red }

**Cause**: Incorrect file assignment or poor controls

**Solution**:
- Verify file assignments match fluorophore channels correctly
- Check control quality - ensure clear positive/negative separation
- Remake controls with proper single-stain preparation
- Verify unstained control is truly unstained

### Spillover Levels >50%
{: .label .label-yellow }

**Cause**: Spectral overlap too high for reliable compensation

**Solution**:
- Consider different fluorophore combinations with less spectral overlap
- Use fluorophores with better spectral separation
- Optimize instrument settings (PMT voltages, laser powers)
- Consider using fewer colors in your panel

### High Negative Events After Compensation
{: .label .label-red }

**Cause**: Over-compensation

**Solution**:
- Reduce spillover values manually in matrix editor
- Check control quality and remake if necessary
- Verify compensation matrix diagonal values are close to 1.0
- Use median rather than mean calculations for spillover

## Gating Issues

### Slow Performance
{: .label .label-yellow }

**Cause**: Large dataset processing

**Solution**:
- Use Static mode for datasets >10,000 events
- Enable data sampling in Raw Data module
- Close other browser tabs to free memory
- Use hexagon plots for better performance with large datasets

### Population Counts Not Updating
{: .label .label-blue }

**Cause**: Display refresh needed

**Solution**:
- Click "Refresh display" to recalculate hierarchy
- Check that gates were properly saved
- Verify parent population selection is correct
- Clear browser cache if issues persist

### Can't See Populations Clearly
{: .label .label-yellow }

**Cause**: Inappropriate visualization settings

**Solution**:
- Try density hotspots or hexagon plots for better visualization
- Adjust point size and transparency settings
- Use appropriate color palettes for your data
- Check data transformation settings

### Gates Not Saving
{: .label .label-red }

**Cause**: Incomplete gating workflow

**Solution**:
- Ensure you click "Save Gate" after drawing each gate
- Check that population name is entered before saving
- Verify you have appropriate browser permissions
- Try refreshing the page and re-drawing the gate

### Sub-gates Appearing Empty
{: .label .label-red }

**Cause**: Incorrect hierarchical setup

**Solution**:
- Check that parent population selection is correct
- Verify parent population contains sufficient events
- Ensure gate boundaries encompass intended cells
- Review gating hierarchy structure

## File Format Issues

### Channels Not Detected
{: .label .label-red }

**Cause**: Non-standard channel naming

**Solution**:
- Ensure FCS files use standard naming conventions (FL1-A, FITC-A, etc.)
- Check parameter descriptions in FCS files
- Verify channel names don't contain special characters
- Use FCS files directly from flow cytometer when possible

### Import Failures
{: .label .label-red }

**Cause**: Corrupted files or invalid format

**Solution**:
- Verify FCS files contain actual flow cytometry data
- Check file size isn't corrupted (partial downloads)
- Try re-exporting files from original software
- Ensure files aren't password-protected or encrypted

### Missing Marker Information
{: .label .label-yellow }

**Cause**: Incomplete FCS file metadata

**Solution**:
- Check parameter descriptions in original software
- Use marker renaming feature to add biological names
- Verify acquisition software properly saved marker information
- Manually map technical names to biological markers

## Performance Issues

### Slow Processing
{: .label .label-yellow }

**Cause**: Computational limitations

**Solution**:
- Reduce number of events analyzed or use sampling
- Enable Barnes-Hut approximation for t-SNE
- Use UMAP instead of t-SNE for large datasets
- Process data in smaller batches

### Browser Freezing
{: .label .label-red }

**Cause**: Memory limitations

**Solution**:
- Use smaller datasets or increase browser memory allocation
- Close other browser tabs and applications
- Restart browser and try again
- Use a computer with more RAM for large datasets

### Out of Memory Errors
{: .label .label-red }

**Cause**: Insufficient system resources

**Solution**:
- Reduce file size through event sampling
- Process files individually rather than in batches
- Use a more powerful computer for analysis
- Clear browser cache and restart

## Analysis Issues

### Clustering Results Don't Make Biological Sense
{: .label .label-yellow }

**Cause**: Inappropriate algorithm or parameters

**Solution**:
- Try different clustering methods (K-means vs. FlowSOM vs. DBSCAN)
- Adjust clustering parameters (epsilon, cluster numbers)
- Check data preprocessing and transformation
- Validate with manual gating approaches

### Populations Not Separating Well
{: .label .label-yellow }

**Cause**: Poor data quality or transformation

**Solution**:
- Check compensation quality first
- Adjust transformation cofactors
- Try different dimensionality reduction methods
- Verify marker panel is appropriate for populations of interest

### Inconsistent Results Between Runs
{: .label .label-blue }

**Cause**: Stochastic algorithms

**Solution**:
- Set random seeds for reproducible results
- Run analysis multiple times to check consistency
- Use deterministic methods when available
- Save successful parameter combinations

## Data Quality Issues

### High Background/Noise
{: .label .label-yellow }

**Cause**: Poor sample preparation or acquisition

**Solution**:
- Check sample preparation protocols
- Verify antibody titrations are optimal
- Review acquisition settings (PMT voltages)
- Use proper controls to identify background

### Poor Signal-to-Noise Ratio
{: .label .label-yellow }

**Cause**: Suboptimal staining or acquisition

**Solution**:
- Optimize antibody concentrations
- Check antibody storage and handling
- Verify cell viability and preparation
- Adjust PMT voltages for better signal

### Batch Effects Between Experiments
{: .label .label-blue }

**Cause**: Technical variation between runs

**Solution**:
- Use consistent protocols across experiments
- Include control samples in each batch
- Monitor instrument performance over time
- Apply batch correction methods if necessary

---

## Getting Help

### When to Contact Support
{: .label .label-green }

- Persistent software crashes or errors
- Features not working as documented
- Questions about analysis interpretation
- Requests for new features or improvements

### Information to Include
{: .label .label-blue }

When reporting issues, please include:
- Browser type and version
- Operating system
- File format and approximate size
- Steps taken before the issue occurred
- Error messages (if any)
- Screenshots of the problem

### Self-Help Resources
{: .label .label-purple }

- **Documentation**: Review relevant sections for your analysis type
- **FAQ**: Check frequently asked questions for common issues
- **Literature**: Consult flow cytometry analysis papers for methodological guidance
- **Community**: Engage with other flow cytometry researchers for advice

### Best Practices for Avoiding Issues
{: .label .label-green }

- **Start simple**: Begin with basic analysis before advanced features
- **Test with controls**: Use known samples to validate your approach
- **Save progress**: Export intermediate results and save parameters
- **Document workflow**: Keep notes on successful parameter combinations
- **Regular updates**: Keep browser updated for best performance

---

## Emergency Troubleshooting

### Application Won't Load
{: .label .label-red }

1. **Clear browser cache** and cookies
2. **Disable browser extensions** temporarily
3. **Try incognito/private browsing** mode
4. **Switch to a different browser**
5. **Check internet connection** stability

### Data Lost During Analysis
{: .label .label-red }

1. **Check browser downloads** folder for auto-saved files
2. **Look for session recovery** options
3. **Re-upload original files** if necessary
4. **Use template system** to quickly recreate analysis
5. **Save work frequently** to prevent future data loss

### Analysis Produces Unexpected Results
{: .label .label-yellow }

1. **Verify input data** quality and format
2. **Check preprocessing** parameters
3. **Compare with manual analysis** methods
4. **Try different algorithms** for validation
5. **Consult literature** for expected results

### Performance Extremely Slow
{: .label .label-yellow }

1. **Reduce dataset size** immediately
2. **Close unnecessary applications**
3. **Try analysis during off-peak hours**
4. **Consider cloud computing** resources
5. **Break analysis into smaller steps**