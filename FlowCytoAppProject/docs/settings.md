---
layout: default
title: Settings
nav_order: 3
---

<link rel="stylesheet" href="custom.css">

# Settings
{: .no_toc }

## Table of contents
{: .no_toc .text-delta }

1. TOC
{:toc}

---

## Global Plot Settings

<img width="852" height="898" alt="image" src="https://github.com/user-attachments/assets/a76122e9-750e-4ce9-9c3d-df679f219a53" />

This allows the plots to be visually adjusted. The width and height of the plot can be changed accordingly by dragging the pointer along the scale. Similarly, by dragging on the pointer on the scale, moving left or right the font size as well as the sizes of the points on the graph can be changed. Finally, the color palette can be customized by selecting the color circle next to the palette selector. The tool offers a variety of options, including colorblind-friendly palettes, as well as categorical, sequential, and diverging schemes to enhance data interpretation.

## Plot Dimensions

### Width and Height Settings

- **Width/Height**: Drag the pointer along the scale to adjust plot size (default: 600x600 pixels)
- **Range**: 400-1200 pixels for both width and height
- **Optimal for presentations**: 800x800 pixels
- **Optimal for publications**: 600x600 pixels (fits journal requirements)
- **Optimal for posters**: 1000x1000 pixels for better visibility

### Aspect Ratio Considerations

**Square plots (1:1 ratio):**
- Best for scatter plots and most flow cytometry visualizations
- Maintains proper data representation
- Standard for publication figures

**Rectangular plots:**
- Useful for time-course or multi-panel figures
- Consider data distribution when choosing dimensions
- May be appropriate for specific visualization types

## Typography

### Font Size Controls

- **Font Size**: Drag the pointer on the scale left or right to adjust text size for better readability (range: 8-24px)
- **Default**: 16px for most applications
- **Presentations**: 18-20px for better visibility from distance
- **Publications**: 12-14px to fit journal requirements
- **Poster presentations**: 20-24px for large format display

### Point Size Settings

- **Point Size**: Drag the pointer to control data point visibility (range: 1-12px)
- **Dense data**: Smaller point sizes (2-4px) prevent overcrowding
- **Sparse data**: Larger point sizes (6-8px) improve visibility
- **High-resolution displays**: May require larger point sizes
- **Print publications**: Consider final figure size when setting point size

### Typography Best Practices

**Readability guidelines:**
- Ensure text is legible at intended viewing size
- Maintain consistent font sizes across related figures
- Consider your audience and presentation medium
- Test readability at final output size

## Color Palettes

### Palette Selection

Customize by selecting the **color circle next to the palette selector**. Choose from scientifically-optimized color schemes:

### Sequential Palettes

**Viridis family:**
- **Viridis**: Blue to green to yellow progression
- **Plasma**: Purple to pink to yellow gradient  
- **Magma**: Black to purple to white transition
- **Inferno**: Black to red to yellow progression

**Best for**: Density plots, continuous data, heatmaps

### Categorical Palettes

**Discrete color schemes:**
- **Set1/Set2**: Distinct colors for population identification
- **Paired**: Related but distinguishable color pairs
- **Dark2**: High contrast colors for clear separation

**Best for**: Population identification, group comparisons, discrete data

### Diverging Palettes

**Two-color progressions:**
- **RdYlBu**: Red-Yellow-Blue for fold-change data
- **BrBG**: Brown-Blue-Green for comparative analysis
- **PiYG**: Pink-Yellow-Green for differential expression

**Best for**: Fold-change analysis, comparative studies, before/after comparisons

### Colorblind-Friendly Options

**Accessibility features:**
- **Viridis family**: Designed for colorblind accessibility
- **ColorBrewer palettes**: Tested for colorblind visibility
- **High contrast options**: Ensure visibility for all users
- **Avoid red-green combinations**: Common colorblindness type

### Scientific Color Guidelines

**Publication standards:**
- Use perceptually uniform color scales
- Avoid rainbow/jet colormaps (not perceptually uniform)
- Ensure colors reproduce well in grayscale
- Consider journal-specific color requirements

## Settings Workflow

### Initial Configuration

**First-time setup:**
1. **Set plot dimensions** based on your typical output needs
2. **Choose font sizes** appropriate for your presentation medium
3. **Select default color palette** for your research area
4. **Test settings** with a sample dataset

### Project-Specific Adjustments

**Adapting for different uses:**
- **Presentations**: Larger fonts and point sizes
- **Publications**: Journal-specific dimensions and fonts
- **Posters**: Maximum visibility settings
- **Web display**: Optimize for screen viewing

### Saving and Loading Settings

**Settings persistence:**
- Settings are saved automatically in your browser
- Export settings configurations for sharing
- Load standard lab settings for consistency
- Create setting templates for different project types

---

## Advanced Settings Tips

### Performance Optimization
{: .label .label-green }

- **Large datasets**: Use smaller point sizes to improve rendering
- **High-resolution displays**: Increase font and point sizes proportionally
- **Slow computers**: Reduce plot dimensions to improve performance
- **Memory constraints**: Use simpler color schemes

### Publication Preparation
{: .label .label-blue }

- **Journal requirements**: Check specific font and size requirements
- **Color specifications**: Verify color palette meets journal standards
- **Resolution settings**: Ensure adequate DPI for print quality
- **Consistency**: Use identical settings across all figures in a paper

### Presentation Optimization
{: .label .label-yellow }

- **Audience size**: Larger audiences need bigger fonts and points
- **Projection quality**: Test visibility with actual projection setup
- **Lighting conditions**: Bright rooms may need high-contrast colors
- **Backup plans**: Prepare high-contrast versions for poor projection

### Accessibility Considerations
{: .label .label-purple }

- **Colorblind users**: Always use colorblind-friendly palettes
- **Visual impairment**: Provide high-contrast alternatives
- **Screen readers**: Ensure plots have descriptive alt text
- **Multiple formats**: Offer both color and grayscale versions

## Troubleshooting Settings

### Common Issues

**Fonts too small/large:**
- Adjust font size slider gradually
- Test at intended viewing distance
- Consider final output medium

**Colors not distinguishable:**
- Switch to higher contrast palette
- Increase point sizes for better visibility
- Test with colorblind simulation tools

**Poor performance:**
- Reduce plot dimensions
- Use simpler color schemes
- Decrease point sizes for large datasets

### Settings Not Saving

**Browser issues:**
- Clear browser cache and reload
- Check if cookies are enabled
- Try incognito/private mode to test
- Update browser to latest version

### Export Quality Issues

**Low resolution output:**
- Increase plot dimensions before export
- Use vector formats when available
- Check export settings in browser
- Consider external screenshot tools for higher quality

---

## Settings Best Practices

### Consistency Guidelines
{: .label .label-green }

- **Within projects**: Use identical settings for related analyses
- **Lab standards**: Establish common settings for team consistency
- **Publication series**: Maintain visual consistency across related papers
- **Template usage**: Create and share setting templates

### Optimization Strategies
{: .label .label-blue }

- **Test early**: Verify settings with sample data before full analysis
- **Document choices**: Record settings used for important analyses
- **Version control**: Save different setting configurations for different uses
- **Collaboration**: Share optimized settings with colleagues

### Quality Assurance
{: .label .label-yellow }

- **Multiple outputs**: Test settings for different export formats
- **Different devices**: Check appearance on various screens and printers
- **Accessibility testing**: Verify visibility for all potential users
- **Peer review**: Have colleagues check readability and clarity