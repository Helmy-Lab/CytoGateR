/* Custom JavaScript for Flow Cytometry Analysis Tool */

// Function to adjust fonts in plotly graphs
function adjustPlotlyFonts() {
  // NOTE: We're no longer overriding font sizes because R is correctly setting them
  // This function will now only make minor layout adjustments
  
  // Make sure all title elements have consistent font weight
  const titleElements = document.querySelectorAll('.gtitle, .plotly .gtitle');
  titleElements.forEach(function(element) {
    if (element) {
      element.style.fontWeight = 'bold';
    }
  });
  
  // Make sure axis titles have consistent font weight
  const axisTitleElements = document.querySelectorAll('.xtitle, .ytitle, .plotly .xtitle, .plotly .ytitle');
  axisTitleElements.forEach(function(element) {
    if (element) {
      element.style.fontWeight = 'bold';
    }
  });
  
  // Ensure legend has proper background for readability
  const legendElements = document.querySelectorAll('.legend');
  legendElements.forEach(function(element) {
    if (element) {
      element.style.backgroundColor = 'rgba(255, 255, 255, 0.85)';
    }
  });
}

// Function to add extra space between plots and buttons
function addPlotButtonSpacing() {
  // Add extra bottom padding to plotly containers
  const plotlyContainers = document.querySelectorAll('.js-plotly-plot, .plotly-html-widget');
  
  plotlyContainers.forEach(function(container) {
    // Get container ID and parent container info
    const containerId = container.id || '';
    const isClusterPlot = containerId.includes('ClusterPlot') || containerId.includes('Heatmap');
    const isBatchModule = containerId.includes('batch_analysis');
    
    // Apply different spacing based on context
    if (isBatchModule && isClusterPlot) {
      // Batch analysis cluster plots need more space
      container.style.marginBottom = '180px';
    } else if (container.closest('#raw_data-cluster_analysis') || 
               container.closest('#batch_analysis-cluster_management')) {
      container.style.marginBottom = '150px';
    } else {
      container.style.marginBottom = '120px';
    }
    
    // Add a spacer div after the plot if there isn't one already
    if (!container.nextElementSibling || !container.nextElementSibling.classList.contains('plot-spacer')) {
      const spacer = document.createElement('div');
      spacer.className = 'plot-spacer';
      spacer.style.height = '100px';
      spacer.style.clear = 'both';
      container.parentNode.insertBefore(spacer, container.nextSibling);
    }
    
    // Find parent row if exists
    const parentRow = container.closest('.row');
    if (parentRow) {
      parentRow.style.marginBottom = '80px';
    }
  });
  
  // Make sure buttons are always on top and positioned properly
  const buttons = document.querySelectorAll('.btn-primary, .btn-warning, .btn-info');
  buttons.forEach(function(button) {
    button.style.position = 'relative';
    button.style.zIndex = '1000';
    
    // If this is a cluster management button in batch analysis, adjust style
    if ((button.id.includes('showMergeModal') || button.id.includes('resetMerging')) && 
        button.id.includes('batch_analysis')) {
      button.style.width = 'auto';
      button.style.marginTop = '0';
      button.style.fontSize = '14px';
      button.style.padding = '6px 12px';
    }
  });
}

// Create a wrapper div around plots and buttons to help with spacing
function createLayoutWrappers() {
  // Find merge cluster buttons and wrap them
  const mergeButtons = document.querySelectorAll(
    '#raw_data-showMergeModal, #raw_data-resetMerging'
  );
  
  mergeButtons.forEach(function(button) {
    // Check if this button is already wrapped
    if (!button.closest('.button-wrapper')) {
      const wrapper = document.createElement('div');
      wrapper.className = 'button-wrapper';
      wrapper.style.marginTop = '20px';
      wrapper.style.position = 'relative';
      wrapper.style.zIndex = '2000';
      
      // Insert wrapper before the button
      button.parentNode.insertBefore(wrapper, button);
      // Move button into wrapper
      wrapper.appendChild(button);
    }
  });
  
  // Adjust the control-panel layout in batch analysis
  const controlPanels = document.querySelectorAll('.control-panel');
  controlPanels.forEach(function(panel) {
    if (panel.closest('#batch_analysis-Sample_Visualization')) {
      panel.style.marginTop = '80px';
      panel.style.zIndex = '200';
    }
  });
}

// Ensure plotly layout is correct
function fixPlotlyLayout() {
  // Find all plotly plots
  const plots = document.querySelectorAll('.js-plotly-plot');
  
  plots.forEach(function(plot) {
    // Get the plot ID
    const plotId = plot.id;
    
    // If this is a Plotly object with a layout
    if (plotId && window.Plotly && window['_' + plotId]) {
      const plotObj = window['_' + plotId];
      
      if (plotObj.layout) {
        // Update margins to ensure more space at bottom
        if (!plotObj.layout.margin || plotObj.layout.margin.b < 100) {
          plotObj.layout.margin = {
            l: 80,
            r: 50,
            t: 100,
            b: 100,
            pad: 10
          };
          
          // Relayout the plot with new margins
          window.Plotly.relayout(plotId, {margin: plotObj.layout.margin});
        }
      }
    }
  });
}

// Run on document ready
document.addEventListener('DOMContentLoaded', function() {
  // Set up a mutation observer to watch for plotly elements
  const observer = new MutationObserver(function(mutations) {
    // When DOM changes, check if we need to adjust plotly elements
    let shouldAdjust = false;
    
    mutations.forEach(function(mutation) {
      // Look for added plotly nodes
      if (mutation.addedNodes.length) {
        for (let i = 0; i < mutation.addedNodes.length; i++) {
          const node = mutation.addedNodes[i];
          if (node.classList && 
              (node.classList.contains('js-plotly-plot') || 
               node.classList.contains('plotly-html-widget'))) {
            shouldAdjust = true;
            break;
          }
        }
      }
    });
    
    if (shouldAdjust) {
      // Wait a bit for plotly to finish rendering
      setTimeout(function() {
        adjustPlotlyFonts();
        addPlotButtonSpacing();
        createLayoutWrappers();
        fixPlotlyLayout();
      }, 300);
    }
  });
  
  // Start observing the document body for changes
  observer.observe(document.body, { 
    childList: true, 
    subtree: true 
  });
  
  // Initial run with a longer delay to ensure everything is loaded
  setTimeout(function() {
    adjustPlotlyFonts();
    addPlotButtonSpacing();
    createLayoutWrappers();
    fixPlotlyLayout();
  }, 1000);
});

// CONSOLIDATED HANDLERS: Clean up the duplicate handlers to prevent conflicts
Shiny.addCustomMessageHandler('refreshPlots', function(message) {
  console.log("Refreshing all plots");
  setTimeout(function() {
    adjustPlotlyFonts();
    addPlotButtonSpacing();
    createLayoutWrappers();
    fixPlotlyLayout();
  }, 300);
});

// Handler for direct plotly replots by ID - BE CAREFUL WITH THIS
Shiny.addCustomMessageHandler("plotly-replot", function(message) {
  if (message.id) {
    // Get the element directly - avoid modifying the DOM
    var plotId = message.id;
    var plotElement = document.getElementById(plotId);
    
    // Don't purge, just update the layout
    if (plotElement && window.Plotly) {
      console.log("Updating layout for " + plotId);
      // Just update the font settings without purging
      Plotly.relayout(plotElement, {
        'font.size': message.fontSize || 18,
        'font.family': 'Arial, sans-serif',
        'title.font.size': (message.fontSize || 18) * 1.2,
        'xaxis.title.font.size': (message.fontSize || 18) * 1.1,
        'yaxis.title.font.size': (message.fontSize || 18) * 1.1
      });
    }
  }
});

// Handler for updating plotly fonts directly from R
Shiny.addCustomMessageHandler("updatePlotlyFonts", function(message) {
  if (message.fontSize) {
    console.log("Setting font size CSS variables to " + message.fontSize);
    
    // Set CSS variables
    document.documentElement.style.setProperty('--plotly-font-size', message.fontSize + 'px');
    document.documentElement.style.setProperty('--plotly-title-size', (message.fontSize * 1.2) + 'px');
    document.documentElement.style.setProperty('--plotly-axis-size', (message.fontSize * 1.1) + 'px');
    
    // DO NOT force redraw here - leave that to the individual plot-specific handlers
  }
}); 