/* Optimized JavaScript for Flow Cytometry Analysis Tool */

// Performance configuration
const CONFIG = {
  ENABLE_PERFORMANCE_MODE: true,
  DEBOUNCE_TIME: 100,      // Debounce time for resize events
  INITIAL_DELAY: 300       // Initial delay for setup
};

// Cache DOM selectors to reduce queries
let PLOTLY_ELEMENTS = null;

// Simple debouncer for expensive operations
function debounce(func, wait) {
  let timeout;
  return function() {
    const context = this;
    const args = arguments;
    clearTimeout(timeout);
    timeout = setTimeout(() => func.apply(context, args), wait);
  };
}

// Initialize core styles and settings once
function initializePlotlyStyles() {
  // Apply CSS variables for fonts once on init
  document.documentElement.style.setProperty('--plotly-font-size', '16px');
  document.documentElement.style.setProperty('--plotly-title-size', '19px');
  document.documentElement.style.setProperty('--plotly-axis-size', '17px');
  
  // Cache elements we'll need to access later (only once)
  PLOTLY_ELEMENTS = {
    plots: document.querySelectorAll('.js-plotly-plot'),
    legends: document.querySelectorAll('.legend')
  };
  
  // Apply any necessary legend styles
  PLOTLY_ELEMENTS.legends.forEach(element => {
    if (element) element.style.backgroundColor = 'rgba(255, 255, 255, 0.85)';
  });
}

// Efficient layout adjustment without excessive DOM manipulations
function optimizePlotlyLayouts() {
  if (!window.Plotly) return;
  
  document.querySelectorAll('.js-plotly-plot').forEach(plot => {
    const plotId = plot.id;
    
    // Only update if this is an active plotly element
    if (plotId && window['_' + plotId]) {
      const plotObj = window['_' + plotId];
      
      // Apply layout updates only if needed
      if (plotObj.layout) {
        // Ensure adequate plot margins for larger plots
        const minBottomMargin = 100;
        const currentMargin = plotObj.layout.margin;
        
        // Only update if margin is missing or inadequate
        if (!currentMargin || currentMargin.b < minBottomMargin) {
          // Use a single relayout call for performance
          window.Plotly.relayout(plotId, {
            margin: {l: 70, r: 40, t: 80, b: 100, pad: 5}
          });
        }
      }
    }
  });
}

// Initialize once on document load
document.addEventListener('DOMContentLoaded', function() {
  // Delay initialization slightly to allow Shiny to finish rendering
  setTimeout(function() {
    initializePlotlyStyles();
    optimizePlotlyLayouts();
  }, CONFIG.INITIAL_DELAY);
});

// Efficient message handlers 
Shiny.addCustomMessageHandler('refreshPlots', debounce(function(message) {
  // Avoid excess style application, just fix layouts
  optimizePlotlyLayouts();
}, CONFIG.DEBOUNCE_TIME));

// Handle font size changes efficiently
Shiny.addCustomMessageHandler("updatePlotlyFonts", debounce(function(message) {
  if (message.fontSize) {
    // Update CSS variables only - much more efficient than DOM manipulation
    document.documentElement.style.setProperty('--plotly-font-size', message.fontSize + 'px');
    document.documentElement.style.setProperty('--plotly-title-size', (message.fontSize * 1.2) + 'px');
    document.documentElement.style.setProperty('--plotly-axis-size', (message.fontSize * 1.1) + 'px');
  }
}, CONFIG.DEBOUNCE_TIME));

// Optimized plot-specific updates
Shiny.addCustomMessageHandler("plotly-replot", debounce(function(message) {
  if (message.id && window.Plotly) {
    const plotElement = document.getElementById(message.id);
    
    if (plotElement) {
      // Batch all font changes into a single relayout call
      const fontSize = message.fontSize || 16;
      Plotly.relayout(plotElement, {
        'font.size': fontSize,
        'font.family': 'Arial, sans-serif',
        'title.font.size': fontSize * 1.2,
        'xaxis.title.font.size': fontSize * 1.1,
        'yaxis.title.font.size': fontSize * 1.1,
        'legend.font.size': fontSize * 0.9,
        'margin.b': 100, // Ensure consistent bottom margin
        'hoverlabel.font.size': fontSize * 0.9
      });
    }
  }
}, CONFIG.DEBOUNCE_TIME));

// Minimal handlers for compatibility
Shiny.addCustomMessageHandler("refreshClusterPlot", function() {}); 