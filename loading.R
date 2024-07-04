loadVIZ <- function(set) {
  
  if (set == "WJP") {
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/line_chart.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/horizontal_bars_chart.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/diverging_stackedBars.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/mirror_chart.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/slope_chart.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/dots%26errors_chart.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/horizontal_edgebars_chart.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/emoji_waffle_chart.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/logit_plot_3panels.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/focus_correlations_bars.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/wjp_barsCategories.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/logit_plot_panel.R")
  } 
  
  if (set == "LAC") {
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/LAC/divBars.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/LAC/lineChart.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/LAC/radarChart.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/LAC/dotsChart.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/LAC/lollipop_chart.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/LAC/roseChart.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/LAC/barsChart.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/LAC/logit_demo.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/LAC/horizontal_edgebars.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/LAC/errorDotsChart.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/NM/dotsChart.R")
  
  }
  
  if (set == "A2J") {
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/A2J/stackBars.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/A2J/scatterPlot.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/A2J/choroplethMaps.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/LAC/divBars.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/A2J/dumbellChart.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/A2J/errorDotsChart.R")
  }
  
  if (set == "NM") {
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/NM/divBars.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/LAC/dotsChart.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/LAC/lineChart.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/LAC/radarChart.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/LAC/lollipop_chart.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/LAC/barsChart.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/LAC/logit_demo.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/LAC/horizontal_edgebars.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/LAC/roseChart.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/LAC/errorDotsChart.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/NM/dotsChart.R")
  }
  if (set == "ENPOL")
  {
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/LAC/lineChart.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/ENPOL/radarChart.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/ENPOL/barsChart.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/ENPOL/index_hor-bars_shades.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/ENPOL/logit.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/ENPOL/barsChartAxis.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/ENPOL/BarChartIndex.R")
    source("https://raw.githubusercontent.com/ctoruno/WJP-Data-Viz/main/ENPOL/errorDotsChart.R")
    
    
  }
} 
