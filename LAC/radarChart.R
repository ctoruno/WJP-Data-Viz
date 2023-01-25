## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            LAC Reports Data Viz functions: Radar Chart
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     November 23rd, 2022
##
## This version:      November 23rd, 2022
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

LAC_radarChart <- function(
    data,             # Data frame with data
    labelling_fn,     # Labelling function that will define labels in markdown and HTML syntax
    colors,           # Colors to apply to lines. First code will be applied to percentages in labels
    percentages       # Named vector with percentages to display in labels
){
  
  # Plotting base radar chart
  # We use ggradar2 to get a basic radar chart. I prefer this package over fmsb given that it works as an
  # extension of ggplot2. Therefore, we can work the aesthetics using ggplot2 tools afterwards.
  base_figure <- ggradar2::ggradar2(
    data,
    radarshape  = "sharp",
    fullscore   = rep(1, 8),
    polygonfill = F,
    background.circle.colour = "white",
    gridline.mid.colour      = "grey",
    gridline.min.linetype    = "solid",
    gridline.mid.linetype    = "solid",
    gridline.max.linetype    = "solid",
    grid.label.size = 3.514598,
    
    # The following part is IMPORTANT. We define the labels using a markdown syntax. However, given that
    # ggradar2 uses geom_text(), markdown language is not supported and all the italics or bold fonts. 
    # will not be reflected. Therefore, we set labels with color == "white" so they are not visible  
    # However, ggradar estimates the optimal X and Y coordinates of these labels and saves them as plot data.
    # We gonna use these coordinates to plot these labels again but using ggtext::geom_richtext which
    # supports markdown syntax in the labels.
    
    axis.labels.color = "black",
    axis.label.size  = 1,
    group.colours    = colors,
    group.line.width = 0.75
  )
  
  # Transforming labels using an externally predefined function
  base_figure$data <- base_figure$data %>%
    mutate(across(text,
                  labelling_fn,
                  color_code    = colors[1],
                  value_vectors = percentages))
  
  # Dropping axis layers from ggplt
  base_figure$layers[[1]] <- NULL
  base_figure$layers[[7]] <- NULL
  base_figure$layers[[7]] <- NULL
  
  # Adding additional aesthetics to radar chart using the "traditional" tools from ggplot2
  radar <- base_figure +
    ggtext::geom_richtext(data  = base_figure$data,
                          # For the aesthetics, we bring the X and Y coordinates saved by ggradar2 with
                          # a minor adjustment depending on the final position of the label.
                          aes(x     = x*c(1.0, 1.2, 1.17, 1.12, 1.0, 1.1, 1.15, 1.1), 
                              y     = y*c(1.1, 1.2, 1.10, 1.12, 1.1, 1.1, 1.10, 1.1), 
                              label = text),
                          family      = "Lato Full",
                          fontface    = "plain",
                          hjust       = 0.5, 
                          size        = 3.514598,
                          text.colour = "#524F4C", 
                          fill        = NA, 
                          label.color = NA) +
    scale_y_continuous(expand = expansion(mult = 0.065)) +
    theme(legend.position    = "none",
          panel.background   = element_blank(),
          plot.background    = element_blank())
  
  return(radar)
  
}
