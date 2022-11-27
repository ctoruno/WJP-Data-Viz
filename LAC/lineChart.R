## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            LAC Reports Data Viz functions: Line Chart
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     November 21st, 2022
##
## This version:      November 21st, 2022
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

LAC_lineChart <- function(
    data,             # Data frame with data
    target_var,       # Variable that will supply the values to plot
    grouping_var,     # Variable containing the grouping values (Axis Labels)
    ngroups,          # Number of groups to plot
    labels_var,       # Variable containing the labels to show in the plot
    colors_var,       # Variable containing the groups by color
    colors,           # Colors to apply to lines
    repel = F         # Do we need to repel the labels?
){
  
  # Renaming variables in the data frame to match the function naming
  data <- data %>%
    rename(target_var    = all_of(target_var),
           grouping_var  = all_of(grouping_var),
           labels_var    = all_of(labels_var),
           colors_var     = all_of(colors_var))
  
  # Creating ggplot
  plt <- ggplot(data, 
                aes(x     = grouping_var,
                    y     = target_var,
                    color = colors_var,
                    label = labels_var,
                    group = ngroups)) +
    geom_point(size = 2,
               show.legend = F) +
    geom_line(size  = 1,
              show.legend = F)
  
  if (repel == F) {
    
    # Applying regular geom_text
    plt <- plt +
      geom_text(aes(y     = target_var + 7.5,
                    x     = grouping_var,
                    label = labels_var),
                family      = "Lato Full",
                fontface    = "bold",
                size        = 3.514598,
                show.legend = F)
    
  } else {
    
    # Applying ggrepel for a better visualization of plots
    plt <- plt +
      geom_text_repel(mapping = aes(y     = target_var,
                                    x     = grouping_var,
                                    label = labels_var),
                      family      = "Lato Full",
                      fontface    = "bold",
                      size        = 3.514598,
                      show.legend = F,
                      
                      # Additional options from ggrepel package:
                      min.segment.length = 1000,
                      seed               = 42,
                      box.padding        = 0.5,
                      direction          = "y",
                      force              = 5,
                      force_pull         = 1)
      
  }
  
  # Continuing with ggplot  
  plt <- plt +
    scale_color_manual(values = colors) +
    scale_y_continuous(limits = c(0, 100),
                       breaks = seq(0,100,20),
                       labels = paste0(seq(0,100,20), "%")) +
    WJP_theme() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "#d1cfd1"),
          axis.title.x       = element_blank(),
          axis.title.y       = element_blank())
    
}
