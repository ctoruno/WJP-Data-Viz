LAC_groupedBarChart <- function(
    data,                    # Data frame with data
    target_var,              # Variable that will supply the values to plot
    grouping_var,            # Variable containing the grouping values (Axis Labels)
    labels_var,              # Variable containing the labels to show in the plot
    colors_var,              # Variable containing the groups by color
    colors,                  # Colors to apply to bars
    repel = FALSE,           # Do we need to repel the labels?
    transparency = FALSE,    # Apply transparency to chart?
    transparencies = NULL,   # Named vector with transparencies to apply
    custom.axis = FALSE,     # Do we want to customize the X-AXIS?
    x.breaks    = NULL,      # Numeric vector with custom breaks for the X-Axis
    x.labels    = NULL       # Character vector with labels for the x-axis. It has to be the same length than x.breaks,
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
                    fill  = colors_var,
                    label = labels_var))
    
  if (transparency == FALSE) {
    plt <- plt +
      geom_bar(stat = "identity",
               position = "dodge",
               show.legend = FALSE) +
      geom_text(position = position_dodge(width = 0.9),
                vjust = -0.5,
                size = 3.514598,
                show.legend = FALSE)
  } else {
    plt <- plt +
      geom_bar(aes(alpha   = colors_var),
               stat = "identity",
               position = "dodge",
               show.legend = FALSE) +
      geom_text(aes(alpha   = colors_var),
                position = position_dodge(width = 0.9),
                vjust = -0.5,
                size = 3.514598,
                show.legend = FALSE) +
      scale_alpha_manual(values = transparencies)
  }
  
  if (repel) {
    plt <- plt +
      geom_text_repel(mapping = aes(label = labels_var),
                      position = position_dodge(width = 0.9),
                      family      = "Lato Full",
                      fontface    = "bold",
                      size        = 3.514598,
                      show.legend = FALSE,
                      min.segment.length = 1000,
                      seed               = 42,
                      box.padding        = 0.5,
                      direction          = "y",
                      force              = 5,
                      force_pull         = 1)
  }
  
  if (custom.axis == FALSE) {
    plt <- plt +
      scale_fill_manual(values = colors) +
      theme_minimal()
  } else {
    plt <- plt +
      scale_fill_manual(values = colors) +
      scale_x_discrete(breaks = x.breaks,
                       labels = x.labels) +
      theme_minimal()
  }
  
  return(plt)
}
