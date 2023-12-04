LAC_groupedBarChart <- function(
  data,                    # Data frame with data
  target_var,              # Variable that will supply the values to plot
  grouping_var,            # Variable containing the grouping values (Axis Labels)
  ngroups,                 # Number of groups to plot
  labels_var,              # Variable containing the labels to show in the plot
  colors_var,              # Variable containing the groups by color
  colors,                  # Colors to apply to bars
  repel = F,               # Do we need to repel the labels?
  transparency = F,        # Apply transparency to char?
  transparencies = NULL,   # Named vector with transparencies to apply
  custom.axis = F,         # Do we want to customize the X-AXIS?
  x.breaks = NULL,         # Numeric vector with custom breaks for the X-Axis
  x.labels = NULL          # Character vector with labels for the x-axis. It has to be the same length than x.breaks,
) {
  
  # Renaming variables in the data frame to match the function naming
  data <- data %>%
    rename(
      target_var = all_of(target_var),
      grouping_var = all_of(grouping_var),
      labels_var = all_of(labels_var),
      colors_var = all_of(colors_var)
    )
  
  # Creating ggplot
  plt <- ggplot(data,
                aes(x = grouping_var,
                    y = target_var,
                    fill = colors_var,
                    label = labels_var))
  
  if (transparency == F) {
    plt <- plt +
      geom_bar(stat = "identity",
               position = "dodge",  # Change to "dodge" for grouped bars
               show.legend = F) +
      geom_text(position = position_dodge(width = 0.9),
                vjust = -0.5,
                size = 3.514598,
                show.legend = F)
  } else {
    plt <- plt +
      geom_bar(aes(alpha = colors_var),
               stat = "identity",
               position = "dodge",  # Change to "dodge" for grouped bars
               show.legend = F) +
      geom_text(aes(alpha = colors_var),
                position = position_dodge(width = 0.9),
                vjust = -0.5,
                size = 3.514598,
                show.legend = F) +
      scale_alpha_manual(values = transparencies)
  }
  
  if (repel) {
    plt <- plt +
      geom_text_repel(mapping = aes(label = labels_var),
                      position = position_dodge(width = 0.9),
                      family = "Lato Full",
                      fontface = "bold",
                      size = 3.514598,
                      show.legend = F,
                      min.segment.length = 1000,
                      seed = 42,
                      box.padding = 0.5,
                      direction = "y",
                      force = 5,
                      force_pull = 1)
  }
  
  # Continuing with ggplot  
  plt <- plt +
    scale_fill_manual(values = colors) +
    scale_y_continuous(limits = c(0, 105),
                       expand = c(0, 0),
                       breaks = seq(0, 100, 20),
                       labels = paste0(seq(0, 100, 20), "%")) +
    theme_minimal()  # Change to the theme you prefer
  
  if (custom.axis) {
    plt <- plt +
      scale_x_discrete(breaks = x.breaks, labels = x.labels)
  }
  
  return(plt)
}
