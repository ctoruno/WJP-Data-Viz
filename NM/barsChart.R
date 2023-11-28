NM_barsChart <- function(
  data,                    # Data frame with data
  target_var,              # Variable that will supply the values to plot
  grouping_var,            # Variable containing the grouping values (Axis Labels)
  labels_var,              # Variable containing the labels to show in the plot
  colors_var,              # Variable containing the groups by color
  colors,                  # Colors to apply to bars
  custom.axis = F,         # Do we want to customize the X-AXIS?
  x.breaks    = NULL,      # Numeric vector with custom breaks for the X-Axis
  x.labels    = NULL,      # Character vector with labels for the x-axis. It has to be the same length than
  # x.breaks,
  sec.ticks   = NULL       # Numeric vector containing the minor breaks
) {

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
                    label = labels_var)) +
    geom_bar(stat = "identity",
             color = "black") +
    geom_text(position = position_stack(vjust = 0.5),    
              family      = "Lato Full",
              fontface    = "bold",
              size        = 3.514598,
              show.legend = F)

  # Continuing with ggplot
  if (custom.axis == F) {
    plt <- plt +
      scale_fill_manual(values = colors) +
      scale_y_continuous(limits = c(0, 105),
                         expand = c(0,0),
                         breaks = seq(0, 100, 20),
                         labels = paste0(seq(0, 100, 20), "%"))
  } else {
    plt <- plt +
      scale_fill_manual(values = colors) +
      scale_y_continuous(limits = c(0, 105),
                         expand = c(0,0),
                         breaks = seq(0, 100, 20),
                         labels = paste0(seq(0, 100, 20), "%")) +
      scale_x_continuous(breaks = x.breaks,
                         expand = expansion(mult = c(0.075, 0.125)),
                         labels = x.labels,
                         guide = "axis_minor",
                         minor_breaks = sec.ticks)
  }

  plt <- plt +
    WJP_theme() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "#d1cfd1"),
          axis.title.x       = element_blank(),
          axis.title.y       = element_blank(),
          axis.line.x        = element_line(color    = "#d1cfd1"),
          axis.ticks.x       = element_line(color    = "#d1cfd1",
                                            linetype = "solid"),
          ggh4x.axis.ticks.length.minor = rel(1))

  return(plt)
}
