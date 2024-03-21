## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            North Macedonia Data Viz functions: Diverging Horizontal Bars Chart
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     November 13th, 2023
##
## This version:      November 13th, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

NM_divBars <- function(
    data,             # Data frame with data
    target_var,       # Variable that will supply the values to plot
    rows_var,         # Variable containing the grouping values (Y-Axis Labels)
    grouping_var,     # Variable that contains the values to diverge,
    negative_value,   # Negative value showed in the diverging_var
    colors,           # Colors to apply to line
    labels_var,       # Variable containing the labels to show in the plot
    custom_order = F, # Do we want a customize order in the graph labels?
    order_var = NULL,  # Variable containing the custom order for the labels
    extreme = F   # Do we have extreme values?
){
  
  # Renaming variables in the data frame to match the function naming
  data <- data %>%
    rename(target_var    = all_of(target_var),
           rows_var      = all_of(rows_var),
           grouping_var  = all_of(grouping_var),
           labels_var    = all_of(labels_var),
           order_var     = any_of(order_var))
  
  # Creating ggplot
  if (custom_order == F) {
    chart <- ggplot(data, aes(x     = rows_var,
                              y     = target_var,
                              fill  = grouping_var,
                              label = labels_var))
  } else {
    chart <- ggplot(data, aes(x     = reorder(rows_var, order_var),
                              y     = target_var,
                              fill  = grouping_var,
                              label = labels_var))
  }
  
  # Axis breaks
  brs <-  c(-100, -75, -50, -25, 0, 25, 50, 75, 100)
  
  # Adding geoms
  chart <- chart +
    geom_bar(stat        = "identity",
             position    = "stack",
             show.legend = F,
             width       = 0.85) +
    geom_hline(yintercept = 0,
               linetype   = "solid",
               size       = 0.5,
               color      = "#262424") + 
    geom_text(aes(y = target_var/2),
              size     = 2.811678,
              color    = "white",
              family   = "Lato Full",
              fontface = "bold") +
    scale_fill_manual(values    = colors) +
    scale_y_continuous(limits   = c(-105,105),
                       breaks   = brs,
                       labels   = paste0(abs(brs), "%"),
                       position = "right") +
    scale_x_discrete(limits   = rev) +
    coord_flip() +
    WJP_theme() +
    theme(panel.grid.major = element_blank(),
          axis.text.x      = element_text(family = "Lato Full",
                                          face   = "bold",
                                          size   = 3.514598*.pt,
                                          color  = "#262424",
                                          hjust  = 0),
          axis.text.y      = element_text(family = "Lato Full",
                                          face   = "bold",
                                          size   = 3.514598*.pt,
                                          color  = "#262424",
                                          hjust  = 0),
          axis.title.x      = element_blank(),
          axis.title.y      = element_blank(),
          axis.line.x       = element_line(linetype   = "solid",
                                           size       = 0.5,
                                           color      = "#262424"))

  
  return(chart)
    
}

