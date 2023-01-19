## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            LAC Reports Data Viz functions: Ordered Rose Chart
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     November 25th, 2022
##
## This version:      November 25th, 2022
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

LAC_roseChart <- function(
    data,             # Data frame with data
    target_var,       # Variable that will supply the values
    grouping_var,     # Variable with the different categories to show
    alabels_var,      # Variable that contains the axis labels to display
    plabels_var,      # Variable that contains the percentages labels to displays
    order_value,      # Variable to order the variables
    colors            # Named vector with the colors to apply to bars.
){
  
  # Renaming variables in the data frame to match the function naming
  data <- data %>%
    rename(target_var    = all_of(target_var),
           grouping_var  = all_of(grouping_var),
           alabels_var   = all_of(alabels_var),
           plabels_var   = all_of(plabels_var))
  
  # Creating ggplot
  plt <- ggplot(data = data, 
                aes(x = alabels_var,
                    y = target_var)) +
    geom_segment(aes(x    = reorder(alabels_var, target_var),
                     y    = 0,
                     xend = reorder(alabels_var, target_var),
                     yend = 0.8),
                 linetype = "solid",
                 color    = "#d1cfd1") +
    geom_hline(yintercept = seq(0, 0.8, by = 0.2), 
               colour     = "#d1cfd1", 
               linetype   = "dashed",
               size       = 0.45) +
    geom_col(aes(x        = reorder(alabels_var, target_var),
                 y        = target_var,
                 fill     = grouping_var),
             position     = "dodge2",
             show.legend  = F) +
    geom_richtext(aes(y       = 1.15,
                      label   = alabels_var),
                  family      = "Lato Full",
                  fontface    = "plain",
                  color       = "#000000",
                  fill        = NA, 
                  label.color = NA) +
    coord_polar(clip = "off") +
    scale_fill_manual(values  = colors) +
    scale_y_continuous(
      limits = c(-0.1, 1.2),
      # expand = expansion(mult = 0.1),
      breaks = c(0, 0.2, 0.4, 0.6, 0.8)
    ) +
    WJP_theme() +
    theme(legend.position    = "none",
          axis.line.x        = element_blank(),
          axis.line.y.left   = element_blank(),
          axis.text.y        = element_blank(),
          axis.text.x        = element_blank(),
          axis.title.x       = element_blank(),
          axis.title.y       = element_blank(), 
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank()) 
  
  return(plt)
  
}



