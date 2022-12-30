## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            LAC Reports Data Viz functions: Diverging Horizontal Bars Chart
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     November 18th, 2022
##
## This version:      November 18th, 2022
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

LAC_divBars <- function(
    data,             # Data frame with data
    target_var,       # Variable that will supply the values to plot
    grouping_var,     # Variable containing the grouping values (Y-Axis Labels)
    diverging_var,    # Variable that contains the values to diverge,
    negative_value,   # Negative value showed in the diverging_var
    colors,           # Colors to apply to line
    labels_var,        # Variable containing the labels to show in the plot
    added_space
){
  
  # Renaming variables in the data frame to match the function naming
  data <- data %>%
    rename(target_var    = all_of(target_var),
           grouping_var  = all_of(grouping_var),
           diverging_var = all_of(diverging_var),
           labels_var    = all_of(labels_var)) %>%
    mutate(added_space   = if_else(diverging_var == negative_value, -20, 20))
  
  # Creating ggplot
  ggplot(data, aes(x     = grouping_var,
                   y     = target_var,
                   fill  = diverging_var,
                   label = labels_var)) +
    geom_bar(stat        = "identity",
             position    = "stack",
             show.legend = F,
             width       = 0.85) +
    geom_text(aes(y = target_var + added_space),
              size     = 3.514598,
              color    = "#4a4a49",
              family   = "Lato Full",
              fontface = "bold") +
    geom_hline(yintercept = 0,
               linetype   = "solid",
               size       = 0.5,
               color      = "#262424") + 
    scale_fill_manual(values  = colors) +
    scale_y_continuous(limits = c(-110,100)) +
    scale_x_discrete(limits   = rev) +
    coord_flip() +
    WJP_theme() +
    theme(panel.grid.major = element_blank(),
          axis.text.x      = element_blank(),
          axis.text.y      = element_text(family = "Lato Full",
                                          face   = "bold",
                                          size   = 3.514598*.pt,
                                          color  = "#262424",
                                          hjust  = 0),
          axis.title.x      = element_blank(),
          axis.title.y      = element_blank())
    
}

