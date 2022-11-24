## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            LAC Reports Data Viz functions: Dots Chart
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     November 24th, 2022
##
## This version:      November 24th, 2022
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

LAC_dotsChart <- function(
    data,             # Data frame with data
    target_var,       # Variable that will supply the values to plot
    grouping_var,     # Variable containing the grouping values. Plot will show a different color per group.
    labels_var,       # Variable containing the Y-Axis labels to show in the plot
    colors,           # Named vector with the colors to apply to lines
    diffOpac = F,     # Should the dots have different opacity levels?
    opacities         # Named vector with opacity levels
){
  
  # Renaming variables in the data frame to match the function naming
  data <- data %>%
    rename(target_var    = all_of(target_var),
           grouping_var  = all_of(grouping_var),
           labels_var    = all_of(labels_var))
  
  strips <- data %>%
    group_by(labels_var) %>%
    summarise() %>%
    mutate(ymin = 0,
           ymax = 100,
           xposition = rev(1:nrow(.)),
           xmin = xposition - 0.5,
           xmax = xposition + 0.5,
           fill = rep(c("grey", "white"), 
                      length.out = nrow(.))) %>%
    pivot_longer(c(xmin, xmax),
                 names_to  = "cat",
                 values_to = "x") %>%
    select(-cat)
    
  
  # Creating ggplot
  plt <- ggplot() +
    geom_blank(data = data,
               aes(x     = labels_var,
                   y     = target_var,
                   label = labels_var,
                   color = grouping_var)) +
    geom_ribbon(data      = strips,
                aes(x     = x,
                    ymin  = ymin,
                    ymax  = ymax,
                    group = xposition,
                    fill  = fill),
                alpha     = 0.25,
                show.legend = F) +
    scale_fill_manual(values = c("grey"  = "#CDD0DD",
                                 "white" = "white"))
  
  if (diffOpac == F) {
    plt <- plt +
      geom_point(data = data,
                 aes(x     = labels_var,
                     y     = target_var,
                     color = grouping_var),
                 size = 4,
                 show.legend = F)
  } else {
    plt <- plt +
      geom_point(data = data,
                 aes(x     = labels_var,
                     y     = target_var,
                     color = grouping_var,
                     alpha = grouping_var),
                 size      = 4,
                 show.legend   = F) +
      scale_alpha_manual(values = opacities)
  }
  
  plt <- plt +
    scale_color_manual(values = colors) +
    scale_y_continuous(limits = c(0,100),
                       breaks = seq(0,100,20),
                       labels = paste0(seq(0,100,20),
                                       "%")) +
    coord_flip() +
    WJP_theme() +
    theme(axis.title.x       = element_blank(),
          axis.title.y       = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.text.x = element_markdown(),
          axis.text.y = element_markdown())
    
  return(plt)
  
}

