## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            LAC Reports Data Viz functions: Bar Chart
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     November 28th, 2022
##
## This version:      November 28th, 2022
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

LAC_barsChart <- function(
    data,              # Data frame with data
    target_var,        # Variable that will supply the values to plot
    grouping_var,      # Variable containing the grouping values (Axis Labels)
    colors_var,        # Variable containing the groups by color
    colors,            # Colors to apply to bars
    direction          # Should the bars go in a "horizontal" or "vertical" way?
){
  
  # Renaming variables in the data frame to match the function naming
  data <- data %>%
    rename(target_var    = all_of(target_var),
           grouping_var  = all_of(grouping_var),
           colors_var     = all_of(colors_var))
  
  # Creating plot
  plt <- ggplot(data, 
                aes(x     = grouping_var,
                    y     = target_var,
                    fill  = colors_var)) +
    geom_bar(stat = "identity",
             show.legend = F) +
    labs(y = "% of respondents") +
    scale_fill_manual(values = colors)
  
  if (direction == "vertical") {
    plt  <- plt +
      scale_y_continuous(limits = c(0, 100),
                         breaks = seq(0,100,20),
                         labels = paste0(seq(0,100,20), "%")) +
      WJP_theme() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "#D0D1D3"),
            axis.title.x       = element_blank())
  }
  
  if (direction == "horizontal") {
    plt  <- plt +
      scale_y_continuous(limits = c(0, 100),
                         breaks = seq(0,100,20),
                         labels = paste0(seq(0,100,20), "%"),
                         position = "right") +
      coord_flip() +
      WJP_theme() +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = "#D0D1D3"),
            axis.title.y       = element_blank())
  }
    
  return(plt)
  
}
