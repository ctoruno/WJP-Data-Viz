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
    labels_var,        # Variable containing the labels to display in plot
    colors_var,        # Variable containing the groups by color
    colors,            # Colors to apply to bars
    direction,         # Should the bars go in a "horizontal" or "vertical" way?
    stacked = F,       # Stack bars on top of each other?
    lab_pos = NULL,    # Variable containing the Y coordinates of the stacked labels
    expand  = F,       # Do we need to give extra space for the labels?
    custom_order = F,  # Do we want a customize order in the graph labels?
    order_var = NULL,
    width = 0.9# Variable containing the custom order for the labels
){
  
  # Renaming variables in the data frame to match the function naming
  data <- data %>%
    dplyr::rename(target_var    = all_of(target_var),
                  grouping_var  = all_of(grouping_var),
                  labels_var    = all_of(labels_var),
                  colors_var    = all_of(colors_var),
                  lab_pos       = all_of(lab_pos),
                  order_var     = any_of(order_var),
                  order_stack   = all_of(order_stack))
  
  # Creating plot
  if(custom_order == F) {
    
    if (stacked == F) {
      plt <- ggplot(data, 
                    aes(x     = grouping_var,
                        y     = target_var,
                        label = labels_var,
                        fill  = colors_var)) +
        geom_bar(stat = "identity",
                 show.legend = F, width = width) +
        geom_text(aes(y    = target_var + 10),
                  color    = "#4a4a49",
                  family   = "Lato Full",
                  fontface = "bold", 
                  size = 2.811678)
    } else {
      plt <- ggplot(data, 
                    aes(x     = grouping_var,
                        y     = target_var,
                        label = labels_var,
                        fill  = colors_var)) +
        geom_bar(stat         = "identity",
                 position     = "stack", 
                 show.legend  = F,  width = width) +
        geom_text(aes(y    = lab_pos),
                  color    = "#ffffff",
                  family   = "Lato Full",
                  fontface = "bold", 
                  size = 2.811678)
    }
    
  } else {
    
    if (stacked == F) {
      plt <- ggplot(data, 
                    aes(x     = reorder(grouping_var, order_var),
                        y     = target_var,
                        label = labels_var,
                        fill  = colors_var)) +
        geom_bar(stat = "identity",
                 show.legend = F,  width = width) +
        geom_text(aes(y    = target_var + 10),
                  color    = "#4a4a49",
                  family   = "Lato Full",
                  fontface = "bold", 
                  size = 2.811678)
    } else {
      plt <- ggplot(data, 
                    aes(x     = reorder(grouping_var, order_var),
                        y     = target_var,
                        label = labels_var,
                        fill  = colors_var)) +
        geom_bar(stat         = "identity",
                 position     = "stack", 
                 show.legend  = F,  width = width) +
        geom_text(aes(y    = lab_pos),
                  color    = "#ffffff",
                  family   = "Lato Full",
                  fontface = "bold", 
                  size = 2.811678)
    }
  }
  
  plt <- plt +
    labs(y = "% of respondents") +
    scale_fill_manual(values = colors)
  
  if (expand == F) {
    uplimitV = 100
    uplimitH = 105
  } else {
    uplimitV = 110
    uplimitH = 105
  }
  
  if (direction == "vertical") {
    plt  <- plt +
      scale_y_continuous(limits = c(0, uplimitV),
                         breaks = seq(0,100,20),
                         labels = paste0(seq(0,100,20), "%")) +
      WJP_theme() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "#D0D1D3"),
            axis.title.x       = element_blank())
  }
  
  if (direction == "horizontal") {
    plt  <- plt +
      scale_y_continuous(limits = c(0, uplimitH),
                         breaks = seq(0,100,20),
                         labels = paste0(seq(0,100,20), "%"),
                         position = "right") +
      scale_x_discrete(limits = rev) +
      coord_flip() +
      WJP_theme() +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = "#D0D1D3"),
            axis.title.y       = element_blank(),
            axis.title.x       = element_blank(),
            axis.text.y        = element_text(hjust = 0))
  }
    
  return(plt)
  
}
