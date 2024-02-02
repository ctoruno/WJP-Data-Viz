NM_dotsChart <- function(
    data,             # Data frame with data
    target_var,       # Variable that will supply the values to plot
    sd_var,           # Variable that will supply the sd to the plot
    n_obs,            # Variable that will supply the observation number to the plot
    alpha = 0.05,            # Variable that will provide the level of confidence to the plot
    grouping_var,     # Variable containing the grouping values. Plot will show a different color per group.
    labels_var,       # Variable containing the Y-Axis labels to show in the plot
    colors,           # Named vector with the colors to apply to lines
    order_var,
    diffOpac = F,     # Should the dots have different opacity levels?
    opacities,        # Named vector with opacity levels
    diffShp = F,      # Should point be displayed using different shapes?
    shapes  = NA,      # Named vector with shapes to be displayed
    draw_ci = T,
    y_upper = 100,
    dsize = 4,
    fsize = 10,
    fsize2 = 10
){
  
  if (y_upper == 100) {
    upbound  <- 100
    ylimits  <- c(0,100)
    ybreaks  <- seq(0, 100, 20)
  }
  if (y_upper == 1) {
    upbound  <- 1
    ylimits  <- c(0,1)
    ybreaks  <- seq(0, 1, 0.2)
  }
  
  
  # Renaming variables in the data frame to match the function naming
  data <- data %>%
    rename(target_var    = all_of(target_var),
           sd_var        = all_of(sd_var),
           n_obs         = all_of(n_obs),
           grouping_var  = all_of(grouping_var),
           labels_var    = all_of(labels_var),
           order_var     = all_of(order_var))
  
  # Creating a strip pattern
  strips <- data %>%
    group_by(labels_var) %>%
    summarise() %>%
    mutate(ymin = 0,
           ymax = upbound,
           xposition = rev(1:nrow(.)),
           xmin = xposition - 0.5,
           xmax = xposition + 0.5,
           fill = rep(c("grey", "white"), 
                      length.out = nrow(.))) %>%
    pivot_longer(c(xmin, xmax),
                 names_to  = "cat",
                 values_to = "x") %>%
    select(-cat) %>%
    filter(fill != "white")
    
  
  # Creating ggplot
  plt <- ggplot() +
    geom_blank(data       = data,
               aes(x      = reorder(labels_var, -order_var),
                   y      = target_var,
                   label  = labels_var,
                   color  = grouping_var))
  
  if (draw_ci == T){
    plt <- plt +
      geom_ribbon(data      = strips,
                  aes(x     = x,
                      ymin  = ymin,
                      ymax  = ymax,
                      group = xposition,
                      fill  = fill),
                  show.legend = F) +
      geom_errorbar(data    = data, 
                    aes(x   = reorder(labels_var, -order_var),
                        y   = target_var,
                        ymin  = target_var - qt(1- alpha/2, (n_obs - 1))*sd_var/sqrt(n_obs),
                        ymax  = target_var + qt(1- alpha/2, (n_obs - 1))*sd_var/sqrt(n_obs),
                        color = grouping_var,
                        alpha = 1),
                    width = 0.2,  # Set the width of the error bars
                    show.legend = F)
  }
  
  plt <- plt +
    scale_fill_manual(values  = c("grey"  = "#EBEBEB",
                                  "white" = "#FFFFFF"),
                      na.value = NULL)
  
  if (diffShp == F) {
    
    if (diffOpac == F) {
      plt <- plt +
        geom_point(data      = data,
                   aes(x     = reorder(labels_var, -order_var),
                       y     = target_var,
                       color = grouping_var),
                   size = dsize,
                   show.legend = F)
    } else {
      plt <- plt +
        geom_point(data = data,
                   aes(x     = reorder(labels_var, -order_var),
                       y     = target_var,
                       color = grouping_var,
                       alpha = grouping_var),
                   size      = dsize,
                   show.legend    = F) +
        scale_alpha_manual(values = opacities)
    }
    
  } else {
    
    if (diffOpac == F) {
      plt <- plt +
        geom_point(data      = data,
                   aes(x     = reorder(labels_var, -order_var),
                       y     = target_var,
                       color = grouping_var,
                       shape = grouping_var),
                   fill   = NA,
                   size   = dsize,
                   stroke = 2,
                   show.legend = F) +
        scale_shape_manual(values = shapes)
      
    } else {
      plt <- plt +
        geom_point(data = data,
                   aes(x     = reorder(labels_var, -order_var),
                       y     = target_var,
                       color = grouping_var,
                       shape = grouping_var,
                       alpha = grouping_var),
                   fill   = NA,
                   size   = dsize,
                   stroke = 2,
                   show.legend    = F) +
        scale_shape_manual(values = shapes) +
        scale_alpha_manual(values = opacities)
    }
    
  }
  
  plt <- plt +
    scale_color_manual(values = colors) +
    scale_y_continuous(limits = ylimits,
                       breaks = ybreaks,
                       labels = paste0(ybreaks,
                                       "%"),
                       position = "right") +
    coord_flip() +
    WJP_theme() +
    theme(plot.margin = unit(c(0, 1, 0, 0), "lines"),
          axis.title.x       = element_blank(),
          axis.title.y       = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.background   = element_blank(), 
          panel.ontop = T,
          axis.text.x = element_text(color = "#222221",
                                     hjust = 0,
                                     size  = fsize2),
          axis.text.y = element_text(color = "#222221",
                                     hjust = 0,
                                     size  = fsize))
    
  return(plt)
  
}
