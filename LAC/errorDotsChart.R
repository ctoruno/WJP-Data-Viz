errorDotsChart <- function(data2plot = data2plot, 
                          labels = "labels", 
                          category = "category", 
                          values = values, 
                          lower = lower,
                          upper = upper, 
                          group = "party",
                          figures = "figure",
                          colors4plot = colors4plot,
                          custom_order = T,
                          order_values = NULL) {
  
  data2plot <- data2plot %>%
    rename(group    = all_of(group),
           labels   = all_of(labels),
           category = all_of(category),
           figures  = all_of(figure)
           )
  #values = all_of(values),
  #lower = all_of(lower),
  #upper = all_of(upper)) 
  
  # Creating a strip pattern
  strips <- data2plot %>%
    group_by(labels) %>%
    summarise() %>%
    mutate(ymin = 0,
           ymax = 1,
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
  
  if(custom_order == T) {
    
  a <- ggplot(data2plot,
              aes(
                label = figures
                )
              ) +
    geom_blank(data       = data2plot,
               aes(x      = labels,
                   y      = {{values}},
                   group  = group,
                   color  = group)
    ) +
    geom_ribbon(data      = strips,
                aes(x     = x,
                    ymin  = ymin,
                    ymax  = ymax,
                    group = xposition,
                    fill  = fill),
                show.legend = F) +
    scale_fill_manual(values = c("grey"  = "#EBEBEB",
                                 "white"  = "#FFFFFF"),
                      na.value = NULL) +
    geom_point(data = data2plot,
               size = 3,
               aes(x      = labels,
                   y      = {{values}},
                   color  = group)
    ) +
    geom_errorbar(data = data2plot,
                  aes(x      = labels,
                      y      = {{values}},
                      color  = group,
                      ymin   = lower,
                      ymax   = upper), 
                  width = 0.5, 
                  alpha = 0.5, 
                  linewidth = 1) +
    geom_text_repel(mapping = aes(y     = {{values}},
                                  x     = labels,
                                  label = figure),
                    family      = "Lato Full",
                    fontface    = "bold",
                    size        = 3.514598,
                    show.legend = F,
                    
                    # Additional options from ggrepel package:
                    min.segment.length = 1000,
                    seed               = 42,
                    box.padding        = 0.5,
                    direction          = "y",
                    force              = 5,
                    force_pull         = 1) +
    scale_color_manual(values = colors4plot) + 
    scale_y_continuous(limits   = c(0,1),
                       breaks   = seq(0,1, 0.25),
                       labels   = paste0(c("0", "25", "50", "75", "100"), "%"),
                       position = "right") +
    scale_x_discrete(expand = c(0,0)) +
    coord_flip() +
    WJP_theme() +
    theme(
      legend.position    = "none",
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(size     = 0.25,
                                        colour   = "#5e5c5a",
                                        linetype = "dashed"),
      panel.ontop        = T,
      panel.background   = element_blank(),
      plot.background    = element_blank(),
      axis.title.x       = element_blank(),
      axis.title.y       = element_blank(),
      axis.text.y        = element_text(family   = "Lato Full",
                                        face     = "plain",
                                        size     = 3.514598*.pt,
                                        color    = "#524F4C", 
                                        hjust    = 0),
      axis.text.x        = element_text(family = "Lato Full",
                                        face   = "plain",
                                        size   = 3.514598*.pt,
                                        color  = "#524F4C",
                                        hjust  = 0.5)
    )
  
  return(a)
  
  } else {
    
    b <- ggplot() +
      geom_blank(data       = data2plot,
                 aes(x      = reorder(labels, -{{order_values}}),
                     y      = {{values}},
                     group  = group,
                     color  = group)
      ) +
      geom_ribbon(data      = strips,
                  aes(x     = x,
                      ymin  = ymin,
                      ymax  = ymax,
                      group = xposition,
                      fill  = fill),
                  show.legend = F) +
      scale_fill_manual(values = c("grey"  = "#EBEBEB",
                                   "white"  = "#FFFFFF"),
                        na.value = NULL) +
      geom_point(data = data2plot,
                 size = 3,
                 aes(x      = reorder(labels, -{{order_values}}),
                     y      = {{values}},
                     color  = group)
      ) +
      geom_errorbar(data = data2plot,
                    aes(x      = reorder(labels, -{{order_values}}),
                        y      = {{values}},
                        color  = group,
                        ymin   = lower,
                        ymax   = upper), 
                    width = 0.5, 
                    alpha = 0.5, 
                    linewidth = 1) +
      scale_color_manual(values = colors4plot) + 
      scale_y_continuous(limits   = c(0,1),
                         breaks   = seq(0,1, 0.25),
                         labels   = paste0(c("0", "25", "50", "75", "100"), "%"),
                         position = "right") +
      scale_x_discrete(expand = c(0,0)) +
      coord_flip() +
      WJP_theme() +
      theme(
        legend.position    = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(size     = 0.25,
                                          colour   = "#5e5c5a",
                                          linetype = "dashed"),
        panel.ontop        = T,
        panel.background   = element_blank(),
        plot.background    = element_blank(),
        axis.title.x       = element_blank(),
        axis.title.y       = element_blank(),
        axis.text.y        = element_text(family   = "Lato Full",
                                          face     = "plain",
                                          size     = 3.514598*.pt,
                                          color    = "#524F4C", 
                                          hjust    = 0),
        axis.text.x        = element_text(family = "Lato Full",
                                          face   = "plain",
                                          size   = 3.514598*.pt,
                                          color  = "#524F4C",
                                          hjust  = 0.5)
      )
    
    return(b)
    
  }

}
