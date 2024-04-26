barsChart.fn <- function(
    data.df                    = data2plot,
    labels_var                 = "labels",
    value2plot                 = "value2plot",
    groupVar                   = T,
    grouping_var               = "group_var",
    orientation                = "vertical",
    categories_grouping_var    = categories,
    label_figures              = "figure",
    order                      = T,
    order_value                = "order_values",
    order_value_bars           = "legend_order_values",
    nbars                      = 6,
    colors4plot                = colors4plot
) {
  
  
  if(groupVar == F){
    
    data2plot <- data.df %>%
      dplyr::rename(
        value2plot       = all_of(value2plot),
        labels           = all_of(labels_var),
        label_figures    = all_of(label_figures),
        order_var        = any_of(order_value),
      )
    
    plot <- ggplot(data2plot,
                   aes(
                     x     = reorder(labels, -order_var),
                     y     = value2plot,
                     fill  = colors4plot,
                     label = label_figures
                   )) +
      geom_bar(stat = "identity",
               show.legend = F,
               position = position_dodge(widt = 0.9)) +
      geom_text(aes(y    = value2plot + 5), 
                position = position_dodge(widt = 0.9),
                color    = "black",
                family   = "Lato Full",
                fontface = "bold", 
                size = 3.514598) +
      scale_fill_manual(values = colors4plot) 
    
    if(orientation == "vertical") {
      plot <- plot +
        scale_y_continuous(limits = c(0, 100),
                           breaks = seq(0,100,20),
                           labels = paste0(seq(0,100,20), "%"),
                           position = "right") + coord_flip() 
    } else {
      plot <- plot +
        scale_y_continuous(limits = c(0, 100),
                           breaks = seq(0,100,20),
                           labels = paste0(seq(0,100,20), "%"),
                           position = "left") 
    }
    
  } else {
    
    data2plot <- data.df %>%
      dplyr::rename(
        value2plot       = all_of(value2plot),
        grouping_var     = all_of(grouping_var),
        labels           = all_of(labels_var),
        label_figures    = all_of(label_figures),
        order_value_bars = any_of(order_value_bars),
        order_var        = any_of(order_value),
      )
    
    if(order == T) {
      
      if(orientation == "vertical") {
      
      plot <- ggplot(data2plot,
                     aes(
                       x     = reorder(labels, -order_var),
                       y     = value2plot,
                       fill  = reorder(grouping_var, -order_value_bars),
                       label = label_figures
                     ))
      } else {
        
        plot <- ggplot(data2plot,
                       aes(
                         x     = reorder(labels, -order_var),
                         y     = value2plot,
                         fill  = reorder(grouping_var, order_value_bars),
                         label = label_figures
                       ))
      }
    } else {
      
      plot <- ggplot(data2plot,
                     aes(
                       x     = labels,
                       y     = value2plot,
                       fill  = grouping_var,
                       label = label_figures
                     ))
    }
    
    plot <- plot +
      geom_bar(stat = "identity",
               show.legend = F,
               position = position_dodge(widt = 0.9))
    
    if(nbars > 1){
      
      plot <- plot +
        geom_vline(xintercept = seq(1.5, nbars - 0.5, by = 1), linetype = "dashed", color = "black")
      
    } else {
      plot <- plot
    }
    
    plot <- plot +
      geom_text(aes(y    = value2plot + 5), 
                position = position_dodge(widt = 0.9),
                color    = "black",
                family   = "Lato Full",
                fontface = "bold", 
                size = 3.514598)  +
      geom_vline(xintercept = 2.5, linetype = "dashed", color = "black") +
      scale_fill_manual(values = colors4plot, 
                        breaks = categories_grouping_var) 
    
    if(orientation == "vertical") {
      
      plot <- plot +
        scale_y_continuous(limits = c(0, 100),
                           breaks = seq(0,100,20),
                           labels = paste0(seq(0,100,20), "%"),
                           position = "right") +
        coord_flip() 
        
    } else {
      
      plot <- plot +
        scale_y_continuous(limits = c(0, 100),
                           breaks = seq(0,100,20),
                           labels = paste0(seq(0,100,20), "%"),
                           position = "left") 
      
    }
    
  } 
  plot <- plot + theme(
    
    panel.background   = element_blank(),
    plot.background    = element_blank(),
    panel.grid.major   = element_line(size     = 0.25,
                                      colour   = "#5e5c5a",
                                      linetype = "dashed"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#D0D1D3")) +
    theme(
      axis.title.y       = element_blank(),
      axis.title.x       = element_blank(),
      axis.ticks         = element_blank(),
      axis.text.y        = element_markdown(family   = "Lato Full",
                                            face     = "bold",
                                            size     = 3.514598*.pt,
                                            color    = "#524F4C",
                                            margin   = margin(0, 10, 0, 0),
                                            hjust = 0), 
      plot.caption = element_markdown(family   = "Lato Full",
                                      face     = "plain",
                                      size     = 3.514598*.pt,
                                      color    = "#524F4C", 
                                      vjust    = 0, 
                                      hjust    = 0, 
                                      margin = margin(20, 0, 0, 0)),
      axis.text.x        = element_markdown(family   = "Lato Full",
                                            face     = "bold",
                                            size     = 3.514598*.pt,
                                            color    = "#524F4C"),
      plot.title          = element_text(family   = "Lato Full",
                                         face     = "bold",
                                         size     = 3.514598*.pt,
                                         color    = "black",
                                         margin   = margin(0, 0, 10, 0),
                                         hjust    = 0), 
      plot.subtitle      = element_text(family   = "Lato Full",
                                        face     = "plain",
                                        size     = 3.514598*.pt,
                                        color    = "black",
                                        margin   = margin(2.5, 0, 20, 0),
                                        hjust    = 0),
      legend.text        =  element_markdown(family   = "Lato Full",
                                             face     = "plain",
                                             size     = 3.514598*.pt,
                                             color    = "#524F4C"),
      legend.title       = element_markdown(family   = "Lato Full",
                                            face     = "plain",
                                            size     = 3.514598*.pt,
                                            color    = "#524F4C")
      )
    
  
  return(plot)
  
}
