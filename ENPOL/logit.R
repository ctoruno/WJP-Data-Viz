## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Logit Chart                                                                                   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

logit_demo_panel <- function(mainData = data2plot,
                             line_color = "#003b8a",
                             line_size  = 2,
                             point_color = "#003b8a",
                             point_size   = 4,
                             shadow = F,
                             shadow_color) {
  
  if(shadow == F) {
    
    plot <- ggplot(mainData, aes(x = reorder(factor, -order_variable), y = AME)) +
      geom_hline(yintercept = 0, lty = 1, color = "#fa4d57", lwd = 1)  +
      geom_linerange(aes(x = reorder(factor, -order_variable),  ymin = lower, ymax = upper),
                     lwd = line_size, position = position_dodge(width = .7), 
                     stat = "identity", color = line_color)+
      geom_point(aes(x = reorder(factor, -order_variable), y = AME), 
                 size = point_size, position = position_dodge(width = .7), color = point_color) +
      geom_point(aes(x = reorder(factor, -order_variable), y = AME), 
                 size = 2, position = position_dodge(width = .7), color = "white") +
      labs(y = "Menos probable                                  Más probable") +
      scale_y_continuous(limits = c(-0.20, 0.20),
                         breaks = seq(-0.18, 0.18, by = 0.09),
                         expand = expansion(mult = 0.025), position = "right",
                         labels = c("-15 p.p", "-7.5 p.p", "0", "+7.5 p.p", "+15 p.p"))+
      WJP_theme()+
      coord_flip() +
      theme(legend.position = "none",
            panel.background   = element_blank(),
            panel.grid.major.x = element_line(colour = "#d1cfd1", 
                                              size = 0.5, linetype = "dashed"),
            legend.title = element_blank(),
            axis.title.y       = element_blank(),
            axis.text.y        = element_text(family = "Lato Medium",
                                              size     = 3.514598*.pt,
                                              color    = "#4a4a49",
                                              hjust    = 0),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.x  = element_blank(),
            panel.grid.minor.y = element_blank())
    
  } else {
    
    plot <- ggplot(mainData, aes(x = reorder(factor, -order_variable), y = AME)) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = shadow_color, alpha = 0.1) +
      geom_hline(yintercept = 0, lty = 1, color = "#fa4d57", lwd = 1)  +
      geom_linerange(aes(x = reorder(factor, -order_variable),  ymin = lower, ymax = upper),
                     lwd = line_size, position = position_dodge(width = .7), 
                     stat = "identity", color = line_color)+
      geom_point(aes(x = reorder(factor, -order_variable), y = AME), 
                 size = point_size, position = position_dodge(width = .7), color = point_color) +
      geom_point(aes(x = reorder(factor, -order_variable), y = AME), 
                 size = 2, position = position_dodge(width = .7), color = "white") +
      labs(y = "Menos probable                                  Más probable") +
      scale_y_continuous(limits = c(-0.20, 0.20),
                         breaks = seq(-0.18, 0.18, by = 0.09),
                         expand = expansion(mult = 0.025), position = "right",
                         labels = c("-15 p.p", "-7.5 p.p", "0", "+7.5 p.p", "+15 p.p")) +
      WJP_theme()+
      coord_flip() +
      theme(legend.position = "none",
            panel.background   = element_blank(),
            panel.grid.major.x = element_line(colour = "#d1cfd1", 
                                              size = 0.5, linetype = "dashed"),
            legend.title = element_blank(),
            axis.title.y       = element_blank(),
            axis.text.y        = element_text(family = "Lato Medium",
                                              size     = 3.514598*.pt,
                                              color    = "#4a4a49",
                                              hjust    = 0),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.x  = element_blank(),
            panel.grid.minor.y = element_blank())
    
  }
  
  return(plot)
}
