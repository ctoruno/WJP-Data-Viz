logit_demo_panel <- function(mainData = data2plot,
                             line_color = "#003b8a",
                             line_size  = 2,
                             point_color = "#003b8a",
                             point_size   = 4) {
  
  plot <- ggplot(mainData, aes(x = reorder(factor, -order_variable), y = AME)) +
    geom_hline(yintercept = 0, lty = 1, color = "#fa4d57", lwd = 1)  +
    geom_linerange(aes(x = reorder(factor, -order_variable),  ymin = lower, ymax = upper),
                   lwd = line_size, position = position_dodge(width = .7), 
                   stat = "identity", color = line_color)+
    geom_point(aes(x = reorder(factor, -order_variable), y = AME), 
               size = point_size, position = position_dodge(width = .7), color = point_color) +
    geom_point(aes(x = reorder(factor, -order_variable), y = AME), 
               size = 2, position = position_dodge(width = .7), color = "white") +
    labs(y = "Less likely                               More likely") +
    scale_y_continuous(limits = c(-0.5, 0.5),
                       breaks = seq(-0.5, 0.5, by = 0.25),
                       expand = expansion(mult = 0.025), position = "right")+
    WJP_theme()+
    coord_flip() +
    theme(legend.position = "none",
          panel.background   = element_blank(),
          panel.grid.major.x = element_line(colour = "#d1cfd1", 
                                            size = 0.5, linetype = "dotted"),
          legend.title = element_blank(),
          axis.title.y       = element_blank(),
          axis.text.y        = element_text(family = "Lato Medium",
                                            size     = 3.514598*.pt,
                                            color    = "#4a4a49",
                                            hjust    = 0),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x  = element_blank(),
          panel.grid.minor.y = element_blank())
  
  return(plot)
}