lollipop_chart <- function(data2plot     = data2plot,
                           line_size     = 3,
                           point_size    = 4,
                           line_color    = "#c4c4c4",
                           point_color   = "#2a2a94",
                           categories    = category,
                           values        = value2plot,
                           order_value     = order_value
) {
  ggplot(data2plot) +
    geom_linerange(aes(y = reorder(category, order_value),  
                       xmin = 0, xmax = value2plot), 
                   size = line_size, color = line_color) +
    geom_point(aes(x = value2plot, y = reorder(category, order_value)),
               size = line_size, shape=16, color = point_color) +
    geom_text(aes(x = value2plot + 0.075, y = category, 
                  label = paste0(value2plot*100,"%"), 
                  family = "Lato Full", fontface = "bold"), 
              size= 3.514598, color = "black") +
    coord_cartesian(clip="off") +
    theme_minimal() +
    scale_x_continuous(breaks = seq(0, 1, by = 0.1),limits = c(0,1),
                       labels = scales::percent_format(accuracy = 1), position = "top") +
    WJP_theme() +
    theme(legend.position="bottom",
          panel.grid.major.x = element_line(colour = "#d1cfd1", 
                                            size = 0.5),
          panel.grid.major.y = element_blank(),
          panel.background = element_blank(),
          panel.grid.minor = element_blank(),
          legend.text = element_text(family = "Lato Bold"),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.y=element_text(family = "Lato Medium",
                                   size = 3.514598*.pt,
                                   color = "Black", hjust = 0))
}
