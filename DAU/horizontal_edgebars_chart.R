horizontal_edgebars_chart <- function(data2plot, 
                                      title_label, 
                                      subtitle_label, 
                                      main_color, 
                                      labelGeom, 
                                      title_size = 12,
                                      subtitle_size = 10,
                                      axis_size = 8) {
  
  plot <- ggplot(data2plot)+ aes(x = target_value, y = reorder(target, -order_value), fill = pos) +
    geom_bar(position="stack", stat="identity", width=0.60, color = main_color) +
    scale_fill_manual(values = c("white", main_color)) +
    labs(title = title_label,
         subtitle = subtitle_label,
         x = "",
         y = "")  +
    geom_text(aes(label = label_value, color = labelGeom, x = 0.5), check_overlap = T, size = 2.5) + 
    coord_cartesian(xlim = c(0, 1), clip = "off") +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, 1),
                       breaks = seq(0, 1, by = 0.2),
                       expand=c(0, 0)) +
    scale_color_manual(values = c("color" = "white", "no_color" = main_color)) +
    theme_minimal()+
    theme(legend.position="none",
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_text(family = "Lato Full",
                                   face = "plain",
                                   size = axis_size), 
          plot.background = element_rect(fill = "white", colour = "white"),
          plot.subtitle=element_text(family = "Lato Full", 
                                     face   = "italic",
                                     size   = subtitle_size, 
                                     vjust  = 0, 
                                     color  = "Black", 
                                     margin = margin(0,0,0,0)),
          plot.title=element_text(family="Lato Black", 
                                  size= title_size, 
                                  hjust = -0.0,
                                  color="Black", 
                                  margin=margin(0,0,10,0)),
          plot.title.position = "plot",
          panel.grid.major.x = element_blank())
  
  return(plot)
}
