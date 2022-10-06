slope_plot <- function(data2plot, group_variable, label_title, label_subtitle, label_caption, mainColor) {
  
  plot <- ggplot(data = data2plot, aes(x = year, y = value2plot, group = {{group_variable}})) +
    geom_line(aes(color = allcolors, alpha = 1), size = 2) +
    geom_point(aes(color = allcolors, alpha = 1), size = 2) +
    geom_text_repel(data = data2plot %>% filter(year == "Two Years Before Covid"), 
                    aes(label = paste0(country, " - ", value2plot, "%")), 
                    hjust = 1.35, 
                    fontface = "bold", 
                    size = 3,
                    nudge_x = -.25, 
                    direction = "y") +
    geom_text_repel(data = data2plot %>% filter(year == "Two Years After Covid"), 
                    aes(label = paste0(country, " - ", value2plot, "%")) , 
                    hjust = -.35, 
                    fontface = "bold", 
                    size = 3,
                    nudge_x = .25, 
                    direction = "y") +
    scale_color_manual(values = c(mainColor,"#E1E0DE")) +
    #  Labelling as desired
    labs(
      title = {{label_title}},
      subtitle = {{label_subtitle}},
      caption = {{label_caption}}
    ) +
    # move the x axis labels up top
    scale_x_discrete(position = "top") +
    theme_bw() +
    # Format tweaks
    # Remove the legend
    theme(legend.position = "none") +
    # Remove the panel border
    theme(panel.border     = element_blank()) +
    # Remove just about everything from the y axis
    theme(axis.title.y     = element_blank()) +
    theme(axis.text.y      = element_blank()) +
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    # Remove a few things from the x axis and increase font size
    theme(axis.title.x     = element_blank()) +
    theme(panel.grid.major.x = element_blank()) +
    theme(axis.text.x.top   = element_text(family = "Lato Black", size = 12, color = "Black")) +
    # Remove x & y tick marks
    theme(axis.ticks       = element_blank()) + 
    theme(plot.subtitle = element_text(family = "Lato Full", size = 14, face = "italic"),
          plot.title = element_text(family = "Lato Black", size = 16, color = "Black"),
          plot.title.position = "plot",
          plot.caption = element_text(family = "Lato Full", size = 10, hjust = 0.5)) #  Labelling as desired
  return(plot)
}