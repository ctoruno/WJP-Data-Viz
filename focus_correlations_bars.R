focus_correlations_bars <- function(data2plot, title_label, subtitle_label, ylab_label, main_colors) {
  
    a <- data2plot %>% 
    mutate(term = factor(term, levels = term[order(value2plot)])) %>%  # Order by correlation strength
    ggplot(aes(x = term, y = value2plot, fill = category)) +
    geom_bar(stat = "identity") +
    ylab(ylab_label) +
    scale_y_continuous(limits = c(-0.5, 0.5),
                       breaks = seq(-0.5, 0.5, by = 0.25),
                       expand=c(0, 0)) +
    scale_fill_manual(values = main_colors) +
    theme_minimal()+
    labs(title = title_label,
         subtitle = subtitle_label) +
    theme(legend.position="top",
          legend.title = element_blank(),
          legend.text = element_text(family = "Lato Light", size = 8),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y = element_text(family = "Lato Full",
                                      face = "plain",
                                      size = 6),
          axis.text.y=element_text(family = "Lato Full",
                                   face = "plain",
                                   size = 6),
          axis.text.x=element_text(family = "Lato Full",
                                   face = "plain",
                                   size = 6, angle = 90, margin=margin(0,0,0,0)), 
          plot.background = element_rect(fill = "white", colour = "white"),
          plot.subtitle=element_text(family = "Lato Full", 
                                     face   = "italic",
                                     size   = 8, 
                                     vjust  = 0, 
                                     color  = "Black", 
                                     margin = margin(0,0,10,0)),
          plot.title=element_text(family="Lato Black", size=10, hjust = -0.0,color="Black", margin=margin(0,0,5,0)),
          plot.title.position = "plot",
          panel.grid.major.x = element_blank())
  
  return(a)
}
