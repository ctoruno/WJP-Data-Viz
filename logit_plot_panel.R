logit_plot_panel <- function(data2plot, title_label, subtitle_label, caption_label, main_colors){

  plot <- ggplot(data2plot, aes(x = reorder(factor, -order_variable), y = AME, color = category)) +
    geom_hline(yintercept = 0, colour = "black", lty = 2)  +
    geom_linerange(aes(x = reorder(factor, -order_variable),  ymin = lower, ymax = upper),lwd = 1.5, position = position_dodge(width = .7), stat = "identity")+
    geom_point(aes(x = reorder(factor, -order_variable), y = AME), size = 3.5, position = position_dodge(width = .7)) +
    geom_point(aes(x = reorder(factor, -order_variable), y = AME), size = 2, position = position_dodge(width = .7), color = "white") +
    #facet_wrap(~Category, strip.position = "top", scales = "free_y") +
    #geom_point(aes(x = reorder(factor, AME, fill = Regime), y = AME), size=1.2, color="white") +
    labs(title = title_label,
         subtitle = subtitle_label,
         caption = caption_label,
         y = "Less likely                               More likely") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(-0.5, 0.5),
                       breaks = seq(-0.5, 0.5, by = 0.25),
                       expand = expansion(mult = 0.025), position = "right")+
    scale_color_manual(values = main_colors)+
    theme_minimal()+
    coord_flip() +
    theme(legend.position = "none",
          panel.background   = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(family = "Lato Black",
                                     size   = 12,
                                     color  = "Black"),
          plot.title = element_text(family="Lato Black",
                                    size = 14,
                                    color = "Black"),
          strip.text = element_text(family = "Lato Black",
                                    size     = 12,
                                    color    = "Black"),
          plot.subtitle=element_text(family = "Lato Full",
                                     face   = "italic",
                                     size   = 12,
                                     vjust  = 0,
                                     color  = "Black",
                                     margin = unit(c(0,0,0.5,0), "cm")),
          plot.caption = element_text(family="Lato Light Italic",
                                      size = 10,
                                      color = "Black",
                                      hjust = 0.5,
                                      vjust = -1,
                                      margin = unit(c(0.5,0.5,0.5,0.5), "cm")),
          axis.title.y       = element_blank(),
          axis.title.x       = element_text(family = "Lato Black",
                                            size     = 10,
                                            color    = "Black"),
          axis.text.y        = element_text(family = "Lato Full",
                                            face     = "plain",
                                            size     = 10,
                                            color    = "Black"),
          axis.text.x = element_text(family = "Lato Full",
                                     face   = "bold",
                                     size   = 10,
                                     color  = "Black", hjust = 0.5),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x  = element_blank(),
          panel.grid.minor.y = element_blank())
  return(plot)

}
