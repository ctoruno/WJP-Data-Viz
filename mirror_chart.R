mirror_plot <- function(data2plot, label_title, comparison, label_value_left, label_value_right) { 
  
  p1 <- ggplot(data2plot)+ aes(x = Value, y = target, fill = {{comparison}})+
    facet_wrap(vars({{comparison}}), scales = "free_x") + 
    geom_bar(position="stack", stat="identity", width=0.6, color = c("white"))+
    scale_fill_manual(values = c("#C6CDF7","#ECCBAE"))+
    labs(title = {{label_title}},x=" ",y=" ",
         subtitle = "",
         fill=" ")+
    #geom_text(aes(x=0,label = label_category, vjust = 2.25, hjust = "left"), size = 3) + 
    geom_text(aes(label = {{label_value_left}}, x = 0.5), color = "black", size = 3) + 
    geom_text(aes(label = {{label_value_right}}, x = -0.5), color = "black", size = 3) + 
    coord_cartesian(xlim = c(-1, 1), clip = "off") +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(-1, 1),
                       breaks = seq(-1, 1),
                       expand=c(0, 0)) +
    theme_minimal()+
    theme(legend.position="none",
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_text(family = "Lato Regular", size = 12, hjust = 0.5),
          plot.background = element_rect(fill = "white", colour = "white"),
          plot.subtitle=element_text(family = "Lato Light Italic", size=14, hjust = 1,color="Black"),
          plot.title=element_text(family="Lato Black",size=16,hjust = 0.5,color="Black"),
          plot.title.position = "plot",
          plot.margin = unit(c(1,1,1,1), "lines"),panel.grid.major.x = element_blank(),
          strip.text = element_text(family = "Lato Black",
                                    size     = 12,
                                    color    = "Black"));p1
  
  p1_g <- ggplotGrob(p1)
  
  p1_g$widths[7] <- p1_g$widths[4] - unit(5, "cm")
  p1g_axis <- gtable_filter(p1_g, "axis-l-1-1") 
  
  g_plot <- p1_g %>% 
    gtable_add_grob(p1g_axis, l = 7, t = 8, name = "middle_axis") %>% 
    delete_col("axis-l-1-1") %>% # delete the original axis
    # add the axis to the middle
    gtable_add_grob(textGrob("", gp = gpar(fontsize = 12, fontfamily="Lato Regular")), l = 7, t = 7) 
  # add the top label
  
  
  plot_final <- ggplotify::as.ggplot(g_plot)
  
  return(plot_final)
}