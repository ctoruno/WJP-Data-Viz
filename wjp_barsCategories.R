wjp_barsCategories <- function(data2plot, title_label, subtitle_label, ylab_label, main_color) {
  
  plot <- data2plot %>%
  ggplot(aes(x   = reorder(category, -value),
             y   = value)) +
    geom_bar(fill  = main_color,
             stat  = "identity",
             width = 0.7) +
    geom_text(aes(y     = value + 0.075,
                  label = paste0(format(round(value*100, 1),
                                        nsmall = 1),
                                 "%")),
              color     = main_color,
              family    = "Lato Full",
              fontface  = "bold.italic") +
    labs(title    = paste0(title_label),
         subtitle = paste0(subtitle_label),
         y        = ylab_label) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0,1),
                       breaks = seq(0,1,0.2),
                       expand = c(0,0)) +
    coord_flip() +
    WJP_theme() +
    theme(
      axis.line.y.left    = element_line(size     = 1.2, 
                                         color    = "black", 
                                         linetype = "solid"),
      axis.title.y        = element_blank(), 
      panel.grid.major    = element_blank())
      
      return(plot)
  
}
