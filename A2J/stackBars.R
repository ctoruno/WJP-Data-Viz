stackBars <- function(data2plot = data2plot,
                      country  = country,
                      category = category,
                      values   = values,
                      data2text = data2text,
                      label_value = prop,
                      colors4plot = colors4plot,
                      axis_label = "",
                      order_values = order_values) {
  
  ggplot() + 
    geom_col(data = data2plot,
             aes(x = reorder({{country}}, {{-order_values}}), y = {{values}}, fill = {{category}}), 
             width = 0.5,
             show.legend = F) +
    scale_y_continuous(limits = c(0, 130),
                       breaks = c(seq(0,100,20), 120),
                       labels = c(paste0(seq(0,100,20), "%"), {{axis_label}}),
                       position = "right", 
                       expand = c(0,0)) +
    scale_fill_manual(values = colors4plot) +
    geom_text(data = data2text, 
              aes(y    = 120, x = reorder({{country}}, {{-order_values}}), label = {{label_value}}),
              color    = "#222221",
              family   = "Lato Full",
              fontface = "bold", size = 2.811678) +
    coord_flip()  +
    WJP_theme() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = c(rep("#5e5c5a", 6), "white")),
          axis.title.y       = element_blank(),
          axis.title.x       = element_blank(),
          axis.text.y        = element_text(hjust = 0),
          axis.line.y        = element_line(color    = "#5e5c5a",
                                            linetype = "solid"),
          axis.text.y        = element_text(family   = "Lato Full",
                                            face     = "plain",
                                            size     = 2.811678*.pt,
                                            color    = "#222221"))
  
}
