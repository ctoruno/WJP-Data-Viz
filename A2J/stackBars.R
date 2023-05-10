barschart <- function(data2plot = data2plot,
                      country  = country,
                      category = category,
                      values   = values,
                      data2text = data2text,
                      label_value = prop,
                      colors4plot = colors4plot,
                      axis_label = "") {
  
  ggplot() + 
    geom_col(data = data2plot,
             aes(x = {{country}}, y = values, fill = category), width = 0.5,
             show.legend = F) +
    scale_y_continuous(limits = c(0, 125),
                       breaks = c(seq(0,100,20), 125),
                       labels = c(paste0(seq(0,100,20), "%"), {{axis_label}}),
                       position = "right") +
    scale_fill_manual(values = colors4plot) +
    geom_text(data = data2text, 
              aes(y    = 125, x = {{country}}, label = {{label_value}}),
              color    = "black",
              family   = "Lato Full",
              fontface = "bold") +
    coord_flip()  +
    WJP_theme() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = c("#5e5c5a", 
                                                      "#5e5c5a", 
                                                      "#5e5c5a", 
                                                      "#5e5c5a", 
                                                      "#5e5c5a", 
                                                      "#5e5c5a", 
                                                      "white")),
          axis.title.y       = element_blank(),
          axis.title.x       = element_blank(),
          axis.text.y        = element_text(hjust = 0))
  
}