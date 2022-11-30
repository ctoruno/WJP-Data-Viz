horizontal_edgebars <- function(data2plot    = NULL,
                                y_value      = NULL,
                                x_var        = NULL,
                                order_value  = NULL,
                                label_var    = NULL,
                                group_var    = NULL,
                                x_lab_pos    = x_pos,
                                y_lab_pos    = 0,
                                bar_color    = "#2a2a94",
                                margin_top   = 20) {
  
  a <- ggplot(data = data2plot, aes(x = reorder({{x_var}},{{order_value}}),
                                    y = {{y_value}})) +
    geom_bar(aes(fill = group),
             position = "stack", 
             stat     = "identity",
             width    = 0.35, 
             show.legend = F) +
    geom_richtext(aes(x = {{x_lab_pos}}, label = {{x_var}}, y = {{y_lab_pos}},
                      family = "Lato Full", fontface = "plain"),
                  fill = NA, label.color = NA, hjust = 0, vjust = 0, size = 3.514598) +
    geom_text(aes(x = reorder({{x_var}}, {{order_value}}),
                  y = value + 0.15,
                  label = label),
              color = "#4a4a49",
              family = "Lato Full",
              fontface = "bold", size = 3.514598) +
    scale_fill_manual(values = c("value" = bar_color,
                                 "empty_value" = "#f3f3f3")) +
    scale_y_continuous(expand = expansion(mult = c(0,0.075))) +
    coord_flip(clip = "off") +
    WJP_theme() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          #axis.text.y   = element_text(vjust = -1, hjust = 0),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          plot.margin = margin(t = margin_top, r = 0, 0, 0),
          plot.background = element_blank())
  
  return(a)
}