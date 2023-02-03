horizontal_edgebars <- function(data2plot    = NULL,
                                y_value      = NULL,
                                x_var        = NULL,
                                label_var    = NULL,
                                group_var    = NULL,
                                x_lab_pos    = x_pos,
                                y_lab_pos    = 0,
                                bar_color    = "#2a2a94",
                                margin_top   = 20) {
  
  a <- ggplot(data = data2plot, aes(x = reorder({{x_var}},{{x_lab_pos}}),
                                    y = {{y_value}}, fill = {{group_var}})) +
    geom_bar(aes(fill = {{group_var}}),
             position = "stack", 
             stat     = "identity",
             width    = 0.35, 
             show.legend = F) +
    geom_richtext(aes(x = {{x_lab_pos}}, label = {{x_var}}, y = {{y_lab_pos}}-0.01,
                      family = "Lato Full", fontface = "plain"),
                  fill = NA, label.color = NA, hjust = 0, vjust = 0, size = 3.514598) +
    geom_text(aes(x = reorder({{x_var}}, {{x_lab_pos}}),
                  y = {{y_value}},
                  label = label),
              color = "#4a4a49",
              family = "Lato Full",
              fontface = "bold", size = 3.514598, hjust = -0.25) +
    scale_fill_manual(values = c("value" = bar_color,
                                 "empty_value" = "#f3f3f3")) +
    scale_y_continuous(expand = expansion(mult = c(0,0.15))) +
    coord_flip(clip = "off") +
    WJP_theme() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          plot.margin = margin(margin_top, 0, -15, 0),
          plot.background = element_blank())
  
  return(a)
}
