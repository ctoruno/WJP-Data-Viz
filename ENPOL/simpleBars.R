BarSimpleChartViz <- function(data = data2plot, 
                              x_var = main_var, 
                              y_var = value2plot, 
                              label_var = figure, 
                              fill_var = main_var,
                              order_var = order_var,
                              labels = labels,
                              shade_xminvalue,
                              shade_xmaxvalue,
                              x_labels = NULL  # Añadido argumento para etiquetas personalizadas
) {
  plt <- ggplot(data, 
                aes(x     = reorder({{x_var}},{{order_var}}),
                    y     = {{y_var}},
                    label = {{label_var}},
                    fill  = {{fill_var}})) +
    geom_bar(stat = "identity",
             show.legend = FALSE, width = 0.9) +
    scale_fill_gradient(low = "#2a2a9A", high = "#2a2a9A") +
    annotate('rect', xmin=0, xmax= shade_xminvalue, ymin=0, ymax=100, alpha=.1, fill="#fa4d57") +
    annotate('rect', xmin=shade_xmaxvalue, xmax= shade_xmaxvalue+1, ymin=0, ymax=100, alpha=.1, fill="#43a9a7") +
    geom_text(aes(y    = {{y_var}} + 10),
              color    = "#4a4a49",
              family   = "Lato Full",
              fontface = "bold") +
    labs(y = "% of respondents") +
    xlab("% de criterios cumplidos") +
    ylab("% de personas sentenciadas") +
    scale_y_continuous(labels = function(y) paste0(y, "%")) +
    scale_x_discrete(labels = x_labels) +  # Añadido para cambiar etiquetas del eje X
    WJP_theme() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "#D0D1D3"),
          axis.title.y       = element_text(color    = "#4a4a49",
                                            family   = "Lato Full"),
          axis.title.x       = element_text(color    = "#4a4a49",
                                            family   = "Lato Full"),
          axis.text.y        = element_text(hjust = 0))
  
  return(plt)
}
