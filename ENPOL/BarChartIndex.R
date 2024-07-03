index_setUp.fn <- function(data = master_data.df,
                           main_var){
  
  data_subset.df <- data %>%
    rename(main_var = all_of(main_var)) %>%
    group_by(main_var) %>%
    summarise(counter = n()) %>%
    drop_na %>%
    mutate(
      value2plot = counter / sum(counter),
      value2plot = value2plot*100,
      figure = paste0(round(value2plot,0), "%"),
      order_var = rank(main_var),
      labelx =paste0(round(main_var*100,0), "%")
    )
  
}




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
    xlab("Porcentaje de criterios cumplidos") +
    ylab("Porcentaje de personas sentenciadas") +
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

