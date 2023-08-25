dumbellChart <- function(
    data,                        # Data to use for plotting
    categories,                  # String with the name of the variable containing the categories to be displayed in the Y-axis          
    values,                      # String with the name of the variable containing the values to be plotted as dots
    group,                       # String with the name of the variable containing the groups to be displayed as different colors
    colors4plot,                 # Named vector containing the groups and their respective hex color codes
    base_color,                  # Color code for the line segment
    custom_order = F,            # Boolean. Should the categories have a custom order?
    order_values = NULL          # String with the name of the variable containing the order of categories
) {    
  
  # Adding a defaault order
  if (custom_order == F){
    data2plot <- data %>%
      ungroup() %>%
      mutate(
        order = row_number()
      )
  }
  
  # Renaming variables
  data2plot <- data2plot %>%
    rename(group    = all_of(group),
           cats     = all_of(categories),
           mean     = all_of(values),
           order    = all_of(order_values))
  
  # Creating a strip pattern
  strips <- data2plot %>%
    group_by(cats) %>%
    summarise() %>%
    mutate(ymin = 0,
           ymax = 1,
           xposition = rev(1:nrow(.)),
           xmin = xposition - 0.5,
           xmax = xposition + 0.5,
           fill = rep(c("grey", "white"), 
                      length.out = nrow(.))) %>%
    pivot_longer(c(xmin, xmax),
                 names_to  = "cat",
                 values_to = "x") %>%
    select(-cat) %>%
    filter(fill != "white")
  
  # Plotting chart
  ggplot() +
    geom_blank(data       = data2plot,
               aes(x      = reorder(cats, -order),
                   y      = mean,
                   group  = group,
                   color  = group)
    ) +
    geom_ribbon(data      = strips,
                aes(x     = x,
                    ymin  = ymin,
                    ymax  = ymax,
                    group = xposition,
                    fill  = fill),
                show.legend = F) +
    scale_fill_manual(values = c("grey"  = "#EBEBEB",
                                 "white"  = "#FFFFFF"),
                      na.value = NULL) +
    geom_line(data = data2plot,
              aes(x      = reorder(cats, -order),
                  y      = mean),
              color = base_color,
              linewidth = 1) +
    geom_point(data = data2plot,
               size = 3,
               aes(x      = reorder(cats, -order),
                   y      = mean,
                   color  = group)
    ) +
    scale_color_manual(values = colors4plot) + 
    scale_y_continuous(limits   = c(0,1),
                       breaks   = seq(0,1, 0.25),
                       labels   = paste0(c("0", "25", "50", "75", "100"), "%"),
                       position = "right") +
    scale_x_discrete(expand = c(0,0)) +
    coord_flip() +
    WJP_theme() +
    theme(
      legend.position    = "none",
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(size     = 0.25,
                                        colour   = "#5e5c5a",
                                        linetype = "dashed"),
      panel.ontop        = T,
      panel.background   = element_blank(),
      plot.background    = element_blank(),
      axis.title.x       = element_blank(),
      axis.title.y       = element_blank(),
      axis.text.y        = element_text(family   = "Lato Full",
                                        face     = "plain",
                                        size     = 3.514598*.pt,
                                        color    = "#524F4C", 
                                        hjust    = 0),
      axis.text.x        = element_text(family = "Lato Full",
                                        face   = "plain",
                                        size   = 3.514598*.pt,
                                        color  = "#524F4C",
                                        hjust  = 0.5)
    )
}
