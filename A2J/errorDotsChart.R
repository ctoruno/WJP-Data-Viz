errorDotsChart <- function(
    data,                        # Data to use for plotting
    categories,                  # String with the name of the variable containing the categories to be displayed in the Y-axis          
    mean_values,                 # String with the name of the variable containing the values to be plotted as dots
    lower,                       # String with the name of the variable containing the lower bound
    upper,                       # String with the name of the variable containing the upper bound
    group,                       # String with the name of the variable containing the groups to be displayed as different colors
    colors4plot,                 # Named vector containing the groups and their respective hex color codes
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
           mean     = all_of(mean_values),
           lower    = all_of(lower),
           upper    = all_of(upper),
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
  
  print(data2plot)
  
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
      geom_point(data = data2plot,
                 size = 3,
                 aes(x      = reorder(cats, -order),
                     y      = mean,
                     color  = group)
      ) +
      geom_errorbar(data = data2plot,
                    aes(x      = reorder(cats, -order),
                        y      = mean,
                        color  = group,
                        ymin   = lower,
                        ymax   = upper), 
                    width = 0.5, 
                    alpha = 0.5, 
                    linewidth = 1) +
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
