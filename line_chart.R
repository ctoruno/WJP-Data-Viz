## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            WJP Data Viz functions: Line Chart
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     September 27th, 2022
##
## This version:      September 27th, 2022
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                  1. Line Plot for indicators (no countries)                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Defining function
wjp_lineINDICATOR <- function(
    data,                    # Data Frame to use, it MUST be the merge.dta or a subset of it.
                             # A data frame with the same structure than merge.dta also works.
                             # For example, df <- merge %>% mutate(new_var = argument)
    
    countries,               # Which countries to display?
    target_variables,        # Variables to display in the Y-axis

    percentage_out = T,      # Logical. Is the final value to be displayed in the plot a percentage?
    
    transform = T,           # Logical. Do variables require a transformation to display the final value?
                             # For example, Likert Scales need to be transformed so the desired values are
                             # equal to 1 and all other variables are equal to zero or NA.
    
    transformation,          # Function that would return a vector or 1 and 0 for the variables to be transformed.
                             # For example, function(tvar){if_else(tvar == 3 | tvar == 4, 1, 0)},
                             # where 3 and 4 are the values desired to be displayed as % in the final plot.
    
    displayColors,           # Colors to use in the plot. It must be the same length as target_variables
    plotTitle,               # Plot Title
    plotSubtitle,            # Plot Subtitle
    yTitle,                  # Y-Axis Title
    legendPos = "none",      # Where should the legend be displayed? Top, Bottom, Right, Left?
    legendLabels             # Variable labels in legend. It must be the same length as target_variables
)
{
  
  # Preparing data to plot: subsetting data
  data2plot <- data %>%
    filter(country %in% countries) %>%
    select(year, all_of(target_variables))
  
  # Preparing data to plot: applying transformation to variables
  if (transform == T) {
    data2plot <- data2plot %>%
      mutate(across(all_of(target_variables), 
                    transformation))
  }
  
  # Preparing data to plot: generating final data for plot
  data2plot <- data2plot %>%
    pivot_longer(!year, 
                 names_to  = "target", 
                 values_to = "target_value") %>%
    group_by(year, target) %>%
    summarise(value2plot = round(mean(target_value, na.rm = T), 3))
  
  # Apply percentage formating to value labels?
  if (percentage_out == T) {
    data2plot <- data2plot %>%
      mutate(labels = paste0(round(value2plot*100, 0), "%"))
  } else {
    data2plot <- data2plot %>%
      mutate(labels = round(value2plot, 0))
  }
  
  # Plotting results
  p <- ggplot(data2plot, 
              aes(y     = value2plot, 
                  x     = year, 
                  color = target)) +
    geom_line(show.legend   = F,
              alpha         = 0.35) + 
    geom_point(size         = 1.75) + 
    
    # Applying ggrepel for a better visualization of plots
    geom_text_repel(mapping = aes(y     = value2plot, 
                                  x     = year, 
                                  label = labels),
                    family      = "Lato Full", 
                    fontface    = "bold",
                    show.legend = F,
                    
                    # Additional options from ggrepel package:
                    min.segment.length = Inf, 
                    seed               = 42, 
                    box.padding        = 0.5, 
                    direction          = "both", 
                    force              = 5, 
                    force_pull         = 1, 
                    nudge_y            = 0) +
    
    labs(title    = plotTitle,
         subtitle = plotSubtitle,
         y        = yTitle) +
    scale_x_continuous(limits = c(min(data2plot$year), 
                                  max(data2plot$year)),
                       breaks = seq(min(data2plot$year), 
                                    max(data2plot$year), 
                                    by = 1),
                       expand = expansion(mult = 0.1)) +
    scale_color_manual(values = displayColors,
                       labels = paste("<span style='color:",
                                      displayColors,
                                      "'>",
                                      legendLabels,
                                      "</span>"))
  
  # Apply percentage formating to Y-Axis?
  if (percentage_out == T) {
    p <- p +
      scale_y_continuous(
        labels = scales::percent_format(accuracy = 1),
        limits = c(0, 1),
        breaks = seq(0, 1, by = 0.2)
      )
  }
  
  # Applying a theme to plot
  p <- p +
    WJP_theme() + 
    theme(legend.position = legendPos,
          legend.text     = element_markdown(size   = 12,
                                             family = "Lato Full",
                                             face   = "bold",
                                             margin = margin(10, 0, 10, 0)),
          panel.grid.major.x = element_blank(), 
          legend.key         = element_rect(fill = "white"),
          axis.title.x       = element_blank())
  
  return(p)
}
