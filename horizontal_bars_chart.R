## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            WJP Data Viz functions: Horizontal Bars Charts
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
##                  1. Horizontal bars per country                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

wjp_barsCountries <- function(
    data,                   # Data Frame to use, it MUST be the merge.dta or a subset of it.
                            # A data frame with the same structure than merge.dta also works.
                            # For example, df <- merge %>% mutate(new_var = argument)
                            # ALL countries in the dataset will be plot. If you want a subset,
                            # you should pass a filtered dataframe, e.g. merge %>% filter()
    
    highlight = T,          # Logical. Highlight a specific country?
    hcountry = NULL,        # Which country?
    labelDis,               # Value to displace the value labels to the right of the bar
    labelDec = 1,           # Number of decimals for the label
    
    target_variable,        # X-Axis variable after coord_flip() --- Values to plot.
                            # Values to display across countries will be the mean of the targeted variable
    
    percentage_out = T,     # Logical. Is the final value to be displayed in the plot a percentage?
    
    displayColors,          # Colors to use in the plot. Length of 2. First color is to highlight a group.
                            # Second color is for non-highlighted groups. In case of just using the same color
                            # but with a different alpha value, write down the alpha value as the second element
                            # of the vector.
    
    plotTitle,              # Plot Title
    plotSubtitle,           # Plot Subtitle
    xTitle,                 # X-Axis Title
    Xlimits = NULL,         # Lower and Upper limit for the X-Axis 
    Xinterval = NULL        # Intervals between lower and Upper limit
)
{
  # Preparing data to plot: subsetting data
  data2plot <- data %>%
    select(country, all_of(target_variable)) %>%
    rename(target = 2) %>%
    group_by(country) %>%
    summarise(value2plot = mean(target, na.rm = T))
  
  if (highlight == T) {
    data2plot <- data2plot %>%
      mutate(highlighted = if_else(country == hcountry,
                                   "Highlighted",
                                   "Comparison"))
  } else {
    data2plot <- data2plot %>%
      mutate(highlighted = "Highlighted")
  }
  
  # Defining color equivalencies
  if (is.character(displayColors[2])) {
    colorEq <- c("Highlighted"  = displayColors[1],
                 "Comparison"   = displayColors[2])
  } else {
    colorEq <- c("Highlighted"  = displayColors[1])
  }
  
  # Defining alpha equivalencies
  if (!is.character(displayColors[2])) {
    alphaEq <- c("Highlighted"  = 1)
  } else {
    alphaEq <- c("Highlighted"  = 1,
                 "Comparison"   = 0.85)
  }
  
  # Generating ggplot
  p <- ggplot(data2plot,
              aes(x     = reorder(country, value2plot),
                  y     = value2plot,
                  fill  = highlighted)) +
    geom_bar(stat    = "identity",
             width   = 0.7,
             alpha   = 0.9) +
    geom_text(aes(y     = value2plot + labelDis,
                  label = format(round(value2plot, labelDec),
                                 nsmall = labelDec),
                  color = highlighted),
              family    = "Lato Full",
              fontface  = "bold.italic") +
    labs(title    = plotTitle,
         subtitle = plotSubtitle,
         y        = xTitle) +
    scale_fill_manual(values  = colorEq) + 
    scale_color_manual(values = colorEq) + 
    scale_alpha_manual(values = alphaEq)
    
    # Apply percentage formating to Y-Axis?
    if (percentage_out == T) {
      p <- p +
        scale_y_continuous(
          labels = scales::percent_format(accuracy = 1),
          limits = c(0, 1),
          breaks = seq(0, 1, by = 0.2)
        )
    } else {
      p <- p +
        scale_y_continuous(expand = c(0,0),
                           limits = Xlimits,
                           breaks = seq(Xlimits[1], Xlimits[2], Xinterval))
    }

  # Applying a theme to plot
  p <- p +
    coord_flip() +
    WJP_theme() +
    theme(legend.position    = "none",
          axis.title.y       = element_blank(),
          panel.grid.major.y = element_blank())
  
  return(p)
  
}
