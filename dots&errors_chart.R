## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            WJP Data Viz functions: Dot + Error Bars Chart
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     September 28th, 2022
##
## This version:      September 28th, 2022
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                  1. Dots + Error Bars Chart                                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

wjp_dotsDEMOGRAPHICS <- function(
    data,               # Data Frame to use, it MUST be the merge.dta or a subset of it.
                        # A data frame with the same structure than merge.dta also works.
                        # For example, df <- merge %>% mutate(new_var = argument)
    
    country,            # For which country is this plot for?
    year,               # Values are going to be subset for this specific year
    target_variable,    # Variables to display in the Y-axis
    groups,             # Groups to 
    title,              # Plot Title
    subtitle,           # Plot Subtitle
    ytitle,             # Plot Title
    colorSet            # Vector of length 2. First color is the dot + errorBar color.
                        # Second color is the panel strip color.
)
{
  
  # Defining data to plot
  data2plot <- 
    map_dfr(groups, 
            function(group_var){
             data %>%
                filter(country == country & year == year) %>%
                select(all_of(target_variable), 
                       all_of(group_var)) %>%
                rename(value = 1,
                       target  = 2) %>%
                mutate(target = as.character(target)) %>%
                group_by(target) %>%
                summarise(value_mean = mean(value, na.rm = T),
                          value_sd   = sd(value, na.rm = T),
                          n          = n()) %>%
                mutate(
                  category = case_when(
                    group_var == "skin_color" ~ "Skin Color",
                    group_var == "gender"     ~ "Gender",
                    group_var == "income_aux" ~ "Income"
                  ),
                  target = case_when(
                    target == "Dark Brown"  ~ "Dark\nBrown",
                    target == "Light Brown" ~ "Light\nBrown",
                    target == "1"           ~ "1\nLow",
                    target == "5"           ~ "5\nHigh",
                    TRUE ~ target
                  ),
                  value_se = value_sd/sqrt(n),
                  value_ci = value_se * qt((1-0.05)/2 + .5, n-1)
                )
            }) %>%
    filter(!is.na(target))
  
  # Creating ggplot
  plot <- ggplot(data2plot,
                 aes(x = target,
                     y = value_mean)) +
    facet_wrap(~category,
               scales = "free_x") +
    geom_errorbar(aes(ymin = value_mean - value_ci,
                      ymax = value_mean + value_ci),
                  width    = 0.2, 
                  colour   = colorSet[1], 
                  alpha    = 0.9, 
                  size     = 1.25) +
    geom_point(color = colorSet[1],
               size  = 2) +
    labs(
      title    = title,
      subtitle = subtitle,
      y        = ytitle,
      x        = "Group"
    ) +
    WJP_theme() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(size     = 0.5,
                                        colour   = "grey93",
                                        linetype = "solid"),
      strip.background   = element_rect(fill     = colorSet[2]),
      strip.text         = element_text(family   = "Lato Full",
                                        face     = "bold",
                                        size     = 13,
                                        color    = "white")
    )
}

