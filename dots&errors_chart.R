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
##                  1. Dots + Error Bars Demmographics                                                      ----
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
    colorSet,           # Vector of length 2. First color is the dot + errorBar color.
                        # Second color is the panel strip color.
    percentage_out = T  # The target variable is in a % scale?
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
    )
  
  if (percentage_out == T) {
    plot <- plot +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  }
  
  plot <- plot +
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


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                  1. Horizontal Dots + Error Bars Chart                                                   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

wjp_dotsGROUPS <- function(
    data,               # Data Frame to use, it MUST be the merge.dta or a subset of it.
                        # A data frame with the same structure than merge.dta also works.
                        # For example, df <- merge %>% mutate(new_var = argument)
    
    country,            # For which country is this plot for?
    year,               # Values are going to be subset for this specific year
    target_variables,   # Variable to display in the Y-axis
    groups,             # Grouping variable on which to perform the comparison
    title,              # Plot Title
    subtitle,           # Plot Subtitle
    xtitle,             # X-Axis title after the coord_flip()
    colorSet,           # Vector of length equal to the number of groups.
    percentage_out = T, # The target variable is in a % scale?
    modified_labs  = T, # Logical value. Are the labels for each target variable going to be modify?
    fn.labels           # Funtion to apply in order to modify labels. 
                        # It has to assume that all target variables are summarized in a variable 
                        # called "variable". For example:
                        #   fn <- case_when(
                        #     variable == "target_variable1" ~ "Label 1",
                        #     variable == "target_variable2" ~ "Label 2"
                        #   )
)
{
  
  # Preparing data for plot
  data2plot <- data %>%
    rename(grouping = groups) %>%
    group_by(grouping) %>%
    summarise(across(all_of(target_variables),
                     mean, 
                     na.rm = T,
                     .names = "{.col}-mean"),
              across(all_of(target_variables),
                     sd, 
                     na.rm = T,
                     .names = "{.col}-sd"),
              across(all_of(target_variables),
                     ~ sum(!is.na(.x)), 
                     na.rm = T,
                     .names = "{.col}-n")) %>%
    pivot_longer(
      !grouping, 
      names_to = c("variable", ".value"), 
      names_sep = "-", 
      values_drop_na = TRUE
    ) %>%
    mutate(se = sd/sqrt(n),
           ci = se * qt((1-0.05)/2 + .5, n-1))
  
  # Modifying labels for grouping variable if required
  if (modified_labs == T) {
    data2plot <- data2plot %>%
      mutate(
        across(all_of("variable"),
               fn.labels)
      )
  }
    
  # Modifying value labels
  if (percentage_out == T) {
    data2plot <- data2plot %>%
      mutate(label    = paste0(format(round(mean*100, 1),
                                      nsmall = 1),
                               "%"))
  } else {
    data2plot <- data2plot %>%
      mutate(label    = format(round(mean, 1), 
                               nsmall = 1))
  }
  
  # Making the ggplot
  plot <- ggplot(data  = data2plot,
                 aes(x = variable,
                     y = mean,
                     color  = grouping,
                     label  = label)) +
    geom_errorbar(aes(ymin  = mean - ci,
                      ymax  = mean + ci,
                      color = grouping),
                  width     = 0.2, 
                  alpha     = 0.9, 
                  size      = 1.25,
                  show.legend = F) +
    geom_point(aes(color = grouping),
               size = 2) +
    labs(
      title    = title,
      subtitle = subtitle,
      y        = xtitle
    ) +
    scale_color_manual(values = colorSet,
                       labels = paste("<span style='color:",
                                      colorSet,
                                      "'>",
                                      names(colorSet),
                                      "</span>"))
  
  if (percentage_out == T) {
    plot <- plot +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  }
  
  plot <- plot +
    coord_flip() +
    WJP_theme() +
    theme(
      axis.line.y.left   = element_line(size     = 1.2, 
                                         color    = "black", 
                                         linetype = "solid"),
      axis.title.y       = element_blank(),
      axis.text.y        = element_text(family   = "Lato Full",
                                        face     = "bold",
                                        size     = 13,
                                        color    = "black"), 
      panel.grid.major.x = element_blank(),
      legend.text        = element_markdown(size   = 9, 
                                            family = "Lato Full", 
                                            face   = "bold"),
      legend.key         = element_rect(fill = "white"),
      legend.position    = "bottom" 
    )
  
}
