## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            WJP Data Viz functions: Diverging Stacked Bars Chart
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     October 3rd, 2022
##
## This version:      October 3rd, 2022
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                  1. Diverging Stacked Bars                                                               ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

wjp_divBarsVARIABLES <- function(
    data,               # Data Frame to use, it MUST be the merge.dta or a subset of it.
                        # A data frame with the same structure than merge.dta also works.
                        # For example, df <- merge %>% mutate(new_var = argument)
    
    country,            # For which country is this plot for?
    year,               # Values are going to be subset for this specific year
    target_variables,   # Variables to display in the Y-axis
    target_labs.fn,     # Function that returns the labes for the target variables.
                        # It must assume that ALL the target variables are within a column named category.
                        # For example:
                        #   fn <- case_when(
                        #     category == "target_variable1" ~ "Label 1",
                        #     category == "target_variable2" ~ "Label 2"
                        #   )
    negLabs,            # Vector containing the negative(contrasting) values of the target values.
                        # IMPORTANT: Order values from the most extreme to the less extreme value
                        # For example: c("Muy en desacuerdo", "En desacuerdo")
    posLabs,            # The opposite to negLabs
                        # IMPORTANT: Order values from the most extreme to the less extreme value
    title,              # Plot Title
    subtitle,           # Plot Subtitle
    ytitle,             # Plot Title
    colorSet,           # Vector of length 4.
    percentage_out = T, # Logical. The target variable is in a % scale?
    transformation = T, # Logical. Do target variables need a transformation?
    trans.fn            # Function that returns the transformed target variables.
                        # For example:
                        # if_else(targetVar == 99, NA_real_, as.numeric(targetVar))
)
{
  
  # Preparing data for plot: Subsetting and labelling
  data2plot <- data %>%
    filter(country == country.c & year == year.y) %>%
    select(all_of(target_variables)) %>%
    mutate(
      across(everything(),
             trans.fn)
    ) 
  
  # Save the original column names
  original_names <- names(data2plot)
  
  # Calculating the N for each value of the target variable
  data2plot <- lapply(original_names,
                       function(var) {
                         
                         data2plot %>%
                           select(all_of(var)) %>%
                           rename(pivot = 1) %>%
                           group_by(pivot) %>%
                           count()
                       }) %>%
    purrr::reduce(left_join, by = "pivot") %>%
    filter(!is.na(pivot))
  
  # Recovering original names
  names(data2plot) <- c("answer", original_names)
  
  # Preparing data for plot: Pivoting
  data2plot <- data2plot %>%
    pivot_longer(!answer,
                 names_to  = "category",
                 values_to = "value") %>%
    mutate(across(all_of("category"),
                  target_labs.fn))
  
  # Preparing data for plot: Calculating percentages
  data2plot <- data2plot %>%
    group_by(category) %>%
    mutate(
      total      = sum(value),
      perc       = round((value/total)*100, 1),
      perc4plot  = if_else(answer %in% negLabs, 
                           perc*-1, perc),
      ordering   = sum(perc4plot, na.rm = T),
      pos_values = if_else(perc4plot > 0, 1, 0),
      neg_values = if_else(perc4plot < 0, 1, 0),
      pos_labels = perc*pos_values,
      neg_labels = perc*neg_values,
      pos_labels = if_else(answer == posLabs[1], 
                           paste0(round(sum(pos_labels, na.rm = T), 0), "%"), 
                           NA_character_),
      neg_labels = if_else(answer == negLabs[2], 
                           paste0(round(sum(neg_labels, na.rm = T), 0), "%"), 
                           NA_character_)
    )
  
  plot <- ggplot(data = data2plot, 
                 aes(x = reorder(category, ordering), 
                     y = perc4plot, 
                     fill = factor(answer,
                                   levels = c(negLabs, posLabs)
                     ))) +
    geom_bar(stat  = "identity",
             width = 0.6) +
    geom_hline(yintercept = 0) +
    geom_text(aes(x     = reorder(category, ordering),
                  label = pos_labels, "%"),
              y         = 90,
              hjust     = 0,
              size      = 4,
              family    = "Lato Full",
              fontface  = "italic") +
    geom_text(aes(x     = reorder(category, ordering),
                  label = neg_labels, "%"),
              y         = -90,
              size      = 4,
              hjust     = 1,
              family    = "Lato Full",
              fontface  = "italic") +
    labs(title    = title,
         subtitle = subtitle,
         y        = xtitle) +
    coord_flip() +
    scale_y_continuous(limits = c(-120, 120),
                       breaks = seq(-100, 100, by = 25),
                       labels = abs(seq(-100, 100, by = 25)),
                       expand = c(0, 0))+
    scale_fill_manual(values = colorSet,
                      labels = paste("<span style='color:",
                                     colorSet,
                                     "'>",
                                     names(colorSet),
                                     "</span>")) +
    WJP_theme() +
    theme(axis.line.x.bottom = element_line(size     = 1.2, 
                                            color    = "black", 
                                            linetype = "solid"),
          legend.position    = "top",
          legend.spacing.x   = unit(0.25, "cm"),
          legend.text        = element_markdown(size   = 7, 
                                         family = "Lato Full", 
                                         face   = "bold"),
          panel.grid.major   = element_blank(),
          plot.margin        = margin(0, 20, 15, 0))
} 