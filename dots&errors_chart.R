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

wjp_dotsCOMPARISON <- function(
    data,
    country,
    overtime = F,
    year = NULL,
    target_indicator = NULL
)
{
  
  if (overtime == T) {
    target_indicator <- "year"
  }
  
  # Defining data to plot: Subsetting data
  data2plot <- data %>%
    filter(country %in% countries) %>%
    {if (overtime == F) filter(year == year) else .} %>%
    select(all_of(target_indicator))
  
  if (overtime == F) {
    data2plot <- data2plot %>%
      pivot_longer(
        cols           = everything(), 
        names_to       = "category", 
        values_to      = "answer"
      )
  }
    

    mutate(
      question = if_else(str_detect(category, "q\\d{1}a"),
                         "Influencing the hiring\nof friends/relatives",
                         "Influencing the award of\ncontracts to friends/relatives"),
      answer   = case_when(
        answer == 99 ~ NA_real_,
        answer < 3   ~ 1,
        answer > 2   ~ 0
      ),
      group   = case_when(
        str_detect(category, "3") ~ "Public Officials",
        str_detect(category, "4") ~ "Private Sector Employees",
        str_detect(category, "5") ~ "Elected Politicians"
      )
    ) %>%
    select(!category) %>%
    group_by(group, question) %>%
    summarise(value_dot = mean(answer, na.rm = T),
              value_sd  = sd(answer, na.rm = T),
              n         = n()) %>%
    mutate(value_se = value_sd/sqrt(n),
           value_ci = value_se * qt((1-0.05)/2 + .5, n-1))
  
  # Making the ggplot
  plot <- 
    ggplot(data = data2plot, 
           aes(x     = question, 
               y     = value_dot, 
               color = group)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = value_dot-value_ci, 
                      ymax = value_dot+value_ci), 
                  width    = 0.25, 
                  alpha    = 0.9, 
                  size     = 1,
                  show.legend = F) +
    labs(title    = paste("Abuse of Power for Economic and Social Gains"),
         subtitle = paste("Percentage of people that believe that the following behaviors", 
                          "happen frequently\nin each of these groups of individuals"),
         y        = "Percentage of respondents (%)"
    ) +
    # scale_color_manual(values = c("#33658A", "#DC9E82", "#86BBD8")) +
    scale_color_manual(values  = c("#33658A", "#DC9E82", "#86BBD8"),
                       labels  = paste("<span style='color:",
                                       c("#33658A", "#DC9E82", "#86BBD8"),
                                       "'>",
                                       c("Elected Politicians", 
                                         "Private Sector Employees", 
                                         "Public Officials"),
                                       "</span>")) +
    scale_y_continuous(labels = scales::percent_format(scale = 100)) +
    coord_flip() +
    WJP_theme() +
    theme(legend.position    = "top",
          legend.key         = element_rect(fill = "white"),
          legend.text        = element_markdown(size   = 8, 
                                                family = "Lato Full", 
                                                face   = "bold"),
          panel.grid.major.x = element_blank(),
          # axis.title.x       = element_blank(),
          axis.ticks.x       = element_line(),
          axis.title.y       = element_blank(), 
          axis.line.y.left   = element_line(size = 0.5,
                                            colour   = "grey55",
                                            linetype = "solid"),
          axis.line.x.bottom = element_line(size = 0.5,
                                            colour   = "grey55",
                                            linetype = "solid")
    )
  
}