## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            LAC Reports Data Viz functions: Radar Chart
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     November 23rd, 2022
##
## This version:      November 23rd, 2022
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

LAC_radarChart <- function(
    data,             # Data frame with data
    axis_var,         # Variable containing the groups for the axis
    target_var,       # Variable containing the values to be plotted
    label_var,        # Variable containing the labels to be displayed
    order_var,        # Variable containing the display order of categories
    colors,           # Colors to apply to lines. First code will be applied to percentages in labels
    latestYear = latestYear       # Category to show in the radar
){
  
  # Renaming variables in the data frame to match the function naming
  data <- data %>%
    rename(axis_var    = all_of(axis_var),
           target_var  = all_of(target_var),
           label_var   = all_of(label_var),
           order_var   = all_of(order_var))
  
  # Counting number of axis for the radar
  nvertix <- length(unique(data$axis_var))
  
  # Distance to the center of the web 
  central_distance <- 0.2
  
  # Function to generate radar coordinates
  circle_coords <- function(r, n_axis = nvertix){
    fi <- seq(0, 2*pi, (1/n_axis)*2*pi) + pi/2
    x <- r*cos(fi)
    y <- r*sin(fi)
    
    tibble(x, y, r)
  }
  
  # Function to generate axis lines
  axis_coords <- function(n_axis = nvertix){
    fi <- seq(0, (1 - 1/n_axis)*2*pi, (1/n_axis)*2*pi) + pi/2
    x1 <- central_distance*cos(fi)
    y1 <- central_distance*sin(fi)
    x2 <- (1 + central_distance)*cos(fi)
    y2 <- (1 + central_distance)*sin(fi)
    
    tibble(x = c(x1, x2), y = c(y1, y2), id = rep(1:n_axis, 2))
  }
  
  # Function to generate axis coordinates
  text_coords <- function(r      = 1.5, 
                          n_axis = nvertix){
    fi <- seq(0, (1 - 1/n_axis)*2*pi, (1/n_axis)*2*pi) + pi/2 + 0.01*2*pi/r
    x <- r*cos(fi)
    y <- r*sin(fi)
    
    tibble(x, y, r = r - central_distance)
  }
  
  # Y-Axis labels
    axis_measure <- tibble(
      r = rep(seq(0, 1, 0.2), each = nvertix),
      parameter = rep(data %>% filter(order_var == 1) %>% distinct(axis_var) %>% pull(axis_var), each = 6)
    )


  # Generating data points
  rescaled_coords <- function(r, n_axis = nvertix){
    fi <- seq(0, 2*pi, (1/n_axis)*2*pi) + pi/2
    tibble(r, fi) %>% 
      mutate(x = r*cos(fi), y = r*sin(fi)) %>% 
      select(-fi)
  }
  
  rescaled_data <- data %>% 
    bind_rows(data %>% 
                filter(axis_var == data %>% 
                         filter(order_var == 1) %>% 
                         distinct(axis_var) %>% 
                         pull(axis_var)) %>%
                mutate(axis_var  = "copy",
                       order_var = nvertix)) %>%
    group_by(year) %>%
    arrange(order_var) %>%
    mutate(coords = rescaled_coords(target_var + central_distance)) %>%
    unnest(cols   = c(coords)) %>%
    mutate(across(x, 
                  ~.x*-1))
  
  # Generating ggplot
  radar <- 
    
    # We set up the ggplot
    ggplot(data = map_df(seq(0, 1, 0.20) + central_distance, circle_coords),
           aes(x = x, 
               y = y)) +
    
    # We draw the outter ring
    geom_polygon(data     = circle_coords(1 + central_distance),
                 linetype = "dotted",
                 color    = "#d1cfd1",
                 fill     = NA) +
    
    # We draw the inner rings
    geom_path(aes(group = r), 
              lty       = 2, 
              color     = "#d1cfd1") +
    
    # We draw the ZERO ring
    geom_polygon(data = map_df(seq(0, 1, 0.20) + central_distance, circle_coords) %>%
                   filter(r == 0.2),
                 fill      = NA,
                 linetype  = "solid",
                 color     = "#d1cfd1") +
    
    # Then, we draw the Y-axis lines
    geom_line(data = axis_coords(), 
              aes(x     = x, 
                  y     = y, 
                  group = id),
              color = "#d1cfd1") +
    
    # Along with its labels
    geom_text(data = axis_measure, 
              aes(x     = x, 
                  y     = y, 
                  label = to_percentage.fn(r*100)), 
              family      = "Lato Full",
              fontface    = "plain",
              color = "#524F4C") +
    
    # Then, we add the axis labels
    geom_richtext(data  = text_coords() %>%
                    mutate(n = row_number(),
                           across(x, 
                                  ~.x*-1),
                           across(c(x,y),
                                  ~if_else(n == 2, .x*1.125, .x))),  # We need to adjust by the long text in axis number 8
                  aes(x = x, 
                      y = y), 
                  label = data %>% 
                    arrange(order_var) %>% 
                    filter(year == latestYear) %>% 
                    pull(label_var),
                  family      = "Lato Full",
                  fontface    = "plain",
                  fill        = NA, 
                  label.color = NA) +
    
    # We add the data points along whith its lines
    geom_point(data = rescaled_data, 
               aes(x     = x, 
                   y     = y, 
                   group = year, 
                   color = as.factor(year)), 
               size      = 3) +
    geom_path(data = rescaled_data, 
              aes(x     = x, 
                  y     = y, 
                  group = year, 
                  color = as.factor(year)), 
              size = 1) +
    
    coord_cartesian(clip = "off") + 
    scale_x_continuous(expand = expansion(mult = 0.125)) + 
    scale_y_continuous(expand = expansion(mult = 0.10)) + 
    scale_color_manual(values = colors) +
    theme_void() +
    theme(panel.background   = element_blank(),
          plot.background    = element_blank(),
          legend.position    = "none")
  
  return(radar)
  
}
