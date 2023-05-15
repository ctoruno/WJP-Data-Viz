## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            A2J Report Data Viz functions: Choropleth Maps
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     May 12th, 2023
##
## This version:      May 12th, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

WJP_cmaps <- function(
    data,             # Data frame with the data to show in the map
    boundaries,       # Simple feature with the geometry boundaries
    iso_code,         # Name of the variable with the ISO three-letter codes per boundary
    target_var,       # Name of the variable with the values to show in the map
    colors,           # Named vector with the colors to use in the map
    breaks,
    categorical = F,
    groups = NULL
){
  
  # Preparing data for mapping
  data4map <- boundaries %>%
    left_join(data, by = iso_code) %>%
    rename(value4map = all_of(target_var))
  
  # Drawing map  
  if (categorical == F){
    
    ggplot(data4map,
           aes(fill = value4map)) +
      geom_sf(color = "white") +
      scale_fill_gradientn(colours  = colors,
                           values   = as.integer(names(colors)), 
                           limits   = c(breaks[1], breaks[length(breaks)]),
                           breaks   = breaks,
                           labels   = names(breaks),
                           na.value = "#cccccc") +
      theme_void() +
      theme(legend.position = "bottom",
            legend.text     = element_text(family   = "Lato Full",
                                           color    = "#222221",
                                           face     = "plain",
                                           size     = 10), 
            legend.title    = element_blank()) +
      guides(fill = guide_colorbar(ticks    = F,
                                   barwidth = unit(75.91532, "mm")))
  }
  
}
