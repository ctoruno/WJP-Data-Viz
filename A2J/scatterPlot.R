scatterPlot <- function(data2plot = data2plot,
                      xvariable  = Incidence,
                      yvariable = Severity,
                      category   = problem_code,
                      colors4plot = colors4plot,
                      labels = labelProblem
                      ) {
  
  ggplot() + 
  geom_point(data = data2plot,
             aes(y = {{yvariable}}, 
                 x = {{xvariable}},
                 color = {{category}}),
             show.legend = F) + # Show dots
  geom_smooth(data = data2plot,
              aes(y = {{yvariable}},
                  x = {{xvariable}}),
              method = "lm", se = T, color = "black", linetype="dashed", size = 1) +
    geom_text(data = data2plot, 
              aes(y    = {{yvariable}}, x = {{xvariable}}, label = {{labels}}),
              color    = "#222221",
              family   = "Lato Full",
              fontface = "bold", size = 2.811678, check_overlap = F,
              vjust = 0, nudge_y = 0.25) +
  labs(x = "Average prevalence of different \ntypes of legal problems in the \nlast two years",
       y = "Average perceived severity",
       note = "Note: Dotted line represents linear regression line") + 
  scale_y_continuous(limits = c(0, 10.5),
                     breaks = seq(0,10,2),
                     labels = paste0(seq(0,10,2)),
                     position = "left", expand = c(0,0)) +
  scale_x_continuous(limits = c(0, 27),
                     breaks = seq(0,25,5),
                     labels = paste0(seq(0,25,5), "%"),
                     position = "bottom", expand = c(0,0)) +
  scale_color_manual(values = colors4plot) +
  WJP_theme() +
  theme(axis.line        = element_line(color    = "#5e5c5a", linetype = "solid"))
}
