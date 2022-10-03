emoji_waffle_chart <- function(data2plot, title_label, subtitle_label, caption_label, main_color){
  parts=c("Yes"= (data2plot$Yes)*100, "No"= (data2plot$No)*100)
  names(parts) = paste0(names(parts)," ","="," ",parts,"%")
  
  waffle(parts,
         rows = 10, colors = c(main_color, "#dee2cc"),
         legend_pos="bottom",
         use_glyph = 'child',
         glyph_size = 3,
         title = title_label) +
    labs(subtitle = subtitle_label,
         caption = caption_label) +
    theme(plot.subtitle= element_text(family = "Lato Light Italic", size=6, hjust = 0.5,color="Black", margin = unit(c(0,0,-3,0), "cm")),
          plot.title= element_text(family="Lato Black",size=10,hjust = 0.5, color="Black", margin = unit(c(0,0,-1,0), "cm")),
          plot.caption = element_text(family="Lato Regular",size=6,hjust = 0.5,color="Black"),
          plot.title.position = "plot",
          legend.box.spacing = unit(0, "pt"),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-50,0,0,0)) 
          
  }
