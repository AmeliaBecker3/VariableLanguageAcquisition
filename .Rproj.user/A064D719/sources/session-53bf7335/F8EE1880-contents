my_theme <- function(legend.pos = "inside", legend.use.title = FALSE,
                     legend.font.size = 28,    
                     x.font.size = 28,         
                     y.font.size = 28,         
                     facet.title.size = 28,   
                     plot.title.size = 28,     
                     plot.title.face = "bold", 
                     remove.y.gridlines = TRUE,
                     remove.x.gridlines = TRUE) {
  
  # Specifying parameters, using theme_bw() as starting point
  plot <- ggplot2::theme_bw() + 
    ggplot2::theme(
      axis.text = element_text(size = 28), # updated to 28
      plot.title = ggplot2::element_text(face = plot.title.face, hjust = 0, 
                                         size = plot.title.size),
      axis.title.x = ggplot2::element_text(size = x.font.size),
      axis.title.y = ggplot2::element_text(size = y.font.size,
                                           angle = 90),
      legend.text = ggplot2::element_text(size = legend.font.size),
      legend.key.size = ggplot2::unit(0.0, "lines"), # switch off the rectangle around symbols
      legend.key.height = unit(1, "cm"), # vertical spacing between lines in legend
      legend.key = ggplot2::element_blank(),
      legend.key.width = grid::unit(3, "lines"),
      strip.text.x = ggplot2::element_text(size = facet.title.size), # facet labs
      strip.text.y = ggplot2::element_text(size = facet.title.size),
      strip.background = ggplot2::element_rect(colour = "white", fill = "white"),
      panel.background = ggplot2::element_rect(fill = "white"),
      plot.title.position = "panel",
      panel.border = element_blank(),
      axis.line.x = element_line(color = "black", linewidth = 0.25),
      axis.line.y = element_line(color = "black", linewidth = 0.25),
      axis.line.x.top = element_blank(),
      axis.line.y.right = element_blank()
    )
  
  # Legend positioning (same as before)
  if (legend.pos == "topleft") {
    plot <- plot + ggplot2::theme(legend.position = c(.05, .95),
                                  legend.justification = c(.05, .95))
  } else if (legend.pos == "topright") {
    plot <- plot + ggplot2::theme(legend.position = c(.95, .95),
                                  legend.justification = c(.95, .95))
  } else if (legend.pos == "top") {
    plot <- plot + ggplot2::theme(legend.position = c(.50, .95),
                                  legend.justification = c(.50, .95))
  } else if (legend.pos == "bottomleft") {
    plot <- plot + ggplot2::theme(legend.position = c(.05, .05),
                                  legend.justification = c(.05, .05))
  } else if (legend.pos == "bottomright") {
    plot <- plot + ggplot2::theme(legend.position = c(.95, .05),
                                  legend.justification = c(.95, .05))
  } else if (legend.pos == "bottommiddle") {
    plot <- plot + ggplot2::theme(legend.position = c(.50, .05),
                                  legend.justification = c(.50, .05))
  } else if (legend.pos == "inside") {
    plot <- plot + ggplot2::theme(legend.position = c(.75, .25),
                                  legend.justification = c(.50, .05),
                                  legend.text = element_text(hjust = 0.5)
    )
  } else if (legend.pos == "none") {
    plot <- plot + ggplot2::theme(legend.position = "none")
  } else {
    plot <- plot + ggplot2::theme(legend.position = legend.pos)
  }
    
  # Legend title options currently commented out in your code.
  
  # Add or remove gridlines using just drop_gridlines()
  plot <- plot + drop_gridlines(x = remove.x.gridlines, y = remove.y.gridlines)
  

  
  return(plot)
}
  
