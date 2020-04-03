library(patchwork)
tl =labs(title=NULL, subtitle=NULL, cappion=NULL)

    (wall.flow.plots[[1]]+tl)/ 
    (wall.flow.plots[[2]]+tl )/
    (wall.flow.plots[[3]]+tl ) +
  plot_annotation(tag_levels = 'A', tag_suffix =")")
 

ggsave("figs/qld.mosaic.png", width=width, height=2.5*height)
