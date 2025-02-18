theme_set(
  theme_linedraw() +
    theme(text=element_text(size=8, color='black', family="Open Sans"),
          plot.subtitle = element_text(size=9, color='black', family="Open Sans"),
          axis.text=element_text(size=8, color='black', family="Open Sans"),
          legend.text=element_text(size=8, color='black', family="Open Sans"),
          legend.position="bottom",panel.grid=element_blank(),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black"))
)
palette4 <- c("#4F3F84","#82AC26","#FFA22A","#FF662A")