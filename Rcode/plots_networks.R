
library('igraph')
library('ggplot2')
library('ggraph')
library('patchwork')
library('colorspace')

source('theme_ggplot.R')

plot_net <- function(net_name) {
  g <- read_graph(paste0("../data/clean/networks/",net_name,".gml"), format = "gml")
  label = textGrob(label = gsub("_"," ",net_name), x = .98, y = 0.98, 
                   just = c("right", "top"),
                   gp=gpar(col = "white", size = 1))
  ggraph(g,"fr") + 
    geom_edge_link(
      alpha=.1,
      arrow=arrow(length = unit(1.5,'mm')),end_cap = circle(1, 'mm')) + 
    geom_node_point(aes(fill = factor(cluster), size=viewcount), shape=21) +
    annotation_custom(label, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    scale_color_discrete(name="Cluster") +
    scale_size_continuous(name="Views") +
    theme(legend.position="none", 
          panel.background = element_rect(fill = lighten(topic_df$color[topic_df$topic == net_name], amount=.5)))
}

plot_net("dominion_voting_system")

net_plots <- lapply(network_names, plot_net)

net_plots[[1]] + net_plots[[2]] + net_plots[[3]] + net_plots[[4]] + net_plots[[5]] +
  net_plots[[6]] + net_plots[[7]] + net_plots[[8]] + net_plots[[9]] + net_plots[[10]] +
  net_plots[[11]] + net_plots[[12]] + net_plots[[13]] + net_plots[[14]] + net_plots[[15]] +
  net_plots[[16]] + net_plots[[17]] + net_plots[[18]] + net_plots[[19]] + net_plots[[20]] +
  net_plots[[21]] + net_plots[[22]] + net_plots[[23]] + net_plots[[24]] + net_plots[[25]] +
  net_plots[[26]] + net_plots[[27]] + net_plots[[28]] + net_plots[[29]] + net_plots[[30]] +
  net_plots[[31]] + net_plots[[32]] + net_plots[[33]] + net_plots[[34]] + net_plots[[35]] +
  net_plots[[36]] + net_plots[[37]] + net_plots[[38]] + net_plots[[39]] + net_plots[[40]] +
  plot_layout(ncol = 5)
ggsave("../../plots/network_plots.png", width=7, height=10, dpi=300)
