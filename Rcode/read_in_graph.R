# EXAMPLE FILE ON HOW TO READ THE GRAPH FILES

library(here)
library(igraph)


here::i_am("README.md")

graph <- igraph::read_graph(
  here("data/interim/networks/roe_v_wade_full.gml"),
  format = "gml"
)
