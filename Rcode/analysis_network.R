# network analysis

library('igraph')
library('statnet')

# create complete nodeslist

g <- read_graph("../data/clean/networks/dominion_voting_system.gml", format = "gml")

net <- intergraph::asNetwork(g)

m1 = ergm(net ~ edges)

summary(m1)