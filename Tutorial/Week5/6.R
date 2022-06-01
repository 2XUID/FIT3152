library("igraph")

setwd("C:/Users/aud/My Drive/Documents/Collection/2-SEM_1/FIT3152/Tutorial/Week5")
nodes <- read.csv("Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
links <- read.csv("Dataset1-Media-Example-EDGES.csv",header=T, as.is=T)

net <- graph_from_data_frame(d=links, vertices=nodes, directed=T)

plot(net)
# Run the codes below and see properties of Edges and Vertex 
# assigned from nodes and links dataframes 
E(net) # The edges of the "net" object
V(net) # The vertices of the "net" object
E(net)$type # Edge attribute "type"
V(net)$media # Vertex attribute "media"
# Previous plot looked so complicated so remove loops -- arraw from and to same vertex --
  net <- simplify(net, remove.multiple = F, remove.loops = T)
## Design and create a network visualisation
#Generate colors based on media type:
colrs <- c("gray50", "tomato", "gold")
V(net)$color <- colrs[V(net)$media.type]
# Compute node degrees (#links) and use that to set node size:
deg <- degree(net, mode="all")
V(net)$size <- deg*3
# We could also use the audience size value:
V(net)$size <- V(net)$audience.size*0.6
# The labels are currently node IDs.
# Setting them to NA will render no labels:
V(net)$label <- NA
# Set edge width based on weight:
E(net)$width <- E(net)$weight/6
#change arrow size and edge color:
E(net)$arrow.size <- .2
E(net)$edge.color <- "gray80"
# We can even set the network layout:
graph_attr(net, "layout") <- layout_with_lgl
plot(net)

# Add a legend
legend(x=-1.5, y=-1.1, c("Newspaper","Television", "Online News"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)
