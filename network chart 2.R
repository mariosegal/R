nodes <- read.csv("C:\\Documents and Settings\\ewnym5s\\My Documents\\sas\\nodes_new.csv.")
edges <- read.csv("C:\\Documents and Settings\\ewnym5s\\My Documents\\sas\\edges_new.csv.")

library(igraph)
adjust <- 0.05
d <- data.frame(p1=edges$sname,p2=edges$tname,width=edges$width*adjust)
g <- graph.data.frame(d, directed=T)
set.seed(1234)
l <- layout.fruchterman.reingold(g,niter=3600)
E(g)$color <- "gray"
V(g)$color <- c("red","red","red","red","red","yellow","yellow","yellow","yellow","yellow","green")
V(g)$size <- nodes$COL1
E(g)$curved <- 0.2
E(g)$arrow.width <- 0.1
E(g)$arrow.size <- 0.1
V(g)$label.dist <- 1
plot(g,layout=l)

l1 <- layout.fruchterman.reingold.grid(g)
plot(g,layout=l1)

#tkplot(g, layout=layout.kamada.kawai, vertex.label.font=2)

#try network to compare;
library(network)
rm(net)
net <- network(edges[,1:2],directed=T)
set.vertex.attribute(net,"vertex.name",as.character(nodes$node))
plot(net,displaylabels=T,mode="circle",edge.col="gray",vertex.col="red")
set.vertex.attribute(net,"vertex.value",as.character(nodes$node))

library(arcplot)
arcplot(get.edgelist(g), las = 1,col.arcs="light blue",lwd.arcs=d[,3]*8,show.nodes=T,
        col.nodes="red",cex.nodes=2,horizontal=T,col.labels="black",cex.labels=1.5,lend=2)