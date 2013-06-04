nodes <- read.csv("C:\\Documents and Settings\\ewnym5s\\Desktop\\nodes.csv.")
edges <- read.csv("C:\\Documents and Settings\\ewnym5s\\Desktop\\edges1.csv.")

data("flo")

data("emon")

edges1 <- edges[,1:3]

library(network)
net <- network(edges1,matrix.type="edgelist",directed=F)
plot(net)

library(igraph)
size <- c(36.4,4.2,16.6,3.2,1.4,21.8,3.6,5.6)
d <- data.frame(p1=edges$sname,p2=edges$tname,width=edges$count*.02)
g <- graph.data.frame(d, directed=F)
l <- layout.fruchterman.reingold(g)
E(g)$color <- "gray"
V(g)$color <- "red"
V(g)$size <- size
E(g)$curved <- 0.2
plot(g,layout=l)



#move labels to outside, add Penetration somwehow

E(g)$curved <- 0.2
plot(g,layout=l)

print(g, e=TRUE, v=TRUE)
tkplot(g, vertex.label=V(g)$name)