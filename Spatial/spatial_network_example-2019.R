## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
library("igraph")

## ----Import Centroids----------------------------------------------------
locs <- read.csv("wetland_centroids.csv", header=T)
dat <- locs
dat$id <- row.names(dat)


## ----Adjacency Matrix----------------------------------------------------
d <- dist(cbind(dat$X_COORD,dat$Y_COORD))
d.m <- as.matrix(d)

d.m.m <- d.m #create a working copy of the distance matrix for further manipulation

## ----Critical Distance Limit---------------------------------------------
c.dist <- 500 #500 m = 0.5 km

d.m.m[d.m.m>c.dist]=0 #sets adjacency to '0' or not connected for distances greater than the critical distance


## ----Create Network------------------------------------------------------
net1 <- graph.adjacency(d.m.m,mode=c("undirected"),weighted=T)
net1a <- graph_from_adjacency_matrix(d.m.m,mode=c("undirected"),weighted=T)


## ----Visualization-------------------------------------------------------
wet.layout <- as.matrix(cbind(dat$X_COORD,dat$Y_COORD))

plot(net1, layout=wet.layout)

## ----Visualization2------------------------------------------------------
plot(net1,layout=wet.layout, vertex.size=3, vertex.color="red", vertex.label=NA, frame=TRUE, 
     xlab="Easting (m)", ylab="Northing (m)", main=paste("Critical distance =",c.dist/1000, "km"))

## ----Network Metrics-----------------------------------------------------
lth <- length(V(net1)) #number of nodes (wetlands)
lth

den <- graph.density(net1) #number of links / number of possible links
den

dia <- diameter(net1) #maximum distance (m) of the shortest paths between pairs of nodes
dia

apl <- average.path.length(net1) #average number of edges between pairs of nodes
apl

clc <- transitivity(net1, type="global") #graph level property: For ‘global’ a single number, or NaN if there are no connected triples in the graph
clc


## ----Node Metrics--------------------------------------------------------
dgr <- degree(net1)
dgr 

dgr.dist <- degree.distribution(net1, cumulative = TRUE, mode="all") #distribution of degree across all nodes in the network
plot( x=0:max(dgr), y=1-dgr.dist, pch=19, cex=1.2, col="orange", 
      xlab="Degree", ylab="Cumulative Frequency")

lcc <- transitivity(net1, type="local") #node-level: For ‘local’ a vector of transitivity scores, one for each node

btw <- betweenness(net1)

## ----Path for Diameter---------------------------------------------------
g.dia <- get.diameter(net1)
g.dia

## ----Diameter Plot-------------------------------------------------------
E(net1)$color <- "gray"
E(net1)$width <- 1
E(net1, path=g.dia)$color <- "red"
E(net1, path=g.dia)$width <- 2
V(net1)$label.color <- "blue"
V(net1)$color <- "SkyBlue2"
V(net1)[g.dia]$label.color <- "black"
V(net1)[g.dia]$color <- "red"

plot(net1, layout=wet.layout, vertex.size=3, vertex.label=NA, frame=TRUE, 
     xlab="Easting (m)", ylab="Northing (m)", main=paste("Critical distance =", c.dist/1000, "km"))

legend("topleft", legend=c("Wetlands (nodes)", "Edges less than 0.5 km", paste("Graph Diameter =", round(dia,1), "m")) ,
           pch=c(21, NA, NA), pt.bg=c("SkyBlue2",NA, NA), pt.cex=1.5, lty=c(NA, 1, 1), lwd=c(NA, 1, 2), seg.len=1, col=c("black", "gray", "red"), cex=0.8, bty="n", ncol=1)


## ----Betweenness Plot----------------------------------------------------
sorted.btw.rank <- order(betweenness(net1), decreasing = TRUE)
n <- 20 #higlight top 20 node

b <- sorted.btw.rank[1:n]

E(net1)$color <- "gray"
E(net1)$width <- 1
V(net1)$label.color <- "blue"
V(net1)$color <- "light blue"
V(net1)[b]$label.color <- "black"
V(net1)[b]$color <- "red"
plot(net1, layout=wet.layout, vertex.size=3, 
            #vertex.color=msum$nodecolor1, 
            vertex.label=NA, frame=TRUE, xlab="Easting (m)", ylab="Northing (m)", 
            main=paste("Critical distance =", c.dist/1000, "km"))

legend("topleft", legend=c("Wetlands (nodes)", "Edges less than 0.5 km", "Top 20 stepping stones"),
           pch=c(21, NA, 21), pt.bg=c("SkyBlue2",NA, "red"), pt.cex=1.5, lty=c(NA, 1, NA), lwd=c(NA, 1, NA), seg.len=1, col=c("black", "gray", "black"), cex=0.8, bty="n", ncol=1)


## ----Connected Components------------------------------------------------
comps <- clusters(net1)$membership
cls <- no.clusters(net1)
cls

## ----Cluster Summary-----------------------------------------------------
graph.clusters <- clusters(net1)
summary(graph.clusters$csize) #min, max, median, mean, IQR 
graph.clusters$csize #number of nodes in each cluster

meanclssize <- mean(graph.clusters$csize) #mean number of nodes in each cluster
meanclssize

## ----Cluster Plot--------------------------------------------------------
samcol <- rainbow(max(comps))
V(net1)$color <- samcol[comps]

plot(net1,layout=wet.layout, vertex.size=5, vertex.label=NA, frame=TRUE, 
     xlab="Easting (m)", ylab="Northing (m)", main=paste("Critical distance =", c.dist/1000, "km"))

plot(net1, layout=wet.layout, vertex.size=5, vertex.label.cex=0.6, vertex.label.dist=2)


## ----Create Subgraph-----------------------------------------------------
V(net1)$membership <- comps
net1.sub <- induced_subgraph(net1, vids=which(V(net1)$membership==1), impl ="copy_and_delete")

dat.sub <- subset(dat, id %in% c(V(net1.sub)$name))

wet.layout.sub <- as.matrix(cbind(dat.sub$X_COORD,dat.sub$Y_COORD))

plot(net1.sub, layout=wet.layout.sub, vertex.label.cex=1, vertex.label.dist=2)

## ----Other Clustering and Community Detection methods--------------------
clqs <- cliques(net1.sub) #Identification of cliques (complete subgraphs of an undirected graph)
sapply(cliques(net1.sub), length)
cl <- largest_cliques(net1.sub)

E(net1.sub)$color <- "gray"
E(net1.sub)$width <- 1
V(net1.sub)$label.color <- "blue"
V(net1.sub)$color <- "SkyBlue2"
V(net1.sub)[cl[[1]]]$label.color <- "black"
V(net1.sub)[cl[[1]]]$color <- "gold"

plot(net1.sub, layout=wet.layout.sub, vertex.label.cex=0.6, main="Largest Clique (complete subgraph)")


ceb <- cluster_edge_betweenness(net1.sub) #Community detection based on edge betweenness (Newman-Girvan)
dendPlot(ceb, mode="hclust")
plot(ceb, net1.sub, layout=wet.layout.sub, main="Communities (based on edge betweenness)")

clp <- cluster_label_prop(net1.sub) #Community detection based on propagating labels
plot(clp, net1.sub, layout=wet.layout.sub, main="Communities (based on propagating labels)")

cfg <- cluster_fast_greedy(net1.sub) #Community detection based on greedy optimization of modularity
plot(cfg, net1.sub, layout=wet.layout.sub, main="Communities (based on greedy optimization of modularity)")

V(net1.sub)$community <- cfg$membership
colrs <- rainbow(10) 
plot(net1.sub, layout=wet.layout.sub, vertex.color=colrs[V(net1.sub)$community], main="Communities (based on greedy optimization of modularity) - Plot 2")


## ----Import Edge List----------------------------------------------------
n.dist <- read.csv("wetland_distances.csv", header=T)

n <- length(unique(n.dist$BAS_NUM)) #check the number of nodes in the edge list
n

locs <- read.csv("wetland_centroids.csv", header=T)

dat2 <- locs
dat2$id <- as.integer(as.character(dat2$BAS_NUM))
dat2$label <- as.character(dat2$BAS_NUM)

## ----Modify Edge List----------------------------------------------------
c.dist <- 500

wet.edge <- subset(n.dist, distance <= c.dist)
  
wet.edge$from <- wet.edge$BAS_NUM
wet.edge$to <- wet.edge$comp_ID
wet.edge$weight <- wet.edge$distance

## ----Create Network with Edge and Node Lists-----------------------------
net.wet <- graph_from_data_frame(d = wet.edge, vertices = dat2, directed = FALSE)

plot(net.wet, layout=wet.layout,vertex.size=3, frame=TRUE, edge.curved=0, vertex.label.cex=0.6, vertex.label.dist=NA,
     xlab="Easting (m)", ylab="Northing (m)", main=paste("Critical distance =", c.dist/1000, "km"))

## ----Create Directed Graph with Edge and Node Lists----------------------
dat.ssp <- subset(dat2, BAS_REGIME != "TEMPORARY") 

wet.edge.dir <- subset(wet.edge, from %in% c(dat.ssp$BAS_NUM))
  
net.wet2 <- graph_from_data_frame(d = wet.edge.dir, vertices = dat2, directed = TRUE)

V(net.wet2)$regime <-dat2$BAS_REGIME
colrs <- c("SkyBlue2", "blue", "gold") 

plot(net.wet2, edge.arrow.size=.3, layout=wet.layout,vertex.size=4, frame=TRUE, edge.curved=0,vertex.color=colrs[V(net.wet2)$regime], vertex.label.cex=0.6, vertex.label.dist=NA,
     xlab="Easting (m)", ylab="Northing (m)", main=paste("Critical distance =", c.dist/1000, "km"))

legend("topleft", legend=c("Wetlands (nodes)", "Temporary", "Seasonal", "Semipermanent", "Edges less than 0.5 km"), pch=c(NA, 21, 21, 21, NA), pt.bg=c(NA, "gold", "SkyBlue2","blue", NA),  pt.cex=1.5, lty=c(NA, NA, NA, NA, 1), lwd=c(NA, NA, NA, NA, 1), seg.len=1, col=c(NA, "black", "black","black","black","gray"), cex=0.8, bty="n", ncol=1)



