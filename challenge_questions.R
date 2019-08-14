### Code for challenges to accompany network analysis workshop

library(igraph)

#### CHALLENGE 1: Are these graphs the same? ####
## Using any of the functions/techniques we have used thus far, 
## determine if `bridgeGraphElDir` and `bridgeGraphEl` are equivalent.

r1<-c(0,1,1,1)
r2<-c(1,0,1,0)
r3<-c(0,0,0,1)
r4<-c(1,0,0,0)
bridgeAdjmat<-rbind(r1,r2,r3,r4)

bridgeEdgelist<-rbind(c(1,2),c(1,2),c(1,3),c(1,4),c(1,4),c(2,3),c(3,4))

bridgeGraphElDir<-graph_from_edgelist(bridgeEdgelist,directed=T)
bridgeGraphAmDir<-graph_from_adjacency_matrix(bridgeAdjmat,mode="directed")

## Look at vertices and edges
V(bridgeGraphElDir)
V(bridgeGraphAmDir)
V(bridgeGraphElDir)==V(bridgeGraphAmDir)

E(bridgeGraphElDir)
E(bridgeGraphAmDir)
E(bridgeGraphElDir)==E(bridgeGraphAmDir)

## Plot graphs and intersection of graph
par(mfcol=c(1,3))
plot(bridgeGraphElDir %s% bridgeGraphAmDir)
plot(bridgeGraphAmDir)
plot(bridgeGraphElDir)

dev.off()
#### CHALLENGE 2: Create a custom vertex attribute ####
r1<-c(0,1,1,1)
r2<-c(1,0,1,0)
r3<-c(0,0,0,1)
r4<-c(1,0,0,0)
bridgeAdjmat<-rbind(r1,r2,r3,r4)

bridgeAdjmat

bridgeGraphAm<-graph_from_adjacency_matrix(bridgeAdjmat,mode="plus")


gr<-bridgeGraphAm
V(gr)$name<-c("A","B","D","C")
E(gr)$name<-c("a","e","c","b","f","g","d")


## Create a new vertex attribute called `'altcolors'` and use it to give these alternate node color assignments. Plot the graph using the new color attribute by given the plot function the argument `vertex.color=V(gr)$altcolors`
# D is white, B is blue, C is red, A is orange

V(gr)$altcolor<-"black"
V(gr)$altcolor[which(V(gr)$name=="D")]<-"white"
V(gr)$altcolor[which(V(gr)$name=="B")]<-"blue"
V(gr)$altcolor[which(V(gr)$name=="C")]<-"red"
V(gr)$altcolor[which(V(gr)$name=="A")]<-"orange"
plot(gr, vertex.color=V(gr)$altcolor)

#### Challenge 3: Creative plotting ####
par(bg="red")
E(gr)$altcolor<-'white'
E(gr)$altcolor[which(E(gr)$name %in% c('a','d','f','g'))]<-'red'
E(gr)$altwidth<-10
E(gr)$altwidth[which(E(gr)$name %in% c('a','d','f','g'))]<-NA


plot(gr,edge.color=E(gr)$altcolor,layout=layout_as_tree, vertex.label.dist=7,vertex.label.degree=pi/2,vertex.shape="none",edge.width=E(gr)$altwidth,
     vertex.label=c(NA,NA,"ATARI",NA),vertex.label.color="white",vertex.label.cex=7,vertex.label.family="sans")

#### CHALLENGE 4: Highlight and plot the/a longest simple path ####
par(bg="white")
spsA<-all_simple_paths(gr,V(gr)[which(E(gr)$name=="A")])

## Finds first instance of longest path
which.max(lengths(spsA))

## Reset default visualizations 
V(gr)$spcolor<-"black"
V(gr)$spsize<-5
E(gr)$spcolor<-"black"

## Color and size to highlight paths
V(gr)$spcolor[which(V(gr)%in%spsA[[3]])]<-"blue"
V(gr)$spsize[which(V(gr)%in%spsA[[3]])]<-10
E(gr,path=spsA[[3]],dir=T)$spcolor<-"blue"

plot(gr,vertex.color=V(gr)$spcolor,edge.color=E(gr)$spcolor,vertex.size=V(gr)$spsize)

#### Final Challenge: Eccentricity
# The eccentricity is the shortest simple path between a node and all other nodes.
# The simple path lengths are the lengths of all the simple paths, which includes the shortest

# The diameter is the longest shortest path between any pair of nodes in the network
# The radius is the shortest shortest path between any pair of nodes in the network

