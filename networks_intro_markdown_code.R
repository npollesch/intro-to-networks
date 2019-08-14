
## ------------------------------------------------------------------------
#install.packages("igraph",repos="http://cran.us.r-project.org")
library("igraph")


## ----EL Build------------------------------------------------------------
bridgeEdgelist<-rbind(c(1,2),c(1,2),c(1,3),c(1,4),c(1,4),c(2,3),c(3,4))
bridgeEdgelist

bridgeGraphEl<-graph_from_edgelist(bridgeEdgelist,directed=FALSE)
plot(bridgeGraphEl)


## ----ADJ Build-----------------------------------------------------------
r1<-c(0,1,1,1)
r2<-c(1,0,1,0)
r3<-c(0,0,0,1)
r4<-c(1,0,0,0)
bridgeAdjmat<-rbind(r1,r2,r3,r4)

bridgeAdjmat

bridgeGraphAm<-graph_from_adjacency_matrix(bridgeAdjmat,mode="plus")
plot(bridgeGraphAm)



## ----ADJalt Build--------------------------------------------------------
r1<-c(0,2,1,2)
r2<-c(0,0,1,0)
r3<-c(0,0,0,1)
r4<-c(0,0,0,0)
bridgeAdjmatAlt<-rbind(r1,r2,r3,r4)

bridgeAdjmatAlt

bridgeGraphAmAlt<-graph_from_adjacency_matrix(bridgeAdjmatAlt,mode="undirected")
plot(bridgeGraphAmAlt)



## ----EQUIV---------------------------------------------------------------
## Vertices of graph from adjacency matrix
V(bridgeGraphAm)
## Edge of graph from adjacency matrix
E(bridgeGraphAm)
## Vertices of graph from edge list
V(bridgeGraphEl)
## Edge of graph from edge list
E(bridgeGraphEl)


## ----EQUIV 2-------------------------------------------------------------
## Test vertex list equivalence 
V(bridgeGraphAm)==V(bridgeGraphEl)
## Test edge list equivalence 
E(bridgeGraphEl)==E(bridgeGraphAm)


## ----EQUIV Intersect-----------------------------------------------------
## Intersection
bridgeGraphAm %s% bridgeGraphEl

## Created intersected graph and plot
intBridge<-(bridgeGraphAm %s% bridgeGraphEl)
plot(intBridge)


## ----Directed Graphs-----------------------------------------------------
bridgeGraphElDir<-graph_from_edgelist(bridgeEdgelist,directed=T)
bridgeGraphAmDir<-graph_from_adjacency_matrix(bridgeAdjmat,mode="directed")



## ----check v attributes--------------------------------------------------
vertex.attributes(bridgeGraphAm)


## ----BRGraph-------------------------------------------------------------
## For ease, let's rename the graph from 'bridgeGraphAm' to 'gr'
gr<-bridgeGraphAm
## Call the '$name' attribute
V(gr)$name
## Assign the '$name' attribute
V(gr)$name<-c("A","B","D","C")

## Call the '$name' attribute to verify assignment
V(gr)$name

## Plot to verify assignment
plot(gr)


## ----color nodes---------------------------------------------------------
V(gr)$color<-"salmon"
plot(gr)


## ----size nodes----------------------------------------------------------
V(gr)$size<-1:4
plot(gr)


## ----size nodes 2--------------------------------------------------------
V(gr)$size<-c(15,20,25,30)
plot(gr)


## ----vertex attributes check---------------------------------------------
vertex.attributes(gr)


## ----edge attributes check-----------------------------------------------
edge.attributes(gr)


## ------------------------------------------------------------------------
E(gr)


## ------------------------------------------------------------------------
E(gr)$name<-c("a","e","c","b","f","g","d")


## ------------------------------------------------------------------------
## Note: Although node labels are plotted by default, edge labels
## need to be specified for plotting
plot(gr,edge.label=E(gr)$name)


## ------------------------------------------------------------------------
E(gr)$width<-c(.5,1,2,3,4,5,6)
plot(gr,edge.label=E(gr)$name)


## ----Call by name--------------------------------------------------------
# This will initialize the altcolor attribute
V(gr)$altcolor<-"black"
V(gr)$altcolor[which(V(gr)$name=="D")]<-"white"
plot(gr, vertex.color=V(gr)$altcolor)


## ----Layouts-------------------------------------------------------------
par(mfrow=c(2,3))
plot(gr,layout=layout_in_circle)
plot(gr,layout=layout_on_grid)
plot(gr,layout=layout_randomly)
plot(gr,layout=layout_as_tree)
plot(gr,layout=layout_as_star)
plot(gr,layout=layout_with_gem)

## Randomness in default layout
par(mfrow=c(1,3))
for(i in 1:3){
plot(gr)}


## ----random network layout viz-------------------------------------------
g <- erdos.renyi.game(50, 50, type = "gnm")

par(mfrow=c(2,3))
plot(g,layout=layout_in_circle)
plot(g,layout=layout_on_grid)
plot(g,layout=layout_randomly)
plot(g,layout=layout_as_tree)
plot(g,layout=layout_as_star)
plot(g,layout=layout_with_gem)

## Randomness in default layout
par(mfrow=c(1,3))
for(i in 1:3){plot(g)}



## ----degree--------------------------------------------------------------
V(gr)$degree<-degree(gr)
V(gr)$degree


## ----degree viz----------------------------------------------------------
# Resetting node size and edge width to default
V(gr)$size<-5
E(gr)$width<-1
plot(gr,vertex.size=V(gr)$size*V(gr)$degree)


## ----degree viz col------------------------------------------------------

degree.gradient <- colorRampPalette(c("white","blue","red"))

V(gr)$degreecolor<-degree.gradient(max(V(gr)$degree)+1)[V(gr)$degree+1]
plot(gr,vertex.size=V(gr)$size*V(gr)$degree,vertex.color=V(gr)$degreecolor)


## ----degree viz random---------------------------------------------------
g <- erdos.renyi.game(100,150, type = "gnm")
V(g)$degreecolor<-NA
V(g)$size<-5

V(g)$degree<-degree(g)
V(g)$degreecolor<-degree.gradient(max(V(g)$degree)+1)[V(g)$degree+1]
set.seed(1)
layoutfr<-layout_with_fr(g)
plot(g,vertex.size=20*(V(g)$degree/max(V(g)$degree)),vertex.color=V(g)$degreecolor,vertex.label=V(g)$degree,layout=layoutfr)
set.seed(1)
par(bg="black")
plot(g,vertex.size=20*(V(g)$degree/max(V(g)$degree)),vertex.color=V(g)$degreecolor,vertex.label=NA,layout=layoutfr)



## ----cc------------------------------------------------------------------
components(g)


## ----cc viz--------------------------------------------------------------
V(g)$component<-components(g)$membership

V(g)$componentcolor<-rainbow(components(g)$no)[V(g)$component]

plot(g, vertex.label=NA,vertex.color=V(g)$componentcolor,layout=layoutfr)


## ----cwt-----------------------------------------------------------------
cwt<-cluster_walktrap(gr)
membership(cwt)

plot(cwt,gr)

cwtg<-cluster_walktrap(g)
membership(cwtg)

plot(cwtg,g,layout=layoutfr)


## ----asp-----------------------------------------------------------------

spsA<-all_simple_paths(gr,V(gr)[which(E(gr)$name=="A")])

## Look at simple path object
spsA[[1]]

## Which nodes are in the specified path?
which(V(gr)%in%spsA[[1]])

## Which edges are in the path?
E(gr,path=spsA[[1]],dir=T)

## Plot the graph with path highlighted
## Set all non-path colors and sizes to a default
V(gr)$spcolor<-"black"
V(gr)$spsize<-5
E(gr)$spcolor<-"black"

## Color and size to highlight paths
V(gr)$spcolor[which(V(gr)%in%spsA[[4]])]<-"blue"
V(gr)$spsize[which(V(gr)%in%spsA[[4]])]<-10
E(gr,path=spsA[[4]],dir=T)$spcolor<-"blue"

plot(gr,vertex.color=V(gr)$spcolor,edge.color=E(gr)$spcolor,vertex.size=V(gr)$spsize)



## ----challenge 4---------------------------------------------------------
# Hint: lengths returns lengths of objects in lists, 'spsA' is a list
lengths(spsA)
## Finds first instance of longest path
which.max(lengths(spsA))



## ----eccentricity--------------------------------------------------------
eccentricity(gr)
diameter(gr)
radius(gr)

eccentricity(g)
diameter(g)
radius(g)

