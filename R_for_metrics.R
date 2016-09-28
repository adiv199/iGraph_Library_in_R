dir_path <-"G:/Fall2016/SocialMedia/Assign2_adv"
setwd(dir_path)

# clear everything out of memory
rm(list=ls()) 

# This is a 10% random sample for class exercises
infile_sub<-"SAPFull_SubGraph_EdgeList.csv"

## Load package
library(igraph)
el=read.csv(infile_sub, header = TRUE, sep = ",")
class(el)
# Create the directed graph object
g_SAPSub=graph.data.frame(el, directed = TRUE, vertices= NULL)

# Edges
ecount(g_SAPSub)
## Vertices
vcount(g_SAPSub)


## Check whether Self_loops exist, as do multiple edges
is.simple(g_SAPSub)

# Create edge weights
E(g_SAPSub)$weight <-1
E(g_SAPSub)$weight 
g_SAPSub_simpl<-simplify(g_SAPSub, edge.attr.comb="sum")
is.simple(g_SAPSub_simpl)

#how is the graph connected
is.connected(g_SAPSub_simpl)
is.connected(g_SAPSub_simpl, mode="strong")
is.connected(g_SAPSub_simpl, mode="weak")
# Edges
ecount(g_SAPSub_simpl)
## Vertices
vcount(g_SAPSub_simpl)

##reciprocity
reciprocity(g_SAPSub_simpl)
transitivity(g_SAPSub_simpl,"global")

##Cliques of 3:
layout_SAP1 = layout.fruchterman.reingold(clique_subgraph_3)
layout_SAP1 <- norm_coords(layout_SAP1, ymin=-1, ymax=1, xmin=-1, xmax=1)
clique_subgraph_3 <- induced.subgraph(g_SAPSub_simpl,vids=unlist(cliques(g_SAPSub_simpl,min=3,max=3)))
plot.igraph(clique_subgraph_3, layout=layout_SAP1*1.4,edge.arrow.size=0.15,vertex.size=5,rescale=FALSE,vertex.label=NA)

##degrees - highest degree vertex:
V(g_SAPSub_simpl)$name[degree(g_SAPSub_simpl,mode="all")==max(degree(g_SAPSub_simpl,mode="all"))]
##plotting the neighbourhood/ego network of the node with highest degree:
sub_net_SAP<-induced.subgraph(g_SAPSub_simpl,vids=unlist(neighborhood(graph=g_SAPSub_simpl,order=1,nodes=c('592540'))))
plot.igraph(sub_net_SAP,layout = layout_SAP2*1.5,edge.arrow.size=0.3,vertex.size=10,vertex.label=NA,rescale=FALSE)

sub_net_SAP_2<-induced.subgraph(g_SAPSub_simpl,vids=unlist(neighborhood(graph=g_SAPSub_simpl,order=2,nodes=c('592540'))))
plot.igraph(sub_net_SAP_2,edge.arrow.size=0.3,vertex.size=10,vertex.label=NA)

## ranks of nodes for betweenness centrality, closeness centrality and degree

ranks_b <- 3416 - rank(sort(betweenness(g_SAPSub_simpl)))
ranks_b["1195854"]

ranks_c <- 3416 - rank(sort(closeness(g_SAPSub_simpl)))
ranks_c["1195854"]

ranks_d <- 3416 - rank(sort(degree(g_SAPSub_simpl)))
ranks_d["1195854"]

sort(betweenness(g_SAPSub_simpl),decreasing=TRUE)
degree(g_SAPSub_simpl,'4183082')

##plot the neighbourhood of order 2 as it has low degree but high betweenness
sub_net_4183082<-induced.subgraph(g_SAPSub_simpl,vids=unlist(neighborhood(graph=g_SAPSub_simpl,order=2,nodes=c('4183082'))))
layout_418 = layout.fruchterman.reingold(sub_net_4183082)
layout_418 <- norm_coords(layout_418, ymin=-1, ymax=1, xmin=-1, xmax=1)
plot.igraph(sub_net_4183082,layout=layout_418*1.5,edge.arrow.size=0.3,vertex.size=10,rescale=FALSE,
            vertex.label=ifelse(V(sub_net_4183082)$name=='4183082'| degree(sub_net_4183082,V(sub_net_4183082)$name)>50,V(sub_net_4183082)$name,NA))
sort(closeness(g_SAPSub_simpl),decreasing=TRUE)
closeness(g_SAPSub_simpl,"1195854")
vertex.label.color='black'

##betweenness centrality - node with highest betweenness centrality
V(g_SAPSub_simpl)$name[betweenness(g_SAPSub_simpl) == max(betweenness(g_SAPSub_simpl))]
##plotting the ego network/neighbourhood of the node with the highest betweenness centrality
sub_net_betweenness<-induced.subgraph(g_SAPSub_simpl,vids=unlist(neighborhood(graph=g_SAPSub_simpl,order=2,nodes=c('1195854'))))
plot.igraph(sub_net_betweenness,edge.arrow.size=0.2,vertex.size=5,vertex.label.color='black',vertex.color='goldenrod',
            vertex.label=ifelse(degree(sub_net_betweenness,V(sub_net_betweenness)$name)>12,V(sub_net_betweenness)$name,NA))
            

##closeness centrality - node with highest closeness centrality
V(g_SAPSub_simpl)$name[closeness(g_SAPSub_simpl) == max(closeness(g_SAPSub_simpl))]
sub_net_closeness<-induced.subgraph(g_SAPSub_simpl,vids=unlist(neighborhood(graph=g_SAPSub_simpl,order=2,nodes=c('3988816'))))
plot.igraph(sub_net_closeness,layout = layout_SAP2*1.3,edge.arrow.size=0.3,vertex.size=20,rescale=FALSE)

##plot the nodes with highest out degrees:
degree_out_SAP = degree(g_SAPSub_simpl,mode="out") 
sortedOut = sort(degree_out_SAP,decreasing = TRUE)
sortedOut = subset(sortedOut,sortedOut>2)

highest_out <- induced.subgraph(g_SAPSub_simpl,
                                vids=unlist(names(sortedOut)))
plot(highest_out,
  edge.arrow.size=0.3,vertex.size=5,vertex.label=ifelse(degree(g_SAPSub_simpl,V(g_SAPSub_simpl)$name,mode="out")>50,V(highest_out)$name,NA))

##plot the nodes with highest in degrees:
degree_in_SAP = degree(g_SAPSub_simpl,mode="in")
sortedIn = sort(degree_in_SAP,decreasing = TRUE)
sortedIn = subset(sortedIn,sortedIn>2)

highest_in <- induced.subgraph(g_SAPSub_simpl,
                                vids=unlist(names(sortedIn)))
plot(highest_in,
     edge.arrow.size=0.3,vertex.size=5,vertex.label=ifelse(degree(g_SAPSub_simpl,V(g_SAPSub_simpl)$name,mode="in")>10,V(g_SAPSub_simpl)$name,NA))

##Plotting by edge weights:
sortedEdge = sort(E(g_SAPSub_simpl)$weight,decreasing=TRUE)
sortedEdge = subset(sortedEdge,sortedEdge>5)
for(i in sortedEdge) 
  if(E(g_SAPSub_simpl)$weight==i)
    edges_of_int <- c(edges_of_int,E(g_SAPSub_simpl))
head_of(g_SAPSub_simpl,edge_of_int)
tail_of(g_SAPSub_simpl,edge_of_int)

for(i in edges_of_int)
  vertices_tail_head <- head_of(g_SAPSub_simpl,i)

subg <- subgraph.edges(g_SAPSub_simpl, edges_of_int)

E(subg)$width = E(subg)$weight*0.2
E(subg)$color = 'black'
layout_subg = layout.fruchterman.reingold(subg)
layout_subg <- norm_coords(layout_subg, ymin=-1, ymax=1, xmin=-1, xmax=1)
plot(subg,layout = layout_subg*1.3,edge.arrow.size=0.3,vertex.size=10,rescale=FALSE)





