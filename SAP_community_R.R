getwd()
# Save the data file to a location on your hard drive and specify the path here (Windows systems use forward slashes)
dir_path <-"G:/Fall2016/SocialMedia/Assign3_adv"
setwd(dir_path)

## Load primary SAP community data, contact data##
infile_edges<-"CollabNetEdgeListFilteredDec7_2012.csv"
infile_nodes<-"NodesNetList_corrected_Feb19_2016.csv"
##
library(igraph)

############### CREATE THE GRAPH ###########################
edge_frame=read.csv(infile_edges, header = TRUE, sep = ",")
node_frame=read.csv(infile_nodes, header = TRUE, sep = ",")
g_sap_comm=graph.data.frame(edge_frame, directed = FALSE, vertices= node_frame)
E(g_sap_comm)$weight <-1
g_SAP_simpl<-simplify(g_sap_comm, edge.attr.comb="sum")
E(g_SAP_simpl)$weight
###########################################################

############################ MANY NODES HAVE 0 DEGREE ############################################
vcount(g_SAP_simpl) ##98288 - total nodes
length(V(g_SAP_simpl)[degree(g_SAP_simpl)==0]) ##61237 - nodes having zero degree
##--------------- create a subgraph of nodes having degree greater than 0 ------------------------#
nozero_subgraph <- induced.subgraph(g_SAP_simpl, v=V(g_SAP_simpl)[degree(g_SAP_simpl)>0]) 
#################################################################################################


############################# BUILD COMMUNITIES AND CHECK THEIR TIME TO CREATE COMMUNITIES###
############################ ON THE ENTIRE GRAPH AS WELL AS THE NODES WHICH HAVE DEGREE GREATER THAN 0########
system.time(sap_comm_fast <- fastgreedy.community(g_SAP_simpl,weights=E(g_SAP_simpl)$weight))
system.time(sap_comm_walk <- walktrap.community(g_SAP_simpl, weights=E(g_SAP_simpl)$weight))
system.time(sap_comm_lab <- label.propagation.community(g_SAP_simpl, weights=E(g_SAP_simpl)$weight))
system.time(sap_comm_spin <- spinglass.community(g_SAP_simpl, weights=E(g_SAP_simpl)$weight)) ## doesnot work with unconnected graph
system.time(sap_comm_eigen <- leading.eigenvector.community(g_SAP_simpl, weights=E(g_SAP_simpl)$weight))
system.time(sap_cluster_fast <- cluster_fast_greedy(g_SAP_simpl,weights=E(g_SAP_simpl)$weight))


system.time(nz_comm_fast <- fastgreedy.community(nozero_subgraph,weights=E(nozero_subgraph)$weight))
system.time(nz_comm_walk <- walktrap.community(nozero_subgraph, weights=E(nozero_subgraph)$weight))
system.time(nz_comm_lab <- label.propagation.community(nozero_subgraph, weights=E(nozero_subgraph)$weight))
system.time(nz_comm_spin <- spinglass.community(nozero_subgraph, weights=E(nozero_subgraph)$weight)) ## doesnot work with unconnected graph
system.time(nz_comm_eigen <- leading.eigenvector.community(nozero_subgraph, weights=E(nozero_subgraph)$weight))
system.time(nz_cluster_fast <- cluster_fast_greedy(nozero_subgraph,weights=E(nozero_subgraph)$weight))
#

head(sort(sizes(sap_comm_fast),decreasing = TRUE),10)
head(sort(sizes(sap_comm_walk),decreasing = TRUE),10)
head(sort(sizes(sap_comm_lab),decreasing = TRUE),10)
head(sort(sizes(sap_comm_eigen),decreasing = TRUE),10)

################################# GET THE COMMUNITY ID HAVING THE LARGEST SIZE ####################
max_fast <- which.max(sizes(nz_comm_fast))
max_walk <- which.max(sizes(nz_comm_walk))
max_eigen <- which.max(sizes(nz_comm_eigen))
max_lab <- which.max(sizes(nz_comm_lab))

################################# GET THE LIST OF VERTICES IN THE LARGEST COMMUNITES IN EACH ALGORITHM ####################
max_fast_v <- V(nozero_subgraph)[which(membership(nz_comm_fast)==max_fast)]
max_walk_v <- V(nozero_subgraph)[which(membership(nz_comm_walk)==max_walk)]
max_lab_v <- V(nozero_subgraph)[which(membership(nz_comm_lab)==max_lab)]
max_eigen_v <- V(nozero_subgraph)[which(membership(nz_comm_eigen)==max_eigen)]

################################# fIND IF THERE ARE ANY VERTICES WHICH ARE RETURNED IN THE LARGEST
################################COMMUNITY OF EVERY ALGORITHM####################

intersect(max_fast_v,max_walk_v)
intersect(max_lab_v,max_walk_v)
intersect(max_fast_v,max_lab_v)
length(intersect(max_fast_v,max_eigen_v))

length(intersect(intersect(max_fast_v,max_walk_v),max_eigen_v))

############################### FIND THE ATTRIBUTES OF THE COMMON VERTICES from above ####################
V(nozero_subgraph)[intersect(intersect(max_fast_v,max_walk_v),max_eigen_v)]$country
V(nozero_subgraph)[intersect(intersect(max_fast_v,max_walk_v),max_eigen_v)]$ln_points
####################################################################################################

############################# RUN THE FAST GREEDY ALGORITHM AGAIN ON THE LARGEST 
#############################COMMUNITY OBTAINED BY THE FAST GREEDY ALGORITHM############################

fastgreedy_on_max <- fastgreedy.community(subg_nz_fast_max,weights=E(subg_nz_fast_max)$weight)
length(sizes(fastgreedy_on_max))
####################3 plot ########################################
par(mar=c(0,0,0,0))
plot(fastgreedy_on_max,subg_nz_fast_max,
     layout=layout.fruchterman.reingold(subg_nz_fast_max),
     vertex.label=NA,vertex.size=3)
plot(subg_nz_fast_max,layout=layout.fruchterman.reingold(subg_nz_fast_max),vertex.label=NA,vertex.size=4)

subgraph_for_9 <- induced.subgraph(subg_nz_fast_max,V(subg_nz_fast_max)[membership(fastgreedy_on_max)==9])
subgraph_for_1 <- induced.subgraph(subg_nz_fast_max,V(subg_nz_fast_max)[membership(fastgreedy_on_max)==1])
subgraph_for_11 <- induced.subgraph(subg_nz_fast_max,V(subg_nz_fast_max)[membership(fastgreedy_on_max)==11])
subgraph_for_7 <- induced.subgraph(subg_nz_fast_max,V(subg_nz_fast_max)[membership(fastgreedy_on_max)==7])
subgraph_for_6 <- induced.subgraph(subg_nz_fast_max,V(subg_nz_fast_max)[membership(fastgreedy_on_max)==6])
subgraph_for_4 <- induced.subgraph(subg_nz_fast_max,V(subg_nz_fast_max)[membership(fastgreedy_on_max)==4])
subgraph_for_30 <- induced.subgraph(subg_nz_fast_max,V(subg_nz_fast_max)[membership(fastgreedy_on_max)==30])


par(mfrow=c(2,2))
par(mar=c(0,0,0,0))
plot(subgraph_for_9,layout=layout.fruchterman.reingold(subgraph_for_9),vertex.label=NA,vertex.size=4)
plot(subgraph_for_1,layout=layout.fruchterman.reingold(subgraph_for_1),vertex.label=NA,vertex.size=4)
plot(subgraph_for_11,layout=layout.fruchterman.reingold(subgraph_for_11),vertex.label=NA,vertex.size=4)
plot(subgraph_for_7,layout=layout.fruchterman.reingold(subgraph_for_7),vertex.label=NA,vertex.size=4)
plot(subgraph_for_6,layout=layout.fruchterman.reingold(subgraph_for_6),vertex.label=NA,vertex.size=4)
plot(subgraph_for_4,layout=layout.fruchterman.reingold(subgraph_for_4),vertex.label=NA,vertex.size=4)
plot(subgraph_for_30,layout=layout.fruchterman.reingold(subgraph_for_30),vertex.label=NA,vertex.size=6)

vcount(subg_nz_fast_max)
V(subg_nz_fast_max)$color <- "gray"
V(subg_nz_fast_max)[V(subg_nz_fast_max)$country=="India"]$color <- "red"
V(subg_nz_fast_max)[V(subg_nz_fast_max)$country=="United States"]$color <- "green"
V(subg_nz_fast_max)[V(subg_nz_fast_max)$country=="Germany"]$color <- "yellow"
V(subg_nz_fast_max)[V(subg_nz_fast_max)$country=="United Kingdom"]$color <- "blue"
V(subg_nz_fast_max)[V(subg_nz_fast_max)$country=="Canada"]$color <- "pink"
V(subg_nz_fast_max)[V(subg_nz_fast_max)$country=="Australia"]$color <- "skyblue"
V(subg_nz_fast_max)[V(subg_nz_fast_max)$country=="Spain"]$color <- "orange"
V(subg_nz_fast_max)[V(subg_nz_fast_max)$country=="China"]$color <- "violet"


################################ NOW CONSIDER THE NODE HAVING THE HIGHEST LN_POINTS AND CHECK THE SIZE OF ITS COMMUNITY ######

vertex_max_points <- V(nozero_subgraph)[which(V(nozero_subgraph)$ln_points==max(V(nozero_subgraph)$ln_points,na.rm = TRUE))]
membership(nz_comm_fast)[(names(membership(nz_comm_fast))==vertex_max_points)]
membership(nz_comm_fast)[vertex_max_points]

sizes(nz_comm_fast)[1]
V(nozero_subgraph)$color <- "gray"
V(nozero_subgraph)[V(nozero_subgraph)$country=="India"]$color <- "red"
V(nozero_subgraph)[V(nozero_subgraph)$country=="United States"]$color <- "green"
V(nozero_subgraph)[V(nozero_subgraph)$country=="Germany"]$color <- "yellow"
V(nozero_subgraph)[V(nozero_subgraph)$country=="United Kingdom"]$color <- "blue"
V(nozero_subgraph)[V(nozero_subgraph)$country=="Canada"]$color <- "pink"
V(nozero_subgraph)[V(nozero_subgraph)$country=="Australia"]$color <- "skyblue"
V(nozero_subgraph)[V(nozero_subgraph)$country=="Spain"]$color <- "orange"
V(nozero_subgraph)[V(nozero_subgraph)$country=="China"]$color <- "violet"
subgraph_for_lnpoints_max <- induced.subgraph(nozero_subgraph,V(nozero_subgraph)[membership(nz_comm_fast)==1])
par(mar=c(0,0,0,0))
plot(subgraph_for_lnpoints_max,layout=layout.fruchterman.reingold(subgraph_for_lnpoints_max),vertex.label=NA,vertex.size=4)

####################
####################### RUN wilcox significance test on the top 25 communities##########
for(i in 10:20)
{
  print(i)
  print(community.significance.test(nozero_subgraph, membership(nz_comm_walk)==i))
}
 community.significance.test(nozero_subgraph, membership(nz_comm_fast)==10)$p.value

 for(i in 10:20)
 {
   print(i)
   print(community.significance.test(nozero_subgraph, membership(nz_comm_fast)==i))
 }
################ Consider the largest community and again try to colour by Country ############

x_sub<-which.max(sizes(fastgreedy_on_max))
subg_fg_on_max <- induced.subgraph(subg_nz_fast_max,V(subg_nz_fast_max)[which(membership(fastgreedy_on_max)==x_sub)])
V(subg_fg_on_max)[count_by_country_sub$Group.1]$color <- as.integer(row.names(count_by_country_sub))+200

plot(subg_fg_on_max)
vcount(subg_fg_on_max)


#########################################################

######################## AGGREGATE THE POINTS BY COMMUNITIES #######################################
##################### points awarded aggregated at the community level ########################
length_fast <- length(sizes(nz_comm_fast))
sum_pointsi <- vector("integer",length_fast)
mean_pointsi <- vector("integer",length_fast)
sd_pointsi <- vector("integer",length_fast)
for(i in 1:length_fast)
{
  mean_pointsi[i] <- mean(V(nozero_subgraph)[comm.fast.nz==i]$ln_points,na.rm=TRUE)
  sum_pointsi[i] <- sum(V(nozero_subgraph)[comm.fast.nz==i]$ln_points,na.rm=TRUE)
  sd_pointsi[i] <- sd(V(nozero_subgraph)[comm.fast.nz==i]$ln_points,na.rm=TRUE)
}

par(mfrow=c(1,1))
par(mar=c(1,1,1,1))

hist(mean_pointsi,main="Mean of ln_points")
hist(sum_pointsi, main = "Sum of points")
hist(sd_pointsi, main="std. deviation of points within a community")
hist(size_fast, main="Size of Communities")
plot(degree.distribution(nozero_subgraph),main="degree distribution")
size_fast <- as.vector(sizes(nz_comm_fast))
plot(size_fast,mean_pointsi,xlab="size of communities", ylab="mean of points")
plot(size_fast,sum_pointsi,xlab="size of communities", ylab="sum of points")
####################################################################################





