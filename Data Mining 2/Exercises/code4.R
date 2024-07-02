library(arules)
library(arulesViz)
library(tidyverse)
library(dplyr)
library(recommenderlab)
library(readr)
library(igraph)


#EX1
gu <- make_graph(c("a", "d", 
                   "a", "f", 
                   "e", "f",
                   "f", "d",
                   "b", "f",
                   "f", "c",
                   "c", "b"),
                   directed=F)
class(gu)
plot(gu)
V(gu)
E(gu)
gu[]
degree(gu)
closeness(gu)
betweenness(gu)


#EX2
gb <- make_graph(c("a", "d", 
                   "a", "f", 
                   "e", "f",
                   "f", "d",
                   "b", "f",
                   "f", "c",
                   "c", "b"),
                   directed=T)
class(gb)
plot(gb)
V(gb)
E(gb)
gb[]
degree(gb)
closeness(gb)
betweenness(gb)

l <- layout_in_circle(gb)
plot(gb, layout=l)
l <- layout_as_star(gb)
plot(gb, layout=l)
l <- layout_as_tree(gb)
plot(gb, layout=l)
plot(gb, layout=layout_nicely(gb))

hs <- hub_score(gb)
sort(hs$vector, decreasing=T)

as <- authority_score(gb)
sort(as$vector, decreasing=T)

pr <- page_rank(gb, damping=0.9)
sort(pr$vector, decreasing=T)


#--------------------------------------------------------------------------------------------------------#


#EX3
karate <- make_graph("Zachary")
plot(karate)

ceb <- cluster_edge_betweenness(karate) #5 comunidades
plot(ceb, karate)

sizes(ceb)
membership(ceb)

plot(as.hclust(ceb))

cfg <- cluster_fast_greedy(karate) #3 comunidades
plot(cfg, karate)
plot(cfg, karate, layout_as_star(karate))
sizes(cfg)
membership(cfg)
plot(as.hclust(cfg))


#EX4
par(mfrow=c(1,2)) #correr as 3 linhas juntas
plot(cluster_edge_betweenness(gu), gu, main="Edge Betweenness") 
plot(cluster_fast_greedy(gu), gu, main="Fast Greedy") 







