library(tidyverse) 
library(cluster) 
library(factoextra) 
library(dbscan)

data(iris)
summary(iris)

iris1 <- iris %>% #novo tiblle
  select(-Species) %>% #retirar o atributo Species
  slice(1:5) #pegar só nas primeiras 5 observações
iris1 
dist(iris1) #matriz com as distâncias

dist(iris1, method = "manhattan") #distância de manhattan
dist(iris1, method = "minkowski") #distância euclidiana (2-norm) 
dist(iris1, method = "minkowski", p = 4) #distância euclidiana (4-norm) 
dist(iris1, method = "maximum") #supreme norm

daisy(iris %>% #matriz de dissimiralidade (distâncias) 
        slice(1:5)) #pegar só nas primeiras 5 observações

iris2 <- iris %>%  #novo tiblle
  select(-Species) %>% #retirar o atributo Species
  mutate_all(scale) #subtrai a média e divide pelo desvio padrão
summary(iris2)

k3 <- kmeans(iris2, centers = 3) #aplica o k-means com k=3
k3
fviz_cluster(k3, iris2) #representa num plot

si_coefs_k3 <- silhouette(k3$cluster, dist(iris2)) #calcular o silhouette coefficient
fviz_silhouette(si_coefs_k3) + coord_flip() #representa num plot e troca os eixos

fviz_nbclust(iris2, kmeans, method = "silhouette") #representa num plot o método silhouette coefficient

fviz_nbclust(iris2, kmeans, method = "wss") + geom_vline(xintercept = 3, linetype = 2) #representa num plot o método do cotovelo

pam3 <- pam(iris2, 3) #algoritmo PAM com 3 clusters
fviz_cluster(pam3, iris2) #representa num plot
si_coefs_pam3 <- silhouette(pam3$cluster, dist(iris2)) #calcular o silhouette coefficient
fviz_silhouette(si_coefs_pam3) + coord_flip() #representa num plot e troca os eixos
fviz_nbclust(iris2, pam, method = "silhouette") #representa num plot o método silhouette coefficient
fviz_nbclust(iris2, pam, method = "wss") + geom_vline(xintercept = 3, linetype = 2) #representa num plot o método do cotovelo

clara3 <- clara(iris2, 3) #algoritmo CLARA com 3 clusters
fviz_cluster(clara3, iris2) #representa num plot
si_coefs_clara3 <- silhouette(clara3$cluster, as.matrix(dist(iris2))) #calcular o silhouette coefficient 
fviz_silhouette(si_coefs_clara3) + coord_flip() #representa num plot e troca os eixos
fviz_nbclust(iris2, clara, method = "silhouette") #representa num plot o método silhouette coefficient
fviz_nbclust(iris2, clara, method = "wss") + geom_vline(xintercept = 3, linetype = 2) #representa num plot o método do cotovelo

dbscan09 <- dbscan(iris2, eps = 0.9) #algoritmo density-based 
fviz_cluster(dbscan09, iris2) #representa num plot

dm <- dist(iris2)

hclust.sing <- hclust(dm, "single") #clustering hierárquico com distância sinle-link 
fviz_dend(hclust.sing, k = 3) #representa um dendograma com 3 clusters
c <- cutree(hclust.sing, k = 3) #cortar o dendograma 
si_coefs_hclust_sing_3 <- silhouette(c, dm) #calcular o silhouette coefficient
fviz_silhouette(si_coefs_hclust_sing_3) + coord_flip() #representa num plot o método silhouette coefficient

hclust.compl <- hclust(dm, "complete") #clustering hierárquico com distância complete-link 
fviz_dend(hclust.compl, k = 3) #representa um dendograma com 3 clusters
c <- cutree(hclust.compl, k = 3) #cortar o dendograma 
si_coefs_hclust_compl_3 <- silhouette(c, dm) #calcular o silhouette coefficient
fviz_silhouette(si_coefs_hclust_compl_3) + coord_flip() #representa num plot o método silhouette coefficient

hclust.avg <- hclust(dm, "average") #clustering hierárquico com distância average-link 
fviz_dend(hclust.avg, k = 3) #representa um dendograma com 3 clusters
c <- cutree(hclust.avg, k = 3) #cortar o dendograma 
si_coefs_hclust_avg_3 <- silhouette(c, dm) #calcular o silhouette coefficient
fviz_silhouette(si_coefs_hclust_avg_3) + coord_flip() #representa num plot o método silhouette coefficient
