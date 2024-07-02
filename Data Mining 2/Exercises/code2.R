library(arules)
library(arulesViz)
library(tidyverse)
library(dplyr)
library(recommenderlab)


d <- read_csv("log.csv") 
#view(d)
#summary(d)


#EX1
d %>% group_by(PAGE) %>% count() %>% arrange(desc(n))
d %>% group_by(PAGE) %>% tally(sort=TRUE)
d %>% group_by(PAGE) %>% tally(sort=TRUE) %>% top_n(2) %>% pull(PAGE)


#EX2
dt <- table(d$USER, d$PAGE)
dt
dist_dt <- dist(dt, method = "euclidean")
dist_dt
dist_dt2 <- dist(dt, method = "manhattan")
dist_dt2
dist_dt3 <- dist(dt, method = "jaccard")
dist_dt3
c1 <- hclust(dist_dt)
#plot(c1, hang=0.1)
ct <- cutree(c1, 2)
ct
#cr <- rect.hclust(c1, 2)


#EX3
dd <- mutate(d, cluster=ct[d$USER])
dd
filter(dd, cluster==1) %>% select(PAGE)
filter(dd, cluster==1) %>% pull(PAGE)
filter(dd, cluster==1) %>% group_by(PAGE) %>% tally()
filter(dd, cluster==1) %>% group_by(PAGE) %>% tally(sort=TRUE)
filter(dd, cluster==1) %>% group_by(PAGE) %>% tally(sort=TRUE) %>% top_n(2) %>% pull(PAGE)


#EX4
filter(dd, cluster==2) %>% group_by(PAGE) %>% tally(sort=TRUE) %>% top_n(2) %>% pull(PAGE)


#EX5
cluster.u2 <- dd %>% filter(USER=='u2') %>% select(cluster) %>% head(1) %>% pull()
cluster.u2
rec.u2 <- filter(dd, cluster==cluster.u2) %>% group_by(PAGE) %>% tally(sort=TRUE) %>% 
          top_n(3) %>% select(PAGE)
seen.u2 <- dd %>% filter(USER=='u2') %>% select(PAGE)
seen.u2
anti_join(rec.u2, seen.u2)

