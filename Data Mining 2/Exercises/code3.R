library(arules)
library(arulesViz)
library(tidyverse)
library(dplyr)
library(recommenderlab)
library(readr)


log <- read_csv("log1.csv", col_types=list(col_factor(), col_factor())) #convertemos strings para fatores
#view(log)


#EX6
brm <- as(as.data.frame(log), "binaryRatingMatrix")
getData.frame(brm) #rating representa se o usuário, visitou a página
brm_offline <- brm[1:6,] #1-6 treino + 7-8 teste
getData.frame(brm_offline)
inspect(getRatingMatrix(brm_offline))
r <- rowCounts(brm_offline)
r
c <- colCounts(brm_offline)
c
image(brm_offline)
modelAR <- Recommender(brm_offline, "AR") #regras de associação
getModel(modelAR)
rules <- getModel(modelAR)$rule_base
inspect(rules)


#EX7
brm_u7 <- brm[7,]
getData.frame(brm_u7)
recsAR <- predict(modelAR, brm_u7, n=2)
getList(recsAR)
r <- subset(rules, lhs %in% c("C", "F")) #o usuário 7, só visitou as páginas C e F
inspect(r)


#EX8
brm_u8 <- brm[8,]
getData.frame(brm_u8)
recsAR <- predict(modelAR, brm_u8, n=2)
getList(recsAR) #não há nenhuma regra com C, por isso não há nenhuma recomendação
r <- subset(rules, lhs %in% c("C")) 
inspect(r2)


#EX9
recommenderRegistry$get_entries(dataType ="binaryRatingMatrix")


#EX10
modelpop <- Recommender(brm_offline, "POPULAR") #recomendamos as páginas mais populares, que o usuário ainda não viu
recspop <- predict(modelpop, brm[7:8,], n=2)
getList(recspop)


#--------------------------------------------------------------------------------------------------------#


#EX11
sCos_users <- similarity(brm_offline, method="cosine")
sCos_users
sCos_items <- similarity(brm_offline, method="cosine", which="items")
sCos_items


#EX12
modelUBCF <- Recommender(brm_offline, "UBCF", parameter=list(method="cosine", nn=3))
getModel(modelUBCF)
modelIBCF <- Recommender(brm_offline, "IBCF", parameter=list(method="cosine", k=3))
getModel(modelIBCF)
recUBCF <- predict(modelUBCF, brm_u8, n=2)
getList(recUBCF)
recIBCF <- predict(modelIBCF, brm_u8, n=2)
getList(recIBCF)
recUBCF <- predict(modelUBCF, brm_u7, n=2)
getList(recUBCF)
recIBCF <- predict(modelIBCF, brm_u7, n=2)
getList(recIBCF)


#EX13
recommenderRegistry$get_entries(dataType="realRatingMatrix")


#EX14
logR <- read_csv("log1Ratings.csv", col_types=list(col_factor(), col_factor(), col_integer()))
rrm <- as(as.data.frame(logR), "realRatingMatrix")
getRatingMatrix(rrm)
image(rrm)
rrm_offline <- rrm[1:6,]
getRatingMatrix(rrm_offline)
similarity(rrm_offline, method="cosine")
similarity(rrm_offline, method="cosine", which="items")

modelUBCF_R <- Recommender(rrm_offline, "UBCF", parameter=list(nn=2))
getModel(modelUBCF_R)
recUBCF_R <- predict(modelUBCF_R, rrm[8,], type="ratings")
getList(recUBCF_R)
recUBCF_R <- predict(modelUBCF_R, rrm[7,], type="ratings")
getList(recUBCF_R)

modelIBCF_R <- Recommender(rrm_offline, "IBCF", parameter=list(nn=2))
getModel(modelIBCF_R)
recIBCF_R <- predict(modelIBCF_R, rrm[8,], type="ratings")
getList(recIBCF_R)
recIBCF_R <- predict(modelIBCF_R, rrm[7,], type="ratings")
getList(recIBCF_R)


#--------------------------------------------------------------------------------------------------------#


#EX15
log <- read_csv("log1.csv", col_types=list(col_factor(), col_factor()))
brm <- as(as.data.frame(log), "binaryRatingMatrix")
set.seed(2021)

#e <- evaluationScheme(brm, method="split", train=0.8, given=2)
rowCounts(brm)
brm <- brm[rowCounts(brm)>=2,]
e <- evaluationScheme(brm, method="split", train=0.8, given=2)
e

inspect(getRatingMatrix(brm))
inspect(getRatingMatrix(getData(e, "train")))
inspect(getRatingMatrix(getData(e, "known")))
inspect(getRatingMatrix(getData(e, "unknown")))

methods <- list(
  "popular"=list(name="POPULAR", param=NULL), 
  "user-based CF"=list(name="UBCF", param=NULL), 
  "item-based CF"=list(name="IBCF", param=NULL)
)

results <- evaluate(e, methods, type="topNList", n=c(1,3,5))

results
class(results)
avg(results)
names(results)
results[["popular"]]

getConfusionMatrix(results[["popular"]])

model2 <- Recommender(getData(e, "train"), "POPULAR")
preds2 <- predict(model2, getData(e, "known"), n=2)
getList(preds2)

model3 <-  Recommender(getData(e, "train"), "IBCF")
preds3 <- predict(model3, getData(e, "known"), n=2)
getList(preds3)

plot(results, annotate=TRUE)
plot(results, "prec/rec", annotate=TRUE)
