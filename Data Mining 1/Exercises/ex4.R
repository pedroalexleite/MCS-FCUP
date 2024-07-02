library(tidyverse)
library(tidymodels)
library(corrplot)
library(devtools)
library(ggbiplot)

data <- load("carIns.Rdata") 
str(carIns)

carIns %>% drop_na() #remove observações que não têm valores
carIns %>% replace_na(list(nDoors = "four")) #substitui todos os valores em falta com o "four"

rec <- recipe(carIns) #definir onde está o recipe (onde vamos fazer o pré-processamento)
rec %>%
  # pre-processing steps imputation methods %>%
  prep() %>% bake(carIns)

carIns_imput <- 
  rec %>%
  step_impute_mode(nDoors) %>% #trocamos os valores em falta pela moda no "nDoors"
  step_impute_mean(normLoss) %>% #trocamos os valores em falta a média no "normLoss"
  prep() %>%
  bake(carIns)

carIns_imput %>% #ver resultados
  select(nDoors, normLoss) %>% 
  summary()

carIns_imput <- 
  rec %>% 
  step_impute_mode(all_nominal()) %>% #trocamos em todos atributos nomiais os valores em falta pela moda
  step_impute_mean(all_numeric()) %>% #trocamos em todos atributos númericos os valores em falta pela média
  prep() %>%
  bake(carIns)

summary(carIns_imput) #ver resultados

#se for a "help" (ali ao laddo ->) e pesquisar "step_impute" aparece outro tipo de imputacions

carIns_imput <- 
  rec %>% 
  step_impute_mode(all_nominal()) %>% #trocamos em todos atributos nomiais os valores em falta pela moda
  step_impute_mean(all_numeric()) %>% #trocamos em todos atributos númericos os valores em falta pela média
  step_range(all_numeric()) %>% #normaliza todos os atributos com valores numéricos (com base na normalização range-based)
  prep() %>%
  bake(carIns)

summary(carIns_imput) #ver resultados

carIns_stand <- rec %>% 
  step_normalize(price) %>% #normaliza o atributo "priceE (com base na normalização z-score)
  prep() %>%
bake(carIns)

carIns_stand %>% 
  select(price) %>% summary() #ver resultados

carIns_discr <- rec %>%
  step_discretize(price, num_breaks = 4) %>%  #divide o atributo em 4
  prep() %>%
  bake(carIns)

carIns_discr %>% 
  select(price) %>% 
  summary() #ver resultados

set.seed(123)
carIns_sample1 <- carIns %>%
  slice_sample(prop = 0.5) #uma sample aleatória com 50% dos dados

set.seed(123)
carIns_sample2 <- carIns %>%
  group_by(fuelType) %>% #crias 2 grupos, 1 com 20 com diesel (10%) e outor com 185 com gas (90%)
  slice_sample(prop = 0.5) #uma sample aleatória com 50% dos dados, de cada um dos grupos

carIns %>% 
  select(fuelType) %>% 
  summary()
carIns_sample1 %>% 
  select(fuelType) %>%
  summary()
carIns_sample2 %>% 
  select(fuelType) %>% 
  summary()

#correlação só pode ser medida entre variáveis númericaas
carIns_num <- carIns_imput %>% 
  select(where(is.numeric))
res <- carIns_num %>% 
  cor()

#matriz de correlação entre as variáveis do dataset
#quão mais perto de 1 (mais escura) quando uma das variáveis cresce a outra cresce também
#quão mais perto de -1 (mais clara) quando uma das variáveis edcresce a outra decresce também
corrplot(res, type = "lower", method = "number", number.cex = 0.5, diag = FALSE) 
#matriz de correlação entre as variáveis do dataset
corrplot.mixed(res, lower = "circle", upper = "number", number.cex = 0.5, tl.col = "black", tl.cex = 0.5)

carIns_corr <- rec %>% 
  step_impute_mean(all_numeric()) %>% 
  step_corr(all_numeric(), threshold = 0.8) %>% #calcular a correlação e eliminar os que têm correlação>=0.8
  prep() %>%
  bake(carIns)

setdiff(carIns %>%
          colnames(), carIns_corr %>% 
          colnames())

carIns_corr_num <- carIns_corr %>% 
  select(where(is.numeric))

res_corr <- carIns_corr_num %>% 
  cor()

corrplot.mixed(res_corr, lower = "circle", upper = "number", number.cex = 0.65, tl.col = "black", tl.cex = 0.65)

#calcular correlação e fazer o teste
res1 <- cor.mtest(carIns_num, conf.level = 0.95)
corrplot(res, p.mat = res1$p, type = "lower", diag = FALSE, sig.level = 0.05, insig = "blank")

data("USArrests")
res_pca <- prcomp(USArrests, scale = TRUE, center = TRUE) 
res_pca
ggbiplot(res_pca, labels = rownames(USArrests))
