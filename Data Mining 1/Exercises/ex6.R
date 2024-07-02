library(tidyverse) 
library(tidymodels)
library(kknn)
library(discrim)
library(klaR)
library(mlbench)

data("PimaIndiansDiabetes", package = "mlbench") 
str(PimaIndiansDiabetes) 
summary(PimaIndiansDiabetes)

set.seed(1234)
# pima_split <- PimaIndiansDiabetes %>% initial_split(prop=.7) 
pima_split <- PimaIndiansDiabetes %>% #inicializamos um tibble
  initial_split(prop = 0.7, strata = diabetes) #separamos o dataset: 70% treino e 30% teste
pima_split #537/231/768 -> train/test/total

train <- training(pima_split) #criamos o dataset de treino
test <- testing(pima_split) #criamos o dataset de teste
summary(train$diabetes) 
summary(test$diabetes)

pima_rec <- recipe(diabetes ~ ., train) #queremos prever "diabetes" em função "~" do resto "."
pima_rec #temos 1 outome (target variable) e 8 predictors (variáveis)

pima_rec <- pima_rec %>% 
  step_normalize(all_numeric_predictors()) %>% #normalizamos todos os atributos (menos a target variable) do dataset de treino
  prep() #média e desvio padrão
pima_train <- pima_rec %>% 
  bake(new_data = NULL) 
pima_test <- pima_rec %>% 
  bake(new_data = test)
summary(pima_train)
summary(pima_test)

model_knn <- nearest_neighbor(mode = "classification") #knn com k=5 (base)

knn_fit <- model_knn %>%
  fit(diabetes ~ ., data = pima_train) #treina os dados com o modelo do knn
knn_fit

knn_preds <- predict(knn_fit, new_data = pima_test) #fazemos as previsões, com base nos resultados anteriores
knn_preds
summary(knn_preds)

#agora conseguimos fazer a matriz de confusão
#com TP, TN, FP, FN
#isto serve para medir a eficácia
knn_preds <- pima_test %>% 
  dplyr::select(diabetes) %>% 
  bind_cols(predict(knn_fit, pima_test))
knn_preds %>%
  conf_mat(diabetes, .pred_class) %>% 
  autoplot(type = "heatmap")
knn_preds %>%
  accuracy(truth = diabetes, estimate = .pred_class) #eficácia=73%

#adicionamos as probabibilidades 
knn_preds <- pima_test %>%
  dplyr::select(diabetes) %>% 
  bind_cols(predict(knn_fit, pima_test)) %>% 
  bind_cols(predict(knn_fit, pima_test, type = "prob"))
knn_preds %>%
  roc_auc(truth = relevel(diabetes, "pos"), estimate = .pred_pos) #calculamos a rurva auc

roc_curve(knn_preds, relevel(diabetes, "pos"), .pred_pos) %>% #calculamos a rurva roc
  autoplot()



