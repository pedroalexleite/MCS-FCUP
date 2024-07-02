library(e1071)
library(ggplot2)
library(tidyverse)
library(MASS)
library(caret)
library(naivebayes)
library(stats)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(class)
library(keras)
library(reticulate)

#Read data
df <- read_csv("/Users/pedroalexleite/Desktop")
df <- df[, !(names(df) %in% "customerID")]
df <- na.omit(df)
spec(df)
df <- df %>%
  mutate(gender = ifelse(gender == "Male", 1, 0),
         SeniorCitizen = as.integer(SeniorCitizen),
         Partner = ifelse(Partner == "Yes", 1, 0),
         Dependents = ifelse(Dependents == "Yes", 1, 0),
         tenure = as.numeric(tenure),
         PhoneService = ifelse(PhoneService == "Yes", 1, 0),
         PaperlessBilling = ifelse(PaperlessBilling == "Yes", 1, 0),
         MonthlyCharges = as.numeric(MonthlyCharges),
         TotalCharges = as.numeric(TotalCharges),
         Churn = ifelse(Churn == "Yes", 1, 0))

columns_to_remove <- c("MultipleLines", "InternetService", "OnlineSecurity", "OnlineBackup",
                       "DeviceProtection", "TechSupport", "StreamingTV", "StreamingMovies",
                       "PaymentMethod", "Contract")

df <- df[, !(names(df) %in% columns_to_remove)]

set.seed(123)
train_indices <- sample(1:nrow(df), 0.7 * nrow(df))
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

#Naive-bayes

class_priors <- table(train_data$Churn) / nrow(train_data)

predictor_names <- colnames(train_data)[!colnames(train_data) %in% "Churn"]

train_data$Churn <- as.factor(train_data$Churn)

naive_model <- naive_bayes(train_data[, predictor_names], train_data$Churn)

calculate_posterior <- function(instance) {
  posterior <- predict(naive_model, instance, type = "class")
  posterior / sum(posterior)
}

test_data$predicted <- rep(NA, nrow(test_data))

for (i in 1:nrow(test_data)) {
  instance <- as.data.frame(test_data[i, predictor_names])
  posterior <- calculate_posterior(instance)
  if (sum(posterior) > 0) {
    test_data$predicted[i] <- names(class_priors)[which.max(posterior)]
  } else {
    # Handle the case when posterior is empty
    test_data$predicted[i] <- NA
  }
  print(posterior)  # Debugging statement to check the posterior values
}

error_rate <- mean(test_data$Churn != test_data$predicted)
print(error_rate)

#LDA
Y <- train_data$Churn
X <- train_data[, -c(10)]  

lda_model <- lda(X, Y)

train_predictions <- predict(lda_model, newdata = X)

plot_data <- data.frame(
  Churn = factor(Y),
  Prob_Churn1 = train_predictions$posterior[, 1],
  Prob_Churn2 = train_predictions$posterior[, 2]
)

ggplot(plot_data, aes(x = Prob_Churn1, y = Prob_Churn2, color = Churn)) +
  geom_point() +
  labs(x = "Probability of Churn 1", y = "Probability of Churn 2", color = "Churn") +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))  # Custom colors for the classes

test_X <- test_data[, -c(10)]  # Exclude the Churn column from predictors
predictions <- predict(lda_model, newdata = test_X)

predicted_classes <- predictions$class

results <- data.frame(
  Predicted = predicted_classes,
  Actual = test_data$Churn
)

ggplot(results, aes(x = Actual, fill = Predicted)) +
  geom_bar() +
  labs(x = "Actual Churn", y = "Count", fill = "Predicted Churn") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))  # Custom colors for the fill

#QDA
predictor_names <- colnames(train_data)[!colnames(train_data) %in% "Churn"]

qda_model <- qda(X, Y)

train_predictions <- predict(qda_model, newdata = X)

plot_data <- data.frame(
  Churn = factor(Y),
  Prob_Churn1 = train_predictions$posterior[, 1],
  Prob_Churn2 = train_predictions$posterior[, 2]
)

ggplot(plot_data, aes(x = Prob_Churn1, y = Prob_Churn2, color = Churn)) +
  geom_point() +
  labs(x = "Probability of Churn 1", y = "Probability of Churn 2", color = "Churn") +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))  # Custom colors for the classes

test_X <- test_data[, -c(10)]  # Exclude the Churn column from predictors
predictions <- predict(qda_model, newdata = test_X)

predicted_classes <- predictions$class

results <- data.frame(
  Predicted = predicted_classes,
  Actual = test_data$Churn
)

ggplot(results, aes(x = Actual, fill = Predicted)) +
  geom_bar() +
  labs(x = "Actual Churn", y = "Count", fill = "Predicted Churn") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))  # Custom colors for the fill

#Logistic regression
model <- glm(Churn ~ gender + Partner + SeniorCitizen + Dependents + tenure + PhoneService + PaperlessBilling + TotalCharges + MonthlyCharges, data = df, family = binomial)
summary(model)

novos_dados <- data.frame(gender = c(0, 1, 0), Partner = c(1, 1, 0))
previsoes <- predict(model, newdata = novos_dados, type = "response")


#Decision tree
modelo <- rpart(Churn ~ gender + SeniorCitizen + Partner + Dependents + PhoneService + PaperlessBilling + MonthlyCharges + TotalCharges + tenure, data = train_data, method = "class",
                control = rpart.control(maxdepth = 3, minsplit = 10, cp = 0.01))

gini <- modelo$cptable[1, "xstd"]

cat("Gini Index:", gini, "\n")

rpart.plot(modelo)
text(modelo)

previsoes <- predict(modelo, newdata = test_data, type = "class")

table(previsoes, test_data$Churn)  

#Kernel method
variable_name <- "tenure"  
data <- df[[variable_name]]  

density_estimate <- density(data)
plot(density_estimate, main = "Kernel Density Estimation", xlab = variable_name, ylab = "Density")

#K-nearest method
train_features <- train_data[, -ncol(train_data)]
train_target <- train_data$Churn

test_features <- test_data[, -ncol(test_data)]
test_target <- test_data$Churn

knn_predictions <- knn(train = train_features, test = test_features, cl = train_target, k = 3)

accuracy <- sum(knn_predictions == test_target) / length(test_target)
print(accuracy)

k <- 3
knn_model <- knn(train_data, test_data, train_data$Churn, k)

train_data$Churn <- as.character(train_data$Churn)
knn_predictions <- as.character(knn_predictions)

colors <- ifelse(train_data$Churn == 0, "red", "black")
prediction_colors <- ifelse(knn_predictions == 0, "red", "black")

plot(train_data[, c("TotalCharges", "tenure")], col = colors)
points(test_data[, c("TotalCharges", "tenure")], col = prediction_colors, pch = 4)

#SVM to classssify
train_data$Churn <- as.factor(train_data$Churn)

svm_model <- svm(Churn ~ ., data = train_data)

predictions <- predict(svm_model, newdata = test_data)

accuracy <- sum(predictions == test_data$Churn) / length(predictions)
print(accuracy)

#SVM to reduce the data
svm_model_dimensionality <- svm(Churn ~ ., data = train_data)

support_vectors <- train_data[svm_model_dimensionality$index, ]

reduced_data <- support_vectors[, names(support_vectors) != "Churn"]

#SVM hyperplane
svm_model <- svm(Churn ~ tenure + MonthlyCharges, data = train_data)

support_vectors <- train_data[svm_model$index, ]

ggplot(train_data, aes(x = MonthlyCharges, y = tenure, color = Churn)) +
  geom_point() +
  geom_point(data = support_vectors, color = "green", size = 3) +
  geom_abline(intercept = -svm_model$rho, slope = -svm_model$coefs[1:2],color="red", size=2) +
  labs(x = "MonthlyCharges", y = "tenure") +
  theme_minimal()

#Neural networks without weight decay
use_python("C:/Users/35191/AppData/Local/r-miniconda/envs/r-reticulate/python.exe")

x_train <- train_data[, !(names(train_data) %in% "Churn")]
y_train <- train_data$Churn
x_test <- test_data[, !(names(test_data) %in% "Churn")]
y_test <- test_data$Churn

model <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu", input_shape = c(9)) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  loss = "binary_crossentropy",
  optimizer = "adam",
  metrics = c("accuracy")
)

x_train <- as.matrix(x_train)

history <- model %>% fit(
  x = x_train,
  y = y_train,
  epochs = 100,
  batch_size = 32
)

print(history)

x_test <- as.matrix(x_test)

metrics <- model %>% evaluate(x_test, y_test)
print(metrics)


#Neural networks with weight decay
model <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu", input_shape = c(9),  kernel_regularizer = regularizer_l2(0.01)) %>%
  layer_dense(units = 32, activation = "relu", kernel_regularizer = regularizer_l2(0.01)) %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  loss = "binary_crossentropy",
  optimizer = "adam",
  metrics = c("accuracy")
)

x_train <- as.matrix(x_train)

history <- model %>% fit(
  x = x_train,
  y = y_train,
  epochs = 100,
  batch_size = 32
)

print(history)

x_test <- as.matrix(x_test)

metrics <- model %>% evaluate(x_test, y_test)
print(metrics)
