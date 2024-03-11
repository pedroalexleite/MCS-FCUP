library(tidyverse)
library(readr)
library(dplyr)
library(tidyr)
library(caret)
library(ggplot2)
library(reshape2)
library(cluster)
library(tidyverse)
library(tidymodels)
library(pheatmap)
library(tidyr)
library(janitor)
library(factoextra)
library(cluster)
library(fpc)

#Read data
df <- read_csv("C:/Users/35191/Desktop/medm/dataset.csv")
df <- na.omit(df)
spec(df)

numeric <- df %>%
  select(-c(customerID, MultipleLines, InternetService, OnlineSecurity, OnlineBackup, DeviceProtection, TechSupport, StreamingTV, StreamingMovies, Contract,PaymentMethod)) %>%
  mutate(gender = ifelse(gender == "Male", 1, 0),
         Partner = ifelse(Partner == "Yes", 1, 0),
         Dependents = ifelse(Dependents == "Yes", 1, 0),
         PhoneService = ifelse(PhoneService == "Yes", 1, 0),
         PaperlessBilling = ifelse(PaperlessBilling == "Yes", 1, 0),
         Churn = ifelse(Churn == "Yes", 1, 0))

numeric <- na.omit(numeric)
df <- df[rownames(numeric),] 

sapply(numeric, class)

#Chi-Tests 
churn_gender <- df %>% 
  select(InternetService, PhoneService) %>% 
  drop_na()

churn_gender_table <- table(churn_gender)
chisq.test(churn_gender_table)

#Correlation matrix between numeric variables
corr_matrix <- cor(numeric)

#Correlation matrix as a heatmap
ggplot(melt(corr_matrix), aes(x = Var2, y = Var1, fill = value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(title = "Correlation Matrix", x = "", y = "")

#Correlation matrix between numeric variables
corr_matrix <- cor(numeric)

#Convert the correlation matrix into a data frame
corr_df <- as.data.frame(as.table(corr_matrix))
colnames(corr_df) <- c("Var1", "Var2", "Corr")

#Correlation matrix as a heatmap with numerical values
ggplot(corr_df, aes(x = Var2, y = Var1, fill = Corr, label = round(Corr, 2))) + 
  geom_tile() + 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_text(color = "black", size = 3) +
  labs(title = "Correlation Matrix", x = "", y = "")

#Select the numerical columns to standardize
num_cols <- df %>%
  select_if(is.numeric) %>%
  names()

#Standardize the selected columns
df[num_cols] <- scale(df[num_cols])

#Check Results
summary(df[num_cols])

numeric <- df %>%
  select(-c(customerID, MultipleLines, InternetService, OnlineSecurity, OnlineBackup, DeviceProtection, TechSupport, StreamingTV, StreamingMovies, Contract,PaymentMethod)) %>%
  mutate(gender = ifelse(gender == "Male", 1, 0),
         Partner = ifelse(Partner == "Yes", 1, 0),
         Dependents = ifelse(Dependents == "Yes", 1, 0),
         PhoneService = ifelse(PhoneService == "Yes", 1, 0),
         PaperlessBilling = ifelse(PaperlessBilling == "Yes", 1, 0),
         Churn = ifelse(Churn == "Yes", 1, 0))
numeric <- na.omit(numeric)

#PCA Calculations
pca <- prcomp(numeric)
loadings <- pca$rotation
summary(pca)

#biplot
biplot(pca,choice=c(1,2))

#Data frame with the first two PCs and any grouping variable(s) of interest
plot_data <- data.frame(PC1 = pca$x[,1], PC2 = pca$x[,2], Group = df$Churn)

#Create the plot
ggplot(plot_data, aes(x = PC1, y = PC2, color = Group)) + 
  geom_point() + 
  labs(title = "Principal Component Analysis", x = "PC1", y = "PC2")

#Loadings of variables on the first four PCs
loadings_df <- data.frame(PC1 = pca$rotation[,1], PC2 = pca$rotation[,2], 
                          PC3 = pca$rotation[,3], PC4 = pca$rotation[,4], 
                          Variable = colnames(numeric))

ggplot(melt(loadings_df, id.vars = "Variable"), 
       aes(x = variable, y = Variable, fill = value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(title = "Loadings PCs", x = "PC", y = "Variable")

#Plot loadings as a barplot
ggplot(loadings_df, aes(x = reorder(Variable, PC1), y = PC1, fill = abs(PC1))) + 
  geom_bar(stat = "identity") + 
  scale_fill_gradient(low = "blue", high = "red") + 
  coord_flip() + 
  labs(title = "Loadings on the First PC", x = "Variable", y = "Loadings")

#Cluster analysis 
dist_matrix <- dist(numeric, method = "euclidean")
hclust_model <- hclust(dist_matrix, method = "ward.D2")

#Plot the dendrogram
plot(hclust_model, hang = 0.1)

#Cut the tree to create clusters
num_clusters <- 3
cluster_cut <- cutree(hclust_model, k = num_clusters)

#Add the cluster assignments to the data frame
df$Cluster <- as.factor(cluster_cut)
df$PC1 <- pca$x[,1]
df$PC2 <- pca$x[,2]

#Scatterplot of the first two principal components colored by cluster
ggplot(df, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point() +
  labs(title = "Cluster Analysis", x = "PC1", y = "PC2")
scaled_data <- scale(numeric)

#Elbow method
wcss <- vector()
for (i in 1:10) {
  kmeans_model <- kmeans(scaled_data, centers = i, nstart = 10)
  wcss[i] <- kmeans_model$tot.withinss
}

plot(1:10, wcss, type = "b", xlab = "Número de clusters", ylab = "WCSS")

diff_wcss <- c(0, diff(wcss))
elbow <- which(diff_wcss == max(diff_wcss)) + 1

cat("Max diff", elbow, "\n")

kmeans_model <- kmeans(scaled_data, centers = num_clusters, nstart = 25)

fviz_cluster(kmeans_model, data = scaled_data, geom = "point",
             ellipse.type = "t", ggtheme = theme_classic(),
             ellipse.alpha = 0.5, palette = "jco",
             main = "K-means clustering results",
             xlab = "PC1", ylab = "PC2")

silhouette_coeff <- silhouette(kmeans_model$cluster, dist(scaled_data))

#Silhouette kmeans
cat("Mean silhouette coefficient: ", mean(silhouette_coeff[,3]), "\n")

#Silhouette scatterplot
silhouette_vals <- silhouette(cluster_cut, dist_matrix)
silhouette_avg <- mean(silhouette_vals[,3])
cat("The average silhouette coefficient is", silhouette_avg, "\n")



