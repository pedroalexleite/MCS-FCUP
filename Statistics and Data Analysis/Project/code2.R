#PART 1: INTRODUCTION-----------------------------------------------------------------------------------


#packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(forcats)
library(scales)
library(gridExtra)
library(corrplot)
library(e1071)
library(MASS)
library(caret)
library(tidyr)
library(FactoMineR)
library(cluster)
library(dendextend)
library(caTools)
library(psych)
library(car)

#restart R if necessary
#.rs.restartR()


#read data
df <- read.csv('/Users/pedroalexleite/Desktop/sda-project/cars.csv')


#rename the variables
names(df)[names(df)=="Manufacturer"] <- "Brand"
names(df)[names(df)=="Sales_in_thousands"] <- "Sales_in_Thousands"
names(df)[names(df)=="X__year_resale_value"] <- "Resale_Value"
names(df)[names(df)=="Vehicle_type"] <- "Type"
names(df)[names(df)=="Price_in_thousands"] <- "Price_in_Thousands"
names(df)[names(df)=="Engine_size"] <- "Engine_Size"
names(df)[names(df)=="Curb_weight"] <- "Curb_Weight"
names(df)[names(df)=="Fuel_capacity"] <- "Fuel_Capacity"
names(df)[names(df)=="Fuel_efficiency"] <- "Fuel_Efficiency"
names(df)[names(df)=="Power_perf_factor"] <- "Power_Performance_Factor"


#visualize
#view(df)
#summary(df)


#questions:
#what is the distribution of sales?
#which manufacturers and models have the highest and lowest sales?
#what is the distribution of car price? Are there any outliers in the price distribution?
#is there a correlation between the initial price and resale value?
#what is the distribution of horsepower?
#are there any relationships between engine size, horsepower, and price?
#top 10 manufacturers with most models manufactured?
#what are the different types of vehicles used in this dataset?
#understanding the variables 'Engine_size' and 'Horsepower' for each manufacturer.
#understanding the variables 'Fuel_capacity' and 'Fuel_efficiency' for each manufacturer.
#how many vehicles are launched every year?
#how is the variable 'Power_performance_factor' distributed?
#manufacturer with the highest average sales.
#correlation between all the variables.


#PART 2: DATA CLEANING----------------------------------------------------------------------------------


#create a new variable, that's the result of the concatenation between "Brand" and "Model"
df$Brand_Model <- paste(df$Brand, df$Model, sep=" ")


#convert "Type" to binary
df$Type <- ifelse(df$Type=="Passenger", 1, 0)


#put the date in “Latest_Launch” in years since launch
df$Latest_Launch <- as.Date(df$Latest_Launch, format="%m/%d/%Y")
df$Years_Launch <- as.numeric(year(Sys.Date())-year(df$Latest_Launch))
df <- df[, !names(df) %in% "Latest_Launch"]


#check for rows with multiple N/A values, and remove them
na_rows_count <- rowSums(is.na(df))
na_rows_count 
#row 34 has 10 N/A values, so it will be removed
df <- df[-34,]


#check for variables with N/A values
na_variables_count <- colSums(is.na(df))
na_variables_count
#"Resale_Value" has 36 N/A values, we're going to replace them with the variables mean
mean_resale <- mean(df$Resale_Value, na.rm = TRUE)
df$Resale_Value[is.na(df$Resale_Value)] <- mean_resale
#"Price_in_Thousands", "Curb_Weight", "Fuel_Efficiency" and "Power_Performance_Factor", have 1 or 2 N/A 
#values, we're going to replace them with the variable mean
mean_price <- mean(df$Price_in_Thousands, na.rm=TRUE)
df$Price_in_Thousands[is.na(df$Price_in_Thousands)] <- mean_price
mean_weight <- mean(df$Curb_Weight, na.rm=TRUE)
df$Curb_Weight[is.na(df$Curb_Weight)] <- mean_weight
mean_fuel <- mean(df$Fuel_Efficiency, na.rm=TRUE)
df$Fuel_Efficiency[is.na(df$Fuel_Efficiency)] <- mean_fuel
mean_power <- mean(df$Power_Performance_Factor, na.rm=TRUE)
df$Power_Performance_Factor[is.na(df$Power_Performance_Factor)] <- mean_power


#check for values=0, in the numeric variables
numeric_columns <- sapply(df, is.numeric)
zero_values <- colSums(df[, numeric_columns]==0)
zero_values 
#no variable has values=0
#create a list with all numerical variables
df_numeric <- df %>% select_if(is.numeric)


#PART 5: MULTIVARIATE ANALYSIS--------------------------------------------------------------------------


#PART 5.1: PRINCIPAL COMPONENT ANALYSIS-----------------------------------------------------------------


#pca
pca_result <- prcomp(df_numeric, scale.=TRUE)
pca_result


#eigenvalues
#eigenvalues <- pca_result$sdev^2
#eigenvalues_df <- data.frame(Eigenvalues = eigenvalues)
#print(eigenvalues_df)


#eigenvectors
#eigenvectors <- pca_result$rotation
#eigenvectors_df <- as.data.frame(eigenvectors)
#print(eigenvectors_df)


#inertia proportion explained by each pc
total_inertia <- sum(pca_result$sdev^2)
inertia_explained <- pca_result$sdev^2/total_inertia
cumulative_inertia <- cumsum(inertia_explained)
inertia_explained_decimal <- sprintf("%.3f", inertia_explained)
cumulative_inertia_decimal <- sprintf("%.3f", cumulative_inertia)
inertia_df <- data.frame(
  Inertia_Explained = as.numeric(inertia_explained_decimal),
  Cumulative_Inertia = as.numeric(cumulative_inertia_decimal)
)
print(inertia_df)


#Pearson's Criteria says that we should only keep the PCs that explain at least 80% of the total
#dispersion, in this case that is the first 4 PCs.


#correlation between the variables and the PCs
pca_first_4_pc <- pca_result$x[, 1:4]
corr_num_pc <- cor(df_numeric, pca_first_4_pc)
corr_num_pc_df <- as.data.frame(corr_num_pc)
print(corr_num_pc_df)
p <- corrplot(corr_num_pc, tl.col="black")  


#PC1 shows an high correlation with almost every variable but Type, Years_Launch and Sales_in_Thousands.
#The correlation with Fuel_Efficiency is negative.
#PC2 shows a positive high correlation with Sales_in_Thousands and Wheelbase, and a negative one with
#Resale_Value and Price_in_Thousands.
#PC3 shows a negative high correlation with Type.
#PC4 shows a negative high correlation with Years_Launch.


#graphical representation of PC1 and PC2
p <- biplot(pca_result,choice=c(1,2))
p


#For the first 2 PCs, we can see that this groups have a positive correlation between them:
# - (Group 1) Wheelbase, Length, Fuel_Capacity, Width and Curb_Weight.
# - (Group 2) Engine_Size, Horsepower, Power_Performance, Price_in_Thousands and Resale_Value.
# - (Group 3) Type, Years_Launch and Fuel_Efficiency.
#Group 1 and Group 3 have a negative correlation.
#Years_Launch has almost no impact in these PCs.


#graphical representation of PC3 and PC4
p <- biplot(pca_result,choice=c(2,3))
p


#For PC3 and PC4,  we can see that this groups have a positive correlation between them:
# - (Group 1) Years_Launch, Fuel_Capacity and Curb_Weight.
# - (Group 2) Sales_in_Thousands, Wheelbase, Width, Length.
# - (Group 3) Fuel_Efficiency, Type, Engine_Size, Horsepower, Power_Performance_Factor, 
#Price_in_Thousands and Resale_Value.
#Group 1 and 2 and Group 1 and 3 have almost no correlation. 
#While Group 2 and 3, have a negative correlation.
#Type is the most important variable in these 2 PCs, and Engine_Size the least important.


#contribution of each variable in the PCs 
loadings_first_4_PCs <- pca_result$rotation[, 1:4]
loadings_contribution <- loadings_first_4_PCs^2
loadings_contribution_df <- format(loadings_contribution, scientific=FALSE)
print(loadings_contribution_df)


#For PC1, Sales_in_Thousands and Years_Launch have almost 0 contribution. Type also a low impact in this 
#PC. The contribution of the rest of the variables varies between 4% and 11%. Any of the variables have
#a big impact in this PC, where the contribution is well distributed.


#For PC2, Engine_Size and Fuel_Efficiency have almost 0 contribution. Width, Curb_Weight and Fuel_Capacity, 
#also have a low impact. Is important to note that this variables with low contribution, are all correlated 
#between each other, like seen previously. The contribution of the rest of the variables varies between 
#7% and 16%. Just like PC1, any of the variables have a big impact in this pc, where the contribution is 
#well distributed.


#PC3 is mainly defined by Type and Length with 57.4% of the total contribution. Resale_Value, Engine_Size
#and Price_in_Thousands have almost no impact.


#PC4 is mainly defined by Years_Launch, contributing to 88.8% of the pc. 


#PC3 explains 8.6% of the total inertia, and is mainly defined by Type and Length, like seen previously.
#We can see by the correlation matrix between the variables and PCs, that both show a negative correlation
#with PC3.This means that vehicles of the type 0, in this case "car", tend to have lower lengths.


#PC4 explains 7% of the total inertia, and is mainly defined by Years_Launch. We also know that this 
#variable and PC4 have a very negative correlation, meaning that PC4 mainly captures characteristics
#from younger cars.


#interpretation with variables
pca_result2 <- PCA(df_numeric)
plot(pca_result2, axes=c(1,2), choix=c("ind","var","varcor"), col.var="black")
pca_result2$ind


#contribution of each brand to the PCs
contrib <- pca_result2$ind$contrib
contrib_df <- as.data.frame(contrib)
contrib_df <- cbind(Brand=df$Brand, contrib_df)
average_per_brand <- contrib_df %>%
                     group_by(Brand) %>%
                     summarise(across(starts_with("Dim."), ~mean(., na.rm = TRUE)))
average_per_brand_df <- as.data.frame(average_per_brand)
average_per_brand_df <- average_per_brand_df[,1:5]
print(average_per_brand_df)


#barplot with the contribution of each brand to PC1
average_per_brand_df<- average_per_brand_df %>% arrange(Dim.1)
p_dim1 <- ggplot(average_per_brand_df, aes(x=reorder(Brand, Dim.1), y=Dim.1))+
          geom_bar(stat="identity", fill="lightblue")+
          labs(x="Brand", y="Average Contribution PC1")+
          theme(axis.text.x=element_text(angle=45, hjust=1)) 
p_dim1


#barplot with the contribution of each brand to PC2
average_per_brand_df<- average_per_brand_df %>% arrange(Dim.2)
p_dim2 <- ggplot(average_per_brand_df, aes(x=reorder(Brand, Dim.2), y=Dim.2))+
          geom_bar(stat="identity", fill="lightblue")+
          labs(x="Brand", y="Average Contribution PC2")+
          theme(axis.text.x=element_text(angle=45, hjust=1)) 
p_dim2


#grid for the PC1 and PC2 barplots
grid.arrange(p_dim1, p_dim2, nrow=2, ncol=1)


#For PC1, Lincoln is by far the most influential brand. This is a brand defined by it's Resale_Value
#Engine_Size, Horsepower, Wheelbase, With, Length, Curb_Weight. On the other hand, Infiniti is the 
#least influential brand in PC1. A bran defined by its very little amount of cars, high Resale_Value
#and Horsepower.


#For PC2, Porsche is by far the most influential brand. This is a brand that generates very little amount 
#of money, is the most expensive, presents the best Resale_Value, the smaller wheelbase and the biggest
#Power_Performance_Factor. Dodge, Ford and Mercedes-B, also show a high contribution, with a massive
#drop to every other brand. 


#barplot with the contribution of each brand to PC3
average_per_brand_df<- average_per_brand_df %>% arrange(Dim.3)
p_dim3 <- ggplot(average_per_brand_df, aes(x=reorder(Brand, Dim.3), y=Dim.3))+
          geom_bar(stat="identity", fill="lightblue")+
          labs(x="Brand", y="Average Contribution PC3")+
          theme(axis.text.x=element_text(angle=45, hjust=1)) 
p_dim3


#barplot with the contribution of each brand to PC4
average_per_brand_df<- average_per_brand_df %>% arrange(Dim.4)
p_dim4 <- ggplot(average_per_brand_df, aes(x=reorder(Brand, Dim.4), y=Dim.4))+
          geom_bar(stat="identity", fill="lightblue")+
          labs(x="Brand", y="Average Contribution PC4")+
          theme(axis.text.x=element_text(angle=45, hjust=1)) 
p_dim4


#grid for the PC3 and PC4 barplots
grid.arrange(p_dim3, p_dim4, nrow=2, ncol=1)


#For PC3, that is mainly defined by Type and Length, Jeep is by far the the most influential brand.
#Jeep is the only brand without a passenger vehicle (type 1) and is one of the brands with the most 
#car vehicles (type 0). Jeep is also the smallest brand in terms of Length. Since this variables have
#such big impact in the PC, and also show negative correlation, the fact that Jeep shows the biggest
#impact makes perfect sense.


#For PC4, that is mainly defined by Years_Launch, a variable correlated negatively with the PC, Mercury 
#is by far the most influential brand. What makes sense since its one of the oldest brands.


#PART 5.2: FACTOR ANALYSIS------------------------------------------------------------------------------


#Kaiser-Meyer-Olkin Test to measure if the dataset is adequate for factor analysis for a given dataset,
#by examining the proportion of variance among variables that might be caused by underlying factors.
KMO(numeric_df)


#We got an overall KMO score of 0.83, which is a very positive score in order to do the factor analysis.
#Every variable got at least a 0.67, so every variable is suitable.


#Communality in factor analysis represents the proportion of variance in each variable that is accounted 
#for by the extracted factors. Higher communalities indicate that the variable shares more variance with 
#the factors and is well-represented by the extracted factors.
correl <- cor(numeric_df)
pc <- principal(correl, 4, rotate="varimax")
pc$communality


#Overall, the variables in the dataset have high communalities, suggesting that they are well-represented 
#by the extracted factors.
#Variables with lower communalities like Resale_Value and Width, may have unique variance that is not 
#explained by the extracted factors, indicating that there may be other factors or variables influencing 
#them.


#factor analysis with the principal axis
pa <- fa(correl, nfactors=4, fm="pa", rotate="varimax", SMC=FALSE) 
pa


#High correlations (>0.8):
# - PA1 with Fuel_Capacity.
# - PA2 with Resale_Value, Price_in_Thousands, Horsepower and Power_Performance_Factor.
# - PA3 with Sales_in_Thousands
# - PA4 with Length. 


#In terms of variance explained by each factor, PA2 (38%) is the factor that performs better,
#followed by PA4 (26%), PA1 (25%), and then PA3 (10%).


#Engine_Size shows the biggest complexity. 
#Mean item complexity shows that, on average, each variable loads on approximately 1.6 factors.


#Root Mean Square of the Residuals represents the average discrepancy between the observed values and 
#the values predicted by the model. In our case, 0.02 indicates a very good fit.


#factor analysis with minimal residuals
pa2 <- fa(correl, nfactors=4, fm="minres", rotate="varimax", SMC=FALSE) 
pa2


#High correlations (>0.8):
# - MR1 with Curb_Weight, Fuel_Capacity, Fuel_Efficiency.
# - MR2 with Resale_Value, Price_in_Thousands, Power_Performance_Factor.
# - MR3 has none. 
# - MR4 with Wheelbase, Length.


#In terms of variance explained by each factor, MR2 (38%) is the factor that performs better,
#followed by MR1 (32%),  MR4 (25%), and then  MR3 (5%).


#Engine_Size shows the biggest complexity, again.
#Mean item complexity shows that, on average, each variable loads on approximately 1.8 factors,a little
#above the factor analysis with the principal axis.


#Root Mean Square of the Residuals, shows even better results than the last one, with 0.01.


#factor analysis with maximum likelihood 
pa3 <- fa(correl, nfactors=4, fm="ml", rotate="varimax", SMC=FALSE) 
pa3


#High correlations (>0.8):
# - ML1 with Resale_Value, Price_in_Thousands, Horsepower.
# - ML2 with Wheelbase, Length.
# - ML3 with Curb_Weight, Fuel_Capacity, Fuel_Efficiency.
# - ML4 has none. 


#In terms of variance explained by each factor, ML1 (38%) is the factor that performs better,
#followed by ML2 (32%),  ML3 (26%), and then  ML4 (5%).


#Engine_Size shows the biggest complexity, again.
#Mean item complexity shows that, on average, each variable loads on approximately 1.8 factors, exactly
#the same has the factor analysis with the minimal residuals


#Root Mean Square of the Residuals, shows the same result as he factor analysis with the principal axis.


#PART 5.3: CLUSTER ANALYSIS-----------------------------------------------------------------------------


#dataframe with the PCs (after being standardized)
df_standardize <- scale(df_numeric)
pca_result3 <- PCA(df_standardize)
models_pc_df <- as.data.frame(pca_result3$ind$coord)
models_pc_df <- models_pc_df[,1:2]
print(models_pc_df)


#elbow method
wcss <- vector()
for (i in 1:10) {
  kmeans_model <- kmeans(models_pc_df, centers=i, nstart=10)
  wcss[i] <- kmeans_model$tot.withinss
}
plot(1:10, wcss, type="b", xlab="K", ylab="WCSS")
diff_wcss <- c(0, diff(wcss))


#We applied the elbow method in order to find the perfect number of clusters. We can see by the graph
#that the WCSS stabilizes at around 6 clusters. So this will be our K.


#non-hierarchical clustering (k-means)
k <- 6
kmeans_result <- kmeans(models_pc_df, centers=k)
cluster_assignments <- kmeans_result$cluster
centroids <- kmeans_result$centers
models_pc_df$cluster <- cluster_assignments
#print(models_pc_df)
p <- ggplot(models_pc_df, aes(x=Dim.1, y=Dim.2, color=factor(cluster)))+
     geom_point()+
     labs(x="PC1", y="PC2", color="Cluster")+
     theme_minimal()
p


#silhouette coefficient 
silhouette_coefficient <- silhouette(kmeans_result$cluster, dist(models_pc_df))
mean_silhouette <- mean(silhouette_coefficient[, "sil_width"])
print(mean_silhouette)


#We calculated the silhouette coefficient in order to evaluate our clustering. We got 0.5417394,
#which indicates a reasonably good clustering quality. This suggests that the clusters are 
#well-separated and the data points within each cluster are relatively close to each other compared 
#to points in other clusters.


#descriptive statistics 
models_pc_df <- cbind(df, models_pc_df)
cluster_summary <- models_pc_df %>%
  group_by(cluster) %>%
  summarise(
    count=n(),
    avg_sales=mean(Sales_in_Thousands, na.rm=TRUE), 
    avg_resale=mean(Resale_Value, na.rm=TRUE),
    most_common_type=ifelse(mean(Type)>0.5, "Type1", "Type0"),
    avg_price=mean(Price_in_Thousands, na.rm=TRUE),
    avg_eng=mean(Engine_Size, na.rm=TRUE),
    avg_hp=mean(Horsepower, na.rm=TRUE),
    avg_wb=mean(Wheelbase, na.rm=TRUE),
    avg_width=mean(Width, na.rm=TRUE),
    avg_length=mean(Length, na.rm=TRUE),
    avg_cw=mean(Curb_Weight, na.rm=TRUE),
    avg_fc=mean(Fuel_Capacity, na.rm=TRUE),
    avg_fe=mean(Fuel_Efficiency, na.rm=TRUE),
    avg_ppf=mean(Power_Performance_Factor, na.rm=TRUE),
    avg_yl=mean(Years_Launch, na.rm=TRUE),
  )
ranking_data <- cluster_summary %>%
  mutate(
    rank_count = rank(count),
    rank_avg_sales = rank(avg_sales),
    rank_avg_resale = rank(avg_resale),
    rank_avg_price = rank(avg_price),
    rank_avg_eng = rank(avg_eng),
    rank_avg_hp = rank(avg_hp),
    rank_avg_wb = rank(avg_wb),
    rank_avg_width = rank(avg_width),
    rank_avg_length = rank(avg_length)
  )
ranking_data <- subset(ranking_data, select = -avg_sales)
ranking_data <- subset(ranking_data, select = -avg_resale)
ranking_data <- subset(ranking_data, select = -most_common_type)
ranking_data <- subset(ranking_data, select = -avg_price)
ranking_data <- subset(ranking_data, select = -avg_eng)
ranking_data <- subset(ranking_data, select = -avg_hp)
ranking_data <- subset(ranking_data, select = -avg_yl)
ranking_data <- subset(ranking_data, select = -avg_wb)
ranking_data <- subset(ranking_data, select = -avg_width)
ranking_data <- subset(ranking_data, select = -avg_length)
ranking_data <- subset(ranking_data, select = -avg_cw)
ranking_data <- subset(ranking_data, select = -avg_fc)
ranking_data <- subset(ranking_data, select = -avg_fe)
ranking_data <- subset(ranking_data, select = -avg_ppf)
ranking_data <- subset(ranking_data, select = -count)
print(ranking_data)
print(ranking_data$rank_avg_width)
print(ranking_data$rank_avg_length)


#Cluster 1:
# - High PC1 and highest PC2.
# - Has the most observations.
# - Has the most average sales.
# - Has the lowest average resale value.
# - Has the lowest average price.
# - Has the lowest average engine size.
# - Has the lowest average horsepower.
# - Has a low average width.
# - Has a low average length.


#Cluster 2: 
# - Low PC1.
# - Has a low amount of observations.
# - Has a low amount of average sales.
# - Has a high amount of average resale value.
# - Has a high amount of average price.
# - Has a high average horsepower.


#Cluster 3:
# - Has the least observations. 
# - Has a low amount of average price.
# - Has a low average horsepower.
# - Has a low average wheelbase.


#Cluster 4:
# - Lowest PC1.
# - Has the highest average resale value.
# - Has the highest average price.
# - Has the highest average engine size.
# - Has the highest average horsepower.
# - Has the highest average wheelbase.
# - Has the highest average width.
# - Has the highest average length.


#Cluster 5:
# - High PC1 and lowest PC2.
# - Has the lowest average sales.
# - Has the lowest average wheelbase.
# - Has the lowest average width.
# - Has the lowest average length.
# - Has a low average engine size.


#Cluster 6:
# - High PC1.
# - Has an high amount of observations.
# - Has an high amount of average sales.
# - Has a low amount of average resale value.
# - Has a high average engine size.
# - Has a high average wheelbase.
# - Has a high average width.
# - Has a high average length.


#hierarchical clustering (ward's method)
df_standardize <- scale(df_numeric)
pca_result3 <- PCA(df_standardize)
models_pc_df <- as.data.frame(pca_result3$ind$coord)
models_pc_df <- models_pc_df[,1:2]
hw <- hclust(dist(models_pc_df), method="ward.D")
clusters <- cutree(hw, k)
plot(hw)
rect.hclust(hw, k=k, border=2:k)


#We decided to use the ward's method and the dataframe with the pc's, because they were the ones showing
#the best results for hierarchical clustering.


#silhouette coefficient 
silhouette_coefficient <- silhouette(clusters, dist(models_pc_df))
mean_silhouette <- mean(silhouette_coefficient[, "sil_width"])
print(mean_silhouette)


#We got a silhouette coefficient of 0.3022739, meaning that the hierarchical clustering (ward's method)
#shows worst results than the non-hierarchical clustering (k-means).


#PART 5.4: LINEAR REGRESSION----------------------------------------------------------------------------


#Since we mostly have numerical variables and we want to predict "Resale_Value", a numerical continuous 
#variable, we're going to use Linear Regression for the prediction task.


#split data 80/20
set.seed(123)  
split <- sample.split(df_numeric$Resale_Value, SplitRatio=0.8)
train_data <- subset(df_numeric, split==TRUE)
test_data <- subset(df_numeric, split==FALSE)


#linear regression model
lm_model <- lm(Resale_Value ~ ., data=train_data)
predictions <- predict(lm_model, newdata=test_data)


#evaluate
rmse <- sqrt(mean((test_data$Resale_Value-predictions)^2))
summary(lm_model)
print(paste("RMSE:", round(rmse, 2)))


#The residuals represent the differences between the observed values and the predicted values. 
#The lowest point the model underestimated the target variable, was by approximately 31 units. 
#Half of the residuals are above -0.27, and half are below. The highest point, the model overestimated 
#the target variable, was by approximately 13 units.
#The min and max residuals, show that the model's predictions have a considerable amount of variability 
#or error.
#The median residual being close to zero suggests that, on average, the model's predictions are 
#relatively accurate, but since the interval of values is quite high, there might be outliers or areas 
#where the model's performance is not as good.


#The Significance Codes indicate the level of significance of each coefficient. Here, some variables 
#have significant p-values, indicating that they are likely to have a significant impact on Resale_Value, 
#while others are not significant. Those variables are Price_in_Thousands, Curb_Weight and Fuel_Capacity.


#The Multiple R-squared is 0.70. This value indicates the proportion of variance in the target 
#variable that is explained by the predictor variables. In this case, about 70% of the variance in 
#Resale_Value is explained by the predictor variables in the model.


#The Residual Standard Error is 5.7, this values represents the standard deviation of the residuals, 
#indicating the average amount by which the model's predictions deviate from the actual values.


#The Root Mean Squared Error is 4.6, and this value represents the average prediction error of the model 
#on the test dataset. It indicates the average difference between the observed values and the values 
#predicted by the model.


#Let's try Linear Regression on a standardized model now, and without the outliers. 


#boxplot with the target variable
df_standardize <- scale(df_numeric)
bp <- boxplot(df_standardize[, "Resale_Value"], ylab="Resale Value",col="lightblue")
outlier_indexes <- which(df_standardize[, "Resale_Value"] %in% bp$out)
text(x=1, y=bp$out, labels=outlier_indexes, pos=4, col="pink")
print(outlier_indexes)


#remove outliers
df_standardize <- df_standardize[-outlier_indexes, ]


#boxplot with the target variable
bp <- boxplot(df_standardize[, "Resale_Value"], ylab="Resale Value",col="lightblue")
outlier_indexes <- which(df_standardize[, "Resale_Value"] %in% bp$out)
text(x=1, y=bp$out, labels=outlier_indexes, pos=4, col="pink")
print(outlier_indexes)


#skewness for the target variable
df_standardize <- as.data.frame(df_standardize)
sk <- skewness(df_standardize$Resale_Value, na.rm=TRUE)
s <-ggplot(df_standardize, aes(x=Resale_Value))+
    geom_density(fill="lightblue", color="black")+
    labs(title = paste("Skewness: ", round(sk, 2)))+
    theme_minimal()+
    theme(plot.title=element_text(hjust=0.5))
s


#We can see now, that the boxplot has no outliers and the skewness level is balanced, since its close 
#to 0.


#split data 80/20
set.seed(123)  
split <- sample.split(df_standardize$Resale_Value, SplitRatio=0.8)
train_data <- subset(df_standardize, split==TRUE)
test_data <- subset(df_standardize, split==FALSE)


#linear regression model
lm_model <- lm(Resale_Value ~ ., data=train_data)
predictions <- predict(lm_model, newdata=test_data)


#evaluate
rmse <- sqrt(mean((test_data$Resale_Value-predictions)^2))
summary(lm_model)
print(paste("RMSE:", round(rmse, 2)))


#Since the data is now standardized, the interval between the residuals is much shorter. And so is the
#Residual Standard Error and the Root Mean Squared Error. This indicates that the model's predictions 
#are more accurate with less variability.


#The variables that have significant p-values are: Engine_Size, Width and Curb_Weight.


#The Multiple R-squared is 0.54. Meaning that we lost variance in the target variable, explained by the 
#predictor variables.


#------------------------------------------------------------------------------------------------------#