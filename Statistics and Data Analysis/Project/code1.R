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

#PART 3: UNIVARIATE ANALYSIS----------------------------------------------------------------------------


#PART 3.1: CATEGORICAL VARIABLES------------------------------------------------------------------------


#visualize the nominal categorical variables "Model"
num_model <- length(unique(df$Model))
num_model 
#155, no need to visualize, every row represents a different model


#visualize the nominal categorical variable "Brand"
num_man <- length(unique(df$Brand))
num_man 
#30, we're going to use a bar plot to visualize, in ascending order, with a horizontal line, 
#representing the mean
Brand_counts <- table(df$Brand)
ordered_Brands <- names(sort(Brand_counts))
total_Brands <- sum(Brand_counts)
average_count <- total_Brands/length(Brand_counts)
p <- ggplot(df, aes(x=factor(Brand, levels=ordered_Brands)))+
     geom_bar(fill="lightblue")+
     theme(axis.text.x=element_text(angle=45, hjust=1)) +
     labs(y="Count", x="Brand")+
     geom_hline(yintercept=average_count, linetype="dashed", color="black")
p


#table with the relative frequency and cumulative frequency of each Brand
total_Brands <- sum(Brand_counts)
relative_frequencies <- paste0(round((Brand_counts/total_Brands)*100, 1), "%")
df_manu_freq <- data.frame(Brand=names(Brand_counts), Frequency=relative_frequencies)
df_manu_freq <- df_manu_freq %>% arrange(desc(Frequency))
df_manu_freq$Frequency <- as.numeric(sub("%", "", df_manu_freq$Frequency))
df_manu_freq$Cumulative_Frequency <- cumsum(df_manu_freq$Frequency)
df_brands_count <- as.data.frame(Brand_counts)
df_brands_count <- df_brands_count %>% rename(Brand=Var1)
df_brands_count <- df_brands_count %>% rename(Total=Freq)
df_manu_freq <- merge(df_manu_freq, df_brands_count, by="Brand", all.x=TRUE)
df_manu_freq <-  df_manu_freq %>% arrange(desc(Frequency))
#view(df_manu_freq)
df_brands <- df_manu_freq


#PART 3.2: NUMERICAL VARIABLES--------------------------------------------------------------------------


#location
summary(df_numeric)

#trimmed means by 5%
calculate_trimmed_mean <- function(x, trim) {mean(x, trim = trim)}
trimmed_means <- sapply(df_numeric, calculate_trimmed_mean, trim = 0.05)

#iqr
calcualte_iqr <- function(x) {IQR(x)}
iqr <- sapply(df_numeric, calcualte_iqr)

#max range
max_range <- function(x) {diff(range(x))}
range <- sapply(df_numeric,max_range)

#variance
variance <- function(x) {var(x)}
var <- sapply(df_numeric, variance)

#standard deviation
stdfunction <- function(x) {sd(x)}
std <- sapply(df_numeric, stdfunction)

#coefficient of variation
cvfunction <- function(x) {(sd(x) / mean(x)) * 100}
cv <- sapply(df_numeric, cvfunction)

#kurtosis
kurtosisval <- function(x) {e1071::kurtosis(x)}
ku <- sapply(df_numeric, kurtosisval)

#we're going to use a pie chart to visualize, and represent the frequency
percent_df <- df %>% count(Type) %>% mutate(prop=percent(n/sum(n))) %>% arrange(desc(n))
pct <- round(100*percent_df$n/sum(percent_df$n))
pie(percent_df$n,labe=paste(percent_df$Type, pct, "%", sep=" "),  col=c("lightblue", "pink"), 
    main="Passenger-Blue Car-Pink")


#histogram for the variable "Sales_in_Thousands"
h1 <- ggplot(df, aes(x=Sales_in_Thousands))+
      geom_histogram(binwidth=50, fill="lightblue", color="black") +
      labs(x="Sales_in_Thousands", y="Frequency", title=NULL)+
      theme(plot.title=element_text(hjust=0.5))
#(...) similar process for the other ones
#h1
#histogram for the variable "Resale_Value"
h2 <- ggplot(df, aes(x=Resale_Value))+
      geom_histogram(binwidth=5, fill="lightblue", color="black")+
      labs(x="Resale_Value", y="Frequency", title=NULL)+
      theme(plot.title=element_text(hjust=0.5))
#h2
#histogram for the variable "Price_in_Thousands"
h3 <- ggplot(df, aes(x=Price_in_Thousands))+
      geom_histogram(binwidth=5, fill="lightblue", color="black")+
      labs(x="Price_in_Thousands", y="Frequency", title=NULL)+
      theme(plot.title=element_text(hjust=0.5))
#h3
#histogram for the variable "Engine_Size" 
h4 <- ggplot(df, aes(x=Engine_Size))+
      geom_histogram(binwidth=0.5, fill="lightblue", color="black")+
      labs(x="Engine_Size", y="Frequency", title=NULL)+
      theme(plot.title=element_text(hjust=0.5))
#h4
#histogram for the variable "Horsepower"
h5 <- ggplot(df, aes(x=Horsepower))+
      geom_histogram(binwidth=30, fill="lightblue", color="black")+
      labs(x="Horsepower", y="Frequency", title=NULL)+
      theme(plot.title=element_text(hjust=0.5))
#h5
#histogram for the variable "Wheelbase"
h6 <- ggplot(df, aes(x=Wheelbase))+
      geom_histogram(binwidth=3, fill="lightblue", color="black")+
      labs(x="Wheelbase", y="Frequency", title=NULL)+
      theme(plot.title=element_text(hjust=0.5))
#h6
#histogram for the variable "Width"
h7 <- ggplot(df, aes(x=Width))+
      geom_histogram(binwidth=1.5, fill="lightblue", color="black")+
      labs(x="Width", y="Frequency", title=NULL)+
      theme(plot.title=element_text(hjust=0.5))
#h7
#histogram for the variable "Length"
h8 <- ggplot(df, aes(x=Length))+
      geom_histogram(binwidth=5, fill="lightblue", color="black")+
      labs(x="Length", y="Frequency", title=NULL)+
      theme(plot.title=element_text(hjust=0.5))
#h8
#histogram for the variable "Curb_Weight
h9 <- ggplot(df, aes(x=Curb_Weight))+
      geom_histogram(binwidth=0.3, fill="lightblue", color="black")+
      labs(x="Curb_Weight", y="Frequency", title=NULL)+
      theme(plot.title=element_text(hjust=0.5))
#h9
#histogram for the variable "Fuel_Capacity"
h10 <- ggplot(df, aes(x=Fuel_Capacity))+
       geom_histogram(binwidth=1.5, fill="lightblue", color="black")+
       labs(x="Fuel_Capacity", y="Frequency", title=NULL)+
       theme(plot.title=element_text(hjust=0.5))
#h10
#histogram for the variable "Fuel_Efficiency"
h11 <- ggplot(df, aes(x=Fuel_Efficiency))+
       geom_histogram(binwidth=2, fill="lightblue", color="black")+
       labs(x="Fuel_Efficiency", y="Frequency", title=NULL)+
       theme(plot.title=element_text(hjust=0.5))
#h11
#histogram for the variable "Power_Performance_Factor"
h12 <- ggplot(df, aes(x=Power_Performance_Factor))+
       geom_histogram(binwidth=12, fill="lightblue", color="black")+
       labs(x="Power_Performance_Factor", y="Frequency", title=NULL)+
       theme(plot.title=element_text(hjust=0.5))
#h12


#grid with a histogram, for every numerical variable 
grid.arrange(h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, nrow=4, ncol=3)


#skewness for "Sales_in_Thousands"
sk1 <- skewness(df$Sales_in_Thousands, na.rm=TRUE)
s1 <-ggplot(df, aes(x=Sales_in_Thousands))+
     geom_density(fill="lightblue", color="black")+
     labs(title = paste("Skewness: ", round(sk1, 2)))+
     theme_minimal()+
     theme(plot.title=element_text(hjust=0.5))
#(...) similar process for the other ones
#s1
#skewness for "Resale_Value"
sk2 <- skewness(df$Resale_Value, na.rm=TRUE)
s2 <-ggplot(df, aes(x=Resale_Value))+
     geom_density(fill="lightblue", color="black")+
     labs(title = paste("Skewness: ", round(sk2, 2)))+
     theme_minimal()+
     theme(plot.title=element_text(hjust=0.5))
#s2
#skewness for "Price_in_Thousands"
sk3 <- skewness(df$Price_in_Thousands, na.rm=TRUE)
s3 <-ggplot(df, aes(x=Price_in_Thousands))+
     geom_density(fill="lightblue", color="black")+
     labs(title = paste("Skewness: ", round(sk3, 2)))+
     theme_minimal()+
     theme(plot.title=element_text(hjust=0.5))
#s3
#skewness for "Engine_Size"
sk4 <- skewness(df$Engine_Size, na.rm=TRUE)
s4 <-ggplot(df, aes(x=Engine_Size))+
     geom_density(fill="lightblue", color="black")+
     labs(title = paste("Skewness: ", round(sk4, 2)))+
     theme_minimal()+
     theme(plot.title=element_text(hjust=0.5))
#s4
#skewness for "Horsepower"
sk5 <- skewness(df$Horsepower, na.rm=TRUE)
s5 <-ggplot(df, aes(x=Horsepower))+
     geom_density(fill="lightblue", color="black")+
     labs(title = paste("Skewness: ", round(sk5, 2)))+
     theme_minimal()+
     theme(plot.title=element_text(hjust=0.5))
#s5
#skewness for "Wheelbase"
sk6 <- skewness(df$Wheelbase, na.rm=TRUE)
s6 <-ggplot(df, aes(x=Wheelbase))+
     geom_density(fill="lightblue", color="black")+
     labs(title = paste("Skewness: ", round(sk6, 2)))+
     theme_minimal()+
     theme(plot.title=element_text(hjust=0.5))
#s6
#skewness for "Width"
sk7 <- skewness(df$Width, na.rm=TRUE)
s7 <-ggplot(df, aes(x=Width))+
     geom_density(fill="lightblue", color="black")+
     labs(title = paste("Skewness: ", round(sk7, 2)))+
     theme_minimal()+
     theme(plot.title=element_text(hjust=0.5))
#s7
#skewness for "Length"
sk8 <- skewness(df$Length, na.rm=TRUE)
s8 <-ggplot(df, aes(x=Length))+
     geom_density(fill="lightblue", color="black")+
     labs(title = paste("Skewness: ", round(sk8, 2)))+
     theme_minimal()+
     theme(plot.title=element_text(hjust=0.5))
#s8
#skewness for "Curb_Weight"
sk9 <- skewness(df$Curb_Weight, na.rm=TRUE)
s9 <-ggplot(df, aes(x=Curb_Weight))+
     geom_density(fill="lightblue", color="black")+
     labs(title = paste("Skewness: ", round(sk9, 2)))+
     theme_minimal()+
     theme(plot.title=element_text(hjust=0.5))
#s9
#skewness for "Fuel_Capacity"
sk10 <- skewness(df$Fuel_Capacity, na.rm=TRUE)
s10 <-ggplot(df, aes(x=Fuel_Capacity))+
      geom_density(fill="lightblue", color="black")+
      labs(title = paste("Skewness: ", round(sk10, 2))) +
      theme_minimal()+
      theme(plot.title=element_text(hjust=0.5))
#s10
#skewness for "Fuel_Efficiency"
sk11 <- skewness(df$Fuel_Efficiency, na.rm=TRUE)
s11 <-ggplot(df, aes(x=Fuel_Efficiency))+
      geom_density(fill="lightblue", color="black")+
      labs(title = paste("Skewness: ", round(sk11, 2)))+
      theme_minimal()+
      theme(plot.title=element_text(hjust=0.5))
#s11
#skewness for "Power_Performance_Factor"
sk12 <- skewness(df$Power_Performance_Factor, na.rm=TRUE)
s12 <-ggplot(df, aes(x=Power_Performance_Factor))+
      geom_density(fill="lightblue", color="black")+
      labs(title = paste("Skewness: ", round(sk12, 2)))+
      theme_minimal()+
      theme(plot.title=element_text(hjust=0.5))
#s12


#grid with the skewness, for every numerical variable 
grid.arrange(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, nrow=4, ncol=3)


#for the new dataframe
#boxplot for the variable "Sales_in_Thousands"
bn1 <- ggplot(df, aes(y=Sales_in_Thousands))+
       geom_boxplot(fill="lightblue")+
       labs(x="Sales_in_Thousands", y="")
#(...) similar process for the other ones
#bn1
#boxplot for the variable "Resale_Value"
bn2 <- ggplot(df, aes(y=Resale_Value))+
       geom_boxplot(fill="lightblue")+
       labs(x="Resale_Value", y="")
#bn2
#boxplot for the variable "Price_in_Thousands"
bn3 <- ggplot(df, aes(y=Price_in_Thousands))+
       geom_boxplot(fill="lightblue")+
       labs(x = "Price_in_Thousands", y = "")
#bn3
#boxplot for the variable "Engine_Size"
bn4 <- ggplot(df, aes(y=Engine_Size))+
       geom_boxplot(fill="lightblue")+
       labs(x="Engine_Size", y="")
#bn4
#boxplot for the variable "Horsepower"
bn5 <- ggplot(df, aes(y=Horsepower))+
       geom_boxplot(fill="lightblue")+
       labs(x="Horsepower", y="")
#bn5
#boxplot for the variable "Wheelbase"
bn6 <- ggplot(df, aes(y=Wheelbase))+
       geom_boxplot(fill="lightblue")+
       labs(x="Wheelbase", y="")
#bn6
#boxplot for the variable "Width"
bn7 <- ggplot(df, aes(y = Width))+
       geom_boxplot(fill="lightblue")+
       labs(x="Width", y="")
#bn7
#boxplot for the variable "Length"
bn8 <- ggplot(df, aes(y=Length))+
       geom_boxplot(fill="lightblue")+
       labs(x="Length", y="")
#bn8
#boxplot for the variable "Curb_Weight"
bn9 <- ggplot(df, aes(y=Curb_Weight))+
       geom_boxplot(fill="lightblue")+
       labs(x="Curb_Weight", y="")
#bn9
#boxplot for the variable "Fuel_Capacity"
bn10 <- ggplot(df, aes(y=Fuel_Capacity))+
        geom_boxplot(fill="lightblue")+
        labs(x="Fuel_Capacity", y="")
#bn10
#boxplot for the variable "Fuel_Efficiency"
bn11 <- ggplot(df, aes(y=Fuel_Efficiency))+
        geom_boxplot(fill="lightblue")+
        labs(x="Fuel_Efficiency", y="")
#bn11
#boxplot for the variable "Power_Performance_Factor"
bn12 <- ggplot(df, aes(y=Power_Performance_Factor))+
        geom_boxplot(fill="lightblue")+
        labs(x = "Power_Performance_Factor", y = "")
#bn12


#grid with the boxplots for the new dataframe, for every numerical variable 
grid.arrange(bn1, bn2, bn3, bn4, bn5, bn6, bn7, bn8, bn9, bn10, bn11, bn12, nrow=4, ncol=3)


#outliers of "Sales_in_Thousands"
o1 <- boxplot.stats(df$Sales_in_Thousands)$out
o1 <- df[df$Sales_in_Thousands %in% o1, c("Brand_Model", "Sales_in_Thousands")]
colnames(o1) <- c("Brand_Model", "Outliers")
o1 <- o1[order(-o1$Outliers), ]
o1
#outliers of "Resale_Value"
o2 <- boxplot.stats(df$Resale_Value)$out
o2 <- df[df$Resale_Value %in% o2, c("Brand_Model", "Resale_Value")]
colnames(o2) <- c("Brand_Model", "Outliers")
o2 <- o2[order(-o2$Outliers), ]
o2
#outliers of "Price_in_Thousands"
o3 <- boxplot.stats(df$Price_in_Thousands)$out
o3 <- df[df$Price_in_Thousands %in% o3, c("Brand_Model", "Price_in_Thousands")]
colnames(o3) <- c("Brand_Model", "Outliers")
o3 <- o3[order(-o3$Outliers), ]
o3
#outliers of "Engine_Size"
o4 <- boxplot.stats(df$Engine_Size)$out
o4 <- df[df$Engine_Size %in% o4, c("Brand_Model", "Engine_Size")]
colnames(o4) <- c("Brand_Model", "Outliers")
o4 <- o4[order(-o4$Outliers), ]
o4
#outliers of "Horsepower"
o5 <- boxplot.stats(df$Horsepower)$out
o5 <- df[df$Horsepower %in% o5, c("Brand_Model", "Horsepower")]
colnames(o5) <- c("Brand_Model", "Outliers")
o5 <- o5[order(-o5$Outliers), ]
o5
#outliers of "Wheelbase"
o6 <- boxplot.stats(df$Wheelbase)$out
o6 <- df[df$Wheelbase %in% o6, c("Brand_Model", "Wheelbase")]
colnames(o6) <- c("Brand_Model", "Outliers")
o6 <- o6[order(-o6$Outliers), ]
o6
#outliers of "Length"
o7 <- boxplot.stats(df$Length)$out
o7 <- df[df$Length %in% o7, c("Brand_Model", "Length")]
colnames(o7) <- c("Brand_Model", "Outliers")
o7 <- o7[order(-o7$Outliers), ]
o7
#outliers of "Curb_Weight"
o8 <- boxplot.stats(df$Curb_Weight)$out
o8 <- df[df$Curb_Weight %in% o8, c("Brand_Model", "Curb_Weight")]
colnames(o8) <- c("Brand_Model", "Outliers")
o8 <- o8[order(-o8$Outliers), ]
o8
#outliers of "Fuel_Capacity"
o9 <- boxplot.stats(df$Fuel_Capacity)$out
o9 <- df[df$Fuel_Capacity %in% o9, c("Brand_Model", "Fuel_Capacity")]
colnames(o9) <- c("Brand_Model", "Outliers")
o9 <- o9[order(-o9$Outliers), ]
o9
#outliers of "Fuel_Efficiency"
o10 <- boxplot.stats(df$Fuel_Efficiency)$out
o10 <- df[df$Fuel_Efficiency %in% o10, c("Brand_Model", "Fuel_Efficiency")]
colnames(o10) <- c("Brand_Model", "Outliers")
o10 <- o10[order(-o10$Outliers), ]
o10
#outliers of "Power_Performance_Factor"
o11 <- boxplot.stats(df$Power_Performance_Factor)$out
o11 <- df[df$Power_Performance_Factor %in% o11, c("Brand_Model", "Power_Performance_Factor")]
colnames(o11) <- c("Brand_Model", "Outliers")
o11 <- o11[order(-o11$Outliers), ]
o11
#all the outliers
all_outliers <- rbind(o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11)
all_outliers <- all_outliers[, !names(all_outliers) %in% c("Outliers")]
all_outliers <- unique(all_outliers)
all_outliers



#PART 4: BIVARIATE ANALYSIS-----------------------------------------------------------------------------


#PART 4.1: CATEGORICAL x NUMERICAL VARIABLES------------------------------------------------------------


#table with the mean sales for "Passenger" and "Car"
mean_sales <- df %>% group_by(Type) %>%  summarize(mean_sales=mean(Sales_in_Thousands, na.rm=TRUE))
mean_sales$Type <- factor(mean_sales$Type, levels=c(1, 0), labels=c("Passenger", "Car"))
#piechart for "Passenger"/"Car" and "Sales_in_Thousands"
p1 <- ggplot(mean_sales, aes(x="", y=mean_sales, fill=Type))+
      geom_bar(stat="identity", width=1)+
      coord_polar(theta="y")+
      scale_fill_manual(values=c("Passenger"="lightblue", "Car"="pink"))+
      labs(x="", y="", fill="Type")+
      ggtitle("Mean Sales")+  
      theme_minimal()+
      theme(axis.ticks=element_blank(), 
            axis.text=element_blank(), 
            panel.grid=element_blank(),
            plot.title=element_text(hjust=0.5),
            legend.position = "none")
#table with the mean resale value for "Passenger" and "Car"
mean_resales <- df %>% group_by(Type) %>%  summarize(mean_resales=mean(Resale_Value, na.rm=TRUE))
mean_resales$Type <- factor(mean_resales$Type, levels=c(1, 0), labels=c("Passenger", "Car"))
#piechart for "Passenger"/"Car" and "Resale_Value"
p2 <- ggplot(mean_resales, aes(x="", y=mean_resales, fill=Type))+
      geom_bar(stat="identity", width=1)+
      coord_polar(theta="y")+
      scale_fill_manual(values=c("Passenger"="lightblue", "Car"="pink"))+
      labs(x="", y="", fill="Type")+
      ggtitle("Mean Resale Value")+  
      theme_minimal()+
      theme(axis.ticks=element_blank(), 
            axis.text=element_blank(), 
            panel.grid=element_blank(),
            plot.title=element_text(hjust=0.5),
            legend.position = "none")
#table with the mean price for "Passenger" and "Car"
mean_prices <- df %>% group_by(Type) %>% summarize(mean_price=mean(Price_in_Thousands, na.rm=TRUE))
mean_prices$Type <- factor(mean_prices$Type, level=c(1, 0), labels=c("Passenger", "Car"))
mean_prices
#piechart for "Passenger"/"Car" and "Price_in_Thousands"
p3 <- ggplot(mean_prices, aes(x="", y=mean_price, fill=Type))+
      geom_bar(stat="identity", width=1)+
      coord_polar(theta="y")+
      scale_fill_manual(values=c("Passenger"="lightblue", "Car"="pink"))+
      labs(x="", y="", fill="Type")+
      ggtitle("Mean Price")+  
      theme_minimal()+
      theme(axis.ticks=element_blank(), 
            axis.text=element_blank(), 
            panel.grid=element_blank(),
            plot.title=element_text(hjust=0.5),
            legend.position = "none")
#table with the mean engine size for "Passenger" and "Car"
mean_engine <- df %>% group_by(Type) %>% summarize(mean_engine=mean(Engine_Size, na.rm=TRUE))
mean_engine$Type <- factor(mean_engine$Type, level=c(1, 0), labels=c("Passenger", "Car"))
mean_engine
#piechart for "Passenger"/"Car" and "Engine_Size"
p4 <- ggplot(mean_engine, aes(x="", y=mean_engine, fill=Type))+
      geom_bar(stat="identity", width=1)+
      coord_polar(theta="y")+
      scale_fill_manual(values=c("Passenger"="lightblue", "Car"="pink"))+
      labs(x="", y="", fill="Type")+
      ggtitle("Mean Engine Size")+  
      theme_minimal()+
      theme(axis.ticks=element_blank(), 
            axis.text=element_blank(), 
            panel.grid=element_blank(),
            plot.title=element_text(hjust=0.5),
            legend.position = "none")
#table with the mean engine size for "Passenger" and "Car"
mean_horsepower <- df %>% group_by(Type) %>% summarize(mean_horsepower=mean(Horsepower, na.rm=TRUE))
mean_horsepower$Type <- factor(mean_horsepower$Type, level=c(1, 0), labels=c("Passenger", "Car"))
mean_horsepower
#piechart for "Passenger"/"Car" and "Horsepower"
p5 <- ggplot(mean_horsepower, aes(x="", y=mean_horsepower, fill=Type))+
      geom_bar(stat="identity", width=1)+
      coord_polar(theta="y")+
      scale_fill_manual(values=c("Passenger"="lightblue", "Car"="pink"))+
      labs(x="", y="", fill="Type")+
      ggtitle("Mean Horsepower")+  
      theme_minimal()+
      theme(axis.ticks=element_blank(), 
            axis.text=element_blank(), 
            panel.grid=element_blank(),
            plot.title=element_text(hjust=0.5),
            legend.position = "none")
#table with the mean wheelbase for "Passenger" and "Car"
mean_wheelbase <- df %>% group_by(Type) %>% summarize(mean_wheelbase=mean(Wheelbase, na.rm=TRUE))
mean_wheelbase$Type <- factor(mean_wheelbase$Type, level=c(1, 0), labels=c("Passenger", "Car"))
mean_wheelbase
#piechart for "Passenger"/"Car" and "Wheelbase"
p6 <- ggplot(mean_wheelbase, aes(x="", y=mean_wheelbase, fill=Type))+
      geom_bar(stat="identity", width=1)+
      coord_polar(theta="y")+
      scale_fill_manual(values=c("Passenger"="lightblue", "Car"="pink"))+
      labs(x="", y="", fill="Type")+
      ggtitle("Mean Wheelbase")+  
      theme_minimal()+
      theme(axis.ticks=element_blank(), 
            axis.text=element_blank(), 
            panel.grid=element_blank(),
            plot.title=element_text(hjust=0.5),
            legend.position = "none")
#table with the mean width for "Passenger" and "Car"
mean_width <- df %>% group_by(Type) %>% summarize(mean_width=mean(Width, na.rm=TRUE))
mean_width$Type <- factor(mean_width$Type, level=c(1, 0), labels=c("Passenger", "Car"))
mean_width
#piechart for "Passenger"/"Car" and "Width"
p7 <- ggplot(mean_width, aes(x="", y=mean_width, fill=Type))+
      geom_bar(stat="identity", width=1)+
      coord_polar(theta="y")+
      scale_fill_manual(values=c("Passenger"="lightblue", "Car"="pink"))+
      labs(x="", y="", fill="Type")+
      ggtitle("Mean Width")+  
      theme_minimal()+
      theme(axis.ticks=element_blank(), 
            axis.text=element_blank(), 
            panel.grid=element_blank(),
            plot.title=element_text(hjust=0.5),
            legend.position = "none")
#table with the mean length for "Passenger" and "Car"
mean_length <- df %>% group_by(Type) %>% summarize(mean_length=mean(Length, na.rm=TRUE))
mean_length$Type <- factor(mean_length$Type, level=c(1, 0), labels=c("Passenger", "Car"))
mean_length
#piechart for "Passenger"/"Car" and "Length"
p8 <- ggplot(mean_length, aes(x="", y=mean_length, fill=Type))+
      geom_bar(stat="identity", width=1)+
      coord_polar(theta="y")+
      scale_fill_manual(values=c("Passenger"="lightblue", "Car"="pink"))+
      labs(x="", y="", fill="Type")+
      ggtitle("Mean Length")+  
      theme_minimal()+
      theme(axis.ticks=element_blank(), 
            axis.text=element_blank(), 
            panel.grid=element_blank(),
            plot.title=element_text(hjust=0.5),
            legend.position = "none")
#table with the mean weight for "Passenger" and "Car"
mean_weight <- df %>% group_by(Type) %>% summarize(mean_weight=mean(Curb_Weight, na.rm=TRUE))
mean_weight$Type <- factor(mean_weight$Type, level=c(1, 0), labels=c("Passenger", "Car"))
mean_weight
#piechart for "Passenger"/"Car" and "Curb_Weight"
p9 <- ggplot(mean_weight, aes(x="", y=mean_weight, fill=Type))+
      geom_bar(stat="identity", width=1)+
      coord_polar(theta="y")+
      scale_fill_manual(values=c("Passenger"="lightblue", "Car"="pink"))+
      labs(x="", y="", fill="Type")+
      ggtitle("Mean Weight")+  
      theme_minimal()+
      theme(axis.ticks=element_blank(), 
            axis.text=element_blank(), 
            panel.grid=element_blank(),
            plot.title=element_text(hjust=0.5),
            legend.position = "none")
#table with the mean fuel capacity for "Passenger" and "Car"
mean_fuelcapacity <- df %>% group_by(Type) %>% summarize(mean_fuelcapacity=mean(Fuel_Capacity, na.rm=TRUE))
mean_fuelcapacity$Type <- factor(mean_fuelcapacity$Type, level=c(1, 0), labels=c("Passenger", "Car"))
mean_fuelcapacity
#piechart for "Passenger"/"Car" and "Fuel_Capacity"
p10 <- ggplot(mean_fuelcapacity, aes(x="", y=mean_fuelcapacity, fill=Type))+
       geom_bar(stat="identity", width=1)+
       coord_polar(theta="y")+
       scale_fill_manual(values=c("Passenger"="lightblue", "Car"="pink"))+
       labs(x="", y="", fill="Type")+
       ggtitle("Mean Fuel Capacity")+  
       theme_minimal()+
       theme(axis.ticks=element_blank(), 
             axis.text=element_blank(), 
             panel.grid=element_blank(),
             plot.title=element_text(hjust=0.5),
             legend.position = "none")
#table with the mean fuel efficiency for "Passenger" and "Car"
mean_fuelefficiency <- df %>% group_by(Type) %>% summarize(mean_fuelefficiency=mean(Fuel_Efficiency, na.rm=TRUE))
mean_fuelefficiency$Type <- factor(mean_fuelefficiency$Type, level=c(1, 0), labels=c("Passenger", "Car"))
mean_fuelefficiency
#piechart for "Passenger"/"Car" and "Fuel_Efficiency"
p11 <- ggplot(mean_fuelefficiency, aes(x="", y=mean_fuelefficiency, fill=Type))+
       geom_bar(stat="identity", width=1)+
       coord_polar(theta="y")+
       scale_fill_manual(values=c("Passenger"="lightblue", "Car"="pink"))+
       labs(x="", y="", fill="Type")+
       ggtitle("Mean Fuel Efficiency")+  
       theme_minimal()+
       theme(axis.ticks=element_blank(), 
             axis.text=element_blank(), 
             panel.grid=element_blank(),
             plot.title=element_text(hjust=0.5),
             legend.position = "none")
#table with the mean factor for "Passenger" and "Car"
mean_factor <- df %>% group_by(Type) %>% summarize(mean_factor=mean(Power_Performance_Factor, na.rm=TRUE))
mean_factor$Type <- factor(mean_factor$Type, level=c(1, 0), labels=c("Passenger", "Car"))
mean_factor
#piechart for "Passenger"/"Car" and "Power_Performance_Factor"
p12 <- ggplot(mean_factor, aes(x="", y=mean_factor, fill=Type))+
       geom_bar(stat="identity", width=1)+
       coord_polar(theta="y")+
       scale_fill_manual(values=c("Passenger"="lightblue", "Car"="pink"))+
       labs(x="", y="", fill="Type")+
       ggtitle("Mean Factor")+  
       theme_minimal()+
       theme(axis.ticks=element_blank(), 
             axis.text=element_blank(), 
             panel.grid=element_blank(),
             plot.title=element_text(hjust=0.5),
             legend.position = "none")


#grid with the piecharts, for every numerical variable 
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, nrow=4, ncol=3)


#t-tests
t_tests <- list(
  sales = t.test(Sales_in_Thousands ~ Type, data = df),
  resale = t.test(Resale_Value ~ Type, data = df),
  price = t.test(Price_in_Thousands ~ Type, data = df),
  engine = t.test(Engine_Size ~ Type, data = df),
  horsepower = t.test(Horsepower ~ Type, data = df),
  wheelbase = t.test(Wheelbase ~ Type, data = df),
  width = t.test(Width ~ Type, data = df),
  length = t.test(Length ~ Type, data = df),
  curb_weight = t.test(Curb_Weight ~ Type, data = df),
  fuel_capacity = t.test(Fuel_Capacity ~ Type, data = df),
  fuel_efficiency = t.test(Fuel_Efficiency ~ Type, data = df),
  power_factor = t.test(Power_Performance_Factor ~ Type, data = df)
)
significant_results <- lapply(t_tests, function(test) {
  if (test$p.value < 0.05) {
    return(test)
  }
})
for (i in seq_along(significant_results)) {
  if (!is.null(significant_results[[i]])) {
    cat("Variable:", names(t_tests)[i], "\n")
    cat("p-value:", significant_results[[i]]$p.value, "\n")
    cat("\n")
  }
}


#table with the amount of each different "Type" for each "Brand"
passenger_count <- df %>% filter(Type==1) %>% group_by(Brand) %>% summarise(Passenger=n())
df_brands <- left_join(df_brands, passenger_count, by="Brand")
df_brands <- df_brands %>% mutate(Passenger=replace_na(Passenger, 0))
car_count <- df %>% filter(Type==0) %>% group_by(Brand) %>% summarise(Car=n())
df_brands <- left_join(df_brands, car_count, by="Brand")
df_brands <- df_brands %>% mutate(Car=replace_na(Car, 0))
#barplot with the amount of "Passenger" for each "Brand"
passenger_mean <- mean(df_brands$Passenger, na.rm=TRUE)
passenger_plot <- ggplot(df_brands, aes(x=reorder(Brand, Passenger), y=Passenger))+
                  geom_bar(stat="identity", fill="lightblue")+
                  labs(x="Brand", y="Passenger Count")+
                  theme(axis.text.x=element_text(angle=45, hjust=1))+
                  geom_hline(yintercept=passenger_mean, linetype="dashed", color="black")
#barplot with the amount of "Car" for each "Brand"
car_mean <- mean(df_brands$Car, na.rm=TRUE)
car_plot <- ggplot(df_brands, aes(x=reorder(Brand, Car), y=Car))+
            geom_bar(stat="identity", fill="pink")+
            labs(x="Brand", y="Car Count")+
            theme(axis.text.x=element_text(angle=45, hjust=1))+
            geom_hline(yintercept=car_mean, linetype="dashed", color="black")
#grid with both plots
grid.arrange(passenger_plot, car_plot, ncol=2)


#table with the total money generated per "Brand"´
df <- df %>% mutate(money_generated=Sales_in_Thousands*Price_in_Thousands) 
df_money <- df %>% group_by(Brand) %>% summarise(Money_Generated=sum(money_generated, na.rm=TRUE)) %>%
            arrange(Money_Generated)
df_money$Brand <- factor(df_money$Brand, levels = df_money$Brand[order(df_money$Money_Generated)])
df_brands <- merge(df_brands, df_money, by="Brand", all=TRUE)
money_generated_index <- which(colnames(df) == "money_generated")
df <- df[, -money_generated_index]
#barplot with the total money generated per "Brand"
money_mean <- mean(df_brands$Money_Generated, na.rm=TRUE)
p <- ggplot(df_money, aes(x=Brand, y=Money_Generated))+
     geom_bar(stat="identity", fill="lightblue")+
     labs(x="Brand", y="Money Generated")+
     theme(axis.text.x=element_text(angle=45, hjust=1))+
     geom_hline(yintercept=money_mean, linetype="dashed", color="black")
p


#table with the mean "Price_in_Thousands" for each "Brand"
df_mean <- df %>% group_by(Brand) %>% summarise(Mean_Price=mean(Price_in_Thousands, na.rm=TRUE)) %>% 
           arrange(desc(Mean_Price))
df_mean <- as.data.frame(df_mean)
df_brands <- merge(df_brands, df_mean, by="Brand", all=TRUE)


#barplot with the mean "Price_in_Thousands" for each "Brand"
price_mean <- mean(df_brands$Mean_Price, na.rm=TRUE)
p <- ggplot(df_mean, aes(x=reorder(Brand, Mean_Price), y=Mean_Price))+
     geom_bar(stat="identity", fill="lightblue")+
     theme(axis.text.x=element_text(angle=45, hjust=1))+
     geom_hline(yintercept=price_mean, linetype="dashed", color="black")
p


#barplot with the average Years_Launch per Brand
df_mean <- df %>% group_by(Brand) %>% summarise(mean_years=mean(Years_Launch, na.rm=TRUE))
overall_mean <- mean(df$Years_Launch, na.rm=TRUE)
p <- ggplot(df_mean, aes(x=reorder(Brand, mean_years), y=mean_years))+
     geom_bar(stat="identity", fill="lightblue")+
     labs(y="Mean Years", x="Brand")+
     theme(axis.text.x=element_text(angle=45, hjust=1))+
     geom_hline(yintercept=overall_mean, linetype="dashed", color="black")
p


#boxplot of "Brand" in relation to "Sales_in_Thousands"
bb1 <- ggplot(df, aes(y=Brand, x=Sales_in_Thousands))+
       geom_boxplot(fill="lightblue", color="black")+
       labs(x="Sales_in_Thousands", y="Brand")
bb1
#boxplot of "Brand" in relation to "Resale_Value"
bb2 <- ggplot(df, aes(y=Brand, x=Resale_Value))+
       geom_boxplot(fill="lightblue", color="black")+
       labs(x="Resale_Value", y="Brand")
bb2
#boxplot of "Brand" in relation to "Price_in_Thousands"
bb3 <- ggplot(df, aes(y=Brand, x=Price_in_Thousands))+
       geom_boxplot(fill="lightblue", color="black")+
       labs(x="Price_in_Thousands", y="Brand")
bb3
#boxplot of "Brand" in relation to "Engine_Size"
bb4 <- ggplot(df, aes(y=Brand, x=Engine_Size))+
       geom_boxplot(fill="lightblue", color="black")+
       labs(x="Engine_Size", y="Brand")
bb4
#boxplot of "Brand" in relation to "Horsepower"
bb5 <- ggplot(df, aes(y=Brand, x=Horsepower))+
       geom_boxplot(fill="lightblue", color="black")+
       labs(x="Horsepower", y="Brand")
bb5
#boxplot of "Brand" in relation to "Wheelbase"
bb6 <- ggplot(df, aes(y=Brand, x=Wheelbase))+
       geom_boxplot(fill="lightblue", color="black")+
       labs(x="Wheelbase", y="Brand")
bb6
#boxplot of "Brand" in relation to "Width"
bb7 <- ggplot(df, aes(y=Brand, x=Width))+
       geom_boxplot(fill="lightblue", color="black")+
       labs(x="Width", y="Brand")
bb7
#boxplot of "Brand" in relation to "Length"
bb8 <- ggplot(df, aes(y=Brand, x=Length))+
       geom_boxplot(fill="lightblue", color="black")+
       labs(x="Length", y="Brand")
bb8
#boxplot of "Brand" in relation to "Curb_Weight"
bb9 <- ggplot(df, aes(y=Brand, x=Curb_Weight))+
       geom_boxplot(fill="lightblue", color="black")+
       labs(x="Curb_Weight", y="Brand")
bb9
#boxplot of "Brand" in relation to "Fuel_Capacity"
bb10 <- ggplot(df, aes(y=Brand, x=Fuel_Capacity))+
        geom_boxplot(fill="lightblue", color="black")+
        labs(x="Fuel_Capacity", y="Brand")
bb10
#boxplot of "Brand" in relation to "Fuel_Efficiency"
bb11 <- ggplot(df, aes(y=Brand, x=Fuel_Efficiency))+
        geom_boxplot(fill="lightblue", color="black")+
        labs(x="Fuel_Efficiency", y="Brand")
bb11
#boxplot of "Brand" in relation to "Power_Performance_Factor"
bb12 <- ggplot(df, aes(y=Brand, x=Power_Performance_Factor))+
        geom_boxplot(fill="lightblue", color="black")+
        labs(x="Power_Performance_Factor", y="Brand")
bb12


#PART 4.2: NUMERICAL x NUMERICAL VARIABLES--------------------------------------------------------------


#scatterplot between "Resale_Value" and "Price_in_Thousands"
x_threshold <- mean(df$Price_in_Thousands)
y_threshold <- mean(df$Resale_Value)
df$ROI <- factor(case_when(
  df$Price_in_Thousands<=x_threshold & df$Resale_Value<=y_threshold~"Yellow",
  df$Price_in_Thousands>x_threshold & df$Resale_Value<=y_threshold~"Red",
  df$Price_in_Thousands<=x_threshold & df$Resale_Value>y_threshold~"Green",
  df$Price_in_Thousands>x_threshold & df$Resale_Value>y_threshold~"Yellow"
))
p <- ggplot(df, aes(x=Price_in_Thousands, y=Resale_Value, color=ROI)) +
  geom_point()+
  labs(x="Price_in_Thousands", y="Resale_Value") +
  theme_minimal()+
  geom_hline(yintercept=mean_resale, linetype="dashed", color="black")+
  geom_vline(xintercept=mean_price, linetype="dashed", color="black")+
  scale_color_manual(values=c("green3", "red2", "yellow")) 
p
#better models in terms of ROI
green <- subset(df[df$ROI=="Green", ], select="Brand_Model")
green
df <- subset(df, select=-ROI)


#correlation matrix
corr_mat <- cor(df_numeric)
corr_mat
p <- corrplot(corr_mat, type="lower", tl.pos="lt", tl.cex=0.8, diag=FALSE, tl.col="black")
#high correlations (>0.7):
#Sales_in_Thousands: None
#Resale_Value: Price_in_Thousands + Horsepower + Power_Performance_Factor
#Type: None
#Price_in_Thousands: Horsepower + Power_Performance_Factor
#Engine_Size: Horsepower + Width + Curb_Weight + Fuel_Efficiency + Power_Performance_Factor
#Horsepower: Power_Performance_Factor
#Wheelbase: Width + Length
#Width: Length + Curb_Weight
#Length: None
#Curb_Weight: Fuel_Capacity + Fuel_Efficiency 
#Fuel_Capacity: Fuel_Efficiency 
#Fuel_Efficiency: None
#Power_Performance_Factor: None


#correlation matrix for df_brands
df_numeric_brands <- df_brands %>% select_if(is.numeric)
corr_mat_brands <- cor(df_numeric_brands)
corr_mat_brands 
p <- corrplot(corr_mat_brands, type="lower", tl.pos="lt", tl.cex=0.8, diag=FALSE, tl.col="black")  


#------------------------------------------------------------------------------------------------------#

