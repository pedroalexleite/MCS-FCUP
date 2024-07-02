library(arules)
library(arulesViz)
library(tidyverse)
library(dplyr)


#EX1
data("Groceries")
Groceries

class(Groceries)
summary(Groceries)
size(Groceries)
inspect(Groceries[1:5])
unique(Groceries)
duplicated(Groceries)

itemFrequency(Groceries)
itemFrequencyPlot(Groceries, topN=5)
itemFrequencyPlot(Groceries, support=0.1)
apriori(Groceries)

fsets <- apriori(Groceries, parameter=list(supp=0.01, target="frequent itemsets"))
class(fsets)
inspect(fsets)
length(fsets)
nitems(fsets)
labels(fsets)

rules <- apriori(Groceries, parameter=list(supp=0.01, conf=0.25))
summary(rules)
quality(rules)
plot(rules)
inspect(rules)
rules_subset <- subset(rules, lift>2)
inspect(rules_subset)
selected_rules <- subset(rules, lift > 2 & (rhs %in% c("whole milk", "yogurt")))
inspect(sort(selected_rules, by="lift", decreasing=TRUE))


#--------------------------------------------------------------------------------------------------------#


#EX2
german_credit <- read_csv("german_credit.csv") 
german_credit <- german_credit[, -1]
german_credit <- german_credit %>% mutate_if(is.character, as.factor)
view(german_credit)

german_credit <- german_credit %>%
  discretizeDF(method=list(
    duration_in_month=list(method="interval", 4, labels=c("short", "med-short", "med-long", "long")),
    credit_amount=list(method="interval", 4, labels=c("small", "med-small", "med-high", "high")),
    age=list(method="interval", 4, labels=c("young adult", "adult", "senior", "golden")),
    default=list(method="interval")))


german_credit <- as(german_credit, "transactions")
itemDF <- itemInfo(german_credit)

rules <- apriori(german_credit)
plot(rules)
rules <- apriori(german_credit, conf=1)

myItems <- subset(itemDF, variables=="purpose")$labels
myItems <- subset(itemDF, variables %in% c("age", "personal_status_sex", "job", 
                                           "housing", "purpose"))$labels
rules <- apriori(german_credit, parameter=list(conf=0.6, minlen=2),
                               appearance=list(both=myItems, default="none"))
plot(rules, method="grouped")

