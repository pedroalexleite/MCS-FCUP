library(tidyverse)

ec <- read_csv("echocardiogram.csv") 
ec
str(ec)

ec <- read_csv("echocardiogram.csv", na = "?") 
ec
str(ec)

#ec <- ec %>% select(-c(mult, name, group))
#ec <- select(ec, -(name))

summary(ec)

ec <- ec %>%
  mutate(survival = as.numeric(survival), still_alive = as.factor(still_alive),
         pericardial_effusion = as.factor(pericardial_effusion), alive_at = as.factor(alive_at)) summary(ec)

data <- load("carIns_noNAs.Rdata")

carIns_noNAs %>% group_by(bodyStyle) %>% count()

carIns_noNAs %>% group_by(bodyStyle, fuelType) %>% count()

carIns_noNAs %>%
  group_by(bodyStyle) %>%
  summarize(cityMpg.mean = mean(cityMpg), cityMpg.sd = sd(cityMpg)) %>% arrange(cityMpg.mean)

library(ggplot2)

ggplot(carIns_noNAs, aes(x = cityMpg, y = highwayMpg)) + geom_point() + ggtitle("Relationship between cityMpg and highwayMpg")
#aes -> nome dos eixos
#geom_point -> marca os pontos
#ggtitle -> título do plot

ggplot(carIns_noNAs, aes(x = bodyStyle)) + geom_bar() + ggtitle("Distribution of cars across bodyStyle")
#geom_bar -> marca as barras

ggplot(carIns_noNAs, aes(x = price)) + geom_histogram(binwidth = 5000) + ggtitle("Histogram of price")
#geom_histogram -> marca as barras do histograma com a dada largura

ggplot(carIns_noNAs, aes(x = price)) + geom_histogram(binwidth = 5000, aes(y = ..density..)) + geom_density(color = "blue") + geom_rug() + ggtitle("Histogram of price")

ggplot(carIns_noNAs, aes(x = make, y = price)) + geom_boxplot() + coord_flip()

ggplot(carIns_noNAs, aes(x = price)) + geom_histogram(binwidth = 5000) + facet_wrap(~nDoors) + ggtitle("Histogram of price by nDoors")

ggplot(carIns_noNAs, aes(x = price)) + geom_histogram(binwidth = 5000) + facet_grid(fuelType ~ aspiration) + ggtitle("Histogram of price by aspiration and fuel type")

ggplot(carIns_noNAs, aes(x = price)) + geom_histogram(binwidth = 5000) + facet_grid(fuelType ~ aspiration, scales = "free_y") + ggtitle("Histogram of price by aspiration and fuel type")
