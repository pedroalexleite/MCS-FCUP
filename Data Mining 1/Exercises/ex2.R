library(tidyverse)

k <- 10
g <- k/2
x <- g * 2
x

euro2dollar <- function(p, tx) p * tx 
euro2dollar(3465, 1.36)

prices <- c(worten=32.4,fnac=35.4, mediaMkt=30.2,radioPop=35,pixmania=31.99)
vat <- 0.23
(1+vat)*prices


#hands on 1
countries <- c(portugal = 10.3, spain = 10.6, italy = 11.5, france = 12.3, germany = 9.9, uk = 11.4, finland = 10.9, belgium = 12.1, austria = 9.1)
vat <- 1.23
countries2 <- vat*countries
countries2

countries2[countries2 > 10]

countries2[countries2 > mean(countries2)]

countries2[countries2 > 10 & countries2 < 11]

countries3 <- countries2*1.1
countries3

countries2[countries2 > mean(countries2)] <- countries2[countries2>mean(countries2)]*0.975
countries2

#hands on 2
exchg <- c(usd=1.35402, gbp=0.82477, aud=1.54171, cad=1.48437,nzd=1.63934, jpy=141.155)
conv <- function(eur,curr) eur*exchg[curr] # depends on "exchg" conv(234,"jpy")
conv(234,"jpy")

prc <- matrix(c(32.40,35.40,30.20, 35.00, 31.99, 32.50, 34.60, 32.00, 34.40, 32.01),
              nrow=2,ncol=5,byrow=TRUE)
prc

prc <- matrix(c(32.40,35.40,30.20, 35.00, 31.99, 32.50, 34.60, 32.00, 34.40, 32.01),
              nrow=2,ncol=5)
prc

a <- array(1:18, dim = c(3, 2, 3))
a

stud <- data.frame(nrs=c("43534543","32456534"), names=c("Ana","John"),
                   grades=c(13.4,7.2))
stud

data(Boston,package="MASS")
Boston
subset(Boston,medv > 45)
