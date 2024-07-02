library(tm)
library(text2vec)
library(wordcloud)
library(SnowballC)
library(dplyr)
library(tidyr)
library(ggplot2)


#EX4
data(acq)
inspect(head(acq))

cleanup <- function(docs, spec.words=NULL){
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeNumbers)
  docs <- tm_map(docs, removeWords, stopwords("english"))
  if(!is.null(spec.words))
    docs <- tm_map(docs, removeWords, spec.words)
  docs <- tm_map(docs, removePunctuation)
  docs <- tm_map(docs, stripWhitespace)
  docs <- tm_map(docs, stemDocument)
  return(docs)
}

par(mfrow=c(1,2))
wordcloud(acq)

acq1 <- cleanup(acq, spec.words = c("said", "the", "and"))
wordcloud(acq1)

mdf <- as_tibble(as.matrix(dtm))

mdf.freq <- mdf %>%
            select(findFreqTerms(tm, nDocs(dtm)/2)) %>%
            summarise_all(sum) %>%
            gather() %>%
            arrange(desc(value))

distM <- dist(as.matrix(dtm))
tree <- hclust(distM)
plot(tree)

h <- hclust(distM, method="ward.D")
plot(h)

clustKey <- cutree(h, 3)
clustKey
rect.hclust(h, 3)

c1 <- dtm[clustKey==1,]
c2 <- dtm[clustKey==2,]
c3 <- dtm[clustKey==3,]

plot.wordcloud <- function(dtmc) {
  mddf.c <- a.tibble(as.matrix(dtimc)) %>%
    summarize_all(sum) %>%
    gather() %>%
    arrange(ddesc(value))
    wordcloud(mdf.c$key, mdf.c$value, min.freq=5)
}

par(mfrow=c(1,3))
plot.wordcloud(c1)
plot.wordcloud(c2)
plot.wordcloud(c3)
par(mfrow=c(1,1))



#--------------------------------------------------------------------------------------------------------#


#EX5



            