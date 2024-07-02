library(tm)
library(text2vec)
library(wordcloud)
library(SnowballC)



#EX1
#D1: Mining is important for finding gold
#D2: Classification and regression are data mining
#D3: Data mining deals with data
#Corpus: (T1) Mining (X) is (T2) important (...) for finding gold Classification and regression 
#are data deals (Tn) with (all of them have to be different)
#DocumentTermMatrix:
#     T1 T2 (...) Tn
# D1                 --> can be binary, frequency, tf-fdf (term frequency, invert document frequency), ...  
# D2
# D3

docs <- c("Mining is important for finding gold", 
          "Classification and regression are data mining", 
          "Data mining deals with data")

vc <- VectorSource(docs)
corpus <- Corpus(vc)
dtm <- DocumentTermMatrix(corpus)
nDocs(dtm)
nTerms(dtm)
Terms(dtm)
inspect(dtm)
as.matrix(dtm)

dtm.tf <- dtm
inspect(dtm.tf)
dtm.bin <- weightBin(dtm)
inspect(dtm.bin)
dtm.tf2 <- weightTf(dtm)
inspect(dtm.tf2)
dtm.tfidf <- weightTfIdf(dtm)
inspect(dtm.tfidf)
as.matrix(dtm.tfidf)

sim2(as.matrix(dtm.bin), method="cosine")
sim2(as.matrix(dtm.tf), method="cosine")
sim2(as.matrix(dtm.tfidf), method="cosine")


#EX2
cq <- Corpus(VectorSource("data mining"))
dtmq <- DocumentTermMatrix(cq)
mq <- matrix(0, ncol=nTerms(dtm.tf), dimnames=list("q", Terms(dtm.tf)))
mq[1, Terms(dtmq)] <-1

sim2(as.matrix(dtm.bin), mq, method="cosine")
sim2(as.matrix(dtm.tf), mq, method="cosine")
sim2(as.matrix(dtm.tfidf), mq, method="cosine")


#--------------------------------------------------------------------------------------------------------#


vector <- c("run", "ran", "running", "stayed", "unconsciously", "betrayed")              
lemmatize_words(vector)
stem_words(vector)


#EX1
reut <- system.file("texts", "crude", package="tm")
reuters <- VCorpus(DirSource(reut), readerControl=list(reader=readReut21578XMLasPlain))
r0 <- reuters

inspect(reuters[[1]])
meta(reuters[[1]])

wordcloud(reuters, colors=rainbow(20))

reuters <- tm_map(reuters, stripWhitespace)
reuters <- tm_map(reuters, content_transformer(tolower))
reuters <- tm_map(reuters, removeswords, stopwords("english"))
reuters <- tm_map(reuters, stemDocument)
reuters <- tm_map(reuters, removePunctuation, 
                  preserve_intra_word_contractions=TRUE,
                  preserve_intra_word_dashes=TRUE)

inspect(reuters[[1]])
wordcloud(reuters, colors=rainbow(20))
wordcloud(reuters[[2]], colors=rainbow(20))

r1 <- tm_map(r0, stemDocument)
wordcloud(r1, colors=rainbow(20))
r2 <- tm_map(r0, lemmatize_words)
wordcloud(r2, colors=rainbow(20))

dtm <- DocumentTermMatrix(reuters)
inspect(dtm[5:10, 740:743])
findFreqTerms(dtm, 10)

findAssocs(dtm, "opec", 0.8)




  