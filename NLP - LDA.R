Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_131')

library(lsa)
library(tm)
library(topicmodels)
library(openNLP)
library(openNLPdata)
library(NLP)
library(dplyr)
library(rJava)
library(RWeka)
library(RWekajars)
library(tm)
library(qdap)
library(corrplot)


data<-read.csv(file.choose())
text<-as.character(data$text)

# Data Cleaning
corpus1<-Corpus(VectorSource(text)) 

corpus1 <- tm_map(corpus1, removeNumbers)
corpus1 <- tm_map(corpus1, removePunctuation)
corpus1 <- tm_map(corpus1 , stripWhitespace)
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, removeWords, c(stopwords("english"),"the","am","gopdeb","gop"))
corpus1 <- tm_map(corpus1, stemDocument, language = "english")

unigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 3))
dtm<-DocumentTermMatrix(corpus1,control = list(tokenize=unigramTokenizer, 
                                               stopwords = c(stopwords("english"),"the","am","gopdeb","gop")))
dtm_keys<-inspect(dtm)

dtm_key1<-removeSparseTerms(dtm,sparse=0.9999)
inspect(dtm_key1)
tdm<-TermDocumentMatrix(corpus1,control = list(tokenize=unigramTokenizer, 
                                               stopwords = c(stopwords("english"),"the","am","gopdeb","gop")))
tdm_keys<-inspect(tdm)


terms1<-findFreqTerms(dtm,lowfreq=693)

associations<-findAssocs(dtm,"good",0.10)

freqr <- colSums(as.matrix(dtm))
ordr <- order(freqr,decreasing=TRUE)

freqr[head(ordr,20)]

freqr[tail(ordr,200)]


library(wordcloud)
wordcloud(names(freqr),freqr, min.freq=450,colors=brewer.pal(7,"Dark2"))


positives<-c("good","great","yoyo")
negatives<-c("nope","oops")

sentence<-c("This is great achievement","oops this is not good")

matches<-term_match(sentence,c("goodone","oops"))

score<-0

if(length(matches$good)>0){
  score<-1
}else if(length(matches$oops)>0)
{
  score<- -2
}


# Topic Modelling


dt_matrix1 <- removeSparseTerms(dtm, 0.98)
rowTotals <- apply(dt_matrix1 , 1, sum) 
dtm.new   <- dt_matrix1[rowTotals> 0, ]

lda <- LDA(dtm.new, k =5)

terms(lda)
topics(lda)

ctm<-CTM(dtm.new, k =5)
terms(ctm)
topics(ctm)
