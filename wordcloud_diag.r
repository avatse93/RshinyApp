wordcloud_diag<-function(data_df){
require(SnowballC)
library("tm")
library("sentimentr")
library("RSentiment")
library("wordcloud")
library(RColorBrewer)
text <- as.character(data_df$text)

sample<-sample(text,(length(text)))
corpus=Corpus(VectorSource(list(sample)))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus, stemDocument)
dtm_up<-DocumentTermMatrix(VCorpus(VectorSource(corpus[[1]]$content)))

require(RSentiment)

freq_up <- colSums(as.matrix(dtm_up))

sentiments_up <- calculate_sentiment(names(freq_up))

sentiments_up<- cbind(sentiments_up,as.data.frame(freq_up))
head(sentiments_up)

sent_pos_up <- sentiments_up[sentiments_up$sentiment=='Positive',]
sent_neg_up <- sentiments_up[sentiments_up$sentiment=='Negative',]

cat("Negative Sentiments: ",sum(sent_neg_up$freq_up),"Positive Sentiments: ",sum(sent_pos_up$freq_up))

wordcloud_rep <- repeatable(wordcloud)
wordcloud_rep(sent_neg_up$text,sent_neg_up$freq_up, scale=c(6,1),min.freq = 5,random.order = FALSE,colors = brewer.pal(6,"Dark2"))
}