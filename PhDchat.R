library('tm')
library('twitteR')
library('wordcloud')
library('RColorBrewer')
library('e1017')
library('ggplot2')
library('class')
library('topicmodels')
library('tidytext')
library('ggplot2')
library('dplyr')
library("syuzhet")



#twitter set up 
setup_twitter_oauth(ckey, skey, token, sectoken)



#what are the stats for phdchat
phdchat_tweets <- searchTwitter('#phdchat', n=2000, lang = 'en')



#here i pull 2000 sweets in english related to PhD 
acad <- searchTwitter('#phdchat', n=2000, lang = 'en')

#now pull the text
acad.text<-sapply(acad, function(x) x$getText())

#convert to txt
acad.text.1<-iconv(acad.text, 'UTF-8', 'ASCII')

# now to corpus
acad.corp<-Corpus((VectorSource(acad.text)))

#now we apply some tm functions. We remove all punctuation, numbers, lower and stop words. 
acad.mata<-TermDocumentMatrix(acad.corp, 
                             control = list(removePunctuation = TRUE, 
                                            removeNumbers = TRUE, 
                                            tolower = TRUE,
                                            stopwords=stopwords(kind = 'en')))
acad.mat<-as.matrix(acad.mata)



#what are the worse
word.freq<-sort(rowSums(acad.mat), decreasing = T)
word.freq.df<-data.frame(word = names(word.freq), freq = word.freq)

#some nice colours
pal2 <- brewer.pal(8,"Dark2")

wordcloud(word.freq.df$word, word.freq.df$freq, scale=c(8,.2),min.freq=15,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)




#what is the frequency of the words? 
wfd50<-word.freq.df[1:50,]
ggplot(wfd50, aes(x=word, y = freq)) + geom_bar(stat = "identity")  + coord_flip()


#find associations with words happy or sad! 
findAssocs(acad.mata, "sad", 0.4)

findAssocs(acad.mata, "happy", 0.4)

#sentiment analysis
sentiment<-get_nrc_sentiment(as.character(word.freq.df$word))
sentimentscores<-data.frame(colSums(sentiment[,]))
names(sentimentscores)<-"Score"
sentimentscores$sentiment<-NA
sentimentscores$sentiment<-rownames(sentimentscores)

#now we plot it
ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("#phdchat") + theme_minimal()


#topic modelling
dtm<-as.DocumentTermMatrix(acad.mata)
lda<-LDA(dtm, k = 4)
terms<-terms(lda, 10)
terms


