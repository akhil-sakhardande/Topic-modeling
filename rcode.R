library(tm)
library(wordcloud)
library(ggplot2)
library(fpc)
library(qdap)
library(igraph)
library(ngram)
library(RWeka)
list.files()
Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jre1.8.0_66")
#tweets.df <- parseTweets("SoniaGandhiTweets.txt", simplify = TRUE)

#View(tweets.df)
#jaya<-tweets.df
jaya<-do.call(rbind,strsplit(readLines('SoniaGandhiTweets.txt'),'###',fixed=T))
jaya<-as.data.frame(jaya)
names(jaya)<-c("time","twitter_handle","location","text")
#write.csv(lalu,"lalu_tweets.csv")

jaya2<-rev

makeCorpus <- function(text){ #Function for making corpus and cleaning the tweets fetched
  #twitterdf <- do.call("rbind", lapply(text, as.data.frame)) #store the fetched tweets as a data frame
  twitterdf<- as.data.frame(text)
  twitterdf$text <- sapply(twitterdf$text,function(row) iconv(row, "latin1", "ASCII", sub=""))#Removing emoticons from tweets
  twitterCorpus <- Corpus(VectorSource(twitterdf$text)) #Creating Corpus
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x)) #function to replace a pattern to white space using regex
  twitterCorpus <- tm_map(twitterCorpus, toSpace, "(RT|via)((?:\\b\\W*@\\w+)+)") #match rt or via
  twitterCorpus <- tm_map(twitterCorpus, toSpace, "@\\w+") #match @
  twitterCorpus <- tm_map(twitterCorpus, toSpace, "[ \t]{2,}") #match tabs
  twitterCorpus <- tm_map(twitterCorpus, toSpace, "[ |\n]{1,}") #match new lines
  twitterCorpus <- tm_map(twitterCorpus, toSpace, "^ ") #match white space at begenning
  twitterCorpus <- tm_map(twitterCorpus, toSpace, " $") #match white space at the end
  twitterCorpus <- tm_map(twitterCorpus, PlainTextDocument)
  twitterCorpus <- tm_map(twitterCorpus, removeNumbers)
  twitterCorpus <- tm_map(twitterCorpus, removePunctuation)
  twitterCorpus <- tm_map(twitterCorpus, toSpace, "http[[:alnum:]]*") #remove url from tweets
  twitterCorpus <- tm_map(twitterCorpus,removeWords,stopwords("en"))
  twitterCorpus <- tm_map(twitterCorpus, content_transformer(tolower))
  twitterCorpus <- tm_map(twitterCorpus,removeWords,c("review","helpful"))
  return(twitterCorpus)
}

#corp<- makeCorpus(jaya)

#Wordcloud
makeWordcloud<-function(getText){ #plotting wordcloud
  twicorpus<-makeCorpus(getText)
  myTdm<-TermDocumentMatrix(twicorpus, control=list(wordLengths=c(4,Inf))) #Create TDM
  matrix<-as.matrix(myTdm)
  wordFreq <- sort(rowSums(matrix), decreasing=TRUE)#find frequency of words and sorting them in descending
  set.seed(375) 
  #grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )
  wordcloud(words=names(wordFreq), freq=wordFreq,scale=c(8, .2),max.words = 150,rot.per = .15, min.freq=3, random.order=F,colors=brewer.pal(8,"Dark2"))
}

makeWordcloud(jaya2)


jaya$created_at<-gsub("+0000","",jaya$created_at)
date<-as.Date(jaya$created_at,format="%a %b %d %H:%M:%S + %Y")
jaya<-cbind(jaya,date)
jaya3<-jaya
jaya3<-jaya3[order(jaya$date),]
View(jaya3)


#Sentiment Analysis


tSentimen<-function (content){
  twicorpus<-makeCorpus(content)
  dataframe<-data.frame(text=unlist(sapply(twicorpus, `[`, "content")), stringsAsFactors=F) # storing corpus as data frame
  (poldat <- with(dataframe, polarity(text))) #getting polarity of the tweets
  return(poldat)
}
pol<-tSentimen(jaya2)
View(pol)
pol2<-data.frame(pol)
pol2 <- data.frame(lapply(pol2, as.character), stringsAsFactors=FALSE)
write.csv(pol2,"review_polarity.csv")
View(jaya)

plot(pol,bar.size = 10,order.by.polarity = TRUE,point.size = 10)


write.csv(pol2,"polarity_jaya2.csv")



jaya<-jaya[order(jaya$date),]
plot(table(jaya$date))


View(jaya)


#Topic Models


mcr <- function(text){ #Function for making corpus and cleaning the tweets fetched
  #twitterdf <- do.call("rbind", lapply(text, as.data.frame)) #store the fetched tweets as a data frame
  twitterdf<- as.data.frame(text)
  twitterdf$text <- sapply(twitterdf$text,function(row) iconv(row, "latin1", "ASCII", sub=""))#Removing emoticons from tweets
  twitterCorpus <- Corpus(VectorSource(twitterdf$text)) #Creating Corpus
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x)) #function to replace a pattern to white space using regex
  twitterCorpus <- tm_map(twitterCorpus, toSpace, "(RT|via)((?:\\b\\W*@\\w+)+)") #match rt or via
  twitterCorpus <- tm_map(twitterCorpus, toSpace, "@\\w+") #match @
  twitterCorpus <- tm_map(twitterCorpus, toSpace, "[ \t]{2,}") #match tabs
  twitterCorpus <- tm_map(twitterCorpus, toSpace, "[ |\n]{1,}") #match new lines
  twitterCorpus <- tm_map(twitterCorpus, toSpace, "^ ") #match white space at begenning
  twitterCorpus <- tm_map(twitterCorpus, toSpace, " $") #match white space at the end
  twitterCorpus <- tm_map(twitterCorpus, PlainTextDocument)
  twitterCorpus <- tm_map(twitterCorpus, removeNumbers)
  twitterCorpus <- tm_map(twitterCorpus, removePunctuation)
  twitterCorpus <- tm_map(twitterCorpus, toSpace, "http[[:alnum:]]*") #remove url from tweets
  twitterCorpus <- tm_map(twitterCorpus,removeWords,stopwords("en"))
  twitterCorpus <- tm_map(twitterCorpus, content_transformer(tolower))
  #twitterCorpus <- tm_map(twitterCorpus,removeWords,c("problem","report","days","xd","mobile","via","img","ago","one","a","helpful","review","reviews","reviewed"))
  return(twitterCorpus)
}

corpus<-mcr(jaya2)


BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
dtm <- DocumentTermMatrix(corpus, control = list(tokenize=BigramTokenizer,weighting=weightTf))
View(as.matrix(dtm))
rownames(dtm)<-c(1:length(jaya2))
rowTotals <- apply(dtm , 1, sum) 
dtm   <- dtm[rowTotals> 0, ]

doc.lengths <- rowSums(as.matrix(DocumentTermMatrix(corpus)))

burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

#Number of topics
k <- 15
#Run LDA using Gibbs sampling
library(topicmodels)
ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
#write out results
#docs to topics
jaya$topics<-NA

jaya3<-as.data.frame(jaya2)
topics(ldaOut)
for(i in 1:nrow(jaya3)){
  jaya3[i,2]<-terms(ldaOut)[topics(ldaOut)[i]]
  
}
View(jaya3)


write.csv(jaya,"sonia_final.csv")

## Donot Run this code 

for(i in 1:20){
  data.temp<-jaya[which(jaya$topics==topics[i]),]
  sentiment_topics<-tSentimen(data.temp)
  polarity.temp<-mean(sentiment_topics$all$polarity)
  sentiment_topics<-tSentimen(data.temp)
  store[i]<-polarity.temp
}


topicsentiment<-as.data.frame(topics)
topicsentiment<-topicsentiment[-21,]
topicsentiment<-as.data.frame(topicsentiment)
topicsentiment<-cbind(topicsentiment,store)

####


##Network Graphs
