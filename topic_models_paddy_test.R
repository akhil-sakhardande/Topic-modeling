# Code to perform topic modeling on paddy dataset

# Required libraries

library(tm)
library(qdap)

getwd()
setwd("/home/asakhardhande19/rallis_files/")

# Fetching sheet names from the excel

library(gdata)
Sheets <- sheetNames("FGD- Paddy sheets.xlsx")
class(Sheets)
Sheets <- as.list(Sheets)
Sheets[1]
class(as.character(Sheets[1]))
as.character(Sheets[1])

# Function for making corpus and cleaning the responses fetched

mcr <- function(text){ 
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

# Reading excel data and extracting the relevant fields

require(xlsx)

  excel <- read.xlsx("FGD- Paddy sheets.xlsx", sheetIndex = 17, rowIndex = 12:54, colIndex = 2:3)
  class(excel)
  head(excel,3)
  dim(excel)
  
  # Collating the response data together
  
  names(excel)
  names(excel) <- c("Question","Response")
  excel$Response
  #aggregate(excel[,2] ~ ., data= excel, paste, sep = " ")
  excel_res <- paste(excel[,2], collapse = " ")
  excel_res
  class(excel_res) # Character class
  
  # Preparing text for topic modeling exercise
  
  # remove special characters
  excel_res <- gsub("@\\w+", "", excel_res)
  # remove punctuation
  excel_res <- gsub("[[:punct:]]", "", excel_res)
  # remove numbers
  excel_res <- gsub("[[:digit:]]", "", excel_res)
  # remove html links
  excel_res <- gsub("http\\w+", "", excel_res)
  # remove unnecessary spaces
  excel_res <- gsub("[ \t]{2,}", "", excel_res)
  excel_res <- gsub("^\\s+|\\s+$", "", excel_res)
  
  # remove NAs in some_txt
  excel_res <- excel_res[!is.na(excel_res)]
  names(excel_res) <- NULL
  
  excel_res <- tolower(excel_res)
  excel_res
  
  # Topic modeling
  
  excel_res  <- gsub("na","", excel_res)
  corpus<-mcr(excel_res)
  
  library(RWeka)
  BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
  dtm <- DocumentTermMatrix(corpus, control = list(tokenize=BigramTokenizer, weighting=weightTf))
  #dtm <- DocumentTermMatrix(corpus, control = list(weighting=weightTf))
  #View(as.matrix(dtm))
  rownames(dtm)<-c(1:length(excel_res))
  rowTotals <- apply(dtm , 1, sum)
  dtm   <- dtm[rowTotals> 0, ]
  
  doc.lengths <- rowSums(as.matrix(DocumentTermMatrix(corpus)))
  
  # LDA hyper-parameters
  
  burnin <- 4000
  iter <- 2000
  thin <- 500
  seed <-list(2003,5,63,100001,765)
  nstart <- 5
  best <- TRUE
  
  # Number of topics
  k <- 5
  dtm
  
  # Run LDA using Gibbs sampling
  library(topicmodels)
  ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
  
  excel_res1 <- as.data.frame(excel_res)
  topics(ldaOut)
  #for(i in 1:nrow(excel_res1)){
  #  excel_res1[i,2]<-terms(ldaOut)[topics(ldaOut)[i]]
  #}
  #excel_res1[i,2]<-terms(ldaOut)
  as.data.frame(terms(ldaOut))
  t(terms(ldaOut))
  excel_res1 <- cbind(excel_res1,as.data.frame(t(terms(ldaOut))))
  #View(excel_res1)
  names(excel_res1)
  names(excel_res1)[1] <- "Topic.Text"
  
  # Adding sheet name to the output file
  excel_res1$Region <- as.character(Sheets[17])
  
  # Writing the results to a file
  
  #write.table(excel_res1,file = "/home/asakhardhande19/rallis_files/final_paddy_topics.csv", sep = ",", row.names=FALSE, append=TRUE, col.names = TRUE)
  write.table(excel_res1,file = "/home/asakhardhande19/rallis_files/test_final_paddy_topics.csv", sep = ",", row.names=FALSE, append=TRUE, col.names = FALSE)
