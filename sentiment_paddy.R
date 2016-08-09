# Code to perform opinion mining for paddy crops

library(tm)
#install.packages("qdap")
library(qdap)

getwd()
setwd("/home/asakhardhande19/rallis_files/")

# Defining functions at first

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

tSentimen<-function (content){
  twicorpus<- makeCorpus(content)
  dataframe<-data.frame(text=unlist(sapply(twicorpus, `[`, "content")), stringsAsFactors=F) # storing corpus as data frame
  (poldat <- with(dataframe, polarity(text))) #getting polarity of the tweets
  return(poldat)
}


# Fetching sheet names from the excel

library(gdata)
Sheets <- sheetNames("FGD- Paddy sheets.xlsx")
class(Sheets)
Sheets <- as.list(Sheets)
Sheets[1]
class(as.character(Sheets[1]))
as.character(Sheets[1])

# Reading excel data and extracting the relevant fields

require(xlsx)

# creating a looping construct to loop through each sheet

for (each in 1:length(Sheets)) 
  {
#excel <- read.xlsx("FGD- Paddy sheets.xlsx", sheetIndex= 1)
excel <- read.xlsx("FGD- Paddy sheets.xlsx", sheetIndex = each, rowIndex = 2:11, colIndex = 2:3)
class(excel)
head(excel,3)
names(excel)
dim(excel)

# Collating the response data together

names(excel)
#names(excel) <- c("Question","Response")
excel$Response
#aggregate(excel[,2] ~ ., data= excel, paste, sep = " ")
excel_res <- paste(excel[,2], collapse = " ")
excel_res
class(excel_res) # Character class

# Preparing text for sentiment analysis

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

# Perform Sentiment Analysis

pol<-tSentimen(excel_res)
#View(pol)
pol2<-data.frame(pol)
pol2 <- data.frame(lapply(pol2, as.character), stringsAsFactors=FALSE)
#write.csv(pol2,"/home/asakhardhande19/rallis_files/paddy_opinions3.csv", row.names = FALSE)
#View(excel_res)
#View(pol2)

# Organizing the output file with required fields

class(pol2)
out_pol2 <- NULL
out_pol2$Polarity.Score <- pol2$all.polarity
out_pol2$Response.Text  <- gsub("na","", excel_res)
#out_pol2$Response.Text <- excel_res
#out_pol2$OPINION <- ifelse(pol2$all.polarity < 0, "Negative","Positive")
out_pol2$Opinion <- ifelse(pol2$all.polarity <= 0, ifelse(pol2$all.polarity == 0,"Neutral","Negative"),"Positive")
out_pol2$Region <- as.character(Sheets[each])

# Writing to a file
#View(out_pol2)
#ifelse(each == 1, write.table(out_pol2,file = "/home/asakhardhande19/rallis_files/final_paddy_opinions.csv", sep = ",", row.names=FALSE, append=TRUE), write.table(out_pol2,file = "/home/asakhardhande19/rallis_files/final_paddy_opinions.csv", sep = ",", row.names=FALSE, append=TRUE, col.names = FALSE))
write.table(out_pol2,file = "/home/asakhardhande19/rallis_files/final_paddy_opinions.csv", sep = ",", row.names=FALSE, append=TRUE, col.names = ifelse(each %in% 1, TRUE, FALSE))
#write.csv(pol2,"/home/asakhardhande19/rallis_files/paddy_opinions.csv", row.names=FALSE, append=TRUE)
}
