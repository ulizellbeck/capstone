setwd("/media/shared/data_scientist/coursera/capstone/final/")

## library(tm)
## library(tau)
library(RWeka)
## library(SnowballC)
library(ggplot2)
library(stringr)

## Load data files
con_news <- file("en_US/en_US.news.txt", "rb",encoding="UTF-8")
con_twitter <- file("en_US/en_US.twitter.txt", "rb",encoding="UTF-8")
con_blogs <- file("en_US/en_US.blogs.txt", "rb",encoding="UTF-8")

## Read files
twitter <- readLines(con_twitter, warn = FALSE)
blogs <- readLines(con_blogs)
news <- readLines(con_news)

## close connection
close(con_twitter)
close(con_blogs)
close(con_news)

## Number of rows for each file 
news.length <- length(news)
blogs.length <- length(blogs)
twitter.length <- length(twitter)

## Number of characters for each file
news.characters <- sum(nchar(news))
twitter.characters <- sum(nchar(twitter))
blogs.characters <- sum(nchar(blogs))

cleanData <- function(data) {
  library(tm)
  data <- tolower(data) # convert to lowercase
  data <- removeNumbers(data) # remove numbers
  pontuacao <- '[.,!:;?]|:-\\)|:-\\(|:\\)|:\\(|:D|=D|8\\)|:\\*|=\\*|:x|:X|:o|:O|:~\\(|T\\.T|Y\\.Y|S2|<3|:B|=B|=3|:3'
  data <- gsub(pontuacao," END ",data) # substitute selected ponctuation (including smileys) with the word END
  data <- gsub("$"," END",data) # make sure every line ends with an END
  data <- gsub("\\b(\\w+)\\s+\\1\\b","\\1",data) # remove duplicate words in sequence (eg. that that)
  data <- gsub("\\b(\\w+)\\s+\\1\\b","\\1",data) # remove duplicate words in sequence (eg. that that)
  data <- gsub("\\b(\\w+)\\s+\\1\\b","\\1",data) # remove duplicate words in sequence (eg. that that)
  data <- removePunctuation(data) # remove all other punctuation
  data <- stripWhitespace(data) # remove excess white space
  data <- gsub("^[[:space:]]","",data) # make sure lines doesn't begin with space
  data <- gsub("[[:space:]]$","",data) # make sure lines doesn't end with space
}
blogsUS <- cleanData(blogs)
save(file="blogsUS-clean.rdata",blogsUS)
newsUS <- cleanData(news)
save(file="newsUS-clean.rdata",newsUS)
twitterUS <- cleanData(twitter)
save(file="twitterUS-clean.rdata",twitterUS)

blogsUS <- unlist(str_split(blogsUS,"\\W+"))
newsUS <- unlist(str_split(newsUS,"\\W+"))
twitterUS <- unlist(str_split(twitterUS,"\\W+"))
save(file="blogsUS-words.rdata",blogsUS)
save(file="newsUS-words.rdata",newsUS)
save(file="twitterUS-words.rdata",twitterUS)

rm(twitterUS)

load("blogsUS-clean.rdata")
load("blogsUS-clean.rdata")
load("newsUS-clean.rdata")

blogsUS <- unlist(str_split(blogsUS,"END"))

blogsUS <- blogsUS[which(blogsUS!="")] ## remove empty rows

tokens_1 <- NGramTokenizer(blogsUS[1:100000],Weka_control(min = 2, max = 3, delimiters = " "))

length(blogsUS)/100000


tokens <- sapply(blogsUS[1:500000],NGramTokenizer,Weka_control(min = 2, max = 3, delimiters = " "))

head(tokens)


for i in ()

## Tokenization

twitter.text <- gsub("(\\.|\\,|\\?|\\!|\\;|\\&|\\(|\\)|\\:|[\"]|\\-\\-|\\=| \\- |\\#| \\'|\\' )","{\\1}",twitter)
news.text <- gsub("(\\.|\\,|\\?|\\!|\\;|\\&|\\(|\\)|\\:|[\"]|\\-\\-|\\=| \\- |\\#| \\'|\\' )","{\\1}",news)
blogs.text <- gsub("(\\.|\\,|\\?|\\!|\\;|\\&|\\(|\\)|\\:|[\"]|\\-\\-|\\=| \\- |\\#| \\'|\\' )","{\\1}",blogs)

## Free memory!
rm(news)
rm(blogs)
rm(twitter)
gc(verbose = FALSE)

## Data cleaning
twitter.text <- iconv(twitter.text, to='ASCII//TRANSLIT')
twitter.text <- tolower(twitter.text)
news.text <- tolower(news.text)
blogs.text <- tolower(blogs.text)


## For this analysis delete punctuation, but for the end product it might be reasonable to keep them.
## ToDo: Parallelize for speed up process
twitter.tokens <- strsplit(twitter.text,"\\{.?}| ")
twitter.tokens <- unlist(twitter.tokens)
news.tokens <- strsplit(news.text,"\\{.?}| ")
news.tokens <- unlist(news.tokens)
blogs.tokens <- strsplit(blogs.text,"\\{.?}| ")
blogs.tokens <- unlist(blogs.tokens)

twitter.words <- length(twitter.tokens)
news.words <- length(news.tokens)
blogs.words <- length(blogs.tokens)

## free memory
rm(news.text)
rm(blogs.text)
rm(twitter.text)
gc()

## Create frequency tables for tokens (how often do they occur)
twitter.tokens.frequency <- sort(table(twitter.tokens), decreasing=TRUE)
news.tokens.frequency <- sort(table(news.tokens), decreasing=TRUE)
blogs.tokens.frequency <- sort(table(blogs.tokens), decreasing=TRUE)

## How many tokens are there for each file?
twitter.tokens.length <- length(twitter.tokens.frequency)
news.tokens.length <- length(news.tokens.frequency)
blogs.tokens.length <- length(blogs.tokens.frequency)

## create dataframe for graphics
tt <- data.frame(name = c("twitter","blogs","news"), 
                 length= c(twitter.length,blogs.length,news.length),
                 tokens = c(twitter.tokens.length, blogs.tokens.length, news.tokens.length),
                 words = c(twitter.words, blogs.words, news.words),
                 characters = c(twitter.characters, blogs.characters, news.characters))

## plot histograms for analysis
qplot(name,weight = length, data= tt,xlab = c("data set"), ylab=c("# lines"), geom = "bar")
qplot(name,weight = words/1000000, data= tt,xlab = c("data set"), ylab=c("# words (in billion)"), geom = "bar")
qplot(name,weight = tokens/10000, data= tt,xlab = c("data set"), ylab=c("# different tokens (in ten thousand)"), geom = "bar")

## Create a nice histogram for the most occuring words  

twitter.word.frequency <- data.frame(word=names(twitter.tokens.frequency), freq=twitter.tokens.frequency)
## the first row is just whitespace so delete it
twitter.word.frequency <- twitter.word.frequency[-1,]

pl <- ggplot(subset(twitter.word.frequency, freq > 300000) ,aes(word, freq))
pl <- pl + geom_bar(stat="identity", fill="red")
pl + theme(axis.text.x=element_text(angle=90))

## and for news
news.word.frequency <- data.frame(word=names(news.tokens.frequency), freq=news.tokens.frequency)
news.word.frequency <- news.word.frequency[-1,]
pl <- ggplot(subset(news.word.frequency, freq > 300000) ,aes(word, freq))
pl <- pl + geom_bar(stat="identity", fill="red")
pl + theme(axis.text.x=element_text(angle=90))

## and for blogs
blogs.word.frequency <- data.frame(word=names(blogs.tokens.frequency), freq=blogs.tokens.frequency)
blogs.word.frequency <- blogs.word.frequency[-1,]

pl <- ggplot(subset(blogs.word.frequency, freq > 300000) ,aes(word, freq))
pl <- pl + geom_bar(stat="identity", fill="red")
pl + theme(axis.text.x=element_text(angle=90))

qplot(name, weight = tt$characters/tt$words, data = tt)

?ggplot




text <- paste(twitter,blogs,news)

## unload data to free memory
twitter <- 0
blogs <- 0
news <- 0


##
gc()

## Clean Data

## Replace accented characters
text <- iconv(text, to='ASCII//TRANSLIT')
text <- tolower(text)

## Tokenize ToDo: Parallelize for speed up process 
text <- gsub("(\\.|\\,|\\?|\\!|\\;|\\&|\\(|\\)|\\:|[\"]|\\-\\-|\\=| \\- |\\#| \\'|\\' )","{\\1}",text)

## For this analysis delete punctuation, but for the end product it might be reasonable to keep them.
## ToDo: Parallelize for speed up process
tokens <- strsplit(text,"\\{.?}| ")
tokens <- unlist(tokens)

twitter.wordlength.frequency <- table(lapply(twitter.tokens, nchar) )

tokens.frequency <- sort(table(tokens), decreasing=TRUE)

##
word.frequency <- data.frame(word=names(tokens.frequency), freq=tokens.frequency)

pl <- ggplot(subset(word.frequency, freq > 2000000) ,aes(word, freq/1000000))
pl <- pl + geom_bar(stat="identity", fill="red")
pl + theme(axis.text.x=element_text(angle=90))