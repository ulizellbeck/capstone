##setwd("/work/R/final/")
setwd("/media/shared/data_scientist/coursera/capstone/final/")

library(RWeka)
library(stringr)
library(plyr)
library(data.table)

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

cleanData <- function(data) 
    {
      library(tm)
      data <- tolower(data) # convert to lowercase
      data <- removeNumbers(data) # remove numbers
      punctuation <- '[.,!:;?]|:-\\)|:-\\(|:\\)|:\\(|:D|=D|8\\)|:\\*|=\\*|:x|:X|:o|:O|:~\\(|T\\.T|Y\\.Y|S2|<3|:B|=B|=3|:3'
      data <- gsub(punctuation," END ",data) # substitute selected ponctuation (including smileys) with the word END
      data <- gsub("$"," END",data) # make sure every line ends with an END
      data <- gsub("\\b(\\w+)\\s+\\1\\b","\\1",data) # remove duplicate words in sequence (eg. that that)
      data <- gsub("\\b(\\w+)\\s+\\1\\b","\\1",data) # remove duplicate words in sequence (eg. that that)
      data <- gsub("\\b(\\w+)\\s+\\1\\b","\\1",data) # remove duplicate words in sequence (eg. that that)
      data <- removePunctuation(data) # remove all other punctuation
      data <- stripWhitespace(data) # remove excess white space
      data <- gsub("^[[:space:]]","",data) # make sure lines doesn't begin with space
      data <- gsub("[[:space:]]$","",data) # make sure lines doesn't end with space
  }

## Clean Data
blogsUS <- cleanData(blogs)
save(file="blogsUS-clean.rdata",blogsUS)
newsUS <- cleanData(news)
save(file="newsUS-clean.rdata",newsUS)
twitterUS <- cleanData(twitter)
save(file="twitterUS-clean.rdata",twitterUS)

## Extract Words
blogsUS <- unlist(str_split(blogsUS,"\\W+"))
newsUS <- unlist(str_split(newsUS,"\\W+"))
twitterUS <- unlist(str_split(twitterUS,"\\W+"))
save(file="blogsUS-words.rdata",blogsUS)
save(file="newsUS-words.rdata",newsUS)
save(file="twitterUS-words.rdata",twitterUS)

## Reload Data
load("blogsUS-clean.rdata")
load("twitterUS-clean.rdata")
load("newsUS-clean.rdata")
load("blogsUS-words.rdata")
load("twitterUS-words.rdata")

## Build vocabulary
build_vocabulary <- function(data)
{
  word_frequency <- data.table(count(data))
  vocabulary <- word_frequency[freq>7]
}

vocabulary <- build_vocabulary(twitterUS)

## Build tokens
## Split by sentence ("END")
build_tokens <- function(data)
{
  data <- unlist(str_split(data,"END"))
  data <- data[which(data!="")] ## remove empty rows
}

twitterUS <- build_tokens(twitterUS)
blogsUS <- build_tokens(blogsUS)

save(file="blogsUS-tokens.rdata",blogsUS)
save(file="twitterUS-tokens.rdata",twitterUS)

load("blogsUS-tokens.rdata")
load("twitterUS-tokens.rdata")
load("vocabulary.rdata")

## Create N-Grams
bigram_tokens_1 <- NGramTokenizer(blogsUS[1:100000],Weka_control(min = 2, max = 2, delimiters = " "))
bigram_tokens_2 <- NGramTokenizer(twitterUS[1:100000],Weka_control(min = 2, max = 2, delimiters = " "))
trigram_tokens_1 <- NGramTokenizer(blogsUS[1:100000],Weka_control(min = 3, max = 3, delimiters = " "))
trigram_tokens_2 <- NGramTokenizer(twitterUS[1:100000],Weka_control(min = 3, max = 3, delimiters = " "))

## Split N-Grams by last-word (n) and preceding words (n-1)
bigram_tokens_1 <- data.table(bigram_tokens_1)
bigram_tokens_1[,word_1:=sapply(strsplit(as.character(bigram_tokens_1)," "), "[[",1)]
bigram_tokens_1[,word_2:=sapply(strsplit(as.character(bigram_tokens_1)," "), "[[",2)]

bigram_tokens_2 <- data.table(bigram_tokens_2)
bigram_tokens_2[,word_1:=sapply(strsplit(as.character(bigram_tokens_2)," "), "[[",1)]
bigram_tokens_2[,word_2:=sapply(strsplit(as.character(bigram_tokens_2)," "), "[[",2)]

trigram_tokens_1 <- data.table(trigram_tokens_1)
trigram_tokens_1[,word_1:=sapply(strsplit(as.character(trigram_tokens_1)," "), "[[",1)]
trigram_tokens_1[,word_2:=sapply(strsplit(as.character(trigram_tokens_1)," "), "[[",2)]
trigram_tokens_1[,word_3:=sapply(strsplit(as.character(trigram_tokens_1)," "), "[[",3)]

trigram_tokens_2 <- data.table(trigram_tokens_2)
trigram_tokens_2[,word_1:=sapply(strsplit(as.character(trigram_tokens_2)," "), "[[",1)]
trigram_tokens_2[,word_2:=sapply(strsplit(as.character(trigram_tokens_2)," "), "[[",2)]
trigram_tokens_2[,word_3:=sapply(strsplit(as.character(trigram_tokens_2)," "), "[[",3)]


ll <- data.table(ifelse(bigram_tokens_1$word_1 %in% vocabulary$x,bigram_tokens_1$word_1,"<UNK>"))
ll$v2 <- data.table(ifelse(bigram_tokens_1$word_2 %in% vocabulary$x,bigram_tokens_1$word_2,"<UNK>"))
ll[,tokens_cleaned:=paste(ll$V1,ll$v2,sep = " ")]

bigram_tokens_1[,tokens_cleaned:=ll$tokens_cleaned]

rm(ll)

ll <- data.table(ifelse(bigram_tokens_2$word_1 %in% vocabulary$x,bigram_tokens_2$word_1,"<UNK>"))
ll$v2 <- data.table(ifelse(bigram_tokens_2$word_2 %in% vocabulary$x,bigram_tokens_2$word_2,"<UNK>"))
ll[,tokens_cleaned:=paste(ll$V1,ll$v2,sep = " ")]

bigram_tokens_2[,tokens_cleaned:=ll$tokens_cleaned]

rm(ll)

ll <- data.table(ifelse(trigram_tokens_1$word_1 %in% vocabulary$x,trigram_tokens_1$word_1,"<UNK>"))
ll$v2 <- data.table(ifelse(trigram_tokens_1$word_2 %in% vocabulary$x,trigram_tokens_1$word_2,"<UNK>"))
ll$v3 <- data.table(ifelse(trigram_tokens_1$word_3 %in% vocabulary$x,trigram_tokens_1$word_3,"<UNK>"))
ll[,tokens_cleaned:=paste(ll$V1,ll$v2,ll$v3,sep = " ")]

trigram_tokens_1[,tokens_cleaned:=ll$tokens_cleaned]

rm(ll)

ll <- data.table(ifelse(trigram_tokens_2$word_1 %in% vocabulary$x,trigram_tokens_2$word_1,"<UNK>"))
ll$v2 <- data.table(ifelse(trigram_tokens_2$word_2 %in% vocabulary$x,trigram_tokens_2$word_2,"<UNK>"))
ll$v3 <- data.table(ifelse(trigram_tokens_2$word_3 %in% vocabulary$x,trigram_tokens_2$word_3,"<UNK>"))
ll[,tokens_cleaned:=paste(ll$V1,ll$v2,ll$v3,sep = " ")]

trigram_tokens_2[,tokens_cleaned:=ll$tokens_cleaned]

rm(ll)


bigram_tokens_1[,bigram_tokens_1:=NULL]
bigram_tokens_1[,word_1:=NULL]
bigram_tokens_1[,word_2:=NULL]
trigram_tokens_1[,trigram_tokens_1:=NULL]
trigram_tokens_1[,word_1:=NULL]
trigram_tokens_1[,word_2:=NULL]
trigram_tokens_1[,word_3:=NULL]

bigram_tokens_2[,bigram_tokens_2:=NULL]
bigram_tokens_2[,word_1:=NULL]
bigram_tokens_2[,word_2:=NULL]
trigram_tokens_2[,trigram_tokens_2:=NULL]
trigram_tokens_2[,word_1:=NULL]
trigram_tokens_2[,word_2:=NULL]
trigram_tokens_2[,word_3:=NULL]

gc()

tokens_1 <- rbindlist(list(bigram_tokens_1, trigram_tokens_1))
tokens_2 <- rbindlist(list(bigram_tokens_2, trigram_tokens_2))

tokens_all_unk <-rbindlist(list(tokens_1,tokens_2))

tokens_1_unk <- rbindlist(list(bigram_tokens_1, trigram_tokens_1))

## Build frequencies
tt <- data.table(count(tokens_1))
tt_twitter <- data.table(tokens_2)

tt_all_unk <- data.table(count(tokens_all_unk))

tt_all_unk <- tt_all

#rm(tt_all)

tt_unk <- data.table(count(tokens_1_unk))

## Build new column for lookup (n_1) and prediction (n)

tt[,n:=regmatches(tt[,x],regexpr("\\w*$",tt[,x]))]
tt[,n_1:=gsub(" \\w+$","",tt[,x])]

tt_unk[,n:=regmatches(tt_unk[,tokens_cleaned],regexpr("\\w*$",tt_unk[,tokens_cleaned]))]
tt_unk[,n_1:=gsub(" \\w+$","",tt_unk[,tokens_cleaned])]

tt_all[,n:=regmatches(tt_all[,bigram_tokens_1],regexpr("\\w*$",tt_all[,bigram_tokens_1]))]
tt_all[,n_1:=gsub(" \\w+$","",tt_all[,bigram_tokens_1])]

tt_all_unk[,n:=regmatches(tt_all_unk[,tokens_cleaned],regexpr("<?\\w*>?$",tt_all_unk[,tokens_cleaned]))]
tt_all_unk[,n_1:=gsub(" <?\\w+>?$","",tt_all_unk[,tokens_cleaned])]

sub("<?\\w*>?$","","U")

## Remove profanity words

setkey(tt,n)
tt <- tt[!c("fuck","cunt","shit","piss","cunt","cocksucker","motherfucker","tits")]
setkey(tt_all,n)
tt_all <- tt_all[!c("fuck","cunt","shit","piss","cunt","cocksucker","motherfucker","tits")]
setkey(tt_unk,n)
tt_unk <- tt_unk[!c("fuck","cunt","shit","piss","cunt","cocksucker","motherfucker","tits")]
setkey(tt_all_unk,n)
tt_all_unk <- tt_all_unk[!c("fuck","cunt","shit","piss","cunt","cocksucker","motherfucker","tits","<UNK>","slut")]


## Calculate probabilities

tt[,freq_n_1:=sum(freq),by=n_1]
tt[,prob:=freq/freq_n_1]

tt_unk[,freq_n_1:=sum(freq),by=n_1]
tt_unk[,prob:=freq/freq_n_1]

tt_all[,freq_n_1:=sum(freq),by=n_1]
tt_all[,prob:=freq/freq_n_1]

tt_all_unk[,freq_n_1:=sum(freq),by=n_1]
tt_all_unk[,prob:=freq/freq_n_1]

## Testing
head(tt[n_1=="score",n,prob][order(-prob)],3)
head(tt[n_1=="the first",n,prob][order(-prob)],3)

tt[n_1=="in the"&n=="years"]

head(tt_unk[n_1=="score a",n,prob][order(-prob)],3)
head(tt[n_1=="the first",n,prob][order(-prob)],3)

head(tt_unk[n_1=="<UNK> <UNK>",n,prob][order(-prob)],3)
head(tt_all[n_1=="score a",n,prob][order(-prob)],3)

head(tt_all[n_1=="back to",n,prob][order(-prob)],10)
head(tt_unk[n_1=="<UNK> to",n,prob][order(-prob)],10)

head(tt_all[n=="goal",n_1,freq][order(-freq)],3)

head(tt_all_unk[n_1=="yauch",n,prob][order(-prob)],3)

head(tt_all_unk)

save(file="tt_unk.rdata",tt_unk)
save(file="tt_all.rdata",tt_all)
save(file="tt_all_unk.rdata",tt_all_unk)

tt_all_unk[,tokens_cleaned:=NULL]
tt_all_unk[,freq:=NULL]
tt_all_unk[,freq_n_1:=NULL]

load("tt_all_unk.rdata")
load("vocabulary.rdata")

## Prediction

ll <- predict_test("rip and all the time to get")

input <- "i believe in"

length(unlist(strsplit(input," ")))

predict_test <- function(input) { 
  input <- cleanData(input)
  input <- regmatches(input,regexpr("\\w+ ?\\w*$",input))
  t <- length(unlist(strsplit(input," ")))
  input <- data.table(input)
  input[,word_1:=sapply(strsplit(as.character(input)," "), "[[",1)]
  ll <- data.table(ifelse(input$word_1 %in% vocabulary$x,input$word_1,"<UNK>"))
  if (t == 2)
  {
  input[,word_2:=sapply(strsplit(as.character(input)," "), "[[",2)]
  ll$v2 <- data.table(ifelse(input$word_2 %in% vocabulary$x,input$word_2,"<UNK>"))
  ll[,tokens_cleaned:=paste(ll$V1,ll$v2,sep = " ")]
  }
  else {
    ll[,tokens_cleaned:=ll$V1]
  }
  ret_val <- head(tt_all_unk[n_1==ll$tokens_cleaned,n,prob][order(-prob)],1) 
  if (nrow(ret_val) == 0)
  {
    input <- regmatches(input,regexpr("\\w*$",input)) 
    ret_val <- head(tt_all_unk[n_1==input,n,prob][order(-prob)],1)
  }  
  return(ret_val[1,n])
}

### Playground ....


## head(tt)

test_data <- function(data){
  data <- head(tt[n_1==data|,n,prob][order(-prob)],3)
  
}

dd <- test_data("in the")

length(tokens_1)

unking <- function(data){
  ii <- length(data)
  for (i in 1:ii)
  {
    t <- unlist(str_split(data[i]," "))
    ll <- length(t)
      for (j in 1:ll){
        t[j] <- ifelse(t[j] %in% vocabulary$x,t[j],"<UNK>")
      }
      data[i] <- paste(t, collapse = " ")
  }
  data <- data
}

test <- unking(tokens_1)

tttt <- tokens_1[1:100]

head(to,1000)

length(tokens_1[1])

tttt <- tokens_1[1]
