library(stringr)
library(data.table)

load("tt_all_unk.rdata")
load("vocabulary_sample.rdata")
setkey(tt_all_unk,n_1)

## Clean

cleanData <- function(data) {
  library(tm)
  data <- tolower(data) # convert to lowercase
  data <- removeNumbers(data) # remove numbers
  punctuation <- '[.,!:;?]|:-\\)|:-\\(|:\\)|:\\(|:D|=D|8\\)|:\\*|=\\*|:x|:X|:o|:O|:~\\(|T\\.T|Y\\.Y|S2|<3|:B|=B|=3|:3'
  ##data <- gsub(punctuation," END ",data) # substitute selected ponctuation (including smileys) with the word END
  ##data <- gsub("$"," END",data) # make sure every line ends with an END
  data <- gsub("\\b(\\w+)\\s+\\1\\b","\\1",data) # remove duplicate words in sequence (eg. that that)
  data <- gsub("\\b(\\w+)\\s+\\1\\b","\\1",data) # remove duplicate words in sequence (eg. that that)
  data <- gsub("\\b(\\w+)\\s+\\1\\b","\\1",data) # remove duplicate words in sequence (eg. that that)
  data <- removePunctuation(data) # remove all other punctuation
  data <- stripWhitespace(data) # remove excess white space
  data <- gsub("^[[:space:]]","",data) # make sure lines doesn't begin with space
  data <- gsub("[[:space:]]$","",data) # make sure lines doesn't end with space
}


###prediction function

predict <- function(input) { 
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
  ret_val <- head(tt_all_unk[n_1==ll$tokens_cleaned,n,prob][order(-prob)],3) 
  if (nrow(ret_val) == 0)
  {
    input <- regmatches(input,regexpr("\\w*$",input)) 
    ret_val <- head(tt_all_unk[n_1==input,n,prob][order(-prob)],3)
  }  
  return(ret_val[1:3,n])
}



