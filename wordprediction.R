library(wordcloud)
library(data.table)
library(dplyr)
library(tm)



if (!exists("ngram4")) { load("ngram4_top20.rds") }
if (!exists("ngram3")) { load("ngram3_top20.rds") }
if (!exists("ngram2")) { load("ngram2_top20.rds") }
if (!exists("ngram1")) { load("ngram1.rds") }
if (!exists("predictions")) { predictions <- head(ngram1,5)}


# nextword prediction function 
nextword <- function(p,n=20, currenttword=FALSE){ 
  p <- tolower(p)
  p <- removePunctuation(p)
  #p <- stri_replace_all(p, regex="[[:punct:]]", replacement = "")
  pwords <- unlist(strsplit(p, split=" "))
  #pwords <- pwords[(length(pwords)-3):length(pwords)]
  l <-length(pwords) 
  
  #predictions<- ""
  predictions <- data.frame(pred=as.character(), N=as.integer(),  stringsAsFactors=FALSE)
  # needs to change so we append to predictions and not overwrite
  if (l>4 & exists("ngram6")) {
    #pred <- top_n(filter(ngram6, word1==pwords[l-4] & word2==pwords[l-3] & word3==pwords[l-2] & word4==pwords[l-1] & word5==pwords[l]  ),n)$word6
    pred <- head(filter(ngram6, word1==pwords[l-4] & word2==pwords[l-3] & word3==pwords[l-2] & word4==pwords[l-1] & word5==pwords[l]),n)[,6:7]
    names(pred) <- c("pred", "N")
    predictions <- unique(rbind(predictions , as.data.frame(pred) ) )
  }
  
  if (nrow(predictions) < n & l>3 & exists("ngram5")) {
    #pred <- top_n(filter(ngram5, word1==pwords[l-3] & word2==pwords[l-2] & word3==pwords[l-1] & word4==pwords[l] ),n)$word5
    pred <- head(filter(ngram5, word1==pwords[l-3] & word2==pwords[l-2] & word3==pwords[l-1] & word4==pwords[l] ),n)[,5:6]
    names(pred) <- c("pred", "N")
    predictions <- unique(rbind(predictions , as.data.frame(pred) ))
  }
  
  if (nrow(predictions) < n & l>2 & exists("ngram4") ) {
    #pred <- top_n(filter(ngram4, word1==pwords[l-2] & word2==pwords[l-1] & word3==pwords[l]),n)$word4
    #pred <- head(filter(ngram4, word1==pwords[l-2] & word2==pwords[l-1] & word3==pwords[l])$word4,n)
    pred <- head(filter(ngram4, word1==pwords[l-2] & word2==pwords[l-1] & word3==pwords[l]),n)[,4:5]
    names(pred) <- c("pred", "N")
    predictions <- unique(rbind(predictions , as.data.frame(pred) ) )
  }
  
  if (nrow(predictions) < n & l>1 & exists("ngram3")) {
    #pred <- top_n(filter(ngram3, word1==pwords[l-1] & word2==pwords[l]),n)$word3
    #pred <- head(filter(ngram3, word1==pwords[l-1] & word2==pwords[l])$word3,n)
    pred <- head(filter(ngram3, word1==pwords[l-1] & word2==pwords[l]),n)[,3:4]
    names(pred) <- c("pred", "N")
    predictions <- unique(rbind(predictions , as.data.frame(pred) ) )
  }
  
  if (nrow(predictions) < n  & exists("ngram2") & !currenttword) {
    #pred <- top_n(filter(ngram2, word1==pwords[l]),n)$word2
    #pred <- head(filter(ngram2, word1==pwords[l])$word2,n)
    pred <- head(filter(ngram2, word1==pwords[l]),n)[,2:3]
    names(pred) <- c("pred", "N")
    predictions <- unique(rbind(predictions , as.data.frame(pred) ))
  }
  
  if (nrow(predictions) < n & l==1 & exists("ngram1") & currenttword) {
    #pred <- top_n(filter(ngram1, word1 %like% paste("^", pwords[l], sep="")),n)$word1
    #pred <- head(filter(ngram1, word1 %like% paste("^", pwords[l], sep=""))$word1,n)
    pred <- head(filter(ngram1, pred %like% paste("^", pwords[l], sep="")),n)
    names(pred) <- c("pred", "N")
    predictions <- unique(rbind(predictions , as.data.frame(pred) ))
  }
 
  #as.character(predictions[1:n,])
  predictions <- as.data.frame(rbind(predictions , head(ngram1,20))[1:20,1:2])
  predictions
}



