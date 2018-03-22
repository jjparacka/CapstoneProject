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
nextword  <- function(p,n=20, currenttword=FALSE){ 
  p       <- tolower(p)
  p       <- removePunctuation(p)
  pwords  <- unlist(strsplit(p, split=" "))
  l       <-length(pwords) 
  
  predictions <- data.frame(pred=as.character(), N=as.integer(),  stringsAsFactors=FALSE)
  if (l>4 & exists("ngram6")) {
    pred        <- head(filter(ngram6, word1==pwords[l-4] & word2==pwords[l-3] & word3==pwords[l-2] & word4==pwords[l-1] & word5==pwords[l]),n)[,6:7]
    names(pred) <- c("pred", "N")
    predictions <- unique(rbind(predictions , as.data.frame(pred) ) )
  }
  
  if (nrow(predictions) < n & l>3 & exists("ngram5")) {
    pred        <- head(filter(ngram5, word1==pwords[l-3] & word2==pwords[l-2] & word3==pwords[l-1] & word4==pwords[l] ),n)[,5:6]
    names(pred) <- c("pred", "N")
    predictions <- unique(rbind(predictions , as.data.frame(pred) ))
  }
  
  if (nrow(predictions) < n & l>2 & exists("ngram4") ) {
    pred        <- head(filter(ngram4, word1==pwords[l-2] & word2==pwords[l-1] & word3==pwords[l]),n)[,4:5]
    names(pred) <- c("pred", "N")
    predictions <- unique(rbind(predictions , as.data.frame(pred) ) )
  }
  
  if (nrow(predictions) < n & l>1 & exists("ngram3")) {
    pred        <- head(filter(ngram3, word1==pwords[l-1] & word2==pwords[l]),n)[,3:4]
    names(pred) <- c("pred", "N")
    predictions <- unique(rbind(predictions , as.data.frame(pred) ) )
  }
  
  if (nrow(predictions) < n  & exists("ngram2") & !currenttword) {
    pred        <- head(filter(ngram2, word1==pwords[l]),n)[,2:3]
    names(pred) <- c("pred", "N")
    predictions <- unique(rbind(predictions , as.data.frame(pred) ))
  }
  
  if (nrow(predictions) < n & l==1 & exists("ngram1") & currenttword) {
    pred        <- head(filter(ngram1, pred %like% paste("^", pwords[l], sep="")),n)
    names(pred) <- c("pred", "N")
    predictions <- unique(rbind(predictions , as.data.frame(pred) ))
  }
  predictions <- as.data.frame(rbind(predictions , head(ngram1,20))[1:20,1:2])
  predictions
}



