library(dplyr)
library(tm)
library(quanteda)
library(data.table)
library(sqldf)
library(tidyr)

## Load data 
conblog <- file("en_US.blogs.txt", open = "rb")
US_Blogs <- readLines(conblog, encoding = "UTF-8", skipNul = TRUE);
close(conblog)

con <- file("en_US.twitter.txt", open = "rb")
US_Twitter <- readLines(con, encoding = "UTF-8", skipNul = TRUE);
close(con)

con <- file("en_US.news.txt", open = "rb")
US_News <- readLines(con, encoding = "UTF-8", skipNul = TRUE);
close(con)
US_News[grepl("Offense still struggling", US_News)]

#Sample Data 
set.seed(12345)
sample_data <- c( sample(US_Twitter, size=200000), sample(US_News, size=200000), sample(US_Blogs, size=200000) )


# clean the text 
sample_data <- removePunctuation(sample_data)
sample_data <- removeNumbers(sample_data)
sample_data <- stripWhitespace(sample_data)
# remove non englist letters
sample_data <- gsub("[^\u0001-\u007F]+", "", sample_data)
sample_data <- tolower(sample_data)
# remove profanities
bad_words=readLines("bad-words.txt", skipNul = TRUE)
sample_data <- removeWords(sample_data,bad_words)


sample_data_Backup <- sample_data
#remove empty rows and rows with few characters
sample_data <- sample_data[nchar(sample_data)!=0]
sample_data <- sample_data[nchar(sample_data)!=1]
sample_data <- sample_data[nchar(sample_data)!=2]
sample_data <- sample_data[nchar(sample_data)!=3]


# search string 
sample_data[grepl("the guy in front of me just bought a pound of bacon", sample_data)]
sample_data[grepl("pound of bacon", sample_data)]

#exploratory analysis
min(nchar(sample_data))
sample_data[nchar(sample_data)==min(nchar(sample_data))]


#exploring unigrams
#there are 232887 words with a single occurance these can be removed from the analysis. these are usually not real words()
# explore lowfrequency words these are obsered as names or spelling mitakes 
lowfreqwords <- filter(ngram1, N==1)
sample_data <- sample_data_Backup


sample_data_Backup <- sample_data

sample_data <- removeWords(sample_data,unlist(lowfreqwords[1:3000,1]))
sample_data <- removeWords(sample_data,unlist(lowfreqwords[3000:5000,1]))
sample_data <- removeWords(sample_data,unlist(lowfreqwords[5000:6000,1]))
sample_data <- removeWords(sample_data,unlist(lowfreqwords[6000:9000,1]))
sample_data <- removeWords(sample_data,unlist(lowfreqwords[9000:12000,1]))
sample_data <- removeWords(sample_data,unlist(lowfreqwords[12000:15000,1]))

lowfreqwords <- lowfreqwords[-(1:15000),]

sampledatacorpus <- sample_data %>% corpus()
# Unigrams 
ngram1  <- sampledatacorpus %>% tokens(ngrams = 1)
ngram1 <- data.table( sort(table(unlist(ngram1)), decreasing = TRUE) ) 
names(ngram1) <- c("word1", "Freq")

singleoccurancewords<- filter(ngram1, N==1)
twooccurancewords<- filter(ngram1, N==2)

# Bigrams 
ngram2  <- sampledatacorpus  %>% tokens(ngrams = 2)
ngram2 <- data.table( sort(table(unlist(ngram2)), decreasing = TRUE) ) 
#check the bigram
#sqldf("select * from ngram2table where V1 LIKE 'your_%'")
ngram2 <- ngram2  %>% separate(V1, c("word1", "word2"), "_")

# trigrams 
ngram3  <- sampledatacorpus %>% tokens(ngrams = 3)
ngram3 <- data.table( sort(table(unlist(ngram3)), decreasing = TRUE) ) 
ngram3 <- ngram3  %>% separate(V1, c("word1", "word2", "word3"), "_")

# four grams 
ngram4  <- sampledatacorpus %>% tokens(ngrams = 4)
#ngram4 <- data.table( sort(table(unlist(ngram4)), decreasing = TRUE) ) 
ngram4_1 <- ngram4[1 : 299572]
ngram4_1 <- unlist(ngram4_1)
ngram4_1 <-  table(ngram4_1)
ngram4_1 <- data.frame(sort(ngram4_1, decreasing = TRUE))

ngram4_2 <- ngram4[length(ngram4)/2: length(ngram4)]
ngram4_2 <- unlist(ngram4_2)
ngram4_2 <-  table(ngram4_2)
ngram4_2 <- data.frame(sort(ngram4_2, decreasing = TRUE))


ngram4 <- data.table( sort(table(ngram4), decreasing = TRUE) ) 



names(ngram4_2) <- c("text", "Freq")
names(ngram4_1) <- c("text", "Freq")

ngram4 < cbind(ngram4_1, ngram4_2)
ngram4 <- ngram4%>% group_by(text) %>% summarise(Freq = sum(Freq))
ngram4 <- ngram4[order(ngram4$Freq, decreasing=TRUE),]

ngram4 <- ngram4  %>% separate(text, c("word1", "word2", "word3", "word4"), "_")
save(ngram4, file="ngram4")



# five grams 
ngram5  <- sampledatacorpus %>% tokens(ngrams = 5)
ngram5 <- data.table( sort(table(unlist(ngram5)), decreasing = TRUE) ) 
ngram5 <- ngram5  %>% separate(V1, c("word1", "word2", "word3", "word4", "word5"), "_")
save(ngram5, file="ngram5")


# six grams 
ngram6  <- sampledatacorpus %>% tokens(ngrams = 6)
ngram6 <- data.table( sort(table(unlist(ngram6)), decreasing = TRUE) ) 

#ngrams with frequency >1 
dim(ngram6[ngram6$N>1,]) 

ngram6 <- ngram6[ngram6$N>1,]
ngram6 <- ngram6  %>% separate(V1, c("word1", "word2", "word3", "word4", "word5", "word6"), "_")

#**************************************************************************************************
# nextword prediction function 
#**************************************************************************************************
nextword <- function(p,n=3, currenttword=FALSE){ 
  p <- tolower(p)
  #p <- removePunctuation(p)
  pwords <- unlist(strsplit(p, split=" "))
  #pwords <- pwords[(length(pwords)-3):length(pwords)]
  l <-length(pwords) 
  
 
  
  #predictions<- ""
  predictions <- data.frame(pred=as.character(), N=as.integer(),  stringsAsFactors=FALSE)
  # needs to change so we append to predictions and not overwrite
  if (l>5 & exists("ngram6")) {
    #pred <- top_n(filter(ngram6, word1==pwords[l-4] & word2==pwords[l-3] & word3==pwords[l-2] & word4==pwords[l-1] & word5==pwords[l]  ),n)$word6
    pred <- head(filter(ngram6, word1==pwords[l-4] & word2==pwords[l-3] & word3==pwords[l-2] & word4==pwords[l-1] & word5==pwords[l]),n)[,6:7]
    names(pred) <- c("pred", "N")
    predictions <- unique(rbind(predictions , as.data.frame(pred) ) )
  }
  
  if (nrow(predictions) < n & l>4 & exists("ngram5")) {
    #pred <- top_n(filter(ngram5, word1==pwords[l-3] & word2==pwords[l-2] & word3==pwords[l-1] & word4==pwords[l] ),n)$word5
    pred <- head(filter(ngram5, word1==pwords[l-3] & word2==pwords[l-2] & word3==pwords[l-1] & word4==pwords[l]),n ) [,5:6]
    names(pred) <- c("pred", "N")
    predictions <- unique(rbind(predictions , as.data.frame(pred) ))
  }
  
  if (nrow(predictions) < n & l>3 & exists("ngram4") ) {
    #pred <- top_n(filter(ngram4, word1==pwords[l-2] & word2==pwords[l-1] & word3==pwords[l]),n)$word4
    #pred <- head(filter(ngram4, word1==pwords[l-2] & word2==pwords[l-1] & word3==pwords[l])$word4,n)
    pred <- head(filter(ngram4, word1==pwords[l-2] & word2==pwords[l-1] & word3==pwords[l]),n)[,4:5]
    names(pred) <- c("pred", "N")
    predictions <- unique(rbind(predictions , as.data.frame(pred) ) )
  }

  if (nrow(predictions) < n & l>2 & exists("ngram3")) {
    #pred <- top_n(filter(ngram3, word1==pwords[l-1] & word2==pwords[l]),n)$word3
    #pred <- head(filter(ngram3, word1==pwords[l-1] & word2==pwords[l])$word3,n)
    pred <- head(filter(ngram3, word1==pwords[l-1] & word2==pwords[l]),n)[,3:4]
    names(pred) <- c("pred", "N")
    predictions <- unique(rbind(predictions , as.data.frame(pred) ) )
  }
  
  if (nrow(predictions) < n & l>1 & exists("ngram2")) {
    #pred <- top_n(filter(ngram2, word1==pwords[l]),n)$word2
    #pred <- head(filter(ngram2, word1==pwords[l])$word2,n)
    pred <- head(filter(ngram2, word1==pwords[l]),n)[,2:3]
    names(pred) <- c("pred", "N")
    predictions <- unique(rbind(predictions , as.data.frame(pred) ))
  }
  
  if (nrow(predictions) < n & l==1 & exists("ngram1") & currenttword) {
    #pred <- top_n(filter(ngram1, word1 %like% paste("^", pwords[l], sep="")),n)$word1
    #pred <- head(filter(ngram1, word1 %like% paste("^", pwords[l], sep=""))$word1,n)
    pred <- head(filter(ngram1, word1 %like% paste("^", pwords[l], sep="")),n)
    names(pred) <- c("pred", "N")
    predictions <- unique(rbind(predictions , as.data.frame(pred) ))
  }
  as.character(predictions[1:n,1])
  #predictions
}


testprediction <- function (input) {
#output <- ""
for (i in input){
  if(exists("output")) {
    output <- rbind(output, cbind(i, nextword(i,3)))
  }
  else {
    output <- cbind(i, nextword(i,3) )
  }
   
}    
output  
}



## function to test the prediction accuracy
testprediction <- function (size) {
test <- sample(sampledatacorpus[], size)
inp <-  list()
res <- list()
output <- list()
for(i in test){
  s <- strsplit(i, split=" ")
  l<- length(s[[1]])
  inp<- paste( s[[1]][1:min(5,l)], collapse =" ")
  res<- paste( s[[1]][min(6,l+1)], collapse =" ")
  #inp<- paste( strsplit(i, split=" ") [[1]][1:5], collapse =" ")
  #res<- paste( strsplit(i, split=" ") [[1]][6], collapse =" ")
  pred <- paste(nextword(inp,3), collapse=",")
  
  if(exists("output")) {
    output <- rbind(output, cbind(inp,  res, pred) )
  }
  else {
    output <- cbind(inp,  res, pred)
  }
}
output
}



i <- sample(sampledatacorpus[], 5)

i<-c("you wont be sorry that said things just werent working", "you wont be sorry that")

testprediction <- function(input) {
  for (i in input){
    s <- strsplit(i, split=" ")
    l<- length(s[[1]])
    inp<- paste( s[[1]][1:min(5,l)], collapse =" ")
    res<- paste( s[[1]][min(6,l+1)], collapse =" ")
    
    if(exists("output")) {
      #output <- rbind(output, cbind(i, nextword(i,3)))
      output <- rbind(output, cbind(inp, paste(nextword(inp,3), collapse=",") , res))
    }
    else {
      #output <- cbind(i, nextword(i,3) )
      output <- cbind(inp, paste(nextword(inp,3), collapse=","), res )
    }
  }    
  output  
}

#******************************************************
#Load ngrams
#******************************************************
load("ngram1")
load("ngram2")
load("ngram3")
load("ngram4")
load("ngram5")
load("ngram6")


#******************************************************
#Optimize ngrams 
#******************************************************

#take only the top 20
ngram6 <- ngram6 %>% arrange(desc(N)) %>% group_by(word1,word2,word3,word4,word5) %>% do(head(., n = 20))

14246340 to 

ngram5 <- ngram5 %>% arrange(desc(Freq)) %>% group_by(word1,word2,word3,pred) %>% do(head(., n = 20))

ngram4 <- ngram4 %>% arrange(desc(Freq)) %>% group_by(word1,word2,word3) %>% do(head(., n = 20))

ngram3 <- ngram3 %>% arrange(desc(Freq)) %>% group_by(word1,word2) %>% do(head(., n = 20))

ngram2 <- ngram2 %>% arrange(desc(Freq)) %>% group_by(word1) %>% do(head(., n = 20))

#ngram1 <- ngram1 %>% arrange(desc(N)) %>% group_by(word1) %>% do(head(., n = 20))


#******************************************************
## top 20 for 4 gram and higher 
#******************************************************
ngram4_1 <- ngram4[ngram4$N==1,]
ngram4 <- ngram4[ngram4$N>1,]
ngram4 <- ngram4 %>% arrange(desc(N)) %>% group_by(word1,word2,word3) %>% do(head(., n = 20))


t <- ngram4 %>% group_by(word1,word2,word3) %>% summarise(n=n()) 
t <- t[t$n<20,]
t <- t %>% arrange(desc(n))

ngram4_1[ngram4_1$word1==t$word1 & ngram4_1$word2==t$word2 & ngram4_1$word3==t$word3, ]

head(ngram4_1[ngram4_1$word1==x$word1 & ngram4_1$word2==x$word2 & ngram4_1$word3==x$word3, ] , 3-x$n)







