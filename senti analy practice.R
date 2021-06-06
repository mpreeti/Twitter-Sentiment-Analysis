install.packages("twitteR")
library(twitteR)
install.packages("purrr")
library(purrr)
library(dplyr)
install.packages("plyr")
library(plyr)
install.packages("stringr")
library(stringr)
install.packages("ROAuth")
require('ROAuth')
require('RCurl')

score.sentiment<-function(sentences,pos.words,neg.words,.progress='none')
{
  require(plyr)
  require(stringr)
  scores<-laply(sentences,function(sentences,pos.words,neg.words){
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub('\\d+', "", sentence)
  sentence<-tolower(sentence)
  word.list<-str_split(sentence,'\\s+')
  words<-unlist(word.list)
  pos.matches<-match(words,pos.words)
  neg.matches<-match(words,neg.words)
  pos.matches<-!is.na(pos.matches)
  neg.matches<-!is.na(neg.matches)
  score<-sum(pos.matches)-sum(neg.matches)
  return(score)},pos.words,neg.words,.progress = .progress)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}
  
pos.words = scan('C:/Users/hp/Desktop/Imarticus/Sentiment Analysis/Sentiment Analysis/positive-words.txt',what='character',comment.char=';')
neg.words = scan('C:/Users/hp/Desktop/Imarticus/Sentiment Analysis/Sentiment Analysis/negative-words.txt',what='character',comment.char=';')
bscore <- score.sentiment(tweet_df$text,pos.words,neg.words,.progress='text')
rscore <- score.sentiment(tweet2_df$text,pos.words,neg.words,.progress='text')
hist(rscore$score)
hist(bscore$score)  
  
consumerKey <- "7SE63ZJugxp2mlTWYCghk3v3S"
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <-"https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerSecret <- "ScEeWoVlWtjoZDlSk9maS6td6RQCyFuG6drASZw82YfCGcVtXb"
accessToken <- "1009055313775845378-oJL81SXMZWNro5nMSJ3w7iazZEws5i"
accessTokenSecret <- "ISDtJyhZge8DZKX4LWlYlDH3gQdK2zhVfrmyZQYUu6oj3"
twitCred <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret,
                             requestURL=reqURL,
                             accessURL=accessURL,
                             authURL=authURL)
twitCred$handshake()
setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)
tweet1 <- userTimeline("@barcalona",n=100)
tweet2 <- userTimeline("@realmadriden",n=100)
tweet_df <- tbl_df(map_df(tweet1,as.data.frame))
tweet2_df <- tbl_df(map_df(tweet2,as.data.frame))



  
  
  
  
  
    
  
  
  
  
}  