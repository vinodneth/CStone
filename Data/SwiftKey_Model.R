######################################
##  This R file reads the SwiftKey data and prepares the model
##  NGramFunc is used to the prepare the model
######################################


##    set work directory and load libraries
setwd("C:/Coursera/CStn/Data")

library(tm)
library(tm.plugin.mail)
library(SnowballC)
library(tokenizers)
library(RWeka)
library(data.table)
library(stringi)
library(textreg)
library(ngram)



##    Read files for blogs, news and twitter 

blogs<-file("./en_US/en_US.blogs.txt","r")
blogs_lines<-readLines(blogs)
close(blogs)

news<-file("./en_US/en_US.news.txt","r")
news_lines<-readLines(news)
close(news)

twitter<-file("./en_US/en_US.twitter.txt","r")
twitter_lines<-readLines(twitter)
close(twitter)


##    get lines in  files  
lenblogs<-length(blogs_lines)
lennews<-length(news_lines)
lentwit<-length(twitter_lines)

cntblogs<-floor(lenblogs*0.1)
cntnews<-floor(lennews*0.1)
cnttwit<-floor(lentwit*0.1)

##    sample 5% of the data from the 3 sets
set.seed(1326)
blogs_lines<-blogs_lines[rbinom(cntblogs,lenblogs,0.5)]
news_lines<-news_lines[rbinom(cntnews,lennews,0.5)]
twitter_lines<-twitter_lines[rbinom(cnttwit,lentwit,0.5)]

##    include all three in single object
sampleLines<-paste(c(blogs_lines,news_lines,twitter_lines),collapse = " ")

##    remove temporary objects not required
rm(blogs_lines,twitter_lines,news_lines,cntblogs,cntnews,cnttwit,lenblogs,lentwit,lennews)

##    Corpus data used (VCorpus)
doc_vector <- VectorSource(sampleLines)
dcorpus <- VCorpus(doc_vector)


##    function for preprocessing 
removelan<- function(x) iconv(x, "UTF-8", "ASCII", sub="")
## removelan<- function(x) iconv(x, to="UTF-8", sub="byte")
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x) 

dcorpus=tm_map(dcorpus,stripWhitespace)
dcorpus=tm_map(dcorpus,content_transformer(tolower))
##dcorpus=tm_map(dcorpus,removeWords,stopwords("en"))
dcorpus=tm_map(dcorpus,removePunctuation)
dcorpus=tm_map(dcorpus,removeNumbers)
dcorpus=tm_map(dcorpus,content_transformer(removeURL))
dcorpus=tm_map(dcorpus,content_transformer(removelan))
##dcorpus=tm_map(dcorpus,stemDocument)
dcorpus=tm_map(dcorpus,PlainTextDocument)


##    write to file
save(dcorpus, file = "./dcorpusV.RData")

cleanChar<-convert.tm.to.character(dcorpus)

##    source NGramFunc prior to calling
source("NGramFunc.R")

model<-NgramModel(cleanChar)

##    save model as RData for use by Predict function (xpc - highlights x% of the data sampled)
save(model,file="./model10pc.RData")


##    Predict function call would be PredictNgram(model,"the input text here")
##    Would be called at prompt or from shiny app



