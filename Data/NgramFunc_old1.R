## R script to model the given text and predict next words

setwd("C:/Coursera/CStn/Data")

library(tm)
library(tm.plugin.mail)
library(SnowballC)
library(tokenizers)
library(RWeka)
library(data.table)
library(stringi)


## Load R workspace files for Corpus and prepare clean Data 
load(file = "./dcorpusV.RData")
## test data load(file = "./dcorpusVTrim.RData")

cleanData <- data.frame(rawtext = sapply(dcorpus, as.character), stringsAsFactors=FALSE)

####################################################################
##### Function to consume the cleaned data provided and 
#####   prepare the model. Steps in modelling captured below
###################################################################


## Main Function
NgramModel <- function (inpTxt,minN = 1, maxN=1 , freq_cutoff = 1, rank_cutoff =3, delimiters=NULL){
## inpTxt : the cleaned text which is used to prepare the model
## min & max N : the range for the tokenizer call being made in the function
## delimiter : for seperating - NOT IMPLEMENTED

## Helper functions for NGramModel

  split_by_word <- function (text) {
    
    stopifnot (is.character (text))
    
    words <- unlist (strsplit (text, split = "[ ]+"))
    words [nchar (words) > 0]
  }
  
  except_last_word <- function (text) {
    
    stopifnot (is.character (text))
    stopifnot (length (text) == 1)
    
    # split the phrase into separate words
    words <- split_by_word (text)
    
    # exclude the last word
    except_last <- words [1:length (words)-1]
    
    # paste the words back together
    paste (except_last, collapse = " ")
  }
  
  last_word <- function (text) {
    
    stopifnot (is.character (text))
    stopifnot (length (text) == 1)
    
    # split the phrase into words
    words <- split_by_word (text)
    
    # the last word only
    words [length (words)]
  }
    
## Step 1 : Tokenize the text provided using TDM & RWeka Tokenizer

#  create tokenizer function and common functions for phrase frequency
  
   bigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
   trigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))
   
#  phrase as table
   freq_dt<-function(tdm){
     freq<- sort(rowSums(as.matrix(tdm)), decreasing=TRUE)
     freq_dt<-data.table(phrase=names(freq),frequency=freq)
     return(freq_dt)
   }
   
#  creating n-grams, using TDM.
   ## unigram using just TDM
   tdm.uni <- TermDocumentMatrix(dcorpus)
   tdm.uni <- removeSparseTerms(tdm.uni, 0.99)
   ngram1<-freq_dt(tdm.uni)
   
   ## bigram using RWeka
   tdm.bi <- TermDocumentMatrix(dcorpus, control=list(tokenize=bigramTokenizer))
   tdm.bi <- removeSparseTerms(tdm.bi, 0.999)
   ngram2<-freq_dt(tdm.bi)
   
   ## trigram ysing Rweka
   tdm.tri <- TermDocumentMatrix(dcorpus, control=list(tokenize=trigramTokenizer))
   tdm.tri <- removeSparseTerms(tdm.tri, 0.999)
   ngram3<-freq_dt(tdm.tri)
   
#  merge the ngrams into 1 data table
   nlist<-list(ngram1,ngram2,ngram3)
   ngrams<-rbindlist(nlist)
   
#  extract the context and the next word for each ngram
    ngrams [, word    := last_word (phrase),        by = phrase]
    ngrams [, context := except_last_word (phrase), by = phrase]
  
  
  
## Step 2 : Summarize to get the frequencies by token value

    # sum the frequency by context
    context <- ngrams [, sum (frequency), by = context]
    setnames (context, c("context", "context_frequency"))
    
    # through merging of context and ngrams, calculate the probability
    setkeyv (context, "context")
    setkeyv (ngrams, "context")
    
    # calculate the maximum liklihood estimate as frequency of last word over context freq
    ngrams [context, p := frequency / context_frequency]
    
    
    # exclude ngrams that are below the frequency cut-off
    ngrams <- ngrams [ frequency >= freq_cutoff, list (phrase, context, word, p) ]
    
    # mark each n-gram as a 1, 2, ... N gram
    ngrams [, n := unlist (lapply (stri_split (phrase, fixed = " "), length)) ]
    
    # keep only most likely words for each context
    ngrams <- ngrams [ order (context, -p)]
    ngrams [, rank := 1:.N, by = context]
    ngrams <- ngrams [ rank <= rank_cutoff ]
    
    return(ngrams)
}


####################################################################
##### Function to use the input text and predict next  
#####   word using the Ngram Model
###################################################################

## Main Function
PredictNgram <- function (model, newtxt, rank = 5){
## model    : the NGram model prepared by NgramModel function above
## newtxt   : text input to function to predict next word
## rank     : limits the list of probables, defaulted to 5
  
  ## Step 1 : Cleanse newtxt
    newtxt <- tolower(newtxt)
    newtxt <- removePunctuation(newtxt)
    newtxt <- removeNumbers(newtxt)
    newtxt <- stripWhitespace(newtxt)
    newtxt <- stemDocument(newtxt)
    newtxt<-removeWords(newtxt,stopwords(language = c("en")))  
    
  ## Step 2 : 
    # split text to words
    words <- split_by_word (newtxt)
    
    predictions <- NULL
    for (i in sort (model$n, decreasing = TRUE)) {
      
      # ensure there are enough previous words
      # for example, a trigram ngrams needs 2 previous words
      if (length (words) >= i-1) {
        
        # grab the necessary context; last 'n-1' words
        ctx <- paste (tail (words, i-1), collapse = " ")
        
        # find matching context in the model
        predictions <- model [ context == ctx, list (word, p, n, rank, context)]
        if (nrow (predictions) > 0) {
          
          # exclude any missing predictions
          predictions <- predictions [complete.cases (predictions)]
          
          # only keep the top 'rank' predictions
          predictions <- predictions [rank <= rank]
          
          break
        }
      }
    }
    
    return (predictions)
}


### model build calls pasted

Rprof("ngram1")
model1<-NgramModel(cleanData,1,1,3)
save(model1,file="./model1.RData")
Rprof(NULL)
summaryRprof("ngram1")

Rprof("ngram2")
model2<-NgramModel(cleanData,2,2)
Rprof(NULL)
save(model2,file="./model2.RData")
summaryRprof("ngram2")

Rprof("ngram3")
model3<-NgramModel(cleanData,3,3)
Rprof(NULL)
save(model3,file="./model3.RData")
summaryRprof("ngram3")

## Combine the ngram 1 to 3
mlist<-list(model1,model2, model3)
model<-rbindlist(mlist)

## Quiz question responses
Rprof("PrTry1")
PredictNgram(model,"pound of bacon, a bouquet, and a case of")
PredictNgram(model,"Can you follow me please? It would mean the")
PredictNgram(model,"Hey sunshine, can you follow me and make me the")
PredictNgram(model,"Very early observations on the Bills game: Offense still struggling but the")
PredictNgram(model,"Go on a romantic date at the")
PredictNgram(model,"Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my")
PredictNgram(model,"Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some")
PredictNgram(model,"After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little")
PredictNgram(model,"Be grateful for the good times and keep the faith during the")
PredictNgram(model,"If this isn't the cutest thing you've ever seen, then you must be")
Rprof(NULL)

# rbindlist example for combining the different grams
DT1 = data.table(A=1:3,B=letters[1:3])
DT2 = data.table(A=4:5,B=letters[4:5])
l = list(DT1,DT2)
rbindlist(l)
