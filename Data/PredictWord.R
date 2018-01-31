
####################################################################
##### Function to use the input text and predict next  
#####   word using the Ngram Model
###################################################################

## Main Function
PredictNgram <- function (model, newtxt, rank = 5){
  ## model    : the NGram model prepared by NgramModel function above
  ## newtxt   : text input to function to predict next word
  ## rank     : limits the list of probables, defaulted to 5
  
  #load libraries
  library(data.table)
  library(stringi)
  library(tm)
  
  ## source helper functions
  ##source("../data/HelperFunc.R")
  
  
  ## Step 1 : Cleanse newtxt
  newtxt <- tolower(newtxt)
  newtxt <- removePunctuation(newtxt)
  newtxt <- removeNumbers(newtxt)
  newtxt <- stripWhitespace(newtxt)
  ##newtxt <- stemDocument(newtxt)
  ##newtxt<-removeWords(newtxt,stopwords(language = c("en")))  
  
  ## Step 2 : 
  # split text to words
  words <- split_by_word (newtxt)
  
  predictions <- NULL
  ## iterates for the unique n (ngram values - in this case 3,2,1
  for (i in sort (unique(model$n), decreasing = TRUE)) {  
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

PredictDebug <-function(model,text) {
  load(file="../data/tmp.RData")
  rettxt <- tmp
  return (rettxt)
}
