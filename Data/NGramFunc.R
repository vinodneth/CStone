## Function to Build NGram Model
## This is final version which uses the ngram package and optimized for performance
## Earlier version of the function are available in git with suffix old1 and old2 etc
## RWeka Tokenizer was found to be the slowest. From the heapsize issue observed had prepared 1,2,3 grams 
##  seperately and merged them. use of TDM to get the frequency table was found to be more performant.
##  This version using ngrams has been found to be better. The suggestions of avoiding data frames and
##  few other optimizations have been completed in here


NgramModel <- function (inpTxt,freq_cutoff = 1, rank_cutoff =3, delimiters=NULL){
  ## inpTxt : the cleaned text which is used to prepare the model
  ## freq_cutoff : the value is used to exclude frequencies below this parameter - default is 1
  ## rank_cutoff : the model returned would have only rows with rank entered. default is 3
  ## delimiter : for seperating - NOT IMPLEMENTED. presently runs with " "
  ## Returns - ngram (model) consumed by the prediction function
  
  
  ## Helper functions for NGramModel loaded
  source("HelperFunc.R")
  
  ## Step 1 : Tokenize the text provided using ngram package
  ##          ngram function is used to create for uni, bi & tri gram and then merged
  
  
  #  convert to table with phrase and frequency
  ret_dt<-function(ngrams_phrasetable, cntgram){
    
    ret_dt<-setDT(ngrams_phrasetable)
    colnames(ret_dt)<- c("phrase","frequency","drop")
    ret_dt<-ret_dt[,.(phrase,frequency),][order (frequency, decreasing = TRUE)]
    ret_dt$n<-cntgram
    return(ret_dt)
  }
  
  #  creating n-grams for 1 - 3
  ngram1<-ngram(cleanChar,n=1)
  ngram1<-get.phrasetable(ngram1)
  ngram1<-ret_dt(ngram1,1)
  
  ngram2<-ngram(cleanChar,n=2)
  ngram2<-get.phrasetable(ngram2)
  ngram2<-ret_dt(ngram2,2)
  
  ngram3<-ngram(cleanChar,n=3)
  ngram3<-get.phrasetable(ngram3)
  ngram3<-ret_dt(ngram3,3)
  
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
  ngrams <- ngrams [ frequency >= freq_cutoff, list (phrase, context, word, p, n) ]
  
  # keep only most likely words for each context
  ngrams <- ngrams [ order (context, -p)]
  ngrams [, rank := 1:.N, by = context]
  ngrams <- ngrams [ rank <= rank_cutoff ]
  
  return(ngrams)
}
