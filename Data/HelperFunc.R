## This R file has the helper functions used by NGram and Prediction functions
## 1. split_words       - to split words based on spaces in a given text
## 2. except_last_word  - to get the text without the last word
## 3. last_word         - to get the last word in text


## function 1 : 
split_by_word <- function (text) {
  
  stopifnot (is.character (text))
  
  words <- unlist (strsplit (text, split = "[ ]+"))
  words [nchar (words) > 0]
}

## function 2 :
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

## function 3 : 
last_word <- function (text) {
  
  stopifnot (is.character (text))
  stopifnot (length (text) == 1)
  
  # split the phrase into words
  words <- split_by_word (text)
  
  # the last word only
  words [length (words)]
}