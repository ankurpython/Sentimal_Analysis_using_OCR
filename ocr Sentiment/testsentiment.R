#install.packages("tidytext")
#install.packages("tidyr")
#install.packages("ggplot2")



# Library Files to be used
library(tidytext)
library(tidyr)
library(dplyr)
library(stringr)
#library(ggplot2)


# Reading the standard positive-negative word libraries
data1 <- get_sentiments("afinn")
data2 <- get_sentiments("bing")
data3 <- get_sentiments("nrc")

# Separating the sentiment column of the dataset
sent2 <- data2$sentiment


# Custom made function for doing sentence by sentence sentiment analysis
sentenceSentiment <- function(dump) {
  
  
  # Cleaning the data for punctuation
  sentence = gsub("[[:punct:]]", "", dump)
  sentence = gsub("[[:cntrl:]]", "", sentence)
  sentence = gsub('\\d+', '', sentence)
  
  
  # Lowering the case
  sentence <- lapply(sentence, tolower)
  
  # Reading as character string
  sentence <- as.character(sentence)
  
  # Splitting the sentence into words
  word <- as.list(strsplit(sentence, '\\s+')[[1]])
  # Finding number of words
  len_word <- length(word)
  
  
  temp_score <- 0
  for (i in 1:len_word) {
    #print(word)
    
    # Checking if word exists in dataset
    t <- word[i] %in% data2$word
    
    if(t==TRUE) {
      
      # Finding index and value of sentiment
      testing <- word[i] == data2$word
      index<- which(testing == TRUE)
      val <- sent2[index]
      
      
      # Updating score based on sentiment
      if(val == "positive") {
        temp_score <- temp_score + 1
      }
      
      else {
        temp_score <- temp_score - 1
      }
      
    }
  }
  
 
  
  return(temp_score)
}


# Setting the Working Directory
setwd("C:/Users/PRAKRIT SETHI/Documents/ocrnew/ocr Sentiment")
projectDir = getwd()

# Read a txt file
testdir = file.path(projectDir, "ocrinput.txt")
my_data <- read.delim(testdir)

# Reading data line by line
data_dump <- readLines(testdir)
#print(data_dump[3])

len <- length(data_dump)
print(len)

for (i in 1:len) {
  
  # Function call
  res <- sentenceSentiment(data_dump[i])
  print(res)
  if(res>0) {
    print(" sentence has positive sentiment.")
  }
  
  else if(res<0) {
    print(" sentence has negative sentiment.")
    
    } 
  
  else {
    print(" sentence has neutral sentiment.")
  }
}

