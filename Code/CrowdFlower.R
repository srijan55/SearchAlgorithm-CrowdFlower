###################################################################################
## This code is part of the Kaggle competition "Search Results Relevance"
## Copyright (C) 2015

## Objective: Cleaning of the train and test data sets
## Data source: Crowdflower data on search queries and their results
## install tm package if not already installed install.packages("tm")
## install package readr install.packages("readr")
###################################################################################
library(readr)
library(tm)
library(SnowballC)
library(stringr)

## Check the directory where this script is running from
getwd()

## DATA EXPLORATION
## below path may not be at the right location. user setwd("..path") 
## to set it so that it matches the below path
## load the relevance training and test sets in R
relevance.train <- read_csv("data files/train.csv/train.csv")
relevance.test <- read_csv("data files/test.csv/test.csv")
## See the first row of each to get a peek at the data
head(relevance.train,1)
head(relevance.test,1)

## explore the data set
str(relevance.train)
str(relevance.test)
summary(relevance.train)
summary(relevance.test)

## Remove any NA rows for median_relevance and relevance_variance
relevance.train <- relevance.train[complete.cases(relevance.train$median_relevance),]
relevance.train <- relevance.train[complete.cases(relevance.train$relevance_variance),]

## Replace the product descriptions with empty strings to <blank>
## Get a count first
## sum(relevance.train$product_desc == "")

## TODO: Replace the product descriptions with empty string with <blank>

CleanTextData <- function(documents)
{
  # Create a corpus of documents
  corpus <- Corpus(VectorSource(documents))
  # Convert to lowercase
  corpus <- tm_map(corpus, content_transformer(tolower))
  # Remove punctuation
  corpus <- tm_map(corpus, removePunctuation)
  # Remove English stop words
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  # Strip whitespace
  corpus <- tm_map(corpus, stripWhitespace)
  # Stem the words
  corpus <- tm_map(corpus, stemDocument)
  #TODO : Consider stem completion at some point in time but need to analyze the data after that to ensure
  #       there are no surprises
  
  corpus.dataframe<- data.frame(text=unlist(sapply(corpus, '[',"content")),stringsAsFactors=F)
  corpus.dataframe
}

# Fuction to take id, description and query_string and find full match (both/none/id/description)
FindFullMatch <- function(query, title, desc)
{
  returnValue = "both";
  ## for( i in length(strsplit(query, " ")))
    matchFoundTitle = pmatch(as.vector(query),as.vector(title), 0)
    matchFoundDesc = pmatch(as.vector(query),as.vector(desc), 0)
    if(matchFoundTitle != 0L && matchFoundDesc  == 0L)
    {
      ##break;
      returnValue = "title"
    }
    if(matchFoundTitle == 0L && matchFoundDesc  != 0L)
    {
      ##break;
      returnValue = "desc"
    }
    if(matchFoundTitle == 0L && matchFoundDesc  == 0L)
    {
      ##break;
      returnValue = "none"
    }  
  
  return(returnValue)
}


CompareTwoVectors <- function(query, document)
{
  
  #print(query)
  # print(document)
  
  train.extendquery <- unique(unlist(strsplit(query," ")))
  train.extendeddoc <- unique(unlist(strsplit(document," ")))
  
  # print(train.extendquery)
  # print(train.extendeddoc)
  #totalquerywordcount<-length(train.extendedquery)
  
  #docmatch <- train.extendquery[train.extendquery %in% train.extendeddoc]
  #docmatch <- train.extendquery[train.extendquery %in% train.extendeddoc] 
  docmatch <- intersect(train.extendquery,train.extendeddoc)
  
  #print(docmatch)
  as.vector(length(docmatch))
  
  #docmatch
  
  # descmatch <- train.extendedquery[train.extendedquery %in% train.extendeddesc] 
}

CompareVectors <- function(data)
{
  
  #print(query)
  # print(document)
  
  
  train.extendquery <- unique(unlist(strsplit(data[,1]," ")))
  train.extendeddoc <- unique(unlist(strsplit(data[,2]," ")))
  
  # print(train.extendquery)
  # print(train.extendeddoc)
  #totalquerywordcount<-length(train.extendedquery)
  
  #docmatch <- train.extendquery[train.extendquery %in% train.extendeddoc]
  #docmatch <- train.extendquery[train.extendquery %in% train.extendeddoc] 
  docmatch <- intersect(train.extendquery,train.extendeddoc)
  
  #print(docmatch)
  length(docmatch)
  
  #docmatch
  
  # descmatch <- train.extendedquery[train.extendedquery %in% train.extendeddesc] 
}
train.query <- CleanTextData(relevance.train$query)
#gsub(pattern = "\\", replacement = "", train.query = train.query, ignore.case = T)
train.producttitle <- CleanTextData(relevance.train$product_title)
train.productdesc <- CleanTextData(relevance.train$product_description)

#change the column names and remove the first column
train <- data.frame(query=train.query$text, title=train.producttitle$text, description=train.productdesc$text, stringsAsFactors = FALSE)


## Unit Test
test1 <- FindFullMatch("a b","a b c", "a b f") #both
test2 <- FindFullMatch("a b","a b c", " a d f") #title

test3 <- FindFullMatch("a b","a c", "a b d f") # desc
test4 <- FindFullMatch("a b","a d c", " b d f") # none

tt <- mapply(CompareTwoVectors, train$query, train$title)

#how to call comparevectors
#test = relevance.train[,c("query", "product_title")]
#testQueries = lapply(test, CompareVectors)