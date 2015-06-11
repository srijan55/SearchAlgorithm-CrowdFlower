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

## Check the directory where this script is running from
getwd()

## DATA EXPLORATION
## below path may not be at the right location. user setwd("..path") 
## to set it so that it matches the below path
## load the relevance training and test sets in R
relevance.train <- read_csv("data files\\train.csv\\train.csv")
relevance.test <- read_csv("data files\\test.csv\\test.csv")
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
sum(relevance.train$product_desc == "")

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
  
  corpus
}




