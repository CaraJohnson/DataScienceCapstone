# Predictive Model Notes

# Setup
install.packages("readr") #need
install.packages("readtext")
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RWeka")
install.packages("ggplot2")
install.packages("ngram")
install.packages("caret") #need
install.packages("data.table") #need
install.packages("quanteda") #need

#library(readtext)
#library(SnowballC)
#library(wordcloud)
#library(RWeka)
#library(ggplot2)
library(ngram)
library(tm)
library(readr)
library(caret)
library(data.table)
library(quanteda)

install.packages("tidyr")
library(tidyr)

# need all of these???

## Load data
setwd("C:/Users/Cara/Documents/Coursera Data Science/Class 10 - Capstone/final/en_US")

file1 <- "en_US.blogs.txt"
file2 <- "en_US.twitter.txt"
file3 <- "en_US.news.txt"

blog <- read_lines(file1)
twitter <- read_lines(file2)
news <- read_lines(file3)

# take a random sample from each object
set.seed(123)
# take 1% of total lines for each source
blog <- sample(blog,length(blog)*0.01, replace=FALSE)
twitter <- sample(twitter,length(twitter)*0.01, replace=FALSE)
news <- sample(news,length(news)*0.01, replace=FALSE)

# compile
data1 <- c(blog,twitter,news)

## Split the dataset into training and testing sets
# use 60% training, 40% test
inTrain <- createDataPartition(seq_len(NROW(data1)),p=.6,list=FALSE)
Train <- data1[inTrain]
Test <- data1[-inTrain]
# count lines/words in Test/Train datasets, summarize


# need to clean data better???

# tokenize the data
TrainTokens <- tokens(char_tolower(Train),remove_numbers=TRUE,remove_punct=TRUE,remove_symbols=TRUE,remove_twitter=TRUE,remove_hyphens=TRUE,remove_url=TRUE)

# create ngrams
OneGram <- tokens_ngrams(TrainTokens,n=1,skip=0,concatenator = " ")
TwoGram <- tokens_ngrams(TrainTokens,n=2,skip=0,concatenator = " ")
TriGram <- tokens_ngrams(TrainTokens,n=3,skip=0,concatenator = " ")
FourGram <- tokens_ngrams(TrainTokens,n=4,skip=0,concatenator = " ")
FiveGram <- tokens_ngrams(TrainTokens,n=5,skip=0,concatenator = " ")

dfmOne <- dfm(OneGram,remove=stopwords("english"),stem=TRUE)
dfmTwo <- dfm(TwoGram,remove=stopwords("english"),stem=TRUE)
dfmThree <- dfm(TriGram,remove=stopwords("english"),stem=TRUE)
dfmFour <- dfm(FourGram,remove=stopwords("english"),stem=TRUE)
dfmFive <- dfm(FiveGram,remove=stopwords("english"),stem=TRUE)

tempOne <- textstat_frequency(dfmOne)
tempTwo <- textstat_frequency(dfmTwo)
tempThree <- textstat_frequency(dfmThree)
tempFour <- textstat_frequency(dfmFour)
tempFive <- textstat_frequency(dfmFive)

table1 <- data.table(ngram=tempOne$feature, frequency=tempOne$frequency)
table2 <- data.table(ngram=tempTwo$feature, frequency=tempTwo$frequency)
table3 <- data.table(ngram=tempThree$feature, frequency=tempThree$frequency)
table4 <- data.table(ngram=tempFour$feature, frequency=tempFour$frequency)
table5 <- data.table(ngram=tempFive$feature, frequency=tempFive$frequency)

table2 <- table2 %>% separate(ngram,into=c("key","value"),sep = "\\s(?=[\\S]+$)", remove = F)
table3 <- table3 %>% separate(ngram,into=c("key","value"),sep = "\\s(?=[\\S]+$)", remove = F)
table4 <- table4 %>% separate(ngram,into=c("key","value"),sep = "\\s(?=[\\S]+$)", remove = F)
table5 <- table5 %>% separate(ngram,into=c("key","value"),sep = "\\s(?=[\\S]+$)", remove = F)


install.packages("RSQLite")
library(RSQLite)
# RSQLite is covered in sqldf
#install.packages("sqldf")
#library(sqldf)
#install.packages("DBI")
#library(DBI)

#db <- dbConnect(SQLite(), dbname="Table2.sqlite")

con <- dbConnect(RSQLite::SQLite(), ":memory:")
# copy data table into sql db
#dbWriteTable(con, "table1", table1, row.names = FALSE)
dbWriteTable(con, "table2", table2, row.names = FALSE)
dbWriteTable(con, "table3", table3, row.names = FALSE)
dbWriteTable(con, "table4", table4, row.names = FALSE)
dbWriteTable(con, "table5", table5, row.names = FALSE)

dbListTables(con)

# Testing
# Fetch results from a query:
res <- dbSendQuery(con, "SELECT * FROM table2 WHERE key = 'today' ")
res <- dbSendQuery(con, "SELECT * FROM table2 WHERE key = 'this' ")
res <- dbSendQuery(con, "SELECT value FROM table2 WHERE key = 'of' ")
res <- dbSendQuery(con, "SELECT * FROM table3 WHERE key = 'i want' ")

dbFetch(res)
head(dbFetch(res))

dbClearResult(res)


# Goal: use sql qry to create a table with the most common value for each key (for each ngram)
# save this table, use as lookup for shiny app

res <- dbSendQuery(con, "SELECT * FROM table2 GROUP BY key HAVING frequency = MAX(frequency) ")
table2ref <- dbFetch(res)
dbClearResult(res)

res <- dbSendQuery(con, "SELECT * FROM table3 GROUP BY key HAVING frequency = MAX(frequency) ")
table3ref <- dbFetch(res)
dbClearResult(res)

res <- dbSendQuery(con, "SELECT * FROM table4 GROUP BY key HAVING frequency = MAX(frequency) ")
table4ref <- dbFetch(res)
dbClearResult(res)

res <- dbSendQuery(con, "SELECT * FROM table5 GROUP BY key HAVING frequency = MAX(frequency) ")
table5ref <- dbFetch(res)
dbClearResult(res)

# checking
dim(table3ref)
length(unique(table3$key))

# disconnect from the database
dbDisconnect(con)

# save table1, table2ref-table5ref as csv, then load into shiny app/prediction function
# use this reference data table for the prediction function

# save to csv
setwd("C:/Users/Cara/Documents/Coursera Data Science/Class 10 - Capstone/DataForShiny")
write.csv(table1,"table1.csv",row.names=FALSE)
write.csv(table2ref,"table2ref.csv",row.names=FALSE)
write.csv(table3ref,"table3ref.csv",row.names=FALSE)
write.csv(table4ref,"table4ref.csv",row.names=FALSE)
write.csv(table5ref,"table5ref.csv",row.names=FALSE)

## Prediction Function Outline ##
# see PredictWordTesting.R

# clean input text (tolower, remove punct, remove numbers, etc)
# 
# find word count of text input
# 
# if word count > 4, trim to keep last 4 words
# 
# if word count = 4, match input to key in table5, return value
# if no match, attempt match last 3 words of input to key in table4, return value
# if no match, attempt match last 2 words of input to key in table3, return value
# if no match, attempt match last word of input to key in table2, return value
# 
# if word count = 3, match input to key in table4, return value
# if no match, attempt match last 2 words of input to key in table3, return value
# if no match, attempt match last word of input to key in table2, return value
# 
# if word count = 2, match input to key in table3, return value
# if no match, attempt match last word of input to key in table2, return value
# 
# if word count = 1, match input to key in table2, return value
# if no match, return generic word (most common word in table1/most common word in english)


# Test the function
Input <- c("where are you")
PredictWord(Input)

Input <- c("The guy in front of me just bought a pound of bacon, a bouquet, and a case of")
PredictWord(Input)

Input <- c("You're the reason why I smile everyday. Can you follow me please? It would mean the")
PredictWord(Input)

# test this
Input <- c("Hey sunshine, can you follow me and make me the")
PredictWord(Input)
Input <- c("make me the")
PredictWord(Input)

Input <- c("Very early observations on the Bills game: Offense still struggling but the")
PredictWord(Input)
Input <- c("struggling but the")
PredictWord(Input)

Input <- c("Go on a romantic date at the")
PredictWord(Input)

Input <- c("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my")
PredictWord(Input)






