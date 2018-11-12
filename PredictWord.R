# Predictive Model Testing

# needed packages
library(tm)
library(ngram)
library(stylo)

# load reference tables
#setwd("C:/Users/Cara/Documents/Coursera Data Science/Class 10 - Capstone/PredictWord ShinyApp/PredictWord")
table2ref <- read.csv(file = "table2ref.csv", encoding="UTF-8",stringsAsFactors = FALSE)
table3ref <- read.csv(file = "table3ref.csv", encoding="UTF-8",stringsAsFactors = FALSE)
table4ref <- read.csv(file = "table4ref.csv", encoding="UTF-8",stringsAsFactors = FALSE)
table5ref <- read.csv(file = "table5ref.csv", encoding="UTF-8",stringsAsFactors = FALSE)

PredictWord <- function(Input) {

        # Clean the input text
        Input <- tolower(Input)
        Input <- removePunctuation(Input)
        Input <- removeNumbers(Input)
        
        # Find word count of input text
        InputCount <- wordcount(Input)
        
        # if word count > 4, trim to keep only the last 4 words
        if (InputCount > 4){
                Input <- txt.to.words.ext(Input, language="English.all", preserve.case = TRUE)
                Input <- Input[(InputCount-3):InputCount]
                Input <- paste(Input,collapse=" ")
        }
        
        # Find word count of (potentially) trimmed input text
        InputCount <- wordcount(Input)
        
        ## if word count = 4, match input to key in table5ref, return value
        if (InputCount == 4){
                prediction <- PredictWord4(Input)
        }  
        
        ## if word count = 3, match input to key in table4ref, return value
        if (InputCount == 3){
                prediction <- PredictWord3(Input)
        }
        
        ## if word count = 2, match input to key in table3ref, return value
        if (InputCount == 2){
                prediction <- PredictWord2(Input)
        }
        
        ## if word count = 1, match input to key in table2ref, return value
        if (InputCount == 1){
                prediction <- PredictWord1(Input)
        }
        
        ## if no input, return nothing
        if (Input == ""){
                prediction <- ""
        }
        
        
        return(prediction)
}


PredictWord4 <- function(Input){
        
        prediction <- subset(table5ref,table5ref$key==Input)$value
        
        # if no match, trim to keep only the last 3 words
        if (length(prediction) == 0){
                Input4 <- txt.to.words.ext(Input, language="English.all", preserve.case = TRUE)
                Input3 <- Input4[2:4]
                Input3 <- paste(Input3,collapse=" ")
                
                #word count = 3, attempt match last 3 words of input to key in table4ref, return value
                prediction <- PredictWord3(Input3)

        }
        return(prediction)
}



PredictWord3 <- function(Input){
        prediction <- subset(table4ref,table4ref$key==Input)$value
        
        # if no match, trim to keep only the last 2 words
        if (length(prediction) == 0){
                Input3 <- txt.to.words.ext(Input, language="English.all", preserve.case = TRUE)
                Input2 <- Input3[2:3]
                Input2 <- paste(Input2,collapse=" ")
                #word count = 2, attempt match last 2 words of input to key in table3ref, return value
                prediction <- PredictWord2(Input2)
        }
        return(prediction)
}


PredictWord2 <- function(Input){
        prediction <- subset(table3ref,table3ref$key==Input)$value 
        
        # if no match, trim to keep only the last word
        if (length(prediction) == 0){
                Input2 <- txt.to.words.ext(Input, language="English.all", preserve.case = TRUE)
                Input1 <- Input2[2]
                #word count = 1, attempt match last word of input to key in table2ref, return value
                prediction <- PredictWord1(Input1)
        }
        return(prediction)
}


PredictWord1 <- function(Input){
        prediction <- subset(table2ref,table2ref$key==Input)$value
        
        # if no match, return most common english word
        if (length(prediction) == 0){
                prediction <- "the"
        }
        return(prediction)
}




