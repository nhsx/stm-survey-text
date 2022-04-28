#### Sentiment Analysis ####
#' This file contains the code to explore sentiment analysis libraries in R. The
#' The sentiment analysis libraries are: VADER, SentimentAnalysis, Tidyverse -
#' Affin, and NRC valence.
#' 

# Load raw data ####

df <- read_csv("~/data/text_data.csv")
df1 <- df

# VADER ####
library(vader)
#example
get_vader("Doctor listened & made me feel comfortable") 


# function runs vader over series of text and produce results in a dataframe 
vader_sa <- vader_df(df$feedback[c(50:55)])


### SentimentAnalysis ####
install.packages("SentimentAnalysis")
library(SentimentAnalysis)

# Analyze a single string to obtain a binary response (positive / negative)
sentiment <- analyzeSentiment("Doctor listened & made me feel comfortable")
convertToBinaryResponse(sentiment)$SentimentGI

sentiment <- analyzeSentiment("This book is horrible, but I love it.")
convertToBinaryResponse(sentiment)$SentimentGI

# sentiment from the different dictionaries 
sentiment$SentimentLM
sentiment$SentimentGI
sentiment$SentimentHE

# convert to positive, negative, neutral 
convertToBinaryResponse(sentiment$SentimentLM)
convertToBinaryResponse(sentiment$SentimentGI)
convertToBinaryResponse(sentiment$SentimentHE)
convertToBinaryResponse(sentiment$SentimentQDAP)

response <- vader_sa$compound
sentiment <- analyzeSentiment(df$feedback[c(50:55)])
compareToResponse(sentiment, response) # response being the gold standard

# VADER SA is most similar to the sentiment QDAP  dictionary

#### Tidytext ####
library(tidytext)
library(dplyr)

maketextdf <- function(df){
  textdf <- tibble(comment = df, text = df)
  textdf <- unnest_tokens(textdf, word, comment) 
  worddf <- textdf %>% count(word) %>% arrange(desc(n))
  
  wordsdf <- inner_join(textdf, worddf, by= "word")
  return(wordsdf)
}

maketextdf(df$feedback)

wordsdf <- inner_join(textdf, worddf, by= "word")
wordsdf

# # Need to download the text dictionaries to use afinn or nrc
# install.packages("textdata") #need to install to use the vocabulary lexicons. 
#get_sentiments("afinn")
#get_sentiments("nrc")


affinsent <- function(df){
  wordsdf <- maketextdf(df)
  wordsdf %>% 
    inner_join(get_sentiments("afinn")) %>%
    group_by(text) %>%
    summarize(sent=mean(value), n=n()) %>%
    arrange(desc(n)) %>%
    head(20) %>%
    arrange(desc(sent)) -> afinndf
  return(afinndf)
}


afinndf # dataframe with each comment labelled witha sentiment score and number 
        # of contributing words/phrases


get_sentiments("nrc")#  the values are the strings " positive" and " negative" 
              # to compare to the other methods will need to convert to 1 and -1 
              # or convert the others to categorises: positive, neutral and negative


# NRC Valence  - positivity/negativity score.
# the above is overcome using the valence score
# download dictionary 
download.file("https://saifmohammad.com/WebDocs/VAD/NRC-VAD-Lexicon-Aug2018Release.zip", destfile="NRCVAD.zip")
unzip("C:/Users/a-lin/Documents/nhsx/preprocessing/NRCVAD.zip")

# create df dictionary
Valencedf <- read.table("NRC-VAD-Lexicon-Aug2018Release/OneFilePerDimension/v-scores.txt", header=F, sep="\t")
names(Valencedf) <- c("word","valence")
vdf <- tibble(Valencedf)
vdf

nrcvalence <- function(df){
  wordsdf <- maketextdf(df)
  wordsdf %>% 
    inner_join(vdf) %>%
    group_by(text) %>%
    summarize(meanvalence=mean(valence), n=n()) %>%
    arrange(desc(n)) %>%
    head(20) %>%
    arrange(desc(meanvalence)) -> vdfdf
  return(vdfdf)
}



###  Compare sentiment analysis results ####
#Create dataframe of text and the scores using the different methods. 
df <- df1[c(100:300),] 

vader_sa <- vader_df(df$feedback)
sentiment_sa <- analyzeSentiment(df$feedback)
affin_sa <- affinsent(df$feedback)
nrc_sa <- nrcvalence(df$feedback)


sa_joined <- tibble(vader_sa, sentiment_sa$WordCount,
                    sentiment_sa$SentimentGI, sentiment_sa$SentimentHE,
                    sentiment_sa$SentimentLM, sentiment_sa$SentimentQDAP)

sa_joined <- inner_join(sa_joined, affin_sa, by= "text")
sa_joined <- inner_join(sa_joined, nrc_sa, by= "text")

sa_joined

df <- df1[c(100:300),] 

vader_sa <- vader_df(df$feedback)
sentiment_sa <- analyzeSentiment(df$feedback)
affin_sa <- affinsent(df$feedback)
nrc_sa <- nrcvalence(df$feedback)

summary(vader_sa$compound)
summary(sentiment_sa$SentimentLM)
summary(nrc_sa$meanvalence)
summary(affin_sa$sent)


plot(sa_joined$compound, sa_joined$`sentiment_sa$SentimentQDAP`, xlab="VADER", ylab="QDAP", main = "VADER vs QDAP")
plot(sa_joined$compound, sa_joined$`sentiment_sa$SentimentLM`, xlab="VADER", ylab="LM", main = "VADER vs LM")
# neutral tendancy between -0.5 and +0.5 vader score
plot(sa_joined$compound, sa_joined$`sentiment_sa$SentimentGI`, xlab="VADER", ylab="GI", main = "VADER vs GI")
# vader has a neutral tendancy compared to GI but Gi is skewed away from negative
plot(sa_joined$compound, sa_joined$`sentiment_sa$SentimentHE`, xlab="VADER", ylab="HE", main = "VADER vs HE")
# HE has a neutral tendancy across all values of vader
plot(sa_joined$compound, sa_joined$sent , xlab="VADER", ylab="AFINN", main = "VADER vs AFINN")



### problem with the length of nrc 
plot(sa_joined$compound, sa_joined$meanvalence , xlab="VADER", ylab="nrc", main = "VADER vs nrc valence")

length(nrc_sa$meanvalence)

## ####
df <- df1 

txt <- gsub("[^[:alnum:][:space:]]","", df1$feedback) 
vader_sa <- vader_df(df1$feedback)
sentiment_sa <- analyzeSentiment(txt) #errors at the number word symbols use gsub to remove non alphanumeric characters
affin_sa <- affinsent(df1$feedback)
nrc_sa <- nrcvalence(df1$feedback)

length(affin_sa$sent)

summary(vader_sa$compound)
length(vader_sa$compound)
summary(sentiment_sa$SentimentLM)
length(sentiment_sa$SentimentLM)
summary(nrc_sa$meanvalence)
summary(affin_sa$sent)
length(affin_sa$sent)

hist(affin_sa$sent)
hist(nrc_sa$meanvalence)
hist(vader_sa$compound)
summary(sentiment_sa$SentimentGI)
summary(sentiment_sa$SentimentHE)
summary(sentiment_sa$SentimentQDAP)

### Sentiment analysis functions #####
#LIBRARIES
library(tidyverse)
library(vader)
library(tidytext)
library(SentimentAnalysis)
library(dplyr)


#Vader 
vader_df()

#SentimentAnalysis 
sentiment <- analyzeSentiment(df$feedback[c(50:55)])
convertToBinaryResponse(sentiment$SentimentLM)
compareToResponse(sentiment, response) 

# tidytext to use afinn or nrc 
maketextdf <- function(df){
  textdf <- tibble(comment = df, text = df)

    textdf <- unnest_tokens(textdf, word, comment) 
  worddf <- textdf %>% count(word) %>% arrange(desc(n))
  
  wordsdf <- inner_join(textdf, worddf, by= "word")
  return(wordsdf)
}

# Afinn
affinsent <- function(df){
  wordsdf <- maketextdf(df)
  wordsdf %>% 
    inner_join(get_sentiments("afinn")) %>%
    group_by(text) %>%
    summarize(sent=mean(value), n=n()) %>%
    arrange(desc(n)) %>%
    arrange(desc(sent)) -> afinndf
  return(afinndf)
}

# NRC 
# download the library via url 
download.file("https://saifmohammad.com/WebDocs/VAD/NRC-VAD-Lexicon-Aug2018Release.zip", destfile="NRCVAD.zip")
unzip("C:/Users/a-lin/Documents/nhsx/preprocessing/NRCVAD.zip")

# create df dictionary
Valencedf <- read.table("NRC-VAD-Lexicon-Aug2018Release/OneFilePerDimension/v-scores.txt", header=F, sep="\t")
names(Valencedf) <- c("word","valence")
vdf <- tibble(Valencedf)
vdf

nrcvalence <- function(df){
  wordsdf <- maketextdf(df)
  wordsdf %>% 
    inner_join(vdf) %>%
    group_by(text) %>%
    summarize(meanvalence=mean(valence), n=n()) %>%
    arrange(desc(n)) %>%
    arrange(desc(meanvalence)) -> vdfdf
  return(vdfdf)
}


#Word cloud of postive and negative words 

#Vader word cloud
textdf <- maketextdf(train_set$feedback)
vader_sent <- vader_df(textdf$word)
  
vader_sent$compound[vader_sent$compound < -0.05] <- -1
vader_sent$compound[vader_sent$compound > 0.05] <- 1
vader_sent1 <- vader_sent[(vader_sent$text != "no"),]

vader_sent2 <- vader_sent1[(vader_sent1$compound == -1 | vader_sent1$compound == 1),]

vader_sent2 %>% count(text, compound, sort = TRUE) %>%
  reshape2::acast(text ~ compound, value.var = "n", fill = 0) %>%
  wordcloud::comparison.cloud(colors = c("gray20", "gray80"),
                              max.words = 100)


## Vader sentiemnt analysis 
# to lower
# n't = true 

#' @param incl_nt defaults to T, indicates whether you wish to incl n't
#'   contractions (e.g., can't) in negation analysis
#' @param neu_set defaults to T, indicates whether you wish to count neutral
#'   words in calculations
#' @importFrom tm stopwords
#'   

lowertext <- train_set$feedback[c(1:1000)]
lowertext <- tolower(lowertext)

vadertext <- vader_df(lowertext, incl_nt = T)
