#### Using WordNet #####

## Installation 
# download jre and jdk 
# install.packages
# Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-18/")
# library(rJava)

# install.packages("wordnet")
library("wordnet")

# initalise wordnet dictionary
initDict()
setDict("C:/Program Files (x86)/WordNet/2.1/dict")
Sys.setenv(WNHOME = "C:/Program Files (x86)/WordNet/2.1") 
getDict()


getFilterTypes()


### Plan 
# for word in list 
# gettermfilter("exact match")
# get term --> get synonyms as list 
# option to expand search terms with synonyms 
# text search 
## repeat with synsets
## compare results

# ! - antonym, @ - hypernym, & - similar(adjectives only) ~ - hyponym


## Getting synonymns
# option a
filter <- getTermFilter("ExactMatchFilter", "company", TRUE)
terms <- getIndexTerms("NOUN", 1, filter)
getSynonyms(terms[[1]])


# option b
syn <- synonyms("spouse", "NOUN")

extendterms <-list()
#list of synonyms in vocab
for (el in syn){
  if(el %in% stmdata$vocab){
    print(el)
    extendterms[[length(extendterms) + 1]] <- el}
}

## list of words 

extendTerms <- function (string){
  synonymlist <- list()
  
  for (i in string){
  # print(i)
  #print(synonyms(i, "NOUN"))
    syn <- unlist(synonyms(i, "NOUN"))
    synonymlist[[length(synonymlist) + 1]] <- syn}
  
  print(paste("synonyms", "i", synonymlist))
  extendterms <-list()
  
  for (el in synonymlist){
    if(el %in% stmdata$vocab){
      print(el)
      extendterms[[length(extendterms) + 1]] <- el}
  }
  return(extendterms)
}


extendTerms(teststring)

searchtext <- function(df, terms){
  
  for (i in c(1:length(terms))){
    rows <- grep(terms[[i]], df$feedback, ignore.case =TRUE)
  }
  
  results <- df[rows,]
  return(results)
}

# search data with extended list of terms
tempdf <- searchtext(data, extendterms)


teststring <- c("staff", "spouse", "cancer")
# for a list of words
results <- list()
for (i in teststring){
  # print(i)
  print(synonyms(i, "NOUN"))
  
}


## get similar words - often producing empty lists
# try finding hypernym and then hyponyms 
filter <- getTermFilter("ExactMatchFilter", "spouse", TRUE)
terms <- getIndexTerms("NOUN", 1, filter)
synsets <- getSynsets(terms[[1]])
related <- getRelatedSynsets(synsets[[1]], "@")
sapply(related, getWord)


related2 <- getRelatedSynsets(related[[2]], "~")
sapply(related2, getWord)
similar <- unlist(sapply(related2, getWord))

# similar words that are in vocab
for (el in similar){
  if(el %in% stmdata$vocab){print(el)}
}


# use hypernyms to get the overarching name for terms. 
# Group terms accordingly - or replace terms with hypernymns in text 
# examples:
# " health centre" --> institution establishment --> organisation
# "hospital" --> medical building --> building, edifice
# "doctor" --> medical practitioner --> health professional 
# 'asthma' --> respiratory disease --> disease
filter <- getTermFilter("WildCardFilter", "asthma", TRUE)
terms <- getIndexTerms("NOUN", 1, filter)
synsets <- getSynsets(terms[[1]])
related <- getRelatedSynsets(synsets[[1]], "@")
sapply(related, getWord)

related2 <- getRelatedSynsets(related[[1]], "@")
sapply(related2, getWord)

related3 <- getRelatedSynsets(related[[1]], "@")
sapply(related3, getWord)

## get the hyponymns
# examples 

filter <- getTermFilter("ExactMatchFilter", "medical practitioner", TRUE)
terms <- getIndexTerms("NOUN", 1, filter)
synsets <- getSynsets(terms[[1]])
related <- getRelatedSynsets(synsets[[1]], "~")
# ! - antonym, @ - hypernym, & - similar
# list of the hyponymns
hypon <- unlist(lapply(related, getWord))

# lists can be very long and there may be a second level of hyponyms
# needed to search the vocab in the text.

# hyponymns in vocab
for (el in hypon){
  if(el %in% stmdata$vocab){print(el)}
}


