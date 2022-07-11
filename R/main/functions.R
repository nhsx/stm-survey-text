################################################################################
## Title: preprocess_functions
## Last Update: 11/07/2022
## Version: 1.0
## Developer Contact: analytics-unit@nhsx.nhs.uk
################################################################################

# function to calculate maximum, minimum and average number of words 
text_length <- function(data) {
  word_max <- max(str_count(data, "\\w+"))
  word_min <- min(str_count(data, "\\w+"))
  word_mean <- mean(str_count(data, "\\w+"))
  print(paste("Word count - max:", word_max, "min:", word_min, "mean:", word_mean))
}

# function to removes NAs and corrects data type. 
prep_dataframe <- function(df, filter_sent = FALSE){
  df <- na.omit(df) 
  
  if (filter_sent == TRUE){
    df <- df[(df$sentiment > 0.05) | (df$sentiment < -0.05),] 
  } else {
    df <- df
  }
  return(df)
}

lemmatiser <- function(data, lemma = TRUE){
  #' Standardises words: lemmatises words if lemma = True
  #' stems words in lemma = False
  if (lemma == TRUE){
    out <- lemmatize_strings(data)
  } else {out <- stem_strings(data)
  }
  return(out)
}

clean_text <- function(data, StopWords, mintermfreq=2, lemma = TRUE, ngram = FALSE){
  #' Takes dataframe -> corpus with metadata.
  #' Cleans text: lowercase, expands contractions, removes digits and punctuation, 
  #' tokenises and remove stopwords.
  #' Uses stopwords('en') as default but own list of stopwords can be used
  #' returns cleaned and tokenised text and document frequency matrix.
  
  #t1 <- corpus(data$feedback) 
  t1 <- corpus(data$Response)
  docvars(t1, "doc_id") <- data$row_index
  docvars(t1) <- data
  
  t1 <- t1 %>% tolower()%>% textclean::replace_contraction()
  t2 <- gsub("[^[:alnum:][:space:]]","", t1) 
  t2 <- gsub("[0-9]+", " ", t2)
  t2 <- gsub(" *\\b[[:alnum:]]{1}\\b *", " ", t2)
  
  if(lemma == TRUE){
  t3 <- lemmatiser(t2)
  } else{
  t3 <- lemmatiser(t2,lemma=FALSE)
  }

  if(ngram == TRUE){
    t4 <- quanteda::tokens(t3)
    t4 <- tokens_ngrams(t4, n = c(1,2), concatenator = " ")
  } else{
    t4 <- quanteda::tokens(t3)
  }
  
  # t4 <- quanteda::tokens(t3)
  token <- tokens_remove(t4, pattern = StopWords)
  
  t5 <- dfm(token)
  docfm <- dfm_trim(t5, min_termfreq = mintermfreq) 

  outputs <- list("Tokens" = token, "DocMatrix" = docfm)
  return(outputs)
}

convert_to_stm <- function(dtm, docva){
  #' takes column/list of text and creates document feature matrix (dfm)
  #' removes empty rows and filters docva dataframe to match dfm 
  #' convert to format for the stm package
  #' returns stm formatted data type.
  rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
  dtm.new   <- dtm[rowTotals> 0, ]
  docva <- docva[rowTotals> 0, ] 
  # dtm.new <- dfm(dtm.new)
  out <- quanteda::convert(dtm.new, to = "stm", docvars = docva)
  out$meta <- out$meta[complete.cases(out$meta),]
  out$documents <- out$documents[complete.cases(out$meta)]
  print("Data contained missing values... removing now....")
  out <- prepDocuments(out$documents, out$vocab, out$meta)
  return(out)
}

sentAnalysis <- function(x, df){
  #' @param x is the column with the column with the text
  #' @param df is the dataframe to add sentiment labels to 
  #' @description  Runs VADER sentiment analysis and labelled text
  sentiment <- vader_df(x)
  
  # removes row with NAs
  filteredsentiment <- na.omit(sentiment)
  newdataset1 <-df[-which(is.na(sentiment$compound)),]
  noRemoved <- length(sentiment$text) - length(filteredsentiment$text)
  print(paste("The dataframe now contains", length(sentiment$text) ,"rows.", 
              noRemoved, "rows have been removed."))
  newdataset1["sentiment"] <- as.numeric(filteredsentiment$compound)
  
  return(newdataset1)
}

labeldf <- function(model, data, k){
  # k - the number of topics in the model
  stmdf <- stm::make.dt(model, meta=data)
  stmdf$`Most Probable Topic` <- 
    apply(stmdf[,2:k], 1, function(x){ which(x == sort(x, decreasing = TRUE)[1])})
  stmdf$`Second Most Probable Topic` <- 
    apply(stmdf[,2:k], 1,function(x){ which(x == sort(x, decreasing = TRUE)[2])})
  return(stmdf)
}

readname <- function(prompttext) # prompt user for the text 
{ 
  n <- readline(prompt = prompttext)
  return(n)
}


# prompt user for search terms 
searchtext <- function(df, terms){
  
  for (i in c(1:length(terms))){
    rows <- grep(terms[[i]], df$Response, ignore.case =TRUE)
  }
  
  results <- df[rows,]
  return(results)
}

# get similar words 
extendTerms <- function (string){
  extendterms <- list()
  syn <- unlist(synonyms(string, "NOUN"))
  
  for (el in syn){
    if(el %in% stmdata$vocab){
      extendterms[[length(extendterms) + 1]] <- el}
  }
  
  return(unlist(extendterms))
}
