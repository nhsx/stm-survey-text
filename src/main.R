source("./src/preprocess_function.R")
source("./src/libraries.R")

### Load Data into R ####
path <- "./data/text_data.csv"
data <- read_csv(path)

#View data 
View(data)

# Summary statistics for the data  
summary(data)
# Idenitfy columns that contain missing values, then idenitfy the rows with 
# these missing values.
data[rowSums(is.na(data)) > 0, ]

# Summary of text data length
text_length(data$feedback)


# Split data training set and test set. The test set is retained to validate 
#the method at the end of development. 
set.seed(1)
sample <- sample(nrow(data), 1030)
test_set <- data[sample, ]
train_set <- data[-sample, ]


### Sentiment Analysis ####
sentiment <- vader_df(train_set$feedback) 

train_set["Sentiment"] <- as.numeric(sentiment$compound)

write.csv(train_set, "./trainsetwithsentiment.csv")

#train_set <- read_csv("./trainsetwithsentiment.csv")

# Filter comments to look at strong sentiment. 
# To look at positive and negative comments only 
dfa <- train_set[train_set$Sentiment > 0.05 | train_set$Sentiment < -0.05  ] 

### Cleaning and pre-processing data ####

# From the training set, remove rows with NAs values and explicitly specify 
#the data types of variables. This is dataset specific.
# Outliers of the "criticality" variable are imputed to be between -4 and 4
# The variables are categorised as numeric, factors (for categorical data),
#or character strings. 
train_set <- prep_dataframe(train_set)

# The feedback responses ("feedback") is cleaned and pre-processed to be 
# used for the topic modelling. 
# A corpus of is generated and the metadata is re-asscoiated with the text
# The text is made lower case, contractions, such as "won't", are expanded,
# digits and punctuation removed, stopwords removed and text is tokenised.
# The text_cleaned object produced contains text_cleaned$Tokens which is 
# the text tokenised and text_cleaned$DocMatrix, which is the document- 
# feature matrix. 
text_cleaned <- clean_text(train_set)

# The document feature matrix is coverted to a format that is used by stm 
# package.
stmdata <- convert_to_stm(text_cleaned$DocMatrix, train_set)


# Plot of the distribution of tokens per document in the pre-processed text. 
w <- rowSums(text_cleaned$DocMatrix)
barplot(w,
        las = 2,
        ylab = "Number of Tokens", main = "Tokens per Document")

boxplot(w, xlab = "Number of Tokens", horizontal = TRUE, 
        main = "Boxplot of Number of Tokens per Document")


### Run STM ####
# Run model with processed data. 
# This model has not been optimised is an example of how the code will run.
# 5 topics have been selected as the number of topics to idenitfy in this 
# data.
# set.seed(123)
# k <- 25
# 
# # The model uses the metadata from the dataset. Data excluding topic
# # associated data (code, subcategory and label), are used as metadata.
# stm_df_fit <- stm(documents = stmdata$documents,
#                   vocab = stmdata$vocab,
#                   data = stmdata$meta, 
#                   K = k, 
#                   prevalence=~question+organization+criticality+Sentiment,
#                   init = "Spectral", 
#                   max.em.its = 50)
# 

