################################################################################
## Title: main
## Last Update: 11/07/2022
## Version: 1.0
## Developer: Anna-Grace Linton & Analytics Unit
## Developer Contact: analytics-unit@nhsx.nhs.uk
################################################################################

library(here) # version 1.0.1
here::here()

# ------------ Load Environment ------------------------
source(here("R","main","libraries.R"))
callLib()
# Separated as the libraries requiring the c compiler can be a bit more awkward
callLib_C()

# ------------ load required functions ------------------------
source(here("R","main","functions.R"))

# ------------ load data and USER INPTUS ------------------------
file <- "text_data.csv"                                                         ## USER INPUT
### Load Data into R ####
data <- read_csv(here("data",file))

# Any Manual data cleaning to be done here
# EXAMPLE
data$criticality[data$criticality < -4] <- -4                                   ## USER INPUT
data$question <- str_remove_all(data$question, "[Trust ABCDQ-]")      

# If any column type needs adapting use data$column <- as.TYPE(data$column)
# EXAMPLE
data$code <- as.factor(data$code)                                               ## USER INPUT
data$label <- as.factor(data$label)                                             ## USER INPUT
data$subcategory <- as.factor(data$subcategory)                                 ## USER INPUT
data$feedback <- as.character(data$feedback)                                    ## USER INPUT
data$criticality <- as.numeric(data$criticality)                                ## USER INPUT
data$organization <- as.factor(data$organization)                               ## USER INPUT
data$question <- as.factor(data$question)                                       ## USER INPUT

# Change the name of the character column of interest as response
data <- data %>% rename(Response = feedback)                                    ## USER INPUT

# Define vaariables to use in prevalence equation                               
prev <- c(substitute(criticality),substitute(organization),substitute(question))## USER INPUT

# Option for additional user set high freq words to remove alongside stop words
highFreq <- c("staff", "helpful", "care", "nothing", "xxxx")                    ## USER INPUT

# iterations will need to be changed depending on the dataset as some models 
# will need more than 50 iterations to converge.
iterations <- 250                                                               ## USER INPUT

# topic numbers to search over
searchTopicNum <- c(5,7,9,11,13) # vector of topic numbers to search over       ## USER INPUT
# topic numbers to model on (update dependent on search)
topicNum <- c(5,7,9)                                                            ## USER INPUT

# ------------ clean and reformat ready for stm ------------
source(here("R","main","preprocess.R"))
write.csv(train_set,here("data","trainsetwithsentiment.csv"))

# ------------ # Run stm ------------------------
source(here("R","main","model.R"))
save.image(here("data","stm_modelselection.RData"))

# ------------ # Visualisations ------------------------
source(here("R","main","visOutputs.R"))

# ------------ Text Search ------------------------
# This searches the feedback comments for that mention terms asked by the 
# user/analyst. This is done through a user prompt that asks the user for the 
# terms to look for in the text. The user can search for multiple terms. A 
# dataframe of the rows for comments that mentions the terms is returned.  The 
# labeled text that is search is `data_labeled` object from `modelSelection.R` 
# file. 
# Currently, this function is not set up for logical searches such as AND, OR 
# and NOT. 

user.input <- dlgInput("Terms to search for (separate by a comma):", 
                       Sys.info()["user"])$res
searchterms <- strsplit(user.input, ", ")
print(searchterms[[1]])

user.similar <- dlgInput("Would you like to look for similar words? (yes/no)", 
                         Sys.info()["user"])$res
user.similar <- tolower(user.similar)

if (user.similar == "yes"){
  for (word in searchterms[[1]]){
    sim <- extendTerms(word)
    searchterms[[length(searchterms) + 1]] <- sim}
} else {searchterms}

# To view the resulting dataframe
viewdf <- searchtext(data_labeled, searchterms[[1]]) 
print(paste("There are", length(viewdf$Response), "results for this search."))
View(viewdf)

print(paste("Most prevalent topic is Topic", 
            names(which.max(table(viewdf$`Most Probable Topic`)))))

barplot(table(viewdf$`Most Probable Topic`), 
        main = "Count of most probable topic", xlab = "Topic", ylab = "Count")
barplot(table(viewdf$`Second Most Probable Topic`), 
        main = "Count of second most probable topic", xlab = "Topic", ylab = "Count")
#barplot(table(viewdf$criticality), 
#        main = "Count of criticality", xlab = "criticality", ylab = "Count")
#barplot(table(viewdf$organization), 
#        main = "Count of organisation", xlab = "Organisation", ylab = "Count")
hist(viewdf$sentiment, main= "Distribution of Sentiment")
