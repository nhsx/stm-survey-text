#### EVALUATIONS #####
# Comparing the effects of ngrams, sentiment and modified pre-processing. 

source("./src/preprocess_function.R") # preprocessing functions
source("./src/libraries.R")


## Load data
path <- "./data/text_data.csv"
data <- read_csv(path)
  
set.seed(1)
sample <- sample(nrow(data), 1030)
test_set <- data[sample, ]
train_set <- data[-sample, ]

heldsample <- sample(nrow(train_set), 1030)
heldset <- train_set[heldsample, ]
train_set <- train_set[-heldsample, ]

train_set <- na.omit(train_set) 
train_set$criticality[train_set$criticality < -4] <- -4

#number of topics
k <- 25
# k <- 44
# k <- 9
set.seed(123)

### Run and save model 
#  Save semantic coherence, exclusivity, prob and frex words, held-out likelihood (perplexity

### Base - CTAS 
t2 <- corpus(train_set$feedback) # text_field = 'text argument not used so removed
docvars(t2, "doc_id") <- train_set$row_index
docvars(t2) <- train_set
# Clean text to include only alphabetical characters
t2 <- str_replace_all(t2, "[']", "")
t2 <- str_replace_all(t2, "[^[a-z]]", " ")
train_set$question <- str_remove_all(train_set$question, "[Trust ABCDQ-]")
train_set$question <- as.integer(train_set$question)
# Create DFM - document feature matrix
d2 <- dfm(t2, remove = stopwords("en"), stem = TRUE, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE)
docnames(d2) <- train_set$row_index  # set the name of each document as the doc_id
# Convert to stm format 
s2 <- convert(d2, to = "stm") 

#find the missing values
keep <- !is.na(s2$meta)
# #keep only observed values in meta and docs
meta <- s2$meta[keep,]
documents <- s2$documents[keep]
vocabs <- s2$vocab

processed <- textProcessor(train_set$feedback, metadata = train_set)
# #rerun prepDocuments to deal with missing vocab
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta


base_metrics44 <-searchK(documents = docs, vocab = vocab, data = meta,
                       K=k, prevalence=~criticality+organization+question,
                       init = "Spectral", max.em.its = 50)


stm_df_fit <- stm(documents = docs, vocab = vocab, data = meta, K = k,
                  prevalence=~criticality+organization+question, init = "Spectral", max.em.its = 50)
plot(stm_df_fit, n = 5, labeltype = "prob", text.cex = .5)
base_topiclabels <- labelTopics(stm_df_fit)

## Modified - Stemming

train_set_prep <- prep_dataframe(train_set)
stem_text_cleaned <- clean_text(train_set_prep, lemma = FALSE)
stem_stmdata <- convert_to_stm(stem_text_cleaned$DocMatrix, train_set_prep)

stem_metrics44 <-searchK(documents = stem_stmdata$documents, 
                       vocab = stem_stmdata$vocab,
                       data = stem_stmdata$meta,
                       K=k, prevalence=~criticality+organization+question,
                       init = "Spectral", max.em.its = 50)


stem_stm_df_fit <-  stm(documents = stem_stmdata$documents,
                        vocab = stem_stmdata$vocab, 
                        data = stem_stmdata$meta, 
                        K = k,
                        prevalence=~question+organization+criticality,
                        init = "Spectral",
                        max.em.its = 50)

plot(stem_stm_df_fit, n = 5, labeltype = "prob", text.cex = .5)

# stem_exclu <- exclusivity(stem_stm_df_fit)
# stem_semcoh <- semanticCoherence(model=stem_stm_df_fit, documents=stem_stmdata$documents)
stem_topics <- labelTopics(stem_stm_df_fit)
stem_topics$prob
stem_topics$frex


# Modified - Lemmatising
train_set_prep <- prep_dataframe(train_set)
lem_text_cleaned <- clean_text(train_set_prep)
lem_stmdata <- convert_to_stm(lem_text_cleaned$DocMatrix, train_set_prep)

lem_metrics44 <-searchK(documents = lem_stmdata$documents, 
                       vocab = lem_stmdata$vocab,
                       data = lem_stmdata$meta,
                       K=k, prevalence=~criticality+organization+question,
                       init = "Spectral", max.em.its = 50)

lem_stm_df_fit <-  stm(documents = lem_stmdata$documents,
                        vocab = lem_stmdata$vocab, 
                        data = lem_stmdata$meta, 
                        K = k,
                        prevalence=~question+organization+criticality,
                        init = "Spectral",
                        max.em.its = 50)


plot(lem_stm_df_fit, n = 5, labeltype = "prob", text.cex = .5)
# lem_exclu <- exclusivity(lem_stm_df_fit)
# lem_semcoh <- semanticCoherence(model=lem_stm_df_fit, documents=lem_stmdata$documents)
lem_topics <- labelTopics(lem_semcoh)
lem_topics$prob
lem_topics$frex

# Modified - N-grams
ngram_text_cleaned <- clean_text(train_set_prep, ngram = TRUE)
ngram_stmdata <- convert_to_stm(ngram_text_cleaned$DocMatrix, train_set_prep)

ngram_metrics44 <-searchK(documents = ngram_stmdata$documents, 
                      vocab = ngram_stmdata$vocab,
                      data = ngram_stmdata$meta,
                      K=k, prevalence=~criticality+organization+question,
                      init = "Spectral", max.em.its = 50)



ngram_stm_df_fit <-  stm(documents = ngram_stmdata$documents,
                       vocab = ngram_stmdata$vocab, 
                       data = ngram_stmdata$meta, 
                       K = k,
                       prevalence=~question+organization+criticality,
                       init = "Spectral",
                       max.em.its = 50)

plot(ngram_stm_df_fit, n = 5, labeltype = "prob", text.cex = .5)
# ngram_exclu <- exclusivity(ngram_stm_df_fit)
# ngram_semcoh <- semanticCoherence(model=ngram_stm_df_fit, documents=ngram_stmdata$documents)
ngram_topics <- labelTopics(ngram_semcoh)
ngram_topics$prob
ngram_topics$frex


# Sentiment Analysis 

train_sets <- read_csv("./outputs/model outputs/trainsetwithsentiment.csv")
train_sets <- train_sets[,c(2:10)]

train_seta <- filter(train_sets, Sentiment <=-0.05 | Sentiment >= 0.05)

# train_seta <- train_sets$Sentiment[train_sets$Sentiment > 0.05 | train_sets$Sentiment < -0.05] 

sent_train_set_prep <- prep_dataframe(train_seta)
sent_text_cleaned <- clean_text(sent_train_set_prep)
sent_stmdata <- convert_to_stm(sent_text_cleaned$DocMatrix, sent_train_set_prep)

sent_metrics <-searchK(documents = sent_stmdata$documents, 
                        vocab = sent_stmdata$vocab,
                        data = sent_stmdata$meta,
                        K=44, prevalence=~criticality+organization+question,
                        init = "Spectral", max.em.its = 50)

sent_metrics$results$semcoh

sent_stm_df_fit <-  stm(documents = sent_stmdata$documents,
                        vocab = sent_stmdata$vocab, 
                        data = sent_stmdata$meta, 
                        K = k,
                        prevalence=~question+organization+criticality+Sentiment,
                        init = "Spectral",
                        max.em.its = 50)

plot(sent_stm_df_fit, n = 5, labeltype = "prob", text.cex = .5)

# sent_exclu <- exclusivity(sent_stm_df_fit)
# sent_semcoh <- semanticCoherence(model=sent_stm_df_fit, documents=sent_stmdata$documents)
sent_topics <- labelTopics(sent_semcoh)
sent_topics$prob
sent_topics$frex

# plotting semCoh vs exclusivity for each topic for each model.

baseExSem<-as.data.frame(cbind(c(1:25), base_exclu , base_semcoh, "Base"))
stemExSem<-as.data.frame(cbind(c(1:25), stem_exclu, stem_semcoh, "Stem"))
lemExSem<-as.data.frame(cbind(c(1:25), lem_exclu , lem_semcoh, "Lemma"))
ngramExSem<-as.data.frame(cbind(c(1:25), ngram_exclu, ngram_semcoh, "N-gram"))


baseExSem<-as.data.frame(cbind(c(1:25), exclusivity(stm_df_fit) , semanticCoherence(model=stm_df_fit, documents=docs), "Base"))
stemExSem<-as.data.frame(cbind(c(1:25), exclusivity(stem_stm_df_fit), semanticCoherence(model=stem_stm_df_fit, documents= stem_stmdata$documents), "Stem"))
lemExSem<-as.data.frame(cbind(c(1:25), exclusivity(lem_stm_df_fit) , semanticCoherence(model=lem_stm_df_fit, documents=lem_stmdata$documents), "Lemma"))
ngramExSem<-as.data.frame(cbind(c(1:25), exclusivity(ngram_stm_df_fit), semanticCoherence(model=ngram_stm_df_fit, documents=ngram_stmdata$documents), "N-gram"))
sentExSem<-as.data.frame(cbind(c(1:25),exclusivity(sent_stm_df_fit), semanticCoherence(model=sent_stm_df_fit,documents=sent_stmdata$documents), "Sentiment"))



ModsExSem<-rbind(baseExSem, stemExSem, lemExSem, ngramExSem)
colnames(ModsExSem)<-c("K","Exclusivity", "SemanticCoherence", "Model")

ModsExSem$Exclusivity<-as.numeric(as.character(ModsExSem$Exclusivity))
ModsExSem$SemanticCoherence<-as.numeric(as.character(ModsExSem$SemanticCoherence))

ModsExSem

options(repr.plot.width=7, repr.plot.height=7, repr.plot.res=100)

#Plot semantic coherence vs exclusivity
plotexcoer<-ggplot(ModsExSem, aes(SemanticCoherence, Exclusivity, color = Model))+geom_point(size = 2, alpha = 0.7) + 
    labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence")
plotexcoer

# plot semantic coherence
plot(ModsExSem$Model, ModsExSem$SemanticCoherence, ylab= "Semantic Coherence",
     xlab= "Model", main= "Comparing Semantic Coherence",
     col=terrain.colors(4))

# plot exclusvity 
plot(ModsExSem$Model, ModsExSem$Exclusivity, ylab= "Exclusivity",
     xlab= "Model", main= "Comparing Exclusivity",
     col=terrain.colors(4))


# heldouts <- list(base_metrics$results$heldout, stem_metrics$results$heldout, lem_metrics$results$heldout,
#                  ngram_metrics$results$heldout, sent_metrics$results$heldout)
# exclus <- list(base_metrics$results$exclus, stem_metrics$results$exclus, lem_metrics$results$exclus,
#                ngram_metrics$results$exclus, sent_metrics$results$exclus)
# semancoh <- list(base_metrics$results$semcoh, stem_metrics$results$semcoh, lem_metrics$results$semcoh,
#                  ngram_metrics$results$semcoh, sent_metrics$results$semcoh)
# label <- list("Base", "Stemmed", "Lemmatised", "N-grams", "Sentiment")
# 
# metricsdf <- as.data.frame(heldouts, exclus, semancoh, label)
# 
# heldouts

#### Plotting the average heldout, semantic coherence and exclusvity
# to compare models. 

bases44 <- list(base_metrics44$results$heldout, base_metrics44$results$exclus,
              base_metrics44$results$semcoh, "Base")
stemlst44 <- list(stem_metrics44$results$heldout, stem_metrics44$results$exclus,
              stem_metrics44$results$semcoh, "Stemmed")
lemlst44 <- list(lem_metrics44$results$heldout, lem_metrics44$results$exclus,
              lem_metrics44$results$semcoh, "Lemmatised")
ngramlst44 <- list(ngram_metrics44$results$heldout, ngram_metrics44$results$exclus,
              ngram_metrics44$results$semcoh, "N-grams")
sentlst44 <- list(sent_metrics44$results$heldout, sent_metrics44$results$exclus,
              sent_metrics44$results$semcoh, "Sentiment")

metricddf44 <-list(bases44, stemlst44, lemlst44, ngramlst44, sentlst44)
metricddf44 <- data.frame(matrix(unlist(metricddf44), nrow = 5, byrow=TRUE))
colnames(metricddf44)<-c("Heldout log likelihood","Exclusivity", 
                       "SemanticCoherence", "Model")



# plot exclusvity 
plot(metricddf$Model, metricddf$Exclusivity, ylab= "Exclusivity",
     xlab= "Model", main= "Comparing Exclusivity",
     col=terrain.colors(4))

plot(metricddf$Model, metricddf$SemanticCoherence, ylab= "Semantic Coherence",
     xlab= "Model", main= "Comparing Semantic Coherence",
     col=terrain.colors(4))

plot(metricddf$Model, metricddf$`Heldout log likelihood`, ylab= "Held out log likelihood",
     xlab= "Model", ylim =c(),main= "Comparing Heldout",
     col=terrain.colors(4))



bases9 <- list(base_metrics9$results$heldout, base_metrics9$results$exclus,
               base_metrics9$results$semcoh, "Base")
stemlst9 <- list(stem_metrics9$results$heldout, stem_metrics9$results$exclus,
                 stem_metrics9$results$semcoh, "Stemmed")
lemlst9 <- list(lem_metrics9$results$heldout, lem_metrics9$results$exclus,
                lem_metrics9$results$semcoh, "Lemmatised")
ngramlst9 <- list(ngram_metrics9$results$heldout, ngram_metrics9$results$exclus,
                  ngram_metrics9$results$semcoh, "N-grams")
sentlst9 <- list(sent_metrics9$results$heldout, sent_metrics9$results$exclus,
                 sent_metrics9$results$semcoh, "Sentiment")

metricddf9 <-list(bases9, stemlst9, lemlst9, ngramlst9, sentlst9)
metricddf9 <- data.frame(matrix(unlist(metricddf9), nrow = 5, byrow=TRUE))
colnames(metricddf9)<-c("Heldout log likelihood","Exclusivity", 
                        "SemanticCoherence", "Model")



# plot exclusvity 
plot(metricddf$Model, metricddf$Exclusivity, ylab= "Exclusivity",
     xlab= "Model", main= "Comparing Exclusivity",
     col=terrain.colors(4))

plot(metricddf$Model, metricddf$SemanticCoherence, ylab= "Semantic Coherence",
     xlab= "Model", main= "Comparing Semantic Coherence",
     col=terrain.colors(4))

plot(metricddf$Model, metricddf$`Heldout log likelihood`, ylab= "Held out log likelihood",
     xlab= "Model", ylim =c(),main= "Comparing Heldout",
     col=terrain.colors(4))
