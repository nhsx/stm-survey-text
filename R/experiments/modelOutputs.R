#' Exploring the outputs that can be generated with the stm package. 
#' A specified model can be run here or alternatively, a model run 
#' and selected from `modelSelection.R`.
#' 

source("./src/libraries.R")
source("./src/main.R")

# Run Model ####
# Run specified model
k <- 9
stm_df_fit <- stm(documents = stmdata$documents,
                    vocab = stmdata$vocab,
                    data = stmdata$meta,
                    K = k,
                    prevalence=~question+organization+criticality,
                    init = "Spectral",
                    max.em.its = 50,
                    verbose=FALSE)

# Run a model from `modelSelection.R`
#stm_df_fit <- model25

# Model Evaluation and Visualisations ####

## Model Quality 
# Semantic coherence 
semcoh <- semanticCoherence(model=stm_df_fit, documents=stmdata$documents)
semcoh
plot(cbind(1:k), semcoh, xlab = "Topic", ylab= "Semantic Coherence")

# topic exclusivity
exclu <- exclusivity(stm_df_fit)
exclu
plot(cbind(1:k), exclu, xlab = "Topic", ylab= "Exclusivity")


## Topic distribution 
plot(stm_df_fit, n = 10, labeltype = "prob", text.cex = .5)
# Visualise topics in an interactive window
toLDAvis(stm_df_fit, stmdata$documents)


## Topic representation
# Associated topic words 
topWords <- labelTopics(stm_df_fit, n= 15) 
topWords
# Create df of topic words
topics <- data.frame(t(topWords$prob))
colnames(topics) <- paste0("topic", 1:ncol(topics))
topics


# Save labels for the topics
topiclabels <- sageLabels(stm_df_fit)
topiclabels




## Word Cloud 
# of corpus 
cloud(stm_df_fit)

# of each topic
par(mfrow=c(2,2))
for (i in c(1:9)){
cloud(stm_df_fit,i, max.words = 100)
}


## Representative text 

# representative text of a single topic
thought <- findThoughts(stm_df_fit, texts = stmdata$meta$feedback, n=3, topics = 3)[[1]]

# representative text of all the topics
for (i in c(1:k)){
  
  thoughts <- findThoughts(stm_df_fit, texts = stmdata$meta$feedback,
                           n = 5, topics = i)$docs[[1]] # number of docs and text provided don't match in length
  
  pdf(file=paste0("stm_quotes9_", i, ".pdf"))
  plotQuote(thoughts, width = 85, text.cex = 0.6, main = paste0("Topic ", i))
  dev.off()
}


## Topic Correlation 
par(mfrow=c(1,1))
topic.corr <- topicCorr(stm_df_fit, method="simple", cutoff = 0.1, verbose = TRUE)
topic.corr
plot(topic.corr)



# Metadata effect  ####
# Question
stm_df_fit.effect <- estimateEffect(c(1:k) ~ question, stm_df_fit,stmdata$meta)
summary(stm_df_fit.effect)

plot(stm_df_fit.effect, "question", model=stm_df_fit, method="difference", verbose.labels =F,
     cov.value1 = "1", cov.value2 = "2", xlab = "  Q1 ........................... Q2",
     topics = 1:k, printlegend=T, main= "Effect of Question")


stm_df_fit.effect <- estimateEffect(c(1:k) ~ organization, stm_df_fit,stmdata$meta)
summary(stm_df_fit.effect)

# Organisation
plot(stm_df_fit.effect, "organization", model=stm_df_fit, method="difference", verbose.labels =F,
     cov.value1 = "Trust A", cov.value2 = "Trust B", xlab = "   Trust A ........................... Trust B", topics = 1:k, printlegend=T, main= "Effect of Trust")

plot(stm_df_fit.effect, "organization", model=stm_df_fit, method="difference", verbose.labels =F,
     cov.value1 = "Trust A", cov.value2 = "Trust C", xlab = "   Trust A ........................... Trust C", topics = 1:k, printlegend=T, main= "Effect of Trust")

plot(stm_df_fit.effect, "organization", model=stm_df_fit, method="difference", verbose.labels =F,
     cov.value1 = "Trust C", cov.value2 = "Trust B", xlab = "   Trust C .........................Trust B", topics = 1:k, printlegend=T, main= "Effect of Trust")


# Criticality 
stm_df_fit.effect <- estimateEffect(c(1:k) ~ criticality, stm_df_fit, stmdata$meta)
summary(stm_df_fit.effect)

plot(stm_df_fit.effect, "criticality", model=stm_df_fit, method = "continuous", printlegend = TRUE,
     ci.level = 0, topics = 1:k, xlab = "Criticality", main =  "Effect of criticality",
     family = "sans")

# dataframe of effect estimates
stminsights::get_effects(criticality9effect , "criticality", type= "pointestimate")

# # Sentiment
stm_df_fit.effect <- estimateEffect(c(1:k) ~ Sentiment, stm_df_fit, stmdata$meta)
summary(stm_df_fit.effect)
plot(stm_df_fit.effect, "Sentiment", model=stm_df_fit, method = "continuous", printlegend = TRUE,
     ci.level = 0, topics = 1:k, xlab = "Sentiment", main =  "Effect of Sentiment",
     family = "sans")

plot(stm_df_fit.effect, "Sentiment", model=stm_df_fit, method = "difference", printlegend = TRUE,
     ci.level = 0, topics = 1:k, xlab = "Sentiment", main =  "Effect of Sentiment",
     family = "sans", cov.value1 = -1, cov.value2 =1)

plot(stm_df_fit.effect, "Sentiment", model=stm_df_fit, method="difference",
     verbose.labels =F,
     cov.value1 = "-1", cov.value2 = "1",
     xlab = "Sentiment",
     topics = 1:k, printlegend=T, main= "Effect of Sentiment")


# Plot perspective - compare two topics 
plot(stm_df_fit, type = 'perspectives',
     topics = 2:3, labeltype = 'frex',
     covarlevels = unique(train_set$Sentiment),
     text.cex = .75)

par(mfrow=c(1,1))

# Plots MAP estimate of the document-topic loadings - red dashed line = median 
for (i in c(1:k)){
  plot(stm_df_fit, n=3,
       type= "hist",
       covarlevels = unique(train_set$question),
            topics = i, main= "Distribution of MAP Estimates of Document-Topic Proportions")
}
