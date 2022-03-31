source("./src/Preprocessing/libraries.R")
source("./src/Preprocessing/main.R")


### Run Model ####
k <- 9
stm_df_fit <- stm(documents = stmdata$documents,
                    vocab = stmdata$vocab,
                    data = stmdata$meta,
                    K = k,
                    prevalence=~question+organization+criticality,
                    init = "Spectral",
                    max.em.its = 50,
                    verbose=FALSE)

### ### Model Evaluation and Visualisations ####
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

write.csv(topics, "./outputs/Topic Words.csv")

# Save labels for the topics
topiclabels <- sageLabels(stm_df_fit)
topiclabels

write.csv(topiclabels, "./outputs/Topic Labels.csv")


## Word Cloud 
# of corpus 
cloud(stm_df_fit)
# of each topic
par(mfrow=c(2,2))
for (i in c(1:9)){
cloud(stm_df_fit,i, max.words = 100)
}


## Representative comments
thought <- findThoughts(stm_df_fit, texts = stmdata$meta$feedback, n=3, topics = 3)[[1]]

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

## Dataframe of text, metadata, and topics. 
stm_df <- make.dt(sent_stm_df_fit, meta = sent_stmdata$meta) 
# Add columns for first and second most prevalent topics in each doc
stm_df[, "Top topic"] <- apply(stm_df[, 2:16], 1, which.max) 
stm_df[, "Second topic"] <- apply(stm_df[, 2:16], 1, function(x){names(which(x == sort(x)[3]))[1]})
stm_df[, "Second topic"]<- sub("Topic", "", stm_df$"Second topic")

stm_df[, "Top topic value"] <- apply(stm_df[, 2:16], 1, max)
maxn <- function(n) function(x) order(x, decreasing = TRUE)[n]
stm_df[, "Second topic value"] <- apply(stm_df[, 2:16], 1, function(x)x[maxn(2)(x)])

#### Metadata effect  ####
# Question
stm_df_fit.effect <- estimateEffect(c(1:k) ~ question, stm_df_fit,stmdata$meta)
summary(stm_df_fit.effect)
write.csv(capture.output(summary(stm_df_fit.effect)), "./outputs/question_effect.csv")
          
plot(stm_df_fit.effect, "question", model=stm_df_fit, method="difference", verbose.labels =F,
     cov.value1 = "1", cov.value2 = "2", xlab = "  Q1 ........................... Q2",
     topics = 1:k, printlegend=T, main= "Effect of Question")


stm_df_fit.effect <- estimateEffect(c(1:k) ~ organization, stm_df_fit,stmdata$meta)
summary(stm_df_fit.effect)
write.csv(capture.output(summary(stm_df_fit.effect)), "./outputs/organization_effect.csv")

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
write.csv(capture.output(summary(stm_df_fit.effect)), "./outputs/criticality_effect.csv")

plot(stm_df_fit.effect, "criticality", model=stm_df_fit, method = "continuous", printlegend = TRUE,
     ci.level = 0, topics = 1:k, xlab = "Criticality", main =  "Effect of criticality",
     family = "sans")


# # Sentiment
# stm_df_fit.effect <- estimateEffect(c(1:k) ~ Sentiment, stm_df_fit, stmdata$meta)
# summary(stm_df_fit.effect)
# write.csv(capture.output(summary(stm_df_fit.effect)), "./outputs/sentiment_effect.csv")
# plot(stm_df_fit.effect, "Sentiment", model=stm_df_fit, method = "continuous", printlegend = TRUE,
#      ci.level = 0, topics = 1:k, xlab = "Sentiment", main =  "Effect of Sentiment",
#      family = "sans")
# 
# plot(stm_df_fit.effect, "Sentiment", model=stm_df_fit, method = "difference", printlegend = TRUE,
#      ci.level = 0, topics = 1:k, xlab = "Sentiment", main =  "Effect of Sentiment",
#      family = "sans", cov.value1 = -1, cov.value2 =1)
# 
# plot(stm_df_fit.effect, "Sentiment", model=stm_df_fit, method="difference",
#      verbose.labels =F,
#      cov.value1 = "-1", cov.value2 = "1", 
#      xlab = "Sentiment",
#      topics = 1:k, printlegend=T, main= "Effect of Sentiment")


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
