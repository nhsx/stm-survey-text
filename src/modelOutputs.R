source("./src/Preprocessing/libraries.R")
source("./src/Preprocessing/main.R")

## Model Selection 
# Run model with processed data. 

# Select number of topics 
set.seed(123)
K <- c(5,10,15,20,25,30,35,40,45, 50, 55, 60, 65, 70)
system.time(kresult <- searchK(stmdata$documents, stmdata$vocab, K, data=stmdata$meta, max.em.its = 50))
plot(kresult)

kresult %>% select(K, exclusivity, semantic_coherence) %>%
  filter(K %in% c(20, 60, 100)) %>%
  unnest() %>%
  mutate(K = as.factor(K))
ggplot(aes(semantic_coherence, exclusivity, color = K)) +
geom_point(size = 2, alpha = 0.7) +
labs(x = "Semantic coherence",
     y = "Exclusivity",
     title = "Comparing exclusivity and semantic coherence",
     subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity")


suppressWarnings(library(ggplot2))
suppressWarnings(library(plotly))

# Evaluating the best models
model20 <-stm(documents = stmdata$documents,
                     vocab = stmdata$vocab,
                     data = stmdata$meta,
                     K = 20,
                     prevalence=~question+organization+criticality+Sentiment,
                     init = "Spectral",
                     max.em.its = 50,
                     verbose=FALSE)

model25<-stm(documents = stmdata$documents,
             vocab = stmdata$vocab,
             data = stmdata$meta,
             K = 25,
             prevalence=~question+organization+criticality+Sentiment,
             init = "Spectral",
             max.em.its = 50,
             verbose=FALSE)

model30<-stm(documents = stmdata$documents,
                     vocab = stmdata$vocab,
                     data = stmdata$meta,
                     K = 30,
                     prevalence=~question+organization+criticality+Sentiment,
                     init = "Spectral",
                     max.em.its = 50,
                     verbose=FALSE)

M20ExSem<-as.data.frame(cbind(c(1:20),exclusivity(model20), semanticCoherence(model=model20, documents=stmdata$documents), "Mod20"))
M25ExSem<-as.data.frame(cbind(c(1:25),exclusivity(model25), semanticCoherence(model=model25, documents=stmdata$documents), "Mod25"))
M30ExSem<-as.data.frame(cbind(c(1:30),exclusivity(model30), semanticCoherence(model=model30,documents=stmdata$documents), "Mod30"))

ModsExSem<-rbind(M20ExSem, M25ExSem, M30ExSem)
colnames(ModsExSem)<-c("K","Exclusivity", "SemanticCoherence", "Model")

ModsExSem$Exclusivity<-as.numeric(as.character(ModsExSem$Exclusivity))
ModsExSem$SemanticCoherence<-as.numeric(as.character(ModsExSem$SemanticCoherence))

options(repr.plot.width=7, repr.plot.height=7, repr.plot.res=100)

#Plot semantic coherence vs exclusivity
plotexcoer<-ggplot(ModsExSem, aes(SemanticCoherence, Exclusivity, color = Model))+geom_point(size = 2, alpha = 0.7) + 
  geom_text(aes(label=K), nudge_x=.05, nudge_y=.05)+
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence")
plotexcoer


### Run Model ####
k <- 25
stm_df_fit <- model25
exclusivity(stm_df_fit)
semanticCoherence(stm_df_fit)


### Visualisations ####
# Topic distribution 
plot(stm_df_fit, n = 10, labeltype = "prob", text.cex = .5)

# Topic representation - top words, representative comments 
# Print topic words
topWords <- labelTopics(stm_df_fit, n= 20) # generates a set of words describing each topic from the fitted STM object.
# can specify desired number of words and the weight for FREX scoring
topWords
#create df of topic words
topics <- data.frame(t(topWords$prob))
colnames(topics) <- paste0("topic", 1:ncol(topics))
topics

write.csv(topics, "./outputs/Topic Words")

# Word Cloud 
cloud(stm_df_fit) # of corpus

#Plot word clouds for each topic
par(mfrow=c(2,2))
for (i in c(1:25)){
cloud(stm_df_fit,i)
}

#Representative comments
thought <- findThoughts(stm_df_fit, texts = stmdata$meta$feedback, n=3, topics = 3)$docs[[1]]

thought 

for (i in c(1:25)){
  
  thoughts <- findThoughts(stm_df_fit, texts = stmdata$meta$feedback,
                           n = 5, topics = i)$docs[[1]] # number of docs and text provided don't match in length
  
  pdf(file=paste0("stm_quotes25_", i, ".pdf"))
  plotQuote(thoughts, width = 85, text.cex = 0.6, main = paste0("Topic ", i))
  dev.off()
}


# Metadata effect 
# Plots effect with the topic label as the label
# Question
stm_df_fit.effect <- estimateEffect(c(1:k) ~ question, stm_df_fit,stmdata$meta)
summary(stm_df_fit.effect)
write.csv(capture.output(summary(stm_df_fit.effect)), "./outputs/question_effect.csv")
          
plot(stm_df_fit.effect, "question", model=stm_df_fit, method="difference", verbose.labels =F,
     cov.value1 = "1", cov.value2 = "2", xlab = "  Q1 ........................... Q2",
     topics = 1:25, printlegend=T, main= "Effect of Question")

par(mfrow=c(1,1))
#type = hist - plots MAP estimate of the document-topic loadings - red dashed line = median 
plot(stm_df_fit, n=3,
     type= "hist",
     #model=stm_df_fit,
     covarlevels = unique(train_set$question),
     # xlab = "  Q1 ........................... Q2",
     topics = 9, main= "Effect of Question")

plot(stm_df_fit, type = 'perspectives',
     topics = 2:3, labeltype = 'frex',
     covarlevels = unique(train_set$Sentiment),
     text.cex = .75)

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

# Sentiment
stm_df_fit.effect <- estimateEffect(c(1:k) ~ Sentiment, stm_df_fit, stmdata$meta)
summary(stm_df_fit.effect)
write.csv(capture.output(summary(stm_df_fit.effect)), "./outputs/sentiment_effect.csv")
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



for (i in 1:length(colnames(train_set[c(5:9)]))){
  print(i+4)
  print(unique(colnames(train_set))[i+4])
}

# Topic Correlation 
library("igraph")
topic.corr <- topicCorr(stm_df_fit, method="simple", cutoff = 0.1, verbose = TRUE)
topic.corr
plot(topic.corr)


### Model Evaluation #####
# most probable word - human evaluation
labelTopics(stm_df_fit, n= 15)
topWords$prob
topWords$frex

# Semantic coherence ####
semcoh <- semanticCoherence(model=stm_df_fit, documents=stmdata$documents)
# topic exclusivity ####
exclu <- exclusivity(stm_df_fit)

## plot line graph of exclusivity over number of topics
plot(cbind(1:k), exclu, xlab = "Topic", ylab= "Exclusivity")

## plot exclusivity over semantic coherence
plot(cbind(1:k), semcoh, xlab = "Topic", ylab= "Semantic Coherence")
# Perplexity (heldout likelihood) - stm output ####
## line graph 

# vector of each --> plot using ggplot2


