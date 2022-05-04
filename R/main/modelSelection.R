## Model Selection ####

#' Using the processed data from `preprocess.R` 
#' SearchK function is used to selection the model with the optimal number of topics. 
#' To visualise the models using stminsights, the following are included in the saved 
#' file `stm_modelselection.RData`: 
#'     out - a list of the data to produce the model (documents , vocab, meta)
#'     stm model - stm model 
#'     estimateEffects - the stm effects
#' 
#' runinsights() generates an interactive dashboard in browser to visualise stm outputs. 
#'
#' To compare the selected model the semantic coherence and exclusivity scores are plotted.  
#' The selected model is used to label the text with most and second most probable topics. 

source("./R/main/libraries.R")
source("./R/main/preprocess.R")


set.seed(123)

# Data used for stm 
out <- list(documents = stmdata$documents,
            vocab = stmdata$vocab,
            meta = stmdata$meta)

# searchK to find models with k number of topics that performs the best.
K <- c(5,10,15,20,25,30,35,40,45,50)
system.time(kresult <- searchK(out$documents, out$vocab, K, data=out$meta, max.em.its = 50))
plot(kresult)

 
# Evaluating the best models based on k selected from searchk

model20 <-stm(documents = out$documents,
              vocab = out$vocab,
              data = out$meta,
              K = 20,
              prevalence=~question+organization+criticality+sentiment,
              init = "Spectral",
              max.em.its = 50,
              verbose=FALSE)

model25<-stm(documents = out$documents,
             vocab = out$vocab,
             data = out$meta,
             K = 25,
             prevalence=~question+organization+criticality+sentiment,
             init = "Spectral",
             max.em.its = 50,
             verbose=FALSE)

model30<-stm(documents = out$documents,
             vocab = out$vocab,
             data = out$meta,
             K = 30,
             prevalence=~question+organization+criticality+sentiment,
             init = "Spectral",
             max.em.its = 50,
             verbose=FALSE)


## Find effect estimates for each model. 

# 20 topics
question20effect <- estimateEffect(c(1:20) ~ question, model20, out$meta)
organisation20effect <- estimateEffect(c(1:20) ~ organization, model20, out$meta)
criticality20effect <- estimateEffect(c(1:20) ~ criticality, model20, out$meta)
sentiment20effect <- estimateEffect(c(1:20) ~ sentiment, model20, out$meta)

# 25 topics
question25effect <- estimateEffect(c(1:25) ~ question, model25, out$meta)
organisation25effect <- estimateEffect(c(1:25) ~ organization, model25, out$meta)
criticality25effect <- estimateEffect(c(1:25) ~ criticality, model25, out$meta)
sentiment25effect <- estimateEffect(c(1:25) ~ sentiment, model25, out$meta)

# 30 topics
question25effect <- estimateEffect(c(1:30) ~ question, model30, out$meta)
organisation25effect <- estimateEffect(c(1:30) ~ organization, model30, out$meta)
criticality25effect <- estimateEffect(c(1:30) ~ criticality, model30, out$meta)
sentiment25effect <- estimateEffect(c(1:30) ~ sentiment, model30, out$meta)


## Save model and effect data
save.image('./data/stm_modelselection.RData')


# Visualise the outputs of the model using stminsights. use_browser = FALSE
# results in a pop up interactive window instead of opening in a browser.
#
# In the interactive browser, load the saved .Rdata file produces from the file
# modelSelection.R. On the webpage you are able to view the topic contents,
# correlation, document distribution etc.

run_stminsights() 



### Plot semantic coherence vs exclusivity #### 
# The semantic coherence score and exclisivity score the topics in each model
# is plotted to compare the models' performances.

M20ExSem<-as.data.frame(cbind(c(1:20),exclusivity(model20), 
                              semanticCoherence(model=model20,
                                                documents=out$documents),
                              "Mod20"))
M25ExSem<-as.data.frame(cbind(c(1:25),exclusivity(model25), 
                              semanticCoherence(model=model25,
                                                documents=out$documents),
                              "Mod25"))
M30ExSem<-as.data.frame(cbind(c(1:30),exclusivity(model30), 
                              semanticCoherence(model=model30,
                                                documents=out$documents),
                              "Mod30"))

ModsExSem<-rbind(M20ExSem, M25ExSem, M30ExSem)
colnames(ModsExSem)<-c("K","Exclusivity", "SemanticCoherence", "Model")

ModsExSem$Exclusivity<-as.numeric(as.character(ModsExSem$Exclusivity))
ModsExSem$SemanticCoherence<-as.numeric(as.character(ModsExSem$SemanticCoherence))

options(repr.plot.width=7, repr.plot.height=7, repr.plot.res=100)


plotexcoer <-
  ggplot(ModsExSem, aes(SemanticCoherence, Exclusivity, color = Model)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_text(aes(label = K), nudge_x = .05, nudge_y = .05) +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence")

plotexcoer



###  Dataframe of text with metadata, and labelled with topics. ####
# The most and second most probable topics for each each text is added as a new
# column to the dataframe. 

labeldf <- function(model, data, k){
  stmdf <- stm::make.dt(model, meta=data)
  stmdf$`Most Probable Topic` <- apply(stmdf[,2:k], 1, 
                                       function(x){ which(x == sort(x, decreasing = TRUE)[1])})
  stmdf$`Second Most Probable Topic` <- apply(stmdf[,2:k], 1,
                                              function(x){ which(x == sort(x, decreasing = TRUE)[2])})
  return(stmdf)
}


data_labeled <- labeldf(model25, out$meta, 25)
data_labeled <- data_labeled[, 27:ncol(data_labeled)]

