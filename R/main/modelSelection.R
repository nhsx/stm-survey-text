################################################################################
## Title: Model Selection
## Last Update: 14/06/2022
## Version: 1.0
## Developer Contact: analytics-unit@nhsx.nhs.uk
################################################################################

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
set.seed(123)

# Data used for stm 
out <- list(documents = stmdata$documents,
            vocab = stmdata$vocab,
            meta = stmdata$meta)

# searchK to find models with k number of topics that performs the best.
system.time(kresult <- searchK(out$documents, out$vocab, searchTopicNum, data=out$meta, max.em.its = iterations))
plot(kresult)
kresult$results

# Create Prevalence string of user defined variables and sentiment
prevalence <- ""
for (ii in 1:length(prev)){
  prevalence <- paste0(prevalence,prev[ii],"+")
}
prevalence <- paste0(prevalence,"sentiment")
prev <- c(prev,substitute(sentiment),substitute(Response))

# Run and evaluate the best models based on k selected from searchk
for (ii in topicNum) {
  model <- stm(documents = out$documents,
               vocab = out$vocab,
               data = out$meta,
               K = ii,
               prevalence = as.formula(paste("~",prevalence)),
               init = "Spectral",
               max.em.its = iterations,
               verbose=FALSE)
  nam <- paste0("model", ii)
  assign(nam, model)

  ## Find effect estimates for each model. 
  for (jj in 1:(length(prev)-1)){
    estimate <- estimateEffect(as.formula(paste(c(1:ii), "~", prev[[jj]])), 
                               get(paste0("model", ii)),out$meta)
    nam <- paste0(prev[[jj]],"_effect_", ii)
    assign(nam, estimate)
  }
}


#------------ Calculate semantic coherence vs exclusivity ------------
# The semantic coherence score and exclisivity score the topics in each model is
# plotted to compare the models' performances. We use the exclusivity function
# and semanticCoherence function from stm package.
ModsExSem <- data.frame()
for (ii in topicNum){
  exSem <- as.data.frame(cbind(c(1:ii),exclusivity(get(paste0("model", ii))), 
                               semanticCoherence(model=get(paste0("model", ii)),
                                                 documents=out$documents),
                               paste0("Mod",ii)))
  nam <- paste0("M",ii,"ExSem")
  assign(nam, exSem)
  ModsExSem <- rbind(ModsExSem,exSem)
}
colnames(ModsExSem)<-c("K","Exclusivity", "SemanticCoherence", "Model")

ModsExSem$Exclusivity<-as.numeric(as.character(ModsExSem$Exclusivity))
ModsExSem$SemanticCoherence<-as.numeric(as.character(ModsExSem$SemanticCoherence))


# ------------ Dataframe with labelled topics ------------------------ 
# The most and second most probable topics for each each text is added as a new
# column to the dataframe. 
data_labeled <- labeldf(model9, out$meta, 9)
