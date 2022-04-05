source("./src/Preprocessing/libraries.R")
source("./src/Preprocessing/main.R")

## Model Selection ####
#' Using the processed data from main.R searckK function is used to 
#' selction the model with the optimal number of topics. 

set.seed(123)
K <- c(5,10,15,20,25,30,35,40,45, 50, 55, 60, 65, 70)
system.time(kresult <- searchK(stmdata$documents, stmdata$vocab, K, data=stmdata$meta, max.em.its = 50))
plot(kresult)


# Evaluating the best models
model9<-stm(documents = stmdata$documents,
             vocab = stmdata$vocab,
             data = stmdata$meta,
             K = 9,
             prevalence=~question+organization+criticality+Sentiment,
             init = "Spectral",
             max.em.its = 50,
             verbose=FALSE)

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
model44<-stm(documents = stmdata$documents,
            vocab = stmdata$vocab,
            data = stmdata$meta,
            K = 44,
            prevalence=~question+organization+criticality+Sentiment,
            init = "Spectral",
            max.em.its = 50,
            verbose=FALSE)

library(stm)

## model effects
#9 topics
question9effect <- estimateEffect(c(1:9) ~ question, model9, stmdata$meta)
organisation9effect <- estimateEffect(c(1:9) ~ organization, model9, stmdata$meta)
criticality9effect <- estimateEffect(c(1:9) ~ criticality, model9, stmdata$meta)
sentiment9effect <- estimateEffect(c(1:9) ~ Sentiment, model9, stmdata$meta)
# 25 topics
question25effect <- estimateEffect(c(1:25) ~ question, model25, stmdata$meta)
organisation25effect <- estimateEffect(c(1:25) ~ organization, model25, stmdata$meta)
criticality25effect <- estimateEffect(c(1:25) ~ criticality, model25, stmdata$meta)
sentiment25effect <- estimateEffect(c(1:25) ~ Sentiment, model25, stmdata$meta)
# 44 topics
question44effect <- estimateEffect(c(1:44) ~ question, model44, stmdata$meta)
organisation44effect <- estimateEffect(c(1:44) ~ organization, model44, stmdata$meta)
criticality44effect <- estimateEffect(c(1:44) ~ criticality, model44, stmdata$meta)
sentiment44effect <- estimateEffect(c(1:44) ~ Sentiment, model44, stmdata$meta)




save.image('stm_modelselection.RData')

M20ExSem<-as.data.frame(cbind(c(1:20),exclusivity(model20), 
                              semanticCoherence(model=model20, documents=stmdata$documents), "Mod20"))
M25ExSem<-as.data.frame(cbind(c(1:25),exclusivity(model25), 
                              semanticCoherence(model=model25, documents=stmdata$documents), "Mod25"))
M30ExSem<-as.data.frame(cbind(c(1:30),exclusivity(model30), 
                              semanticCoherence(model=model30,documents=stmdata$documents), "Mod30"))

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