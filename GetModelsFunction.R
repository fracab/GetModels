# Arguments
# low= lower number of topics to train a model on 
# up= upper number of topics to train a model on
# out = output from prepDocuments
#formula = formula for topic prevalence covariates (optional)
#formulaContent = formula for topic content covariates (optional)
# init= method of initialization, by default the spectral initialization
# selection = method of selection for the final model, can be "All" (returns all models), "Dispersion" (returns the model with less dispersion), "Random" (random choice), "Min" (model with less topics). "All" by default, "Dispersion" needs K>2. 
# seed= seed for "Random" selection (optional)
#method dispersion won't work with K=2
GetSTModels<-function(low, up, out, formula=NULL, 
                           formulaContent=NULL,
                           init=c("Spectral", "LDA", "Random", "Custom"), selection=c("All","Dispersion","Random","Min"), seed=NA)
{  library(stm)
  library(tidyverse)
  library(furrr)
  library(rPref)
  library(aspace)
  ModelsTibble <- tibble(K = c(low:up)) %>%
    mutate(topic_model = future_map(K, ~stm(documents=out$documents, 
                                            vocab=out$vocab, prevalence= formula, content=formulaContent,
                                            K=., data=out$meta, init.type = init, verbose=FALSE)))
  
  
  AllModelsExclSemCohTibble<-tibble(K = c(low:up)) %>%
    mutate(SemanticCoherence = future_map(ModelsTibble$topic_model,
                                          ~semanticCoherence(model=.,out$documents)), 
           Exclusivity=future_map(ModelsTibble$topic_model,
                                  ~exclusivity(model=.)))
  
  centroidsT <- data.frame(K=c(low:up))%>%
    mutate(SemanticCoherence=future_map(AllModelsExclSemCohTibble$SemanticCoherence,
                                        ~mean(.)),
           Exclusivity=future_map(AllModelsExclSemCohTibble$Exclusivity,
                                  ~mean(.))) 
  
  PrefSemcohExclusLevsT <- psel(centroidsT, high(unlist(SemanticCoherence)) * high(unlist(Exclusivity)),top=nrow(centroidsT))# Pareto frontiers https://journal.r-project.org/archive/2016-2/roocks.pdf
  PrefSemcohExclusTopKT <- PrefSemcohExclusLevsT[PrefSemcohExclusLevsT$.level==1,]#Dominating Pareto frontier 
  
  PrefSemcohExclusLevsT$SemanticCoherence<-unlist(PrefSemcohExclusLevsT$SemanticCoherence)
  PrefSemcohExclusLevsT$Exclusivity<-unlist(PrefSemcohExclusLevsT$Exclusivity)
  
  AllModelsExclSemCohSelT<-AllModelsExclSemCohTibble[AllModelsExclSemCohTibble$K%in%PrefSemcohExclusTopKT[,1],]   
  
  
  #centroids
  ScaledCentroidsT<-data.frame(SemantiCoherenceSc=scale(unlist(AllModelsExclSemCohSelT$SemanticCoherence)),
                               Exclusivitysc=scale(unlist(AllModelsExclSemCohSelT$Exclusivity)),
                               K=unlist(future_map(AllModelsExclSemCohSelT$K,~rep(.,.))))
  
  if (selection=="Random"){
    if (is.na(seed)){
      selected<-sample(ModelsTibble$K,1)
      SelectedModel<-ModelsTibble[ModelsTibble$K==selected,][[2]][[1]]  
    }else { set.seed(seed)
      selected<-sample(ModelsTibble$K,1)
      SelectedModel<-ModelsTibble[ModelsTibble$K==selected,][[2]][[1]]  
    }
  }else if (selection=="Min") {
    SelectedModel<-ModelsTibble[ModelsTibble$K==min(ModelsTibble$K),][[2]][[1]]  
  } else if (selection=="Dispersion") {
    DispersionT<-(future_map(unique(ScaledCentroidsT$K),~calc_sdd(id=.,
                                                                  filename="disp.txt", 
                                                                  points=(ScaledCentroidsT[,c(1,2)][ScaledCentroidsT$K==.,]))))
    
    DispersionT<-bind_rows(DispersionT)
    
    lessDispT<-DispersionT[DispersionT$SDD.area == min(DispersionT$SDD.area),]
    
    SelectedModel<-ModelsTibble[ModelsTibble$K==lessDispT$id,][[2]][[1]]
  }else {
    SelectedModel<-ModelsTibble[[2]]
  }
  
  ParetoPlotT<-ggplot(PrefSemcohExclusLevsT, aes(SemanticCoherence, Exclusivity))+
    geom_point(size = 2.5, alpha = 0.7, aes(colour=as.factor(.level)),show.legend = FALSE) + 
    geom_text(aes(label=K,colour=as.factor(.level)), nudge_x=.001, nudge_y=.005,show.legend = FALSE) +
    geom_step(direction = "vh",aes(colour=as.factor(.level)),show.legend = FALSE)+ 
    labs(x = "Semantic coherence",
         y = "Exclusivity",
         title = "Exclusivity and semantic coherence (model average), with Pareto front line for each level")+
    theme_bw()
  
  AllResultsT<-list(SelectedModel,ParetoPlotT,PrefSemcohExclusLevsT)
  return(AllResultsT)
  
  
}
