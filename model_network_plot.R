library(magrittr)
library(dplyr)
library(stringr)
library(igraph)

#Define covariates A, B, and interaction term AB
covariates <- c("A","B","C") 

#This function checks for model dependencies by comparing the strings for each candidate model. For example, is model A+AB and extension of model A?
model_check <- function(p1,p2){
  grep(pattern = p1, x = p2) %>% length
}

#This function pastes the covariates together and collapses with the * (I'm bad at regular expressions, so it's hard to describe the star in this context).
#This is used by the previous function to examine nested models with gaps between covariates.
#E.g. Is Y~A+B+C an extension of Y~A+C?
#Probably not the best comment to describe what this is doing, but just another skill to get better at with practice.
collapse_fun <- function(x){
  paste(x,sep = '',collapse = '*')
  }

#Function for generating model network plot.
model_plot <- function(variables){

model_list <- list()

for(i in 1:length(covariates)){
  model_list[[i]] <- expand.grid(
    combn(x = covariates,
          m = i-1,
          FUN = collapse_fun),
    combn(x = covariates,
          m = i,
          FUN = collapse_fun),
    stringsAsFactors = F
  )
}

model_list <- do.call('rbind',model_list) %>% data.frame

model_list$length <- c(rep(x = NA, length(covariates)),mapply(
  model_check, 
  p1 = model_list$Var1[-c(1:length(covariates))], 
  p2 = model_list$Var2[-c(1:length(covariates))]
) %>% 
  unlist
)

model_list <- rbind(model_list[c(1:length(covariates)),],
      model_list[-c(1:length(covariates)),] %>% filter(length == 1))

model_list <- apply(X = model_list[,-ncol(model_list)], 2, function(x){paste("Y ~",x)}) %>% 
  data.frame %>% 
  apply(X = ., 2, function(x){gsub("\\*", "+",x) }) %>% 
  data.frame %>% 
  graph_from_data_frame(., directed = T)

}

```

# Candidate Model Network
## C = AxB Interaction Term

```{r, echo=T, message=F,warning=F,strip.white=T,tidy=TRUE,results=T,fig.align='center',fig.height=20,fig.width=30}
model_graph <- model_plot(c("A","B","C"))

plot(model_graph,
     layout = layout_in_circle,
     vertex.shape = 'none',
     edge.arrow.size = 0.5,
     vertex.size = 10,
     vertex.label.cex = 5,
     vertex.label.family="Helvetica", 
     edge.width = 10,
     rescale = T,
       asp = 0.8,
       margin = -0.1)
