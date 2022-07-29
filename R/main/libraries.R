################################################################################
## Title: Libraries
## Last Update: 14/06/2022
## Version: 1.0
## Developer Contact: analytics-unit@nhsx.nhs.uk
################################################################################

# function to install and load required packages
callLib <- function(){
  
  list.of.packages <- c("dplyr", "quanteda", "here", "tidyverse", "tibble", "tidyr",
                        "tidytext", "textclean", "textstem", "stringr", "vader",
                        "stm","ggplot2", "LDAvis", "servr", "svDialogs")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)

  library(dplyr)# version 1.0.6
  library(quanteda) # version: 3.0.0
  library(tidyverse) # verison 1.3.1
  library(tibble) # version 3.1.1 
  library(tidyr) # version 1.1.3
  
  library(tidytext) # version 0.3.2
  library(textclean) # version 0.9.3
  library(textstem) # version 0.1.4
  library(stringr) # version 1.4.0
  
  library(vader) # version 0.2.1
  library(stm) # version 1.3.6
  library(ggplot2) # version 3.3.5
  
  library(servr) #  version 0.2.4
  library(LDAvis) # version 0.3.2
  library(svDialogs)  # version 1.0.3
  # update.packages()
}


# function to install and load required packages with c compiler
callLib_C <- function(){
  
  list.of.packages.c <- c("igraph", "stminsights","wordnet")
  new.packages.c <- list.of.packages.c[!(list.of.packages.c %in% installed.packages()[,"Package"])]
  if(length(new.packages.c)) install.packages(new.packages.c)
  
  library(igraph) # version 1.2.6
  library(stminsights)  # version 0.4.1
  library(wordnet)
  initDict()
  Sys.setenv(WNHOME = "/usr/local/cellar/wordnet/3.1_1") 
  setDict("/usr/local/cellar/wordnet/3.1_1/dict")
  getDict()
}

