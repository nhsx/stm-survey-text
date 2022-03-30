list.of.packages <- c("dplyr", "quanteda", "here", "tidyverse", "tibble", "tidyr",
                      "tidytext", "textclean", "textstem", "stringr", "vader",
                      "stm","ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(dplyr)# version 1.0.6
library(quanteda) # version: 3.0.0
library(here) # version 1.0.1
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


