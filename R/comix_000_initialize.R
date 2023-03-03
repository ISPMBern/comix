#' ---
#' title: "Initialize"
#' author: "mreichmuth"
#' date: "19/10/2021"
#' ---

#' Load packages (make sure you installed all packages!)
require(tidyverse)
require(knitr)
library(tinytex)
require(lubridate)
require(cowplot)
require(sf)
require(mice)
require(foreign)
require(reshape2)
library(xlsx)
library(downloader)
library(httr)
library(rmarkdown)
library(knitr)
library(RColorBrewer)
library(ggplot2)
library(gridExtra)
library(grid)
library(dplyr)
library(patchwork)
library(socialmixr)
library(kableExtra)
library(sf)# =simple feature, to deal with spatial vectors,
library(ggrepel)# for geographic maps
library(table1) 
library(survey)
library(forestplot)
library(lme4)  # Fitting Generalized Linear Mixed-Effects Models
library(parameters)
library(splines)
library(naniar)
library(riskRegression)
library(lmtest)
library(survminer)


# Paths
path_function = "R/"
path_saveoutput = "output/"

setwd("./data_cleaning")
# run functions to use
files_functions = list.files(getwd(),full.names = TRUE, include.dirs = TRUE)
files_functions = files_functions[!grepl(pattern = "000|backup",files_functions)]
sapply(files_functions, source)
rm(files_functions)

setwd("../vaccination_uptake")
# run functions to use
files_functions = list.files(getwd(),full.names = TRUE, include.dirs = TRUE)
files_functions = files_functions[!grepl(pattern = "000|backup",files_functions)]
sapply(files_functions, source)
rm(files_functions)


from_cantons_to_great_regions <- function(x){
  if(x== "AG"){return("Northwestern Switzerland")}
  else if(x== "AI"){return("Eastern Switzerland")}
  else if(x== "AR"){return("Eastern Switzerland")}
  else if(x== "BE"){return("Espace Mittelland")}
  else if(x== "BL"){return("Northwestern Switzerland")}
  else if(x== "BS"){return("Northwestern Switzerland")}
  else if(x== "FR"){return("Espace Mittelland")}
  else if(x== "GE"){return("Lake Geneva region")}
  else if(x== "GL"){return("Eastern Switzerland")}
  else if(x== "GR"){return("Eastern Switzerland")}
  else if(x== "JU"){return("Espace Mittelland")}
  else if(x== "LU"){return("Central Switzerland")}
  else if(x== "NE"){return("Espace Mittelland")}
  else if(x== "NW"){return("Central Switzerland")}
  else if(x== "OW"){return("Central Switzerland")}
  else if(x== "SG"){return("Eastern Switzerland")}
  else if(x== "SH"){return("Eastern Switzerland")}
  else if(x== "SO"){return("Espace Mittelland")}
  else if(x== "SZ"){return("Central Switzerland")}
  else if(x== "TG"){return("Eastern Switzerland")}
  else if(x== "TI"){return("Ticino")}
  else if(x== "UR"){return("Central Switzerland")}
  else if(x== "VD"){return("Lake Geneva region")}
  else if(x== "VS"){return("Lake Geneva region")}
  else if(x== "ZG"){return("Central Switzerland")}
  else if(x== "ZH"){return("Zurich")}
}



#' Graphics
theme_set(theme_bw())
col_9 <- (brewer.pal(9,"Set1"))
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

FUN_sig <- function(x){
  if (is.na(x)){return(" ")}
  else if (x =="NA"){return(" ")}
  else if (x ==" "){return(" ")}
  else if (as.numeric(x) < 0.0001) {return("<.0001")}
  else if (as.numeric(x) < 0.001) {return(round(as.numeric(x),5))}
  else if (as.numeric(x) <= 0.01) {return(round(as.numeric(x),4))}
  else if (as.numeric(x) > 0.01) {return(round(as.numeric(x),4))}
}

