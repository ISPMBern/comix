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
library(readxl)
library(EpiNow2)


# Paths
path_function = "R/"
path_saveoutput = "output/"

setwd("./data_cleaning")
# run functions to use
files_functions = list.files(getwd(),full.names = TRUE, include.dirs = TRUE)
files_functions = files_functions[!grepl(pattern = "000|backup",files_functions)]
sapply(files_functions, source)
#rm(files_functions)

setwd("../data_processing/vaccination_uptake")
# run functions to use
files_functions = list.files(getwd(),full.names = TRUE, include.dirs = TRUE)
files_functions = files_functions[!grepl(pattern = "000|backup|/README",files_functions)]
sapply(files_functions, source)
rm(files_functions)

setwd(path_name)
setwd("./data_processing/contact_matrices")
# run functions to use
files_functions = list.files(getwd(),full.names = TRUE, include.dirs = TRUE)
files_functions = files_functions[!grepl(pattern = "000|backup|/README",files_functions)]
sapply(files_functions, source)
rm(files_functions)

setwd(path_name)


ch_cantons <- c("CH", "AG","AI","AR","BE","BL","BS","FR","GE",
                "GL","GR", "JU","LU","NE","NW","OW","SG","SH",
                "SO","SZ","TG",  "TI","UR","VD","VS","ZG","ZH")

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

from_cantons_to_abrev <- function(x){
  if(grepl("Aargau|Argovie|AG|Ag",x)){return("AG")}
  else if(grepl("Basel-Land|BL|Bl",x)){return("BL")}
  else if(grepl("Basel-Stadt|Basel|BS|Bs",x)){return("BS")}
  else if(grepl("Fribourg|Freiburg|FR|Ft",x)){return("FR")}
  else if(grepl("Genf|Geneva|Geneva|GE|Ge",x)){return("GE")}
  else if(grepl("Glarus|GL|Gl",x)){return("GL")}
  else if(grepl("Jura|JU|Ju",x)){return("JU")}
  else if(grepl("Lucerne|Luzern|LU|Lu",x)){return("LU")}
  else if(grepl("Nidwalden|NW|Nw",x)){return("NW")}
  else if(grepl("Obwalden|Obwald|OW|Ow",x)){return("OW")}
  else if(grepl("Schaffhausen|Schaffhouse|SH|Sh",x)){return("SH")}
  else if(grepl("Solothurn|Olten|SO|So",x)){return("SO")}
  else if(grepl("Schwyz|Schwytz|SZ|Sz",x)){return("SZ")}
  else if(grepl("Thurgau|Turgovia|Thurgovie|TG|Tg",x)){return("TG")}
  else if(grepl("Ticino|Tessin|TI|Ti",x)){return("TI")}
  else if(grepl("Uri|UR|Ur",x)){return("UR")}
  else if(grepl("Vaud|Waadt|VD|Vd|Lausanne|Yverdon-les-Bains",x)){return("VD")}
  else if(grepl("Valais|Wallis|VALAIS|VS|Vs|Sion",x)){return("VS")}
  else if(grepl("Zug|ZG|Zg|Zoug",x)){return("ZG")}
  else if(grepl("Zürich|Zurich|Zaerich|Zoerich|Zuerich|ZH|Zh|Z\xfcrich",x)){return("ZH")}
  else if(grepl("Neuchâtel|Neuenburg|Neuchã¢Tel|NE|Ne",x)){return("NE")}#Ne brings problem
  else if(grepl("Appenzell Innerrrhoden|AI|Ai",x)){return("AI")}#AI brings problem due to VALAIS, Appenzell is in AI
  else if(grepl("Appenzell Ausserrhoden|Appenzell-Ausserrhoden|AR|Ar",x)){return("AR")}#Ar brings problem, attention not SG
  else if(grepl("Appenzell",x)){return("AI")}# Appenzell is in AI
  else if(grepl("Sankt Gallen|St. Gallen|St.Gallen|Saint-Gallen|Saint-Gall|SG|Sg|St Gall",x)){return("SG")}
  else if(grepl("Bern|BE|Be",x)){return("BE")}# Be brings problem , attention not SG
  else if(grepl("Graubünden|Graubã¼Nden|Grisons|Graubunden|GR|Gr",x)){return("GR")}#Gr brings problem
  else {return("Unknown")} 
}

from_cantons_to_regions <- function(x){
  if(is.na(x)){return(NA)}
  else if(x== "AG"){return("region_5")}
  else if(x== "AI"){return("region_5")}
  else if(x== "AR"){return("region_5")}
  else if(x== "BE"){return("region_2")}
  else if(x== "BL"){return("region_3")}
  else if(x== "BS"){return("region_3")}
  else if(x== "FR"){return("region_2")}
  else if(x== "GE"){return("region_1")}
  else if(x== "GL"){return("region_5")}
  else if(x== "GR"){return("region_6")}
  else if(x== "JU"){return("region_2")}
  else if(x== "LU"){return("region_4")}
  else if(x== "NE"){return("region_1")}
  else if(x== "NW"){return("region_4")}
  else if(x== "OW"){return("region_4")}
  else if(x== "SG"){return("region_5")}
  else if(x== "SH"){return("region_5")}
  else if(x== "SO"){return("region_3")}
  else if(x== "SZ"){return("region_4")}
  else if(x== "TG"){return("region_5")}
  else if(x== "TI"){return("region_6")}
  else if(x== "UR"){return("region_4")}
  else if(x== "VD"){return("region_1")}
  else if(x== "VS"){return("region_1")}
  else if(x== "ZG"){return("region_4")}
  else if(x== "ZH"){return("region_5")}
  # else {return("Unknown")} 
}

europe_eu <- c("Belgium", "Bulgaria", "Croatia", "Czech Republic", "Denmark", "Germany", "Estonia", "Greece", "Spain", "France", "Ireland",
               "Italy", "Republic of Cyprus", "Cyprus", "Latvia", "Lithuania", "Luxembourg", "Hungary", "Malta", "The Netherlands", "Netherlands","Holland",
               "Austria", "Poland", "Portugal", "Romania", "Slovenia", "Slovakia", "Finland", "Sweden") #https://taxation-customs.ec.europa.eu/list-non-eu-countries_en (accessed 19 Dec 2022)


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

