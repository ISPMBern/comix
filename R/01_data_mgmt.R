#' ---
#' title: "CoMix Data from Switzerland
#' author: "Martina Reichmuth"
#' date: "'01/12/2021"
#' ---

### loading of cleaned variables
# check initialize file if you installed all packages (open R/comix_000_initialize.R)
path_name <- rstudioapi::getSourceEditorContext()$path
path_name <- gsub("01_data_mgmt.R","",path_name)
setwd(path_name)
source(paste0(path_name,"comix_000_initialize.R"))
options(scipen=999) 

## cleaning data from 'Ipsos'
comixdata_part = comix_001_load_participants()
# get Swiss population data (needs internet connection):
#swiss_pop_data = comix_005_swiss_population_data()
#saveRDS(swiss_pop_data, file="./data/swiss_pop_data.RData")
swiss_pop_data <- readRDS("./data/swiss_pop_data.RData")
#load original contact data from Ipsos files (takes time):
comixdata_cont = comix_002_load_contacts() 
# re-organizing contacts file:
comixdata_cont = comix_004_combine_clean_variables(comixdata_cont)
# re-organizing participant file (approx. 5min):
comixdata_part = comix_003_combine_clean_variables(comixdata_part_backups)


## vaccination uptake (only panel B)
comixdata_reg <- comix_010_cleaning_regression(comixdata_part)
output_tte <- comix_011_vaccination_coxpropreg(comixdata_reg)
output_log_missing <- comix_012_missingparticipating(comixdata_reg)
output_poissonreg <- comix_013_vaccination_poissonregression(comixdata_reg)
output_reg <-cbind(output_poissonreg[c(1:4)], output_tte[[2]][-c(1,2,4,6,8)], output_poissonreg[-c(1:5,7,9,11,13)])# Combine regression models
all_regression <- output_reg[,!grepl("P-value", colnames(output_reg))]
write.csv(all_regression, "../../tables/vaccination_uptake/SupTable2.csv")

# save data generated (is needed to generate rmd output file):
save.image(paste0(path_name,"comix_project_",Sys.Date(),".RData"))

