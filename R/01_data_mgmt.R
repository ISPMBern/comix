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
path_name <- gsub("R/","",path_name)
setwd(path_name)

## cleaning data from 'Ipsos'
comixdata_part = comix_001_load_participants()
comixdata_part_backup <- comixdata_part
#load original contact data from Ipsos files (takes time):
comixdata_cont = comix_002_load_contacts() 
# re-organizing participant file (approx. 5min):
comixdata_part = comix_003_combine_clean_variables(comixdata_part_backup)
# re-organizing contacts file:
comixdata_cont = comix_004_combine_clean_variables(comixdata_cont)

## Data structure for Zenodo 
comix_005_zenodo(comixdata_part,comixdata_cont)

# get Swiss population data (needs internet connection):
options(timeout=120)
swiss_pop_data = comix_006_ch_info()


## contact matrices
comix_008_agematrix

## vaccination uptake (only panel B)
comixdata_reg <- comix_010_cleaning_regression(comixdata_part)
table(is.na(comixdata_reg$vac_date[!duplicated(comixdata_reg$part_id)& comixdata_reg$vaccinated==1]))
output_tte <- comix_011_vaccination_coxpropreg(comixdata_reg)
output_log_missing <- comix_012_missingparticipating(comixdata_reg)
output_poissonreg <- comix_013_vaccination_poissonregression(comixdata_reg, output_tte)
output_tte_exactdates <- comix_014_vaccination_coxpropreg_exactdates(comixdata_reg)
output_reg <-cbind(output_poissonreg[c(1:4)], output_tte[[2]][-c(1,2,4,6,8)], output_poissonreg[-c(1:5,7,9,11,13)])# Combine regression models
all_regression <- output_reg[,!grepl("P-value", colnames(output_reg))]
write.csv(all_regression, "./output/tables/vaccination_uptake/SupTable3.csv")



# save data generated (is needed to generate rmd output file):
#save.image(paste0(path_name,"output/comix_project",Sys.Date(),".RData"))

