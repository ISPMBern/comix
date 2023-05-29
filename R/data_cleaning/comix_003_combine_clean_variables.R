#' ---
#' title: "Clean data"
#' author: "Martina Reichmuth"
#' date: "19/10/2021"
#' ---

comix_003_combine_clean_variables = function(comixdata_part) {
  panel_waves_ordered <-  unique(na.omit(comixdata_part$panel_wave))[order(unique(na.omit(comixdata_part$panel_wave)))]
  comixdata_part$panel_wave <- factor(comixdata_part$panel_wave, levels =panel_waves_ordered)
  
  comixdata_part$date <- as_date(comixdata_part$date)
  comixdata_part<- comixdata_part[order(comixdata_part$date),]

  comixdata_part$survey_group <-  ifelse(grepl("A|B|F",comixdata_part$panel_wave_id),"adults", "children")
  
  
  # fill all variables that are static for all waves of participant:
  for (i in unique(comixdata_part$part_id)) {
    comixdata_part$grossregion[comixdata_part$part_id==i] <- unique(na.omit(comixdata_part$grossregion[comixdata_part$part_id==i]))[1]
    comixdata_part$region_ch[comixdata_part$part_id==i] <- unique(na.omit(comixdata_part$region_ch[comixdata_part$part_id==i]))[1]
    comixdata_part$canton[comixdata_part$part_id==i] <- unique(na.omit(comixdata_part$canton[comixdata_part$part_id==i]))[1]
    comixdata_part$zip_code[comixdata_part$part_id==i] <- unique(na.omit(comixdata_part$zip_code[comixdata_part$part_id==i]))[1]
    comixdata_part$employment[comixdata_part$part_id==i] <- unique(na.omit(comixdata_part$employment[comixdata_part$part_id==i]))[1]
    comixdata_part$occupation[comixdata_part$part_id==i] <- unique(na.omit(comixdata_part$occupation[comixdata_part$part_id==i]))[1]
    comixdata_part$language_ch[comixdata_part$part_id==i] <- unique(na.omit(comixdata_part$language_ch[comixdata_part$part_id==i]))[1]
    comixdata_part$years_ch[comixdata_part$part_id==i] <- unique(na.omit(comixdata_part$years_ch[comixdata_part$part_id==i]))[1]
    comixdata_part$country_birth[comixdata_part$part_id==i] <- unique(na.omit(comixdata_part$country_birth[comixdata_part$part_id==i]))[1]
    comixdata_part$household_income[comixdata_part$part_id==i] <- unique(na.omit(comixdata_part$household_income[comixdata_part$part_id==i]))[1]
    comixdata_part$age_part[comixdata_part$part_id==i] <- unique(na.omit(comixdata_part$age_part[comixdata_part$part_id==i]))[1]
    if(unique(comixdata_part[comixdata_part$part_id==i,]$survey_group=="children")){
      comixdata_part$lower.age.limit[comixdata_part$part_id==i] <- unique(na.omit(comixdata_part$lower.age.limit[comixdata_part$part_id==i & comixdata_part$lower.age.limit<18]))[1]}
    if(unique(comixdata_part[comixdata_part$part_id==i,]$survey_group=="children")){
      comixdata_part$part_age_est_max[comixdata_part$part_id==i] <- unique(na.omit(comixdata_part$part_age_est_max[comixdata_part$part_id==i & comixdata_part$part_age_est_max<=18]))[1]}
    if(unique(comixdata_part[comixdata_part$part_id==i,]$survey_group=="children")){
      comixdata_part$part_age_est_min[comixdata_part$part_id==i] <- unique(na.omit(comixdata_part$part_age_est_min[comixdata_part$part_id==i & comixdata_part$part_age_est_min<=18]))[1]}
    #comixdata_part$lower.age.limit[comixdata_part$part_id==i] <- unique(na.omit(comixdata_part$lower.age.limit[comixdata_part$part_id==i]))[1]
    #comixdata_part$part_age_est_max[comixdata_part$part_id==i] <- unique(na.omit(comixdata_part$part_age_est_max[comixdata_part$part_id==i]))[1]
    #comixdata_part$part_age_est_min[comixdata_part$part_id==i] <- unique(na.omit(comixdata_part$part_age_est_min[comixdata_part$part_id==i]))[1]
    comixdata_part$gender_responder[comixdata_part$part_id==i] <- unique(na.omit(comixdata_part$gender_responder[comixdata_part$part_id==i]))[1]
    comixdata_part$gender[comixdata_part$part_id==i] <- unique(na.omit(comixdata_part$gender[comixdata_part$part_id==i]))[1]
    comixdata_part$education_level[comixdata_part$part_id==i] <- unique(na.omit(comixdata_part$education_level[comixdata_part$part_id==i & comixdata_part$education_level != "Unknown"]))[1]
    comixdata_part$riskgroup[comixdata_part$part_id==i] <- unique(na.omit(comixdata_part$riskgroup[comixdata_part$part_id==i]))[1]
    comixdata_part$household_riskgroup[comixdata_part$part_id==i] <- unique(na.omit(comixdata_part$household_riskgroup[comixdata_part$part_id==i]))[1]
    comixdata_part$vaccine_dose1_date[comixdata_part$part_id==i] <- unique(na.omit(comixdata_part$vaccine_dose1_date[comixdata_part$part_id==i]))[1]
    comixdata_part$vaccine_dose2_date[comixdata_part$part_id==i] <- unique(na.omit(comixdata_part$vaccine_dose2_date[comixdata_part$part_id==i]))[1]
    comixdata_part$vaccine_dose3_date[comixdata_part$part_id==i] <- unique(na.omit(comixdata_part$vaccine_dose3_date[comixdata_part$part_id==i]))[1]
    comixdata_part$country_birth[comixdata_part$part_id==i] <-   unique(na.omit(comixdata_part$country_birth[comixdata_part$part_id==i]))[1]
    comixdata_part$country_mother[comixdata_part$part_id==i] <-   unique(na.omit(comixdata_part$country_mother[comixdata_part$part_id==i]))[1]
    comixdata_part$country_father[comixdata_part$part_id==i] <-  unique(na.omit(comixdata_part$country_father[comixdata_part$part_id==i]))[1]
    comixdata_part$household_size[comixdata_part$part_id==i] <-  unique(na.omit(comixdata_part$household_size[comixdata_part$part_id==i]))[1]
    
  }
  

  FUN_gender <- function(x){
    if(is.na(x)){return(NA)}
    else if(x=="Male"){return("Male")}
    else if(x=="Female"){return("Female")}
    else if(x=="In another way"){return("Other")}
    else if(x=="Prefer not to answer"){return("Other")}
  }
  comixdata_part$sex <-   sapply(comixdata_part$gender, FUN_gender)
  comixdata_part$sex <- factor(comixdata_part$sex, level=c("Female","Male", "Other"))
  comixdata_part$gender <- factor(comixdata_part$gender, level=c("Female","Male", "In another way","Prefer not to answer"))

  comixdata_part$sex_pluspregnant <- as.character(comixdata_part$sex)
  comixdata_part$sex_pluspregnant[comixdata_part$pregnant %in% "1"] <- "Female and pregnant"
  comixdata_part$sex_pluspregnant <- factor(comixdata_part$sex_pluspregnant, level=c("Female","Female and pregnant","Male", "Other"))

  
  panel_wave_id_na <- comixdata_part$panel_wave_id[grepl("I individually included every person I had contact with.",comixdata_part$no_contacts) & !comixdata_part$panel_wave_id %in% unique(comixdata_cont$panel_wave_id)]
  
  FUN_contacts <- function(x){
    if(is.na(x)){return(NA)}
    else if(x=="I did not have any contacts"){return(0)}
    #else if(x=="I individually included every person I had contact with."& is.na(c)){return(0)}
    else {return("will be added")}
  }
  comixdata_part$no_contacts <-   sapply(comixdata_part$no_contacts, FUN_contacts)
  comixdata_part$no_contacts[comixdata_part$panel_wave_id %in% panel_wave_id_na] <- 0
  
  FUN_occupation <- function(x){
    if(is.na(x)){return(NA)}
    else if(x=="Teaching professionals"){return("Teaching professionals")}
    else if(x=="Nursing and midwifery professionals"){return("Health professionals")}
    else if(x=="Health professionals"){return("Health professionals")}
    else if(x=="Teaching associate professionals"){return("Teaching professionals")}
    else {return("Other")}
  }
  comixdata_part$occupation_cat <-   sapply(comixdata_part$occupation, FUN_occupation)
  comixdata_part$occupation_cat <- factor(comixdata_part$occupation_cat, level=c("Health professionals","Teaching professionals","Other"))
  
  
  FUN_employment <- function(x){
    if(is.na(x)){return(NA)}
    else if(x=="Employed full-time (34 hours or more)"){return("Employed")}
    else if(x=="Employed part-time (less than 34 hours)"){return("Employed")}
    else if(x=="Self employed"){return("Employed")}
    else if(x=="Unemployed but looking for a job"){return("Unemployed")}
    else if(x=="Unemployed and not looking for a job"){return("Unemployed")}
    else if(x=="Retired"){return("Retired")}
    else if(x=="Student/Pupil"){return("Student")}
    else if(x=="Long-term sick or disabled"){return("Other unemployed situation")}
    else if(x=="Full-time parent, homemaker"){return("Homemaker")}
  }
  comixdata_part$employment_cat <-   sapply(comixdata_part$employment, FUN_employment)
  comixdata_part$employment_cat <- factor(comixdata_part$employment_cat, level=c("Employed","Unemployed","Student","Homemaker","Retired", "Other unemployed situation"))
  

  
  
  #clean country of birth
 europe_eu <- c("Belgium", "Bulgaria", "Croatia", "Czech Republic", "Denmark", "Germany", "Estonia", "Greece", "Spain", "France", "Ireland",
                 "Italy", "Republic of Cyprus", "Cyprus", "Latvia", "Lithuania", "Luxembourg", "Hungary", "Malta", "The Netherlands", "Netherlands","Holland",
                 "Austria", "Poland", "Portugal", "Romania", "Slovenia", "Slovakia", "Finland", "Sweden") #https://taxation-customs.ec.europa.eu/list-non-eu-countries_en (accessed 19 Dec 2022)
  FUN_country <- function(x){
    if(is.na(x)){return(NA)}
    else if(x=="Switzerland"){return("Switzerland")}
    else if(x=="Prefer not answer"){return("Unknown")}
    else if(grepl("Don’t know",x)){return("Unknown")}
    else if(x %in% europe_eu){return("EU")}
    else {return("Non-EU")}
  }
  comixdata_part$country_cat_birth <-   sapply(comixdata_part$country_birth, FUN_country)
  comixdata_part$country_cat_mother <-   sapply(comixdata_part$country_mother, FUN_country)
  comixdata_part$country_cat_father <-   sapply(comixdata_part$country_father, FUN_country)
  eLevels <- c("Switzerland","EU", "Non-EU","Unknown")
  comixdata_part[,c(colnames(comixdata_part[,grepl("country_cat", colnames(comixdata_part))]))] <- lapply(comixdata_part[,grepl("country_cat", colnames(comixdata_part))], function(x) factor(x, eLevels))
  
  FUN_birth <- function(x){
    if(is.na(x)){return(NA)}
    else if(x=="Switzerland"){return("Switzerland")}
    else if(x=="Unknown"){return("Unknown")}
    else {return("Abroad")}
  }
  comixdata_part$swiss_abroad_birth <-   sapply(comixdata_part$country_cat_birth, FUN_birth)
  comixdata_part$swiss_abroad_birth_mother <-   sapply(comixdata_part$country_cat_mother, FUN_birth)
  comixdata_part$swiss_abroad_birth_father <-   sapply(comixdata_part$country_cat_father, FUN_birth)
  eLevels <- c("Switzerland","Abroad", "Unknown")
  comixdata_part[,c(colnames(comixdata_part[,grepl("swiss_abroad_birth", colnames(comixdata_part))]))] <- lapply(comixdata_part[,grepl("swiss_abroad_birth", colnames(comixdata_part))], function(x) factor(x, eLevels))
  
  rename_education1<- function(x){
    if(is.na(x)){return(NA)}
    else if(x== "Advanced vocational education"){return("Gymnasium and advanced vocational education")}
    else if(x== "Gymnasium"){return("Gymnasium and advanced vocational education")}
    else if(x== "Higher education (e.g., Bachelor, Master or PhD)"){return("Higher education (e.g., Bachelor, Master or PhD)")}
    else if(x== "Higher education (e.g., Bachelor, Master or PhD)"){return("Higher education (e.g., Bachelor, Master or PhD)")}
    else if(x== "Obligatory school"){return("Obligatory school and vocational education")}
    else if(x== "Higher education (e.g., Bachelor, Master or PhD)"){return("Higher education (e.g., Bachelor, Master or PhD)")}
    else if(x== "Gymnasium"){return("Gymnasium and advanced vocational education")}
    else if(x== "Vocational education"){return("Obligatory school and vocational education")}
    #else {return("Unknown")} 
  }
  comixdata_part$education_level3 <- sapply(comixdata_part$education_level, rename_education1)
  comixdata_part$education_level3 <- factor(comixdata_part$education_level3, levels =c("Obligatory school and vocational education", "Gymnasium and advanced vocational education", "Higher education (e.g., Bachelor, Master or PhD)"))
  
  
  FUN_testing <- function(x){
    if(is.na(x)){return(NA)}
    else if(x%in%"Tested and the test showed {#i_they.response.label} did have Coronavirus at the time"){return("Positive")}
    else if(x%in%"{#i_they.response.label} have been tested and at least one test showed {#i_they.response.label} did have Coronavirus at"){return("Positive")}
    else if(x%in%"Tested and the test showed {#i_they.response.label} have Coronavirus currently"){return("Positive")}
    else if(x%in%"{#i_they.response.label} have been tested and at least one test showed {#i_they.response.label} did have Coronavirus at"){return("Positive")}
    else if(x%in%"Tested, and the test showed {#i_they.response.label} did not have Coronavirus"){return("Negative")}
    else if(x%in%"{#i_they.response.label} have been tested, and all the tests showed {#i_they.response.label} did not have Coronavirus"){return("Negative")}
    else if(x%in%"Tested, and the test showed {#i_they.response.label} do not have Coronavirus currently"){return("Negative")}
    else if(x%in%"{#i_they.response.label} have been tested, and all the tests showed {#i_they.response.label} did not have Coronavirus"){return("Negative")}
    else if(x%in%"Yes, and {#im_are.response.label} still waiting to hear the result"){return("Tested")}# dont know yet..
    else if(x%in%"Not tested"){return("Not tested")}
    else if(x%in%"Don’t know"){return("Prefer not to answer")}
    else if(x%in%"Prefer not to answer"){return("Prefer not to answer")}
    else return("?")
  }
  comixdata_part$pos <-   sapply(comixdata_part$pos, FUN_testing)
  comixdata_part$pos <- factor(comixdata_part$pos, levels = c("Positive","Tested","Negative","Not tested","Prefer not to answer"))# prior to 14days
  comixdata_part$pos_last2w <-   sapply(comixdata_part$pos_last2w, FUN_testing)
  comixdata_part$pos_last2w <- factor(comixdata_part$pos_last2w, levels = c("Positive","Tested","Negative","Not tested","Prefer not to answer"))
  comixdata_part$pos[comixdata_part$pos_last2w %in% "Positive"] <-  "Positive"
  comixdata_part$pos <- ifelse(comixdata_part$pos_last2w %in%"Tested"& !comixdata_part$pos %in% "Positive", "Tested", as.character(comixdata_part$pos))
  comixdata_part$pos <- ifelse(comixdata_part$pos_last2w %in%"Tested"& comixdata_part$pos %in%"Not tested", "Tested", as.character(comixdata_part$pos_last2w))
  comixdata_part$pos <- ifelse(comixdata_part$pos_last2w %in%"Negative"& !comixdata_part$pos %in%"Positive", "Negative", as.character(comixdata_part$pos_last2w))
  comixdata_part$pos <- ifelse(comixdata_part$pos_last2w %in%"Positive","Positive", as.character(comixdata_part$pos_last2w))
  comixdata_part$pos <- factor(comixdata_part$pos, levels = c("Positive","Tested","Negative","Not tested","Prefer not to answer"))
  
  
  comixdata_part$region[grepl("Urban",comixdata_part$region)] <- "Urban"
  comixdata_part$region[grepl("Rural",comixdata_part$region)] <- "Rural"
  comixdata_part$region <- factor(comixdata_part$region, levels = c("Urban","Rural"))
  
  comixdata_part$grossregion <- factor(comixdata_part$grossregion, order = FALSE, levels = c(names(table(comixdata_part$grossregion))[rev(order(table(comixdata_part$grossregion)))]))
  
  #all that once said positive tested always after that positive:
  for (i in unique(comixdata_part[,1])) {
    if(sum(na.omit(comixdata_part$pos[comixdata_part$part_id ==i]) =="Positive")>0){
      yes <- min(grep("Positive",na.omit(comixdata_part$pos[comixdata_part$part_id ==i]),useBytes = TRUE))
      comixdata_part$pos[comixdata_part$part_id ==i][1:yes] <- comixdata_part$pos[comixdata_part$part_id ==i][1:yes]
      comixdata_part$pos[comixdata_part$part_id ==i][yes:length(comixdata_part$pos[comixdata_part$part_id ==i])] <- "Positive"
    }
  }
#all that once said vaccinated always after that vaccinated:
  for (i in unique(comixdata_part[,1])) {
    if(sum(na.omit(comixdata_part$vaccinated[comixdata_part$part_id ==i]) ==1)>0){
      yes <- min(grep(1,na.omit(comixdata_part$vaccinated[comixdata_part$part_id ==i]),useBytes = TRUE))
      comixdata_part$vaccinated[comixdata_part$part_id ==i][1:yes] <- comixdata_part$vaccinated[comixdata_part$part_id ==i][1:yes]
      comixdata_part$vaccinated[comixdata_part$part_id ==i][yes:length(comixdata_part$vaccinated[comixdata_part$part_id ==i])] <- 1
    }
    else if(sum(na.omit(comixdata_part$vaccinated[comixdata_part$part_id ==i]) == 0)>0){
      no <- min(grep(0,na.omit(comixdata_part$vaccinated[comixdata_part$part_id ==i]),useBytes = TRUE))
      comixdata_part$vaccinated[comixdata_part$part_id ==i][no:length(comixdata_part$vaccinated[comixdata_part$part_id ==i])] <- 0
    }
  }
  
  for (i in unique(comixdata_part[,1])) {
    if(sum(na.omit(comixdata_part$got_booster[comixdata_part$part_id ==i]) =="Yes, I already had one")>0){
  yes <- min(grep("Yes, I already had one",na.omit(comixdata_part$got_booster[comixdata_part$part_id ==i]),useBytes = TRUE))
  comixdata_part$got_booster[comixdata_part$part_id ==i][1:yes] <- comixdata_part$got_booster[comixdata_part$part_id ==i][1:yes]
  comixdata_part$got_booster[comixdata_part$part_id ==i][yes:length(comixdata_part$got_booster[comixdata_part$part_id ==i])] <- "Yes, I already had one"
}
  }
  
  for (i in unique(comixdata_part[,1])) {
    if(sum(na.omit(comixdata_part$vaccinated[comixdata_part$part_id ==i]) ==1)>0){
      yes <- min(grep(1,na.omit(comixdata_part$vaccinated[comixdata_part$part_id ==i]),useBytes = TRUE))
      comixdata_part$vaccinated[comixdata_part$part_id ==i][1:yes] <- comixdata_part$vaccinated[comixdata_part$part_id ==i][1:yes]
      comixdata_part$vaccinated[comixdata_part$part_id ==i][yes:length(comixdata_part$vaccinated[comixdata_part$part_id ==i])] <- 1
    }
    else if(sum(na.omit(comixdata_part$vaccinated[comixdata_part$part_id ==i]) == 0)>0){
      no <- min(grep(0,na.omit(comixdata_part$vaccinated[comixdata_part$part_id ==i]),useBytes = TRUE))
      comixdata_part$vaccinated[comixdata_part$part_id ==i][no:length(comixdata_part$vaccinated[comixdata_part$part_id ==i])] <- 0
    }
  }
  
  
  FUN_vaccine_want <- function(x){
    if(is.na(x)){return(NA)}
    else if(x=="Don’t know/have not decided yet whether my child will get vaccination"){return("Don't know")}
    else if(x=="No, I do not want to be vaccinated"){return("No")}
    else if(x=="I don’t know/I have not decided yet"){return("Don't know")}
    else if(x=="No, my child will not get vaccinated"){return("No")}
    else if(x=="Prefer not to say"){return("Prefer not to say")}
    else if(x=="Yes"){return("Yes")}
    else if(x=="Yes, but I do not yet have an appointment for my vaccination"){return("Yes")}
    else if(x=="Yes, I already have an appointment for my vaccination"){return("Yes")}
    else return("?")
  }
  comixdata_part$vaccine_want_cat <-   sapply(comixdata_part$vaccine_want, FUN_vaccine_want)
  comixdata_part$vaccine_want_cat <- factor(comixdata_part$vaccine_want_cat, levels = c("Yes","No","Don't know", "Prefer not to say"))


  
  # reorder household income
  notNA <-grep("Prefer not to answer",comixdata_part$household_income)
  comixdata_part$household_income[notNA] <- NA
  comixdata_part$household_income_min <- gsub("([0-9]+).*$", "\\1", comixdata_part$household_income)
  comixdata_part$household_income_min <- as.numeric(comixdata_part$household_income_min )
  salary_class1 <- names(table(comixdata_part$household_income))
  salary_class <- names(table(comixdata_part$household_income_min))
  salary_class1 <- salary_class1[order(as.numeric(na.omit(salary_class)))]
  salary_class <- salary_class[order(as.numeric(na.omit(salary_class)))]
  comixdata_part$household_income_min[notNA] <- "Prefer not to answer"
  comixdata_part$household_income[notNA] <- "Prefer not to answer"
  salary_class <- c(na.omit(salary_class), "Prefer not to answer")
  salary_class1 <- c(na.omit(salary_class1), "Prefer not to answer")
  comixdata_part$household_income<- factor(comixdata_part$household_income,levels=salary_class1)
  comixdata_part$household_income_min<- factor(comixdata_part$household_income_min,levels=salary_class)
  
  # less household income categories (plus ordering), 3 groups: 0-5000, 5001-10000, 10000+
  y <- as.numeric(c(0,5001,10000))#as.numeric(levels(unique(comixdata_part$household_income_min))[-13])
  fun <- function(x) {
    for(i in 3:1){
      if(is.na(x)){return(NA)}
      if(x=="Prefer not to answer"){return("Preferred not to answer")}
      if(as.integer(as.character(x))>=y[i]){return(y[i])}
      #else if(as.numeric(x)>=y[i]){return(y[i])}
    }
  }
  comixdata_part$household_income_min_3cat <- sapply(comixdata_part$household_income_min, fun)
  fun <- function(x) {
    if(is.na(x)){return(NA)}
    else if(x==0){return("0-5,000")}
    else if(x==5001){return("5,001-10,000")}
    else if(x==10000){return("10,000+")}
    else if(x=="Preferred not to answer"){return("Preferred not to answer")}
  }
  comixdata_part$household_income_3cat <- sapply(comixdata_part$household_income_min_3cat, fun)
  comixdata_part$household_income_3cat <- factor(comixdata_part$household_income_3cat,levels=c("0-5,000", "5,001-10,000", "10,000+","Preferred not to answer"))
  
  # age categories, age (18-39, 40-64, 65+)
  fun <- function(x) {
    x <- as.numeric(gsub("([0-9]+).*$", "\\1",  x))
    if(is.na(x)){return(NA)}
    else if(x>=65){return("65+")}
    else if(x>=40){return("40-64")}
    else if(x>18){return("19-39")}
    else if(x<18){return("0-18")}
  }
  comixdata_part$age_part_3cat <- sapply(as.numeric(gsub("([0-9]+).*$", "\\1", comixdata_part$age_part)), fun)
  comixdata_part$age_part_3cat <- factor(comixdata_part$age_part_3cat,levels=c("0-18","19-39", "40-64", "65+"))
  
  # want to be vaccinated + if vaccinated
  comixdata_part$vaccine_want_cat_plus <- ifelse(comixdata_part$vaccinated==1,1,as.character(comixdata_part$vaccine_want_cat))
  comixdata_part$vaccine_want_cat_plus[comixdata_part$vaccine_want_cat_plus==1] <- "Vaccinated"
  
  
  
  # reason to adhere to measures:
  adherence_delete <-grep("adherence",colnames(comixdata_part))
  comixdata_part$adherence_reason[grepl("Yes",comixdata_part$adherence_fines,ignore.case = T)] <- "law or fines"
  comixdata_part$adherence_reason[grepl("Yes",comixdata_part$adherence_law,ignore.case = T)] <- "law or fines"
  comixdata_part$adherence_reason[grepl("Yes",comixdata_part$adherence_righthing,ignore.case = T)] <- "to protect me or others, or it is the right thing to do"
  comixdata_part$adherence_reason[grepl("Yes",comixdata_part$adherence_protect,ignore.case = T)] <- "to protect me or others, or it is the right thing to do"
  comixdata_part$adherence_reason[grepl("Yes",comixdata_part$adherence_protect_me,ignore.case = T)] <- "to protect me or others, or it is the right thing to do"
  comixdata_part$adherence_reason[grepl("Yes",comixdata_part$adherence_want_agree,ignore.case = T)] <- "to protect me or others, or it is the right thing to do"
  comixdata_part$adherence_reason[grepl("Yes",comixdata_part$adherence_want,ignore.case = T)] <- "to protect me or others, or it is the right thing to do"
  comixdata_part$adherence_reason[grepl("Yes",comixdata_part$adherence_no,ignore.case = T)] <- "do not adhere to it"
  comixdata_part$adherence_reason[grepl("Yes",comixdata_part$adherence_dontknow,ignore.case = T)] <- "don't know"
  
  comixdata_part <- comixdata_part[,-adherence_delete]
  
  comixdata_part$agreement_measures_cat <- gsub('[0-9]+', '',  comixdata_part$agreement_measures)
  comixdata_part$agreement_measures_cat <- sub("^\\s+", "", comixdata_part$agreement_measures_cat)
  comixdata_part$agreement_measures_cat <- factor(comixdata_part$agreement_measures_cat,  levels = c("About right","A bit too lenient","Far too lenient","A bit too strict","Far too strict","Don’t know"))
  fun <- function(x) {
    if(is.na(x)){return(NA)}
    else if(grepl("About right", x)){return("About right")}
    else if(grepl("A bit too lenient", x)){return("Too lenient")}
    else if(grepl("Far too lenient", x)){return("Too lenient")}
    else if(grepl("A bit too strict", x)){return("Too strict")}
    else if(grepl("Far too strict", x)){return("Too strict")}
    else if(grepl("Don’t know", x)){return("Don't know")}
  }
  comixdata_part$agreement_measures_3cat  <- sapply(comixdata_part$agreement_measures_cat, fun)
  comixdata_part$agreement_measures_3cat <- factor(comixdata_part$agreement_measures_3cat,levels = c("About right","Too lenient","Too strict","Don't know"))
  
  
  
  # contact change after vaccination
  fun <- function(x) {
    if(is.na(x)){return(NA)}
    else if(grepl("I have contact with or meet with fewer people in person now than I did before I have my vaccination", x)){return("Fewer contacts")}
    else if(grepl("I have contact with or meet with more people in person now than I did before I had my vaccination", x)){return("More contacts")}
    else if(grepl("There is no change in the number of people I have contact or met in person now compared with before I had my vaccination", x)){return("No change")}
    else if(grepl("Prefer not to say", x)){return("Prefer not to say")}
    else if(grepl("Don’t know", x)){return("Don't know")}
  }
  comixdata_part$change_behavior_after_vaccine  <- sapply(comixdata_part$change_behavior_after_vaccine, fun)
  comixdata_part$change_behavior_after_vaccine  <- factor(comixdata_part$change_behavior_after_vaccine, levels=c("Fewer contacts","More contacts", "No change", "Don't know"))
  fun <- function(x) {
    if(is.na(x)){return(NA)}
    else if(grepl("In the last two weeks I have had contact with or met with fewer people in person than I did before then", x)){return("Fewer contacts")}
    else if(grepl("In the last two weeks I have had contact with or met with more people in person than I did before then", x)){return("More contacts")}
    else if(grepl("There is no change in the number of people I have contact with or met person in the last two weeks compared with before", x)){return("No change")}
    else if(grepl("Prefer not to say", x)){return("Prefer not to say")}
    else if(grepl("Don’t know", x)){return("Don't know")}
  }
  comixdata_part$change_behavior  <- sapply(comixdata_part$change_behavior, fun)
  comixdata_part$change_behavior  <- factor(comixdata_part$change_behavior, levels=c("Fewer contacts","More contacts", "No change", "Prefer not to say","Don't know"))
  

  comixdata_part$motivation_vaccinated  <- factor(comixdata_part$motivation_vaccinated, levels=c("To help end the pandemic","To protect my health from COVID-19/To avoid getting seriously ill", "To protect the health of others (e.g. family, friends, etc.)", "To protect the child's health from COVID-19 (e.g. Long Covid, PIMS)","Advice by a health/medical professional","To get a Covid certificate","Because of the vaccination campaign","Because of pressure from others (e.g. family, friends, employer, government, etc.)","Most of my friends and family vaccinate their children","Another reason","Don’t know","Prefer not to say"))
  comixdata_part$why_notvaccinated <- factor(comixdata_part$why_notvaccinated, levels=c("Advice by a health/medical professional not to get a vaccine","I already had COVID-19 and don’t need a vaccine","My child already had COVID-19 and doesn’t need a vaccine", "I feel healthy and/or prefer to develop natural immunity", "Lower risk of severe disease in children than in adults, therefore I prefer a natural infection", "Lack of trust in available vaccines", "Lack of trust in available vaccines for children","Lack of trust in government recommendations","Most of my friends and family have also not been vaccinated","Vaccines are limited and other people need it more than me", "Vaccines are limited and other people need it more than my child","Vaccine has not been recommended for the age group yet","Worry about side effects","Worry about side effects for children","It has been too difficult to get an appointment","Another reason","Don’t know","Prefer not to say"))

  comixdata_part$vaccinated <- suppressWarnings(as.numeric(comixdata_part$vaccinated)) 
  comixdata_part$household_size <- as.numeric(comixdata_part$household_size)
  comixdata_part$household_size_cat <- ifelse(comixdata_part$household_size >4, "5+", comixdata_part$household_size)

  comixdata_part <- comixdata_part[!is.na(comixdata_part$panel_wave_id),]
  
  
  comixdata_cont_part <- comixdata_cont %>% group_by(panel_wave_id) %>% summarise(contact_per_id =length(panel_wave_id))
 
  comixdata_part <- merge(comixdata_part, comixdata_cont_part, by="panel_wave_id", all = TRUE)
  comixdata_part$contact_per_id <- ifelse(comixdata_part$contact_per_id >50, 50, comixdata_part$contact_per_id)
  comixdata_part$no_contacts <- ifelse(comixdata_part$no_contacts %in% 0, 0, as.numeric(comixdata_part$contact_per_id))
  comixdata_part$no_contacts <- as.numeric( comixdata_part$no_contacts )
  
  
  # participants missed waves, droped out or participated in all waves:
  num_participating <- comixdata_part %>% group_by(part_id)%>% summarise(waves_num= length(panel_wave))
 
   fun_drops <- function(x) { 
    drops <- c()
    num<- as.numeric(gsub("[[:alpha:]]","",comixdata_part$panel_wave[comixdata_part$part_id %in% x]))
    num <- num[order(num)]
    panel <- gsub("[0-9]","",comixdata_part$panel_wave[comixdata_part$part_id == x])[1]
    num_waves <- 1:length(grep(panel,names(table(comixdata_part$panel_wave))))
    
    if(!max(num_waves) %in% max(num)){drops <- "Drop out"}
    else if(sum(!num %in% num_waves) %in% 0 & sum(!diff(num) %in% 1) %in% 0 ){drops <- "No missing"}#if(sum(!c(2,3) %in% c(1,2,3))==0  & diff(c(2,3)) %in% 1){print("ja")}
    else{drops <- "Missings"}
    
    return(drops)
  }
  
  num_participating$drops  <- sapply(num_participating$part_id, fun_drops)
  table(num_participating$drops)
  table(num_participating$waves_num)
  hist(num_participating$waves_num, xlab = "Number of participation", ylab= "Number of participants", main="", las=1,breaks = max(num_participating$waves_num))
  comixdata_part$waves_num <- NULL
  comixdata_part$drops <- NULL
  comixdata_part <- merge(comixdata_part, num_participating, by ="part_id")
  comixdata_part$drops <- factor(comixdata_part$drops, levels = c("No missing","Missings", "Drop out"))
  comixdata_part$missing <- ifelse(comixdata_part$drops %in% c("Missings","Drop out"),1,0)
print(table(comixdata_part$drops))
  
comixdata_part <- comixdata_part[!is.na(comixdata_part$part_id),]
  return(as.data.frame(comixdata_part))
  
}
