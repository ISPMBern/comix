#' ---
#' title: "Load data"
#' author: "Martina Reichmuth"
#' date: "19/10/2021"
#' ---

comix_001_load_participants = function() {
  # Load all raw data from Ipsos
  ipsos_files <- list.files(paste0(path_name,"/data/raw/Ipsos/"))
  ipsos_files<- ipsos_files[-grep("20-030971_G2_Wave3 recontact_Parents_CH_QPXX4_Final_v1_160921_IntClientUse.sav",ipsos_files,fixed = TRUE)]
  ipsos_files<- ipsos_files[-grep("20-030971_G2_Main_Wave8_(1st additional)_CH_v1_22062021_IntClientUse.sav",ipsos_files,fixed = TRUE)]
  ipsos_files<- ipsos_files[-grep("20-030971_G2_Main_Wave11_(4th additional)_CH_v1_19082021_IntClientUse.sav",ipsos_files,fixed = TRUE)]
  ipsos_files<- ipsos_files[-grep("20-030971_G2_Main_Wave9_(2nd additional)_CH_v1_23072021_IntClientUse.sav",ipsos_files,fixed = TRUE)]
  ipsos_files<- ipsos_files[-grep("20-030971_G2_Main_Wave8_(1st additional)_CH_v2_24092021_IntClientUse.sav",ipsos_files,fixed = TRUE)]
  ipsos_files<- ipsos_files[-grep("20-030971_G2_Main_Wave9_(2nd additional)_CH_v2_24092021_IntClientUse.sav",ipsos_files,fixed = TRUE)]
  ipsos_files<- ipsos_files[-grep("20-030971_G2_Main_Wave10_(3rd additional)_CH_v2_24092021_IntClientUse.sav",ipsos_files,fixed = TRUE)]
  ipsos_files<- ipsos_files[-grep("20-030971_G2_Main_Wave11_(4th additional)_CH_v2_24092021_IntClientUse.sav",ipsos_files,fixed = TRUE)]
  ipsos_files<- ipsos_files[-grep("20-030971_G2_Main_Wave12_(5th additional)_CH_v2_24092021_IntClientUse.sav",ipsos_files,fixed = TRUE)]
  ipsos_files<- ipsos_files[-grep("20-030971_G2_Main_Wave13_(6th additional)_CH_v2_24092021_IntClientUse.sav",ipsos_files,fixed = TRUE)]
  ipsos_files<- ipsos_files[-grep("21-086409_01_LSHTM_CH_Wave1_Final_v1_29122021_ICUO.sav",ipsos_files,fixed = TRUE)]
  ipsos_files<- ipsos_files[-grep("22-069789 LSHTM_DCE2022_CH_Final_V1_IntClientUse.sav",ipsos_files,fixed = TRUE)] # new waves need adaption - different variable names.
   part_data <- c()
  for (n in 1:length(ipsos_files)) {#n <- 7 
    ipsos_data <- c() 
    options(warn=-1)
    ipsos_data <- read.spss(paste0(path_name,"/data/raw/Ipsos/",ipsos_files[n]),reencode ="UTF-8")
    ipsos_data <- as.data.frame(ipsos_data)
    if(n==grep("20-030971_G2_Wave3_Parents_CH_Final_v1_23072021_IntClientUse.sav",ipsos_files,fixed = TRUE)){
      ipsos_data1 <- read.spss(paste0(path_name,"/data/raw/Ipsos/20-030971_G2_Wave3 recontact_Parents_CH_QPXX4_Final_v1_160921_IntClientUse.sav"),reencode ="UTF-8")
      ipsos_data1 <- as.data.frame(ipsos_data1)
      ipsos_data <- merge(ipsos_data1, ipsos_data, by=c("Respondent_ID","resp_age","GENDER_NonBinary"))
    rm(ipsos_data1) 
    }
    if(n==grep("20-030971_G2_Merged_Wave2_Parents_FI LI CH EL_Final_v1_14042021_IntClientUse.sav",ipsos_files,fixed = TRUE)){
      ipsos_data <- ipsos_data[ipsos_data$CultureInfo %in% c("fr-ch","it-ch", "de-ch"),]
    }
    if(n==grep("20-030971_G2_Merged_Wave1_Parents_Final_v1_12022021_IntClientUse.sav",ipsos_files,fixed = TRUE)){
      ipsos_data <- ipsos_data[ipsos_data$CultureInfo %in% c("fr-ch","it-ch", "de-ch"),]
    }
    options(warn=0)
  
  ### Participant file:
  ## get variable "part_id"
  ipsos_data$part_id <- ipsos_data$Respondent_ID
  if(sum(is.na(ipsos_data$Respondent_ID))>1){
    ipsos_data$part_id <- c(1: length(ipsos_data$CurrentYear))
  }
  ## get variable "panel_wave"
  # get variable "date" (date of participation)
  ipsos_data$date <- as_date(paste0(ipsos_data$CurrentYear,"-", ipsos_data$CurrentMonth,"-", ipsos_data$CurrentDay),format="%Y-%m-%d")
  # get variable panel
  
  if( is.null(ipsos_data$Sampletype)){

    if(sum(as.numeric(ipsos_data$resp_age)>=18)!=0){
     if(ipsos_data$date[1] %in% seq(as_date("2022-07-01"),as_date("2023-01-01"),1)){
        ipsos_data$panel <- "G"
        ipsos_data$Wave <-1
        ipsos_data$Sampletype <- 1
      }
    }
    if(sum(as.numeric(ipsos_data$resp_age)<18)!=0){
      if(ipsos_data$date[1] %in% seq(as_date("2022-07-01"),as_date("2023-01-01"),1)){
        ipsos_data$panel <- "H"
        ipsos_data$Wave <-1
        ipsos_data$Sampletype <- 2
      }
    }
  }
  
  if(unique(ipsos_data$Sampletype) %in% c("Sampletype=1 Main sample",1)){
    if(ipsos_data$date[1] %in% seq(as_date("2021-01-20"),as_date("2021-05-17"),1)){
      ipsos_data$panel <- "A"
    }
    else if(ipsos_data$date[1] %in% seq(as_date("2021-06-01"),as_date("2021-09-15"),1)){
      ipsos_data$panel <- "B"
    }
    else if(ipsos_data$date[1] %in% seq(as_date("2021-11-01"),as_date("2022-06-01"),1)){
      ipsos_data$panel <- "F"
    }
    else if(ipsos_data$date[1] %in% seq(as_date("2022-07-01"),as_date("2023-01-01"),1)){
      ipsos_data$panel <- "G"
    }
  }
  if(unique(ipsos_data$Sampletype) %in% c("Sampletype=2 Parent sample",2)){
    if(ipsos_data$date[1] %in% seq(as_date("2021-01-20"),as_date("2021-05-17"),1)){
      ipsos_data$panel <- "C"
    }
    else if(ipsos_data$date[1] %in% seq(as_date("2021-06-01"),as_date("2021-09-15"),1)){
      ipsos_data$panel <- "D"
    }
    else if(ipsos_data$date[1] %in% seq(as_date("2021-11-01"),as_date("2022-06-01"),1)){
      ipsos_data$panel <- "E"
    }
    else if(ipsos_data$date[1] %in% seq(as_date("2022-07-01"),as_date("2023-01-01"),1)){
      ipsos_data$panel <- "H"
    }
  }
  ipsos_data <- ipsos_data[order(as.numeric(ipsos_data$Wave)),]
  ipsos_data$panel_wave<- paste0(ipsos_data$panel, gsub("Wave ","",ipsos_data$Wave))
  ipsos_data$panel_wave[ipsos_data$panel_wave=="F7"] <- "F1"
  ipsos_data$panel_wave[ipsos_data$panel_wave=="F8"] <- "F2"
  ipsos_data$panel_wave[ipsos_data$panel_wave=="F9"] <- "F3"
  ipsos_data$panel_wave[ipsos_data$panel_wave=="F10"] <- "F4"
  ipsos_data$panel_wave[ipsos_data$panel_wave=="F11"] <- "F5"
  
  # get variable "panel_wave_id"
  ipsos_data$panel_wave_id <- paste0(ipsos_data$panel_wave, "_",ipsos_data$Respondent_ID)
  ## get variable "age_part", "part_age_est_min", "part_age_est_max"
  if(unique(ipsos_data$Sampletype) %in% c("Sampletype=1 Main sample",1)){
  ipsos_data$age_part <- as.numeric(ipsos_data$resp_age)
  ipsos_data$part_age_est_min <- ipsos_data$part_age_est_max <- ipsos_data$age_part
   }
  
  if(unique(ipsos_data$Sampletype) %in% c("Sampletype=2 Parent sample",2)){
    ipsos_data$age_part <- "0-18"
    for (l in 1:length(ipsos_data$age_part)){
      child_age <- paste0("Q23_LOOP_",regmatches(ipsos_data$QP53a_choosen[l], gregexpr("[[:digit:]]+", ipsos_data$QP53a_choosen[l]))[[1]][2],"_Q23")
      if(ipsos_data$Respondent_ID[l]=="902011"){
        child_age <- "Q23_LOOP_3_Q23"#ipsos_data$Q23_LOOP_3_Q23[ipsos_data$Respondent_ID=="902011"]# most likely wrong coding
      }
      if(ipsos_data$Respondent_ID[l]=="902168"){
        child_age <- "Q23_LOOP_2_Q23"#ipsos_data$Q23_LOOP_2_Q23[ipsos_data$Respondent_ID=="902168"]# most likely wrong coding
      }
      i <- grep(child_age,colnames(ipsos_data))
      min_age <- ipsos_data[l,i]
      if(is.na(min_age)){
        ipsos_data$age_part[l] <- "0-18"
      }
      else if(grepl("Prefer not to answer",min_age)){
        ipsos_data$age_part[l] <- "0-18"
      }
      else if(grepl("Under 1",min_age)){
        ipsos_data$age_part[l] <- "0-1"
      }
      else {
        ipsos_data$age_part[l] <- as.character(ipsos_data[l,i])
      }
    }
    ipsos_data$part_age_est_min <- as.numeric(sub("-.*","",ipsos_data$age_part))
    ipsos_data$part_age_est_max <- as.numeric(sub(".*-","",ipsos_data$age_part))
  }


  age_bands <- as.numeric(c("0", "5", "12", "18", "30", "40", "50", "60", "70"))
  ipsos_data$lower.age.limit <- NA
  for (i in 1:length(ipsos_data[,1])) {
  ipsos_data$lower.age.limit[i] <- max(age_bands[age_bands <= as.numeric(ipsos_data$part_age_est_min[i])])
    
  }

  ## get variable gender ("female", "male")

    ipsos_data$gender <- ipsos_data$GENDER_NonBinary
    ipsos_data$gender_responder <- ipsos_data$resp_gender

  ### geography; 
  ## get variable "zip_code" (in QMktSize_23_1)
    ipsos_data$zip_code <- NA
    if(length(ipsos_data$QMktSize_23_1)>0){
      ipsos_data$zip_code <- ipsos_data$QMktSize_23_1 
    }
    ipsos_data$region <- ipsos_data$QMktSize_18_1
 ## get variable "canton" (in QMktSize_6_1)
ipsos_data$canton  <- sapply(ipsos_data$QMktSize_6_1, from_cantons_to_abrev)
  ## get variable "six_region"

  ipsos_data$region_ch  <- sapply(ipsos_data$canton, from_cantons_to_regions)
  ## get variable "grossregion" (in QMktSize_4_1), 
  ipsos_data$grossregion <-  str_trim(ipsos_data$QMktSize_4_1, "right")
  ipsos_data$grossregion <- factor(ipsos_data$grossregion,level=sort(unique(ipsos_data$grossregion)))
  ipsos_data$region_urban_rural <- ipsos_data$QMktSize_18_1
  ## get variable "labor_areas" (not defined yet!)
  #ipsos_data$labor_areas <- NA
  
  ### socio-economic factors; 
  ## get variable "occupation" (in CH01OCCR) 
  ipsos_data$occupation <- NA
  if("CH01OCCR" %in% colnames(ipsos_data)){
    ipsos_data$occupation <- ipsos_data$CH01OCCR
  }
  ## get variable "employment" (in Q3) 
  ipsos_data$employment <- ipsos_data$Q3
  ## get variable "household_size" 
  if("Q20_original" %in% colnames(ipsos_data)){
    ipsos_data$household_size <- ipsos_data$Q20_original
  }
  if("Q20_new" %in% colnames(ipsos_data)){
    ipsos_data$household_size <- ipsos_data$Q20_new
  }

  ## get variable "household_income" (in CH01INC)
  ipsos_data$household_income <- NA
  if(sum(grepl("CH01INC", colnames(ipsos_data)))>0){
    ipsos_data$household_income <- ipsos_data$CH01INC
    notNA <-grep("Prefer not to answer",ipsos_data$household_income)
    ipsos_data$household_income[notNA] <- NA
    ipsos_data$household_income_min <- gsub("([0-9]+).*$", "\\1", ipsos_data$household_income)
    ipsos_data$household_income_min <- as.numeric(ipsos_data$household_income_min )
    salary_class1 <- names(table(ipsos_data$household_income))
    salary_class <- names(table(ipsos_data$household_income_min))
    salary_class1 <- salary_class1[order(as.numeric(na.omit(salary_class)))]
    salary_class <- salary_class[order(as.numeric(na.omit(salary_class)))]
    ipsos_data$household_income_min[notNA] <- "Prefer not to answer"
    ipsos_data$household_income[notNA] <- "Prefer not to answer"
    salary_class <- c(na.omit(salary_class), "Prefer not to answer")
    salary_class1 <- c(na.omit(salary_class1), "Prefer not to answer")
    ipsos_data$household_income<- factor(ipsos_data$household_income,levels=salary_class1)
    ipsos_data$household_income_min<- factor(ipsos_data$household_income_min,levels=salary_class)
    
  }
  ## get variable "education_level" (in CH01EDU)
  ipsos_data$education_level <- NA
  if(sum(grepl("CH01EDU", colnames(ipsos_data)))>0){
    ipsos_data$education_level <- ipsos_data$CH01EDU 
  }
  rename_education<- function(x){
    if(is.na(x)){return(NA)}
    else if(x== "Advanced Vocational College / Higher vocational education"){return("Advanced vocational education")}
    else if(x== "Lower Secondary School / Gymnasium"){return("Gymnasium")}
    else if(x== "Postgraduate studies – Doctorate"){return("Higher education (e.g., Bachelor, Master or PhD)")}
    else if(x== "Postgraduate studies – Master, MBA"){return("Higher education (e.g., Bachelor, Master or PhD)")}
    else if(x== "Primary School"){return("Obligatory school")}
    else if(x== "University"){return("Higher education (e.g., Bachelor, Master or PhD)")}
    else if(x== "Upper Secondary School / Lyceum"){return("Gymnasium")}
    else if(x== "Vocational / Technical education"){return("Vocational education")}
    #else {return("Unknown")} 
  }
  ipsos_data$education_level <- sapply(ipsos_data$education_level, rename_education)
  ipsos_data$education_level <- factor(ipsos_data$education_level, levels =c("Obligatory school","Vocational education", "Gymnasium", "Advanced vocational education", "Higher education (e.g., Bachelor, Master or PhD)"))

  ## get variable "language_ch" (in QMktSize_10_1)
  rename_language_ch <- function(x){
    if(is.na(x)){return(NA)}
    else if(grepl("Linguistic Region French",x)){return("French")}
    else if(grepl("Région linguistique française",x)){return("French")}
    else if(grepl("Linguistic Region German",x)){return("German")}
    else if(grepl("Région linguistique allemande",x)){return("German")}
    else if(grepl("Linguistic Region Italian",x)){return("Italian")}
    else if(grepl("Région linguistique italienne",x)){return("Italian")}
    else if(grepl("Région linguistique romanche",x)){return("Romanche")}
    #else {return("Unknown")} 
  }
  ipsos_data$language_ch <- sapply(ipsos_data$QMktSize_10_1, rename_language_ch)
  ## get variable "country_birth" (in QRORIGIN +QRORIGIN_rec)
  ipsos_data$country_birth <- ipsos_data$QRORIGIN_rec
  ipsos_data$country_mother <- ipsos_data$QMORIGIN_rec
  ipsos_data$country_father <- ipsos_data$QFORIGIN_rec

  ## get variable "years_ch" i.e. years in Switzerland if born abroad (only for after Sep 2021) 
  ipsos_data$years_ch <- NA
  if(min(ipsos_data$date) > as_date("2021-10-01")){
    if(length(ipsos_data$QRORIGINYEARS._1)>0){
      ipsos_data$years_ch[!is.na(ipsos_data$QRORIGINYEARS._1)] <- as.numeric(ipsos_data$QRORIGINYEARS._1[!is.na(ipsos_data$QRORIGINYEARS._1)])
      ipsos_data$years_ch[is.na(ipsos_data$QRORIGINYEARS._1)] <- ipsos_data$QRORIGINYEARS[is.na(ipsos_data$QRORIGINYEARS._1)]
    }
    else if(length(ipsos_data$QRORIGINYEARS_1)>0){
      ipsos_data$years_ch[!is.na(ipsos_data$QRORIGINYEARS_1)] <- as.numeric(ipsos_data$QRORIGINYEARS_1[!is.na(ipsos_data$QRORIGINYEARS_1)])
      ipsos_data$years_ch[is.na(ipsos_data$QRORIGINYEARS_1)] <- ipsos_data$QRORIGINYEARS[is.na(ipsos_data$QRORIGINYEARS_1)]
      
    }
  }
  ## get variable "years_ch" i.e. years in Switzerland if born abroad (only for after Sep 2021) 

  ### vaccination; 
  ## get variable "vaccinated" ("yes", "no", in QXX1) at least one dose
#!!check later that once yes always yes)  
  yes_no_na_fun <- function(x){
    if(is.na(x))return(NA)
    else if(x=="Yes")return(1)
    else if(x=="No")return(0)
    else if(x=="Prefer not to answer")return("Prefer not to answer")
    else("?")
  }
  ipsos_data$vaccinated <- sapply(ipsos_data$QXX1,FUN=yes_no_na_fun)

  
  ## get variable "vaccine_doses" (0,1,2, in QXX2 if once 2 always 2), 
  ipsos_data$vaccine_doses <- ipsos_data$QXX2
  if(unique(ipsos_data$Sampletype) %in% c("Sampletype=2 Parent sample",2)){
    ipsos_data$vaccine_doses <- ipsos_data$QPXX2
  }
  ## get variable "vaccine_dose1_date" (in QXX3_1_scale),

  ipsos_data$vaccine_dose1_date <- as_date(as.POSIXct(ipsos_data$QXX3_1_scale, tz = "UTC",origin = as_date("1582-10-14"), '%Y-%m-%d'))
  if(unique(ipsos_data$Sampletype) %in% c("Sampletype=2 Parent sample",2)){
    ipsos_data$vaccine_dose1_date <- as_date(as.POSIXct(ipsos_data$QPXX3_1_scale, tz = "UTC",origin = as_date("1582-10-14"), '%Y-%m-%d'))
  }
  ## get variable "vaccine_dose2_date" (in QXX3_2_scale)
  ipsos_data$vaccine_dose2_date <- as_date(as.POSIXct(ipsos_data$QXX3_2_scale, tz = "UTC",origin = as_date("1582-10-14"), '%Y-%m-%d'))
  if(unique(ipsos_data$Sampletype) %in% c("Sampletype=2 Parent sample",2)){
    ipsos_data$vaccine_dose2_date <- as_date(as.POSIXct(ipsos_data$QPXX3_2_scale, tz = "UTC",origin = as_date("1582-10-14"), '%Y-%m-%d'))
  }
  ## get variable "vaccine_dose3_date" (in QXX3_3_scale)
  ipsos_data$vaccine_dose3_date <- as_date(as.POSIXct(ipsos_data$QXX3_3_scale, tz = "UTC",origin = as_date("1582-10-14"), '%Y-%m-%d'))
  if(unique(ipsos_data$Sampletype) %in% c("Sampletype=2 Parent sample",2)){
    ipsos_data$vaccine_dose3_date <- as_date(as.POSIXct(ipsos_data$QPXX3_3_scale, tz = "UTC",origin = as_date("1582-10-14"), '%Y-%m-%d'))
  }
  ## get variable "change_behavior_after_vaccine"  (only for after May 2021),
  ipsos_data$change_behavior_after_vaccine <- NA
  if(min(ipsos_data$date) > as_date("2021-06-01") & unique(ipsos_data$Sampletype) %in% c("Sampletype=1 Main sample",1)){ipsos_data$change_behavior_after_vaccine <- ipsos_data$QXX5A}
  ## get variable "change_behavior" last two weeks (only for after May 2021),
  ipsos_data$change_behavior <- NA
  if(min(ipsos_data$date) >as_date("2021-06-01") & unique(ipsos_data$Sampletype) %in% c("Sampletype=1 Main sample",1)){
    ipsos_data$change_behavior <- ipsos_data$QXX5B }
  ## get variable "vaccine_want" (only for after May 2021), these that got vaccine also wanted. and question also asked for children!
  ipsos_data$vaccine_want <- NA
  if(min(ipsos_data$date) > as_date("2021-06-01")& unique(ipsos_data$Sampletype) %in% c("Sampletype=1 Main sample",1)){
    ipsos_data$vaccine_want <- ipsos_data$QXX4}
  if(min(ipsos_data$date) > as_date("2021-06-01")& unique(ipsos_data$Sampletype) %in% c("Sampletype=2 Parent sample",2)){
    ipsos_data$vaccine_want <- ipsos_data$QPXX4}
  ## get variable "motivation_vaccinated" (only for after Sep 2021)
  ipsos_data$motivation_vaccinated <- NA
  if(min(ipsos_data$date) >as_date("2021-10-01") & unique(ipsos_data$Sampletype) %in% c("Sampletype=1 Main sample",1)){
    ipsos_data$motivation_vaccinated <- ipsos_data$QXX6A }
  ## motivation_vaccinated (only for after Sep 2021)
  if(min(ipsos_data$date) >as_date("2021-10-01") & unique(ipsos_data$Sampletype) %in% c("Sampletype=2 Parent sample",2)){
    ipsos_data$motivation_vaccinated <- ipsos_data$QPXX6A }
  ## get variable "why_notvaccinated" (only for after Sep 2021)
  ipsos_data$why_notvaccinated <- NA
  if(min(ipsos_data$date) >as_date("2021-10-01") & unique(ipsos_data$Sampletype) %in% c("Sampletype=1 Main sample",1)){
    ipsos_data$why_notvaccinated <- ipsos_data$QXX6b }
  ## reason_child_notvaccinating (only for after Sep 2021)
  if(min(ipsos_data$date) >as_date("2021-10-01") & unique(ipsos_data$Sampletype) %in% c("Sampletype=2 Parent sample",2)){
    ipsos_data$why_notvaccinated <- ipsos_data$QPXX6B }
  ## get variable "got_booster" (only for after Sep 2021)
  ipsos_data$got_booster <- NA
  if(min(ipsos_data$date) >as_date("2021-10-01") & unique(ipsos_data$Sampletype) %in% c("Sampletype=1 Main sample",1)){
    ipsos_data$got_booster <- ipsos_data$QXX7 }
  ## get variable "motivating_booster" (only for after Sep 2021)
  ipsos_data$encourage_vaccination <- NA
  if(min(ipsos_data$date) >as_date("2021-10-01") & unique(ipsos_data$Sampletype) %in% c("Sampletype=1 Main sample",1)){
    for (i in grep("QXX8", colnames(ipsos_data))) {##length(ipsos_data[,grep("QXX8", colnames(ipsos_data))]))
      if(1==(i -(grep("QXX8", colnames(ipsos_data))[1]-1))){
        ipsos_data[,i] <- gsub("Yes","More information and reassurance on the safety of vaccines",ipsos_data[,i])#1. More information and reassurance on the safety of vaccines 
      }
      if(2==(i -(grep("QXX8", colnames(ipsos_data))[1]-1))){
        ipsos_data[,i] <- gsub("Yes","Easier to obtain a Covid certificate (e.g. for events, traveling, etc.",ipsos_data[,i])#2. Easier to obtain a Covid certificate (e.g. for events, traveling, etc.
      }
      if(3==(i -(grep("QXX8", colnames(ipsos_data))[1]-1))){
        ipsos_data[,i] <- gsub("Yes","There are no reasons that will convince me to get vaccinated.",ipsos_data[,i])#3. There are no reasons that will convince me to get vaccinated.
      }
      if(4==(i -(grep("QXX8", colnames(ipsos_data))[1]-1))){
        ipsos_data[,i] <- gsub("Yes","More information about efficacy of vaccine to reduce disease (e.g. Long Covid).",ipsos_data[,i])#4. More information about efficacy of vaccine to reduce disease (e.g. Long Covid).
      }
      if(5==(i -(grep("QXX8", colnames(ipsos_data))[1]-1))){
        ipsos_data[,i] <- gsub("Yes","When a health/medical professional personally advises me to get a vaccine.",ipsos_data[,i])#5. When a health/medical professional personally advises me to get a vaccine.
        }
      if(6==(i -(grep("QXX8", colnames(ipsos_data))[1]-1))){
          ipsos_data[,i] <- gsub("Yes","It becomes easier to make an appointment.",ipsos_data[,i])#6. It becomes easier to make an appointment.
        }
        if(7==(i -(grep("QXX8", colnames(ipsos_data))[1]-1))){
          ipsos_data[,i] <- gsub("Yes","More support from people I trust (family, friends, etc.)",ipsos_data[,i])#7. More support from people I trust (family, friends, etc.)
        }
        if(8==(i -(grep("QXX8", colnames(ipsos_data))[1]-1))){
          ipsos_data[,i] <- gsub("Yes","Other vaccines become available",ipsos_data[,i])#8. Other vaccines become available
        }
        if(9==(i -(grep("QXX8", colnames(ipsos_data))[1]-1))){
          ipsos_data[,i] <- gsub("Yes","Another reason",ipsos_data[,i])#9. Another reason
        }
        if(10==(i -(grep("QXX8", colnames(ipsos_data))[1]-1))){
          ipsos_data[,i] <- gsub("Yes","Don’t know",ipsos_data[,i])#10. Don’t know
        }
        if(11==(i -(grep("QXX8", colnames(ipsos_data))[1]-1))){
          ipsos_data[,i] <- gsub("Yes","Prefer not to say",ipsos_data[,i])#11. Prefer not to say
        }
      }
    for (i in 1:length(ipsos_data[1,])) {
      if(sum(!is.na(ipsos_data[i,grep("QXX8", colnames(ipsos_data))]))>0){
        ipsos_data$encourage_vaccination[i] <- paste0(ipsos_data[i,grep("QXX8", colnames(ipsos_data))],collapse =";")
      }
    }
    ipsos_data$encourage_vaccination <- gsub("No;","",ipsos_data$encourage_vaccination)
    ipsos_data$encourage_vaccination <- gsub(";No","",ipsos_data$encourage_vaccination)
  }
  
  
  ### adherence to measures;
  ipsos_data$agreement_measures <- ipsos_data$Q90 #how would you describe the measures currently in place to control the Coranavirus (COVID-19) epidemic in Switzerland?
  #adherence: #_1, to _9 # Which, if any, of these reasons apply to why you follow the current regulations and measures in place to control the Coranavirus (COVID-19) epidemic in Switzerland?
  ipsos_data$adherence_fines <- ipsos_data$Q91_1 #I will be fined if I don’t
  ipsos_data$adherence_law <- ipsos_data$Q91_2 #It is the law
  ipsos_data$adherence_righthing <- ipsos_data$Q91_3 #It is the right thing to do
  ipsos_data$adherence_protect <- ipsos_data$Q91_4 #To protect my family and friends from the virus
  ipsos_data$adherence_protect_me <- ipsos_data$Q91_5 #To protect myself for the virus
  ipsos_data$adherence_want_agree <- ipsos_data$Q91_6 #I want to - most of these measures make sense to me
  ipsos_data$adherence_want <- ipsos_data$Q91_7 #I want to - some of these measures make sense to me
  ipsos_data$adherence_no <- ipsos_data$Q91_8 #I do not follow the current regulations and measures
  ipsos_data$adherence_dontknow <- ipsos_data$Q91_9 #Don’t know
  
  # feelings about covid
  ipsos_data$covid_serious_forme <- ipsos_data$Q35_1_scale #  Coronavirus would be a serious illness for me
  ipsos_data$covid_likelytochatch <- ipsos_data$Q35_2_scale #  I am likely to catch coronavirus
  ipsos_data$covid_worried_spreading <- ipsos_data$Q35_3_scale #  I’m worried that I might spread coronavirus to someone who is vulnerable

  # contacts:
  ipsos_data$no_contacts <- ipsos_data$Q74
  
  ### health
  # get variable "pos_last2w" in Q32
  ipsos_data$pos_last2w <- ipsos_data$Q32
  # get variable "pos" in Q32a
  ipsos_data$pos <- ipsos_data$Q32a
  # get variable "symptoms" Q29_0_scale_ 
  
  # get variable "pregnant"
  ipsos_data$pregnant <- NA
  if(unique(ipsos_data$Sampletype) %in% c("Sampletype=1 Main sample",1)){
    ipsos_data$pregnant <-  sapply(ipsos_data$Q15,FUN=yes_no_na_fun)
    }
  # get variable "riskgroup"
  ipsos_data$riskgroup <- sapply(ipsos_data$Q28a_0_scale,FUN=yes_no_na_fun) # _0_ person itself as.numeric
  if(unique(ipsos_data$Sampletype) %in% c("Sampletype=2 Parent sample",2)){
    for (l in 1:length(ipsos_data$Respondent_ID)){
      num <- regmatches(ipsos_data$QP53a_choosen[l], gregexpr("([0-9]+)", ipsos_data$QP53a_choosen[l]))[[1]][2]
      ipsos_data[l,grep(paste0("Q28a_",num,"_scale"), colnames(ipsos_data))] <- ipsos_data[l,grepl("Q28a_0_scale", colnames(ipsos_data))] # risk group
      rm(num)
    }
    }
  # get variable "household_riskgroup"
  for (l in 1:length(ipsos_data$riskgroup)){
    risk <- NA
    for (i in grep("Q28a_", colnames(ipsos_data))) {
      riskgroup <- sapply(ipsos_data[l,i],FUN=yes_no_na_fun)
      if(is.na(riskgroup)){
        riskgroup <- riskgroup
      }
      else{
        if(is.numeric(riskgroup)){
          risk <- sum(na.omit(c(risk, riskgroup))) 
        }
        else if(!is.numeric(riskgroup)){
        risk <- sum(na.omit(risk))
      }
      }
    }
    ipsos_data$household_riskgroup[l] <- risk
  }
#print(n)
  ## get variable
  part_data <- rbind(part_data,ipsos_data[,c("part_id","date","panel_wave","panel_wave_id","age_part","part_age_est_min","part_age_est_max","lower.age.limit",
                                             "gender","gender_responder","zip_code","region","canton","region_ch","grossregion","region_urban_rural","occupation","employment","household_income",
                                             "education_level","language_ch","country_birth","country_mother","country_father","years_ch","vaccinated","vaccine_doses","vaccine_dose1_date","vaccine_dose2_date","vaccine_dose3_date",
                                             "change_behavior_after_vaccine","change_behavior", "vaccine_want","motivation_vaccinated", "why_notvaccinated","got_booster", "encourage_vaccination",
                                             "agreement_measures","adherence_fines","adherence_law","adherence_righthing","adherence_protect","adherence_protect_me",
                                             "adherence_want_agree","adherence_want","adherence_no","adherence_dontknow","no_contacts",
                                             "covid_serious_forme", "covid_likelytochatch","covid_worried_spreading","pos_last2w","pos", "pregnant","riskgroup","household_size","household_riskgroup")])
  

  }
  part_data$panel_wave <- as.character(factor(part_data$panel_wave,level=sort(unique(part_data$panel_wave))))
  
  #write.csv(part_data, "data/part_ch.csv")
  return(as.data.frame(part_data))
  
}
