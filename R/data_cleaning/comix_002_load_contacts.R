#' ---
#' title: "Load data"
#' author: "Martina Reichmuth"
#' date: "19/10/2021"
#' ---

comix_002_load_contacts = function() {
  MyMerge <- function(x, y){
    df <- merge(x, y, by= c("panel_wave_id","variable"), all=TRUE)
    return(df)
  }
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
  contacts_data <- c()
  for (n in 1:length(ipsos_files)) {#n <- 7 #2){#
    ipsos_data <- c() 
    options(warn=-1)
    #ipsos_data <- read_sav(paste0(path_name,"/data/raw/Ipsos//",ipsos_files[n]))
    ipsos_data <- read.spss(paste0(path_name,"/data/raw/Ipsos//",ipsos_files[n]),reencode ="UTF-8")
    ipsos_data <- as.data.frame(ipsos_data)
    if(n==grep("20-030971_G2_Wave3_Parents_CH_Final_v1_23072021_IntClientUse.sav",ipsos_files,fixed = TRUE)){
      ipsos_data1 <- read.spss(paste0(path_name,"/data/raw/Ipsos//20-030971_G2_Wave3 recontact_Parents_CH_QPXX4_Final_v1_160921_IntClientUse.sav"),reencode ="UTF-8")
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
    
  ### Contact file:
      
      ##  get variable "date"
      ipsos_data$date <- as_date(paste0(ipsos_data$CurrentYear,"-", ipsos_data$CurrentMonth,"-", ipsos_data$CurrentDay),format="%Y-%m-%d")
      # get variable "panel"
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
      
      
      # SECTION Q62, Q63, Q66, Q67, Q68, Q69, Q71, Q77, Q72, Q73, Q74, Q79a-c, Q80a-c, Q81a-c only asked if  QSAMPLETYPE=1
      
      # individual vs mass contacts
      if(unique(ipsos_data$Sampletype) %in% c("Sampletype=1 Main sample",1)){#
        ipsos_data <- ipsos_data[ipsos_data$Q74 != "I did not have any contacts",]
      }
      if(unique(ipsos_data$Sampletype) %in% c("Sampletype=2 Parent sample",2)){#
        ipsos_data <- ipsos_data[ipsos_data$QP74 != "I did not have any contacts",]
      }
      
      
      ## household members
      if(unique(ipsos_data$Sampletype) %in% c("Sampletype=2 Parent sample",2)){
        ## household members
        child_num <- c()
        for (l in 1:length(ipsos_data$Respondent_ID)){
          pat_data <- ipsos_data[l,]
          num <- regmatches(pat_data$QP53a_choosen, gregexpr("([0-9]+)", pat_data$QP53a_choosen))[[1]][2]
          pat_data[,grep(paste0("Q23_LOOP_",num,"_Q23"), colnames(pat_data))] <- pat_data$resp_age
          pat_data[,grep(paste0("Q23_LOOP_",num,"_Q24"), colnames(pat_data))] <- pat_data$GENDER_NonBinary# gender
          pat_data[,grep(paste0("Q23_LOOP_",num,"_Q25"), colnames(pat_data))] <- pat_data$Q3# occupation
          pat_data[,grep(paste0("Q23_LOOP_",num,"_Q26"), colnames(pat_data))] <- pat_data$CH01EDU# education
          pat_data[,grep(paste0("Q28a_",num,"_scale"), colnames(pat_data))] <- pat_data[,grepl("Q28a_0_scale", colnames(pat_data))] # risk group
          child_num <- c(child_num,num)
          rm(num)
          
        }
      }
      #melt(ipsos_data[,grep("panel_wave_id|resp_age|Q23_LOOP_([0-9]+)_Q23", colnames(ipsos_data))], id.vars = c("panel_wave_id"), value.name  = c("cnt_age"))# library(reshape2)
      
      
      cnt_age <-  melt(ipsos_data[,grep("panel_wave_id|Q23_LOOP_([0-9]+)_Q23", colnames(ipsos_data))], id.vars = c("panel_wave_id"), value.name  = c("cnt_age"))# library(reshape2)
      cnt_age$variable<- as.character(gsub("^Q23_LOOP_*\\s*|_Q23","",cnt_age$variable))
      cnt_gender <-  melt(ipsos_data[,grep("panel_wave_id|Q23_LOOP_([0-9]+)_Q24", colnames(ipsos_data))], id.vars = c("panel_wave_id"), value.name  = c("cnt_gender")) #gender
      cnt_gender$variable<- as.character(gsub("^Q23_LOOP_*\\s*|_Q24","",cnt_gender$variable))
      cnt_dis_physical <- melt(ipsos_data[,grep("panel_wave_id|Q62", colnames(ipsos_data))], id.vars = c("panel_wave_id"), value.name  = c("cnt_dis_physical")) #physcial 
      cnt_dis_physical$variable<- as.character(gsub("^Q62_*\\s*|_scale","",cnt_dis_physical$variable))
      cnt_occupation <- melt(ipsos_data[,grep("panel_wave_id|Q23_LOOP_([0-9]+)_Q25", colnames(ipsos_data))], id.vars = c("panel_wave_id"), value.name  = c("hh_occupation")) #occupation 
      cnt_occupation$variable<- as.character(gsub("^Q23_LOOP_*\\s*|_Q25","",cnt_occupation$variable))
      Q26 <- ipsos_data[,grep("panel_wave_id|Q23_LOOP_([0-9]+)_Q26", colnames(ipsos_data))] #education level
      cnt_education <- melt(Q26[,!grepl("filter", colnames(Q26))], id.vars = c("panel_wave_id"), value.name  = c("hh_education")) #education
      cnt_education$variable<- as.character(gsub("^Q23_LOOP_*\\s*|_Q26","",cnt_education$variable))
      Q28 <- ipsos_data[,!grepl("Q28a_0_scale", colnames(ipsos_data))] #risk groups
      cnt_risk_group <- melt(Q28[,grep("panel_wave_id|Q28a_([0-9]+)_scale", colnames(Q28))], id.vars = c("panel_wave_id"), value.name  = c("hh_riskgroup")) #risk groups
      cnt_risk_group$variable<- as.character(gsub("^Q28a_*\\s*|_scale","",cnt_risk_group$variable))
      #Q29 <- ipsos_data[,!grepl("Q29_0_scale", colnames(ipsos_data))] #symptoms 1-14 variables
      #length(Q29[,grep("Q29_[1-9]_scale([0-9]+)", colnames(Q29))]) #symptoms 1-14 variables
      cnt_data <- Reduce(MyMerge, list(cnt_age,cnt_gender,cnt_dis_physical,cnt_occupation,cnt_education, cnt_risk_group))
      
      cnt_data <- cnt_data[,!grepl("variable", colnames(cnt_data))]
      cnt_data <- cnt_data[rowSums(is.na(cnt_data)) < (ncol(cnt_data)-1), ]
      
      cnt_data$household_member <- "Yes"
      cnt_data$cnt_home <- "Yes"
      cnt_data$cnt_house <- "No"
      cnt_data$cnt_work <-  "No"
      cnt_data$cnt_worship <-  "No"
      cnt_data$cnt_publictansport <-  "No"
      cnt_data$cnt_school <-  "No"
      cnt_data$cnt_shop <-  "No"
      cnt_data$cnt_shop_notessential <-  "No"
      cnt_data$cnt_entertainmentplace <-  "No"
      cnt_data$cnt_sport <-  "No"
      cnt_data$cnt_outside <-  "No"
      cnt_data$cnt_othersetting1 <-  "No"
      cnt_data$cnt_othersetting2 <- "home"
      cnt_data$cnt_healthcare <-  "No"
      cnt_data$cnt_saloncare <-  "No"
      cnt_data$cnt_freq <- "household_member"
      cnt_datacnt_dis_physical <- "household_member"
      cnt_data$cnt_dis_2m_plus <- "household_member"
      cnt_data$cnt_dis_1m_plus <- "household_member"
      cnt_data$cnt_dis_1m_minus <- "household_member"
      cnt_data$cnt_dis_mask <- "household_member"
      cnt_data$cnt_dis_wash_before <- "household_member"
      cnt_data$cnt_dis_wash_after <- "household_member"
      cnt_data$cnt_dis_notsaid <- "household_member"
      cnt_data$cnt_dis_other <- "household_member"
      cnt_data$cnt_duration <- "houshold member"
      cnt_data$cnt_dis_dontknow <-  "household_member"
      cnt_data$cnt_relationship <- "household_member"
      cnt_data$num_cnt_beforeCOVID19 <- "household_member"
      cnt_data_hh <- cnt_data
      
      if(unique(ipsos_data$Sampletype) %in% c("Sampletype=1 Main sample",1)){
      ## not household members
      cnt_age <-  melt(ipsos_data[,grep("panel_wave_id|Q66_LOOP_([0-9]+)_Q66", colnames(ipsos_data))], id.vars = c("panel_wave_id"), value.name  = c("cnt_age"))# library(reshape2)
      cnt_age$variable<- as.character(gsub("^Q66_LOOP_*\\s*|_Q66","",cnt_age$variable))
      cnt_gender <-  melt(ipsos_data[,grep("panel_wave_id|Q66_LOOP_([0-9]+)_Q67", colnames(ipsos_data))], id.vars = c("panel_wave_id"), value.name  = c("cnt_gender")) #gender
      cnt_gender$variable<- as.character(gsub("^Q66_LOOP_*\\s*|_Q67","",cnt_gender$variable))
      cnt_relationship <- melt(ipsos_data[,grep("panel_wave_id|Q66_LOOP_([0-9]+)_Q68", colnames(ipsos_data))], id.vars = c("panel_wave_id"), value.name  = c("cnt_relationship")) #relationship 
      cnt_relationship$variable<- as.character(gsub("^Q66_LOOP_*\\s*|_Q68","",cnt_relationship$variable))
      cnt_duration <- melt(ipsos_data[,grep("panel_wave_id|Q66_LOOP_([0-9]+)_Q72", colnames(ipsos_data))], id.vars = c("panel_wave_id"), value.name  = c("cnt_duration")) #duration of contact 
      cnt_duration$variable<- as.character(gsub("^Q66_LOOP_*\\s*|_Q72","",cnt_duration$variable))
      num_cnt_beforeCOVID19<- melt(ipsos_data[,grep("panel_wave_id|Q66_LOOP_([0-9]+)_Q69", colnames(ipsos_data))], id.vars = c("panel_wave_id"), value.name  = c("num_cnt_beforeCOVID19")) #number of contacts before covid-19
      num_cnt_beforeCOVID19$variable<- as.character(gsub("^Q66_LOOP_*\\s*|_Q69","",num_cnt_beforeCOVID19$variable))
      
      cnt_setting<- ipsos_data[,grep("panel_wave_id|Q71_", colnames(ipsos_data))]
      cnt_home <-  melt(cnt_setting[,c(grep("panel_wave_id", colnames(cnt_setting)), grep("_1$", colnames(cnt_setting)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_home")) #
      cnt_home$variable<- as.character(gsub("^Q66_LOOP_*\\s*|_Q71_([0-9]+)","",cnt_home$variable))
      cnt_house <-  melt(cnt_setting[,c(grep("panel_wave_id", colnames(cnt_setting)), grep("_2$", colnames(cnt_setting)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_house")) #
      cnt_house$variable<- as.character(gsub("^Q66_LOOP_*\\s*|_Q71_([0-9]+)","",cnt_house$variable))
      cnt_work <-  melt(cnt_setting[,c(grep("panel_wave_id", colnames(cnt_setting)), grep("_3$", colnames(cnt_setting)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_work")) #
      cnt_work$variable<- as.character(gsub("^Q66_LOOP_*\\s*|_Q71_([0-9]+)","",cnt_work$variable))
      cnt_worship <-  melt(cnt_setting[,c(grep("panel_wave_id", colnames(cnt_setting)), grep("_4$", colnames(cnt_setting)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_worship")) #
      cnt_worship$variable<- as.character(gsub("^Q66_LOOP_*\\s*|_Q71_([0-9]+)","",cnt_worship$variable))
      cnt_publictansport <-  melt(cnt_setting[,c(grep("panel_wave_id", colnames(cnt_setting)), grep("_5$", colnames(cnt_setting)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_publictansport")) #
      cnt_publictansport$variable<- as.character(gsub("^Q66_LOOP_*\\s*|_Q71_([0-9]+)","",cnt_publictansport$variable))
      cnt_school <-  melt(cnt_setting[,c(grep("panel_wave_id", colnames(cnt_setting)), grep("_6$", colnames(cnt_setting)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_school")) #
      cnt_school$variable<- as.character(gsub("^Q66_LOOP_*\\s*|_Q71_([0-9]+)","",cnt_school$variable))
      cnt_shop <-  melt(cnt_setting[,c(grep("panel_wave_id", colnames(cnt_setting)), grep("_7$", colnames(cnt_setting)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_shop")) #
      cnt_shop$variable<- as.character(gsub("^Q66_LOOP_*\\s*|_Q71_([0-9]+)","",cnt_shop$variable))
      cnt_shop_notessential <-  melt(cnt_setting[,c(grep("panel_wave_id", colnames(cnt_setting)), grep("_8$", colnames(cnt_setting)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_shop_notessential")) #
      cnt_shop_notessential$variable<- as.character(gsub("^Q66_LOOP_*\\s*|_Q71_([0-9]+)","",cnt_shop_notessential$variable))
      cnt_entertainmentplace <-  melt(cnt_setting[,c(grep("panel_wave_id", colnames(cnt_setting)), grep("_9$", colnames(cnt_setting)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_entertainmentplace")) #
      cnt_entertainmentplace$variable<- as.character(gsub("^Q66_LOOP_*\\s*|_Q71_([0-9]+)","",cnt_entertainmentplace$variable))
      cnt_sport <-  melt(cnt_setting[,c(grep("panel_wave_id", colnames(cnt_setting)), grep("_10$", colnames(cnt_setting)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_sport")) #
      cnt_sport$variable<- as.character(gsub("^Q66_LOOP_*\\s*|_Q71_([0-9]+)","",cnt_sport$variable))
      cnt_outside <-  melt(cnt_setting[,c(grep("panel_wave_id", colnames(cnt_setting)), grep("_11$", colnames(cnt_setting)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_outside")) #
      cnt_outside$variable<- as.character(gsub("^Q66_LOOP_*\\s*|_Q71_([0-9]+)","",cnt_outside$variable))
      cnt_othersetting1 <-  melt(cnt_setting[,c(grep("panel_wave_id", colnames(cnt_setting)), grep("_12$", colnames(cnt_setting)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_othersetting1")) #
      cnt_othersetting1$variable<- as.character(gsub("^Q66_LOOP_*\\s*|_Q71_([0-9]+)","",cnt_othersetting1$variable))
      cnt_healthcare <-  melt(cnt_setting[,c(grep("panel_wave_id", colnames(cnt_setting)), grep("_14$", colnames(cnt_setting)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_healthcare")) #
      cnt_healthcare$variable<- as.character(gsub("^Q66_LOOP_*\\s*|_Q71_([0-9]+)","",cnt_healthcare$variable))
      cnt_saloncare <-  melt(cnt_setting[,c(grep("panel_wave_id", colnames(cnt_setting)), grep("_15$", colnames(cnt_setting)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_saloncare")) #
      cnt_saloncare$variable<- as.character(gsub("^Q66_LOOP_*\\s*|_Q71_([0-9]+)","",cnt_saloncare$variable))
      cnt_othersetting2 <-  melt(cnt_setting[,c(grep("panel_wave_id", colnames(cnt_setting)), grep("_121$", colnames(cnt_setting)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_othersetting2")) #
      cnt_othersetting2$variable<- as.character(gsub("^Q66_LOOP_*\\s*|_Q71_([0-9]+)","",cnt_othersetting2$variable))
      
      cnt_freq <-  melt(ipsos_data[,grep("panel_wave_id|Q66_LOOP_([0-9]+)_Q69", colnames(ipsos_data))], id.vars = c("panel_wave_id"), value.name  = c("cnt_freq"))# library(reshape2)
      cnt_freq$variable<- as.character(gsub("^Q66_LOOP_*\\s*|_Q69","",cnt_freq$variable))
      
      cnt_distance<- ipsos_data[,grep("panel_wave_id|Q66_LOOP_([0-9]+)_q77_", colnames(ipsos_data))]
      cnt_dis_physical <- melt(cnt_distance[,c(grep("panel_wave_id", colnames(cnt_distance)), grep("_1$", colnames(cnt_distance)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_dis_physical")) #
      cnt_dis_physical$variable<- as.character(gsub("^Q66_LOOP_*\\s*|_q77_([0-9]+)","",cnt_dis_physical$variable))
      cnt_dis_2m_plus <- melt(cnt_distance[,c(grep("panel_wave_id", colnames(cnt_distance)), grep("_2$", colnames(cnt_distance)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_dis_2m_plus")) #
      cnt_dis_2m_plus$variable<- as.character(gsub("^Q66_LOOP_*\\s*|_q77_([0-9]+)","",cnt_dis_2m_plus$variable))
      cnt_dis_1m_plus <- melt(cnt_distance[,c(grep("panel_wave_id", colnames(cnt_distance)), grep("_3$", colnames(cnt_distance)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_dis_1m_plus")) #
      cnt_dis_1m_plus$variable<- as.character(gsub("^Q66_LOOP_*\\s*|_q77_([0-9]+)","",cnt_dis_1m_plus$variable))
      cnt_dis_1m_minus <- melt(cnt_distance[,c(grep("panel_wave_id", colnames(cnt_distance)), grep("_4$", colnames(cnt_distance)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_dis_1m_minus")) #
      cnt_dis_1m_minus$variable<- as.character(gsub("^Q66_LOOP_*\\s*|_q77_([0-9]+)","",cnt_dis_1m_minus$variable))
      cnt_dis_mask <- melt(cnt_distance[,c(grep("panel_wave_id", colnames(cnt_distance)), grep("_5$", colnames(cnt_distance)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_dis_mask")) #
      cnt_dis_mask$variable<- as.character(gsub("^Q66_LOOP_*\\s*|_q77_([0-9]+)","",cnt_dis_mask$variable))
      cnt_dis_wash_before <- melt(cnt_distance[,c(grep("panel_wave_id", colnames(cnt_distance)), grep("_6$", colnames(cnt_distance)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_dis_wash_before")) #
      cnt_dis_wash_before$variable<- as.character(gsub("^Q66_LOOP_*\\s*|_q77_([0-9]+)","",cnt_dis_wash_before$variable))
      cnt_dis_wash_after <- melt(cnt_distance[,c(grep("panel_wave_id", colnames(cnt_distance)), grep("_7$", colnames(cnt_distance)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_dis_wash_after")) #
      cnt_dis_wash_after$variable<- as.character(gsub("^Q66_LOOP_*\\s*|_q77_([0-9]+)","",cnt_dis_wash_after$variable))
      cnt_dis_notsaid <- melt(cnt_distance[,c(grep("panel_wave_id", colnames(cnt_distance)), grep("_9$", colnames(cnt_distance)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_dis_notsaid")) #
      cnt_dis_notsaid$variable<- as.character(gsub("^Q66_LOOP_*\\s*|_q77_([0-9]+)","",cnt_dis_notsaid$variable))
      cnt_dis_other <- melt(cnt_distance[,c(grep("panel_wave_id", colnames(cnt_distance)), grep("_10$", colnames(cnt_distance)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_dis_other")) #
      cnt_dis_other$variable<- as.character(gsub("^Q66_LOOP_*\\s*|_q77_([0-9]+)","",cnt_dis_other$variable))
      
  
      
     cnt_data <- Reduce(MyMerge, list(cnt_age,cnt_gender,cnt_relationship,cnt_duration, num_cnt_beforeCOVID19,cnt_home,cnt_house,cnt_work,cnt_worship,cnt_publictansport,cnt_school,cnt_shop,cnt_shop_notessential,cnt_entertainmentplace,cnt_sport,cnt_outside,cnt_othersetting1,cnt_healthcare,cnt_saloncare ,cnt_othersetting2,cnt_freq,cnt_dis_physical,cnt_dis_2m_plus,cnt_dis_1m_plus,cnt_dis_1m_minus,cnt_dis_mask,cnt_dis_wash_before,cnt_dis_wash_after,cnt_dis_notsaid,cnt_dis_other))
      cnt_data <- cnt_data[,!grepl("variable", colnames(cnt_data))]
      #cnt_data <- cbind(cnt_age,cnt_gender,cnt_relationship,cnt_duration, num_cnt_beforeCOVID19,cnt_home,cnt_house,cnt_work,cnt_worship,cnt_publictansport,cnt_school,cnt_shop,cnt_shop_notessential,cnt_entertainmentplace,cnt_sport,cnt_outside,cnt_othersetting1,cnt_healthcare,cnt_saloncare ,cnt_othersetting2,cnt_dis_physical,cnt_dis_2m_plus,cnt_dis_1m_plus,cnt_dis_1m_minus,cnt_dis_mask,cnt_dis_wash_before,cnt_dis_wash_after,cnt_dis_notsaid,cnt_dis_other)
      #cnt_data <- cbind(cnt_data[,1],cnt_data[,!grepl("variable|panel_wave_id", colnames(cnt_data))])
      #colnames(cnt_data)[1] <-"panel_wave_id"
      cnt_data <- cnt_data[rowSums(is.na(cnt_data)) < (ncol(cnt_data)-2), ]
      cnt_data$cnt_dis_dontknow <- "not_asked"
      cnt_data$household_member <- 0
      cnt_data$hh_education <- "not_asked"
      cnt_data$hh_riskgroup <- "not_asked"
      cnt_data$hh_occupation <- "not_asked"
      cnt_data_hh<- cnt_data_hh[,match(colnames(cnt_data_hh),colnames(cnt_data))]
      cnt_data <- rbind(cnt_data_hh,cnt_data)
      rm(cnt_data_hh)
      }
    
      
      if(unique(ipsos_data$Sampletype) %in% c("Sampletype=2 Parent sample",2)){
        #QP74,  (work:) QP79a, QP79c, QP81d,  (school:) QP80a, QP80c, QP80d,  (other:) QP81a, QP81c, QP81d
        #control_measures <-  ipsos_data[,grep("panel_wave_id|QP79c|QP80c|QP81c", colnames(ipsos_data))]# a, c,d
        cnt_age <-  melt(ipsos_data[,grep("panel_wave_id|QP66_LOOP_([0-9]+)_QP66", colnames(ipsos_data))], id.vars = c("panel_wave_id"), value.name  = c("cnt_age"))# library(reshape2)
        cnt_age$variable<- as.character(gsub("^QP66_LOOP_*\\s*|_QP66","",cnt_age$variable))
        cnt_gender <-  melt(ipsos_data[,grep("panel_wave_id|QP66_LOOP_([0-9]+)_QP67", colnames(ipsos_data))], id.vars = c("panel_wave_id"), value.name  = c("cnt_gender")) #gender
        cnt_gender$variable<- as.character(gsub("^QP66_LOOP_*\\s*|_QP67","",cnt_gender$variable))
        cnt_relationship <- melt(ipsos_data[,grep("panel_wave_id|QP66_LOOP_([0-9]+)_QP68", colnames(ipsos_data))], id.vars = c("panel_wave_id"), value.name  = c("cnt_relationship")) #relationship 
        cnt_relationship$variable<- as.character(gsub("^QP66_LOOP_*\\s*|_QP68","",cnt_relationship$variable))
        cnt_duration <- melt(ipsos_data[,grep("panel_wave_id|QP66_LOOP_([0-9]+)_QP72", colnames(ipsos_data))], id.vars = c("panel_wave_id"), value.name  = c("cnt_duration")) #duration of contact 
        cnt_duration$variable<- as.character(gsub("^QP66_LOOP_*\\s*|_QP72","",cnt_duration$variable))
        #cnt_inoutdoors<-  melt(ipsos_data[,grep("panel_wave_id|QP66_LOOP_([0-9]+)_QP73", colnames(ipsos_data))], id.vars = c("panel_wave_id"), value.name  = c("cnt_inoutdoors"))# library(reshape2)
        num_cnt_beforeCOVID19<- melt(ipsos_data[,grep("panel_wave_id|QP66_LOOP_([0-9]+)_QP69", colnames(ipsos_data))], id.vars = c("panel_wave_id"), value.name  = c("num_cnt_beforeCOVID19")) #number of contacts before covid-19
        num_cnt_beforeCOVID19$variable<- as.character(gsub("^QP66_LOOP_*\\s*|_QP69","",num_cnt_beforeCOVID19$variable))
        
        cnt_setting<- ipsos_data[,grep("panel_wave_id|QP71_", colnames(ipsos_data))]
        cnt_home <-  melt(cnt_setting[,c(grep("panel_wave_id", colnames(cnt_setting)), grep("_1$", colnames(cnt_setting)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_home")) #
        cnt_home$variable<- as.character(gsub("^QP66_LOOP_*\\s*|_QP71_([0-9]+)","",cnt_home$variable))
        cnt_house <-  melt(cnt_setting[,c(grep("panel_wave_id", colnames(cnt_setting)), grep("_2$", colnames(cnt_setting)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_house")) #
        cnt_house$variable<- as.character(gsub("^QP66_LOOP_*\\s*|_QP71_([0-9]+)","",cnt_house$variable))
        cnt_work <-  melt(cnt_setting[,c(grep("panel_wave_id", colnames(cnt_setting)), grep("_3$", colnames(cnt_setting)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_work")) #
        cnt_work$variable<- as.character(gsub("^QP66_LOOP_*\\s*|_QP71_([0-9]+)","",cnt_work$variable))
        cnt_worship <-  melt(cnt_setting[,c(grep("panel_wave_id", colnames(cnt_setting)), grep("_4$", colnames(cnt_setting)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_worship")) #
        cnt_worship$variable<- as.character(gsub("^QP66_LOOP_*\\s*|_QP71_([0-9]+)","",cnt_worship$variable))
        cnt_publictansport <-  melt(cnt_setting[,c(grep("panel_wave_id", colnames(cnt_setting)), grep("_5$", colnames(cnt_setting)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_publictansport")) #
        cnt_publictansport$variable<- as.character(gsub("^QP66_LOOP_*\\s*|_QP71_([0-9]+)","",cnt_publictansport$variable))
        cnt_school <-  melt(cnt_setting[,c(grep("panel_wave_id", colnames(cnt_setting)), grep("_6$", colnames(cnt_setting)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_school")) #
        cnt_school$variable<- as.character(gsub("^QP66_LOOP_*\\s*|_QP71_([0-9]+)","",cnt_school$variable))
        cnt_shop <-  melt(cnt_setting[,c(grep("panel_wave_id", colnames(cnt_setting)), grep("_7$", colnames(cnt_setting)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_shop")) #
        cnt_shop$variable<- as.character(gsub("^QP66_LOOP_*\\s*|_QP71_([0-9]+)","",cnt_shop$variable))
        cnt_shop_notessential <-  melt(cnt_setting[,c(grep("panel_wave_id", colnames(cnt_setting)), grep("_8$", colnames(cnt_setting)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_shop_notessential")) #
        cnt_shop_notessential$variable<- as.character(gsub("^QP66_LOOP_*\\s*|_QP71_([0-9]+)","",cnt_shop_notessential$variable))
        cnt_entertainmentplace <-  melt(cnt_setting[,c(grep("panel_wave_id", colnames(cnt_setting)), grep("_9$", colnames(cnt_setting)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_entertainmentplace")) #
        cnt_entertainmentplace$variable<- as.character(gsub("^QP66_LOOP_*\\s*|_QP71_([0-9]+)","",cnt_entertainmentplace$variable))
        cnt_sport <-  melt(cnt_setting[,c(grep("panel_wave_id", colnames(cnt_setting)), grep("_10$", colnames(cnt_setting)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_sport")) #
        cnt_sport$variable<- as.character(gsub("^QP66_LOOP_*\\s*|_QP71_([0-9]+)","",cnt_sport$variable))
        cnt_outside <-  melt(cnt_setting[,c(grep("panel_wave_id", colnames(cnt_setting)), grep("_11$", colnames(cnt_setting)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_outside")) #
        cnt_outside$variable<- as.character(gsub("^QP66_LOOP_*\\s*|_QP71_([0-9]+)","",cnt_outside$variable))
        cnt_othersetting1 <-  melt(cnt_setting[,c(grep("panel_wave_id", colnames(cnt_setting)), grep("_12$", colnames(cnt_setting)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_othersetting1")) #
        cnt_othersetting1$variable<- as.character(gsub("^QP66_LOOP_*\\s*|_QP71_([0-9]+)","",cnt_othersetting1$variable))
        cnt_healthcare <-  melt(cnt_setting[,c(grep("panel_wave_id", colnames(cnt_setting)), grep("_14$", colnames(cnt_setting)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_healthcare")) #
        cnt_healthcare$variable<- as.character(gsub("^QP66_LOOP_*\\s*|_QP71_([0-9]+)","",cnt_healthcare$variable))
        cnt_saloncare <-  melt(cnt_setting[,c(grep("panel_wave_id", colnames(cnt_setting)), grep("_15$", colnames(cnt_setting)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_saloncare")) #
        cnt_saloncare$variable<- as.character(gsub("^QP66_LOOP_*\\s*|_QP71_([0-9]+)","",cnt_saloncare$variable))
        cnt_othersetting2 <-  melt(cnt_setting[,c(grep("panel_wave_id", colnames(cnt_setting)), grep("_121$", colnames(cnt_setting)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_othersetting2")) #
        cnt_othersetting2$variable<- as.character(gsub("^QP66_LOOP_*\\s*|_QP71_([0-9]+)","",cnt_othersetting2$variable))
        
        cnt_freq <-  melt(ipsos_data[,grep("panel_wave_id|QP66_LOOP_([0-9]+)_QP69", colnames(ipsos_data))], id.vars = c("panel_wave_id"), value.name  = c("cnt_freq"))# library(reshape2)
        cnt_freq$variable<- as.character(gsub("^QP66_LOOP_*\\s*|_QP69","",cnt_freq$variable))
        
        cnt_distance<- ipsos_data[,grep("panel_wave_id|QP66_LOOP_([0-9]+)_qP77_", colnames(ipsos_data))]
        cnt_dis_physical <- melt(cnt_distance[,c(grep("panel_wave_id", colnames(cnt_distance)), grep("_1$", colnames(cnt_distance)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_dis_physical")) #
        cnt_dis_physical$variable<- as.character(gsub("^QP66_LOOP_*\\s*|_qP77_([0-9]+)","",cnt_dis_physical$variable))
        cnt_dis_2m_plus <- melt(cnt_distance[,c(grep("panel_wave_id", colnames(cnt_distance)), grep("_2$", colnames(cnt_distance)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_dis_2m_plus")) #
        cnt_dis_2m_plus$variable<- as.character(gsub("^QP66_LOOP_*\\s*|_qP77_([0-9]+)","",cnt_dis_2m_plus$variable))
        cnt_dis_1m_plus <- melt(cnt_distance[,c(grep("panel_wave_id", colnames(cnt_distance)), grep("_3$", colnames(cnt_distance)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_dis_1m_plus")) #
        cnt_dis_1m_plus$variable<- as.character(gsub("^QP66_LOOP_*\\s*|_qP77_([0-9]+)","",cnt_dis_1m_plus$variable))
        cnt_dis_1m_minus <- melt(cnt_distance[,c(grep("panel_wave_id", colnames(cnt_distance)), grep("_12$", colnames(cnt_distance)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_dis_1m_minus")) #
        cnt_dis_1m_minus$variable<- as.character(gsub("^QP66_LOOP_*\\s*|_qP77_([0-9]+)","",cnt_dis_1m_minus$variable))
        cnt_dis_mask <- melt(cnt_distance[,c(grep("panel_wave_id", colnames(cnt_distance)), grep("_4$", colnames(cnt_distance)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_dis_mask")) #
        cnt_dis_mask$variable<- as.character(gsub("^QP66_LOOP_*\\s*|_qP77_([0-9]+)","",cnt_dis_mask$variable))
        cnt_dis_wash_before <- melt(cnt_distance[,c(grep("panel_wave_id", colnames(cnt_distance)), grep("_5$", colnames(cnt_distance)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_dis_wash_before")) #
        cnt_dis_wash_before$variable<- as.character(gsub("^QP66_LOOP_*\\s*|_qP77_([0-9]+)","",cnt_dis_wash_before$variable))
        cnt_dis_wash_after <- melt(cnt_distance[,c(grep("panel_wave_id", colnames(cnt_distance)), grep("_6$", colnames(cnt_distance)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_dis_wash_after")) #
        cnt_dis_wash_after$variable<- as.character(gsub("^QP66_LOOP_*\\s*|_qP77_([0-9]+)","",cnt_dis_wash_after$variable))
        
        cnt_dis_notsaid <- melt(cnt_distance[,c(grep("panel_wave_id", colnames(cnt_distance)), grep("_9$", colnames(cnt_distance)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_dis_notsaid")) #
        cnt_dis_notsaid$variable<- as.character(gsub("^QP66_LOOP_*\\s*|_qP77_([0-9]+)","",cnt_dis_notsaid$variable))
        cnt_dis_other <- melt(cnt_distance[,c(grep("panel_wave_id", colnames(cnt_distance)), grep("_10$", colnames(cnt_distance)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_dis_other")) #
        cnt_dis_other$variable<- as.character(gsub("^QP66_LOOP_*\\s*|_qP77_([0-9]+)","",cnt_dis_other$variable))
        
        cnt_dis_dontknow <- melt(cnt_distance[,c(grep("panel_wave_id", colnames(cnt_distance)), grep("_11$", colnames(cnt_distance)))], id.vars = c("panel_wave_id"), value.name  = c("cnt_dis_dontknow")) #
        cnt_dis_dontknow$variable<- as.character(gsub("^QP66_LOOP_*\\s*|_qP77_([0-9]+)","",cnt_dis_dontknow$variable))
        
        
        cnt_data <- Reduce(MyMerge, list(cnt_age,cnt_gender,cnt_relationship,cnt_duration, num_cnt_beforeCOVID19,cnt_home,cnt_house,cnt_work,cnt_worship,cnt_publictansport,cnt_school,cnt_shop,cnt_shop_notessential,cnt_entertainmentplace,cnt_sport,cnt_outside,cnt_othersetting1,cnt_healthcare,cnt_saloncare ,cnt_othersetting2,cnt_freq,cnt_dis_physical,cnt_dis_2m_plus,cnt_dis_1m_plus,cnt_dis_1m_minus,cnt_dis_mask,cnt_dis_wash_before,cnt_dis_wash_after,cnt_dis_notsaid,cnt_dis_other,cnt_dis_dontknow))
        cnt_data <- cnt_data[,!grepl("variable", colnames(cnt_data))]
        cnt_data <- cnt_data[rowSums(is.na(cnt_data)) < (ncol(cnt_data)-2), ]
        cnt_data$household_member <- 0
        cnt_data$hh_education <- "not_asked"
        cnt_data$hh_riskgroup <- "not_asked"
        cnt_data$hh_occupation <- "not_asked"
        cnt_data_hh<- cnt_data_hh[,match(colnames(cnt_data_hh),colnames(cnt_data))]
        cnt_data <- rbind(cnt_data_hh,cnt_data)
        rm(cnt_data_hh)
        if( sum(colnames(cnt_data)=="cnt_dis_physical")==0){
          cnt_data$cnt_dis_physical <- NA
        }
        
      }
      
      cnt_data$cnt_age[cnt_data$cnt_age=="Under 1"] <- "0-1"
      cnt_data$cnt_age[cnt_data$cnt_age=="This person is {#CHild_name}"] <- "0-18"
      
      if(unique(ipsos_data$Sampletype) %in% c("Sampletype=1 Main sample",1)){
        cnt_data <- cnt_data[!cnt_data$cnt_age %in%"This person is me",]  
        cnt_data <- cnt_data[!cnt_data$cnt_age %in%"This is me",]   
      }
      if(unique(ipsos_data$Sampletype) %in% c("Sampletype=2 Parent sample",2)){
        cnt_data[cnt_data$cnt_age %in%"This person is me",]  <- ipsos_data$resp_age[ipsos_data$panel_wave_id %in% cnt_data$panel_wave_id[cnt_data$cnt_age%in%"This person is me"]]
        cnt_data[cnt_data$cnt_age %in%"This is me",]  <- ipsos_data$resp_age[ipsos_data$panel_wave_id %in% cnt_data$panel_wave_id[cnt_data$cnt_age%in%"This is me"]]
      }
      
      
      
      cnt_data$cnt_dis_measures <- NA
      cnt_data$contact <- "individual"
      
      if(unique(ipsos_data$Sampletype) %in% c("Sampletype=1 Main sample",1)){#
        ipsos_data<- ipsos_data[ipsos_data$Q74 == "I did not individually include every person I had contact with.",]
        cnt_data_mass <- ipsos_data[,grep("panel_wave_id|Q79", colnames(ipsos_data))]
        cnt_data_mass <-cnt_data_mass[rowSums(is.na(cnt_data_mass)) < 2,]
        ## work
        cnt_data_mass$cnt_work <- "work"
        cnt_data_mass_work <- cbind(cnt_data_mass[rep(rownames(cnt_data_mass), cnt_data_mass$Q79a_1_scale), c(4:7)],rep("0-18", sum(cnt_data_mass$Q79a_1_scale)))
        colnames(cnt_data_mass_work) <- c("cnt_dis_measures", "cnt_duration", "panel_wave_id", "cnt_work","cnt_age")
        cnt_data_mass_work1 <- cbind(cnt_data_mass[rep(rownames(cnt_data_mass), cnt_data_mass$Q79a_2_scale), c(4:7)],rep("18-65", sum(cnt_data_mass$Q79a_2_scale)))
        colnames(cnt_data_mass_work1) <- c("cnt_dis_measures", "cnt_duration", "panel_wave_id", "cnt_work","cnt_age")
        cnt_data_mass_work2 <- cbind(cnt_data_mass[rep(rownames(cnt_data_mass), cnt_data_mass$Q79a_3_scale), c(4:7)],rep("65+", sum(cnt_data_mass$Q79a_3_scale)))
        colnames(cnt_data_mass_work2) <- c("cnt_dis_measures", "cnt_duration", "panel_wave_id", "cnt_work","cnt_age")
        
        cnt_data_mass_work <- rbind(cnt_data_mass_work, cnt_data_mass_work1, cnt_data_mass_work2)
        cnt_data_mass_work$cnt_school <- NA
        cnt_data_mass_work$cnt_othersetting2 <- NA
         remove(cnt_data_mass_work1)
         remove(cnt_data_mass_work2)
         
        ## school
         cnt_data_mass <- ipsos_data[,grep("panel_wave_id|Q80", colnames(ipsos_data))]
         cnt_data_mass <-cnt_data_mass[rowSums(is.na(cnt_data_mass)) < 2,]
         cnt_data_mass$cnt_school <- "school"
         cnt_data_mass_school <- cbind(cnt_data_mass[rep(rownames(cnt_data_mass), cnt_data_mass$Q80a_1_scale), c(4:7)],rep("0-18", sum(cnt_data_mass$Q80a_1_scale)))
         colnames(cnt_data_mass_school) <- c("cnt_dis_measures", "cnt_duration", "panel_wave_id", "cnt_school","cnt_age")
         cnt_data_mass_school1 <- cbind(cnt_data_mass[rep(rownames(cnt_data_mass), cnt_data_mass$Q80a_2_scale), c(4:7)],rep("18-65", sum(cnt_data_mass$Q80a_2_scale)))
         colnames(cnt_data_mass_school1) <- c("cnt_dis_measures", "cnt_duration", "panel_wave_id", "cnt_school","cnt_age")
         cnt_data_mass_school2 <- cbind(cnt_data_mass[rep(rownames(cnt_data_mass), cnt_data_mass$Q80a_3_scale), c(4:7)],rep("65+", sum(cnt_data_mass$Q80a_3_scale)))
         colnames(cnt_data_mass_school2) <- c("cnt_dis_measures", "cnt_duration", "panel_wave_id", "cnt_school","cnt_age")
         
         cnt_data_mass_school <- rbind(cnt_data_mass_school, cnt_data_mass_school1, cnt_data_mass_school2)
         cnt_data_mass_school$cnt_work <- NA
         cnt_data_mass_school$cnt_othersetting2 <- NA
         remove(cnt_data_mass_school1)
         remove(cnt_data_mass_school2)
         
        ## other
         cnt_data_mass <- ipsos_data[,grep("panel_wave_id|Q81", colnames(ipsos_data))]
         cnt_data_mass <-cnt_data_mass[rowSums(is.na(cnt_data_mass)) < 2,]
         cnt_data_mass$cnt_othersetting2 <- "other"
         cnt_data_mass_other <- cbind(cnt_data_mass[rep(rownames(cnt_data_mass), cnt_data_mass$Q81a_1_scale), c(4:7)],rep("0-18", sum(cnt_data_mass$Q81a_1_scale)))
         colnames(cnt_data_mass_other) <- c("cnt_dis_measures", "cnt_duration", "panel_wave_id", "cnt_othersetting2","cnt_age")
         cnt_data_mass_other1 <- cbind(cnt_data_mass[rep(rownames(cnt_data_mass), cnt_data_mass$Q81a_2_scale), c(4:7)],rep("18-65", sum(cnt_data_mass$Q81a_2_scale)))
         colnames(cnt_data_mass_other1) <- c("cnt_dis_measures", "cnt_duration", "panel_wave_id", "cnt_othersetting2","cnt_age")
         cnt_data_mass_other2 <- cbind(cnt_data_mass[rep(rownames(cnt_data_mass), cnt_data_mass$Q81a_3_scale), c(4:7)],rep("65+", sum(cnt_data_mass$Q81a_3_scale)))
         colnames(cnt_data_mass_other2) <- c("cnt_dis_measures", "cnt_duration", "panel_wave_id", "cnt_othersetting2","cnt_age")
         
         cnt_data_mass_other <- rbind(cnt_data_mass_other, cnt_data_mass_other1, cnt_data_mass_other2)
         cnt_data_mass_other$cnt_work <- NA
         cnt_data_mass_other$cnt_school <- NA
         remove(cnt_data_mass_other1)
         remove(cnt_data_mass_other2)
         
         cnt_data_mass <-  rbind(cnt_data_mass_work,cnt_data_mass_school,cnt_data_mass_other)
         cnt_data_mass$contact <- "mass"
#cnt_data_mass$lower.age.limit <- cnt_data_mass$lower_age_by10 <- cnt_data_mass$age_cnt
         
      }
      
      
      if(unique(ipsos_data$Sampletype) %in% c("Sampletype=2 Parent sample",2)){#
        ipsos_data <- ipsos_data[ipsos_data$QP74 == "I did not individually include every person {#child_name.response.value} had contact with.",]
        cnt_data_mass <- ipsos_data[,grep("panel_wave_id|QP79", colnames(ipsos_data))]
        cnt_data_mass <-cnt_data_mass[rowSums(is.na(cnt_data_mass)) < 2,]
        if(length(cnt_data_mass[,1])>0){
        ## work
        cnt_data_mass$cnt_work <- "work"
        cnt_data_mass_work <- cbind(cnt_data_mass[rep(rownames(cnt_data_mass), cnt_data_mass$QP79a_1_scale), c(4:7)],rep("0-18", sum(cnt_data_mass$QP79a_1_scale)))
        colnames(cnt_data_mass_work) <- c("cnt_dis_measures", "cnt_duration", "panel_wave_id", "cnt_work","cnt_age")
        cnt_data_mass_work1 <- cbind(cnt_data_mass[rep(rownames(cnt_data_mass), cnt_data_mass$QP79a_2_scale), c(4:7)],rep("18-65", sum(cnt_data_mass$QP79a_2_scale)))
        colnames(cnt_data_mass_work1) <- c("cnt_dis_measures", "cnt_duration", "panel_wave_id", "cnt_work","cnt_age")
        cnt_data_mass_work2 <- cbind(cnt_data_mass[rep(rownames(cnt_data_mass), cnt_data_mass$QP79a_3_scale), c(4:7)],rep("65+", sum(cnt_data_mass$QP79a_3_scale)))
        colnames(cnt_data_mass_work2) <- c("cnt_dis_measures", "cnt_duration", "panel_wave_id", "cnt_work","cnt_age")
        
        cnt_data_mass_work <- rbind(cnt_data_mass_work, cnt_data_mass_work1, cnt_data_mass_work2)
        cnt_data_mass_work$cnt_school <- NA
        cnt_data_mass_work$cnt_othersetting2 <- NA
        remove(cnt_data_mass_work1)
        remove(cnt_data_mass_work2)
        
        ## school
        cnt_data_mass <- ipsos_data[,grep("panel_wave_id|QP80", colnames(ipsos_data))]
        cnt_data_mass <-cnt_data_mass[rowSums(is.na(cnt_data_mass)) < 2,]
        cnt_data_mass$cnt_school <- "school"
        cnt_data_mass_school <- cbind(cnt_data_mass[rep(rownames(cnt_data_mass), cnt_data_mass$QP80a_1_scale), c(4:7)],rep("0-18", sum(cnt_data_mass$QP80a_1_scale)))
        colnames(cnt_data_mass_school) <- c("cnt_dis_measures", "cnt_duration", "panel_wave_id", "cnt_school","cnt_age")
        cnt_data_mass_school1 <- cbind(cnt_data_mass[rep(rownames(cnt_data_mass), cnt_data_mass$QP80a_2_scale), c(4:7)],rep("18-65", sum(cnt_data_mass$QP80a_2_scale)))
        colnames(cnt_data_mass_school1) <- c("cnt_dis_measures", "cnt_duration", "panel_wave_id", "cnt_school","cnt_age")
        cnt_data_mass_school2 <- cbind(cnt_data_mass[rep(rownames(cnt_data_mass), cnt_data_mass$QP80a_3_scale), c(4:7)],rep("65+", sum(cnt_data_mass$QP80a_3_scale)))
        colnames(cnt_data_mass_school2) <- c("cnt_dis_measures", "cnt_duration", "panel_wave_id", "cnt_school","cnt_age")
        
        cnt_data_mass_school <- rbind(cnt_data_mass_school, cnt_data_mass_school1, cnt_data_mass_school2)
        cnt_data_mass_school$cnt_work <- NA
        cnt_data_mass_school$cnt_othersetting2 <- NA
        remove(cnt_data_mass_school1)
        remove(cnt_data_mass_school2)
        
        ## other
        cnt_data_mass <- ipsos_data[,grep("panel_wave_id|QP81", colnames(ipsos_data))]
        cnt_data_mass <-cnt_data_mass[rowSums(is.na(cnt_data_mass)) < 2,]
        cnt_data_mass$cnt_othersetting2 <- "other"
        cnt_data_mass_other <- cbind(cnt_data_mass[rep(rownames(cnt_data_mass), cnt_data_mass$QP81a_1_scale), c(4:7)],rep("0-18", sum(cnt_data_mass$QP81a_1_scale)))
        colnames(cnt_data_mass_other) <- c("cnt_dis_measures", "cnt_duration", "panel_wave_id", "cnt_othersetting2","cnt_age")
        cnt_data_mass_other1 <- cbind(cnt_data_mass[rep(rownames(cnt_data_mass), cnt_data_mass$QP81a_2_scale), c(4:7)],rep("18-65", sum(cnt_data_mass$QP81a_2_scale)))
        colnames(cnt_data_mass_other1) <- c("cnt_dis_measures", "cnt_duration", "panel_wave_id", "cnt_othersetting2","cnt_age")
        cnt_data_mass_other2 <- cbind(cnt_data_mass[rep(rownames(cnt_data_mass), cnt_data_mass$QP81a_3_scale), c(4:7)],rep("65+", sum(cnt_data_mass$QP81a_3_scale)))
        colnames(cnt_data_mass_other2) <- c("cnt_dis_measures", "cnt_duration", "panel_wave_id", "cnt_othersetting2","cnt_age")
        
        cnt_data_mass_other <- rbind(cnt_data_mass_other, cnt_data_mass_other1, cnt_data_mass_other2)
        cnt_data_mass_other$cnt_work <- NA
        cnt_data_mass_other$cnt_school <- NA
        remove(cnt_data_mass_other1)
        remove(cnt_data_mass_other2)
        
        cnt_data_mass <-  rbind(cnt_data_mass_work,cnt_data_mass_school,cnt_data_mass_other)
        cnt_data_mass$contact <- "mass"
        }
      }
      
      col_names <- colnames(cnt_data)[!colnames(cnt_data) %in% colnames(cnt_data_mass)]
      col_matrix <- matrix(ncol=length(col_names),nrow=length(cnt_data_mass[,1]))
      colnames(col_matrix) <- col_names 
      
      cnt_data_mass <- cbind(cnt_data_mass,col_matrix)
      cnt_data_mass<- cnt_data_mass[,match(colnames(cnt_data), colnames(cnt_data_mass))]
      cnt_data <- rbind(cnt_data_mass,cnt_data)
      
      
      
      
      
      # age (in Q23)); cnt_age_est_min, cnt_age_est_max, age_comix, age_by10
      cnt_data$cnt_age_est_max <- 120
      for (i in 1:length(cnt_data[,1])) {
        if(is.na(cnt_data$cnt_age[i])){
          cnt_data$cnt_age_est_max[i] <- NA
        }
        else if(cnt_data$cnt_age[i]=="Prefer not to answer"){
          cnt_data$cnt_age_est_max[i] <- NA
        }
        else if(cnt_data$cnt_age[i]=="Don’t know"){
          cnt_data$cnt_age_est_max[i] <- "Don’t know"
        }
        else if(cnt_data$cnt_age[i]==0){
          cnt_data$cnt_age_est_max[i] <- 0
        }
        else if(cnt_data$cnt_age[i]=="This person is me"){
          cnt_data$cnt_age_est_max[i] <- "This is me"#ipsos_data$resp_age[ipsos_data$panel_wave_id== names(table(cnt_data$panel_wave_id[cnt_data$cnt_age=="This person is me"])) & ipsos_data$panel_wave_id== cnt_data$panel_wave_id[i]]
        }
        else if(cnt_data$cnt_age[i]=="This is me"){
          cnt_data$cnt_age_est_max[i] <- "This is me"#ipsos_data$resp_age[ipsos_data$panel_wave_id== names(table(cnt_data$panel_wave_id[cnt_data$cnt_age=="This is me"])) & ipsos_data$panel_wave_id== cnt_data$panel_wave_id[i]]
        }
        
        else if(is.numeric(cnt_data$cnt_age[i])){
          cnt_data$cnt_age_est_max[i] <- cnt_data$cnt_age[i]
        }
        else if(cnt_data$cnt_age[i]=="85+"){
          cnt_data$cnt_age_est_max[i] <- 120
        }
        else{cnt_data$cnt_age_est_max[i] <- as.numeric(sub(".*-","",cnt_data$cnt_age[i]))
        }
      }
      
      ## get variable "cnt_age_est_min"
      cnt_data$cnt_age_est_min <- 0
      for (i in 1:length(cnt_data[,1])) {
        if(is.na(cnt_data$cnt_age[i])){
          cnt_data$cnt_age_est_min[i] <- NA
        }
        else if(cnt_data$cnt_age[i]=="Prefer not to answer"){
          cnt_data$cnt_age_est_min[i] <- NA
        }
        else if(cnt_data$cnt_age[i]=="Don’t know"){
          cnt_data$cnt_age_est_min[i] <- "Don’t know"
        }
        else if(cnt_data$cnt_age[i]==0){
          cnt_data$cnt_age_est_min[i] <- 0
        }
        else if(cnt_data$cnt_age[i]=="This person is me"){
          cnt_data$cnt_age_est_min[i] <- "This is me"#ipsos_data$resp_age[ipsos_data$panel_wave_id== names(table(cnt_data$panel_wave_id[cnt_data$cnt_age=="This person is me"])) & ipsos_data$panel_wave_id== cnt_data$panel_wave_id[i]]
        }
        else if(cnt_data$cnt_age[i]=="This is me"){
          cnt_data$cnt_age_est_min[i] <- "This is me"#ipsos_data$resp_age[ipsos_data$panel_wave_id== names(table(cnt_data$panel_wave_id[cnt_data$cnt_age=="This is me"])) & ipsos_data$panel_wave_id== cnt_data$panel_wave_id[i]]
        }
        
        else if(is.numeric(cnt_data$cnt_age[i])){
          cnt_data$cnt_age_est_min[i] <- cnt_data$cnt_age[i]
        }
        else if(cnt_data$cnt_age[i]=="85+"){
          cnt_data$cnt_age_est_min[i] <- 85
        }
        else{
          cnt_data$cnt_age_est_min[i] <- as.numeric(sub("-.*","",cnt_data$cnt_age[i]))
          
        }
      }
      
      
      cnt_data$cnt_age_est_min <- ifelse(is.na(cnt_data$cnt_age_est_min), 0, cnt_data$cnt_age_est_min)
      cnt_data$cnt_age_est_max <- ifelse(is.na(cnt_data$cnt_age_est_max), 120, cnt_data$cnt_age_est_max)
      
      
      

      print(n)
      #warning()
      #print(unique(cnt_data$panel_wave_id))
      ## get variable
      if(is.data.frame(contacts_data)){
        cnt_data<- cnt_data[,match(colnames(cnt_data),colnames(contacts_data))]
      }
      contacts_data <- rbind(contacts_data,cnt_data)
      contacts_data <- contacts_data[!is.na(contacts_data$panel_wave_id),]
      
      
    }
    #write.csv(contacts_data, "data/contacts_data.csv")
    return(as.data.frame(contacts_data))
}
