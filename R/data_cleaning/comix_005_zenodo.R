
comix_005_zenodo = function(comixdata_part,comixdata_cont){
  part <- comixdata_part
 # part$part_id <- as.numeric(part$part_id)
  
  part$panel <- gsub("[:1-9:]","",part$panel_wave)
  part<- part %>%
    group_by(part_id) %>%
    mutate(part_id_num =  sprintf("%02d",1:length(part_id))) %>%
    mutate(part_id = paste0(part_id, part_id_num))
  
  part$age_part1 <- suppressWarnings(as.numeric(part$age_part))
  part$age_part[is.na(part$age_part1)] <- (as.numeric(sub("\\-.*","",part$age_part[is.na(part$age_part1)]))+as.numeric(sub(".*-","",part$age_part[is.na(part$age_part1)])))/2
  part$age_part <- as.numeric(part$age_part)
  
 
  cont <- merge(comixdata_cont, part[c("panel_wave_id","part_id")], by="panel_wave_id", all.x = T)
  #cont$part_id <- as.numeric(cont$part_id)
  #cont$part_id<- sub(".*_","",cont$panel_wave_id)
  cont$wave_num <-  sprintf("%02d",as.numeric(gsub("[[:alpha:]]","",  sub("\\_.*","",cont$panel_wave_id))))
  #cont$part_id<- paste0(cont$part_id, cont$wave_num)
  cont$cnt_age_exact <- suppressWarnings(as.numeric(cont$cnt_age))
  
 
#CoMix_ch_contact_common.csv ##cnt_dis_physical
CoMix_ch_contact_common<- cont[,c("part_id","wave_num","cnt_age_exact","cnt_age_est_min","cnt_age_est_max", "cnt_gender","cnt_freq","cnt_dis_physical","cnt_place","cnt_duration","contact","cnt_relationship" )]
levels(CoMix_ch_contact_common$cnt_duration)
fun <- function(x) {
  if(is.na(x)){return(NA)}
  else if("Less than 5 minutes" %in% x){return(1)}
  else if("5 minutes or more, but less than 15 minutes" %in% x){return(2)}
  else if("15 minutes or more, but less than 1 hour" %in% x){return(3)}
  else if("1 hour or more, but less than 4 hours"  %in% x){return(4)}
  else if("4 hours or more"  %in% x){return(5)}
  else if("houshold member"   %in% x){return(6)}
  else if("household_member"   %in% x){return(6)}
  else if("Don’t know"  %in% x){return(7)}
}
CoMix_ch_contact_common$cnt_duration <- unlist(sapply(CoMix_ch_contact_common$cnt_duration, fun))
fun <- function(x) {
  if(is.na(x)){return(NA)}
  else if("About once or twice a week" %in% x){return(3)}
  else if("About once per month" %in% x){return(5)}
  else if("Every 2-3 weeks" %in% x){return(4)}
  else if("Every day or almost every day"  %in% x){return(2)}
  else if("Less often than once per month"  %in% x){return(6)}
  else if("Never met them before" %in% x){return(7)}
  else if("household_member"   %in% x){return(1)}
  else if("Prefer not to answer"  %in% x){return(8)}
}
CoMix_ch_contact_common$cnt_freq <- unname(sapply(CoMix_ch_contact_common$cnt_freq, fun))


CoMix_ch_contact_common$cnt_home <- ifelse(CoMix_ch_contact_common$cnt_place %in% "home", TRUE, FALSE)
CoMix_ch_contact_common$cnt_work <- ifelse(CoMix_ch_contact_common$cnt_place %in% "work", TRUE, FALSE)
CoMix_ch_contact_common$cnt_school <- ifelse(CoMix_ch_contact_common$cnt_place %in% "school", TRUE, FALSE)
CoMix_ch_contact_common$cnt_transport <- ifelse(CoMix_ch_contact_common$cnt_place %in% "transport", TRUE, FALSE)
CoMix_ch_contact_common$cnt_leisure <- ifelse(CoMix_ch_contact_common$cnt_place %in% "leisure", TRUE, FALSE)
CoMix_ch_contact_common$cnt_other <- ifelse(!CoMix_ch_contact_common$cnt_place %in% c("home", "work", "school", "transport", "leisure"), TRUE, FALSE)
CoMix_ch_contact_common <- CoMix_ch_contact_common %>% group_by(part_id) %>% mutate(cont_id = sprintf("%03d", row_number()))
CoMix_ch_contact_common$cont_id <- paste0(CoMix_ch_contact_common$part_id, CoMix_ch_contact_common$cont_id)
CoMix_ch_contact_common$cont_id <- as.numeric(CoMix_ch_contact_common$cont_id)
#CoMix_ch_contact_common$part_id<- paste0(cont$part_id, cont$wave_num)
#get lower.age.limit
CoMix_ch_contact_common$cnt_age_est_min <- suppressWarnings(as.numeric(CoMix_ch_contact_common$cnt_age_est_min))
CoMix_ch_contact_common$cnt_age_est_max <- suppressWarnings(as.numeric(CoMix_ch_contact_common$cnt_age_est_max))
CoMix_ch_contact_common$cnt_age_est_min[is.na(CoMix_ch_contact_common$cnt_age_est_min)] <- 0
CoMix_ch_contact_common$cnt_age_est_max[is.na(CoMix_ch_contact_common$cnt_age_est_max)] <- 120
#CoMix_ch_contact_common$lower.age.limit <- as.numeric(CoMix_ch_contact_common$cnt_age_est_min)
CoMix_ch_contact_extra <- CoMix_ch_contact_common[,c("cont_id","cnt_gender","cnt_freq","cnt_place","contact")]

#CoMix_ch_contact_extra
CoMix_ch_contact_extra$cnt_outside_other <- ifelse(CoMix_ch_contact_extra$cnt_place %in% "outdoors", TRUE, FALSE)
CoMix_ch_contact_extra$cnt_other_house <- ifelse(CoMix_ch_contact_extra$cnt_place %in% "house", TRUE, FALSE)
CoMix_ch_contact_extra$cnt_worship <- ifelse(CoMix_ch_contact_extra$cnt_place %in% "worship", TRUE, FALSE)
CoMix_ch_contact_extra$cnt_supermarket <- ifelse(CoMix_ch_contact_extra$cnt_place %in% "shop", TRUE, FALSE)
CoMix_ch_contact_extra$cnt_shop <- ifelse(CoMix_ch_contact_extra$cnt_place %in% "shop but not essential", TRUE, FALSE)
CoMix_ch_contact_extra$contact_type <- CoMix_ch_contact_extra$contact
CoMix_ch_contact_extra <- CoMix_ch_contact_extra[,!colnames(CoMix_ch_contact_extra) %in% c("cnt_gender","cnt_freq","cnt_place","contact")]
 # write.csv(CoMix_ch_contact_extra,"./data/processed/zenodo/CoMix_ch_contact_extra.csv",  quote = FALSE, sep = ";")
write.table(CoMix_ch_contact_extra, "./data/processed/zenodo/CoMix_ch_contact_extra.csv", quote = FALSE, row.names = FALSE, sep = ";")

#CoMix_ch_hh_common.csv
#CoMix_ch_hh_common <- CoMix_ch_contact_common[,c("cont_id","part_id","cnt_relationship")]
CoMix_ch_hh_common <- part[,c("part_id","household_size")]
#CoMix_ch_hh_common <- merge(CoMix_ch_hh_common, CoMix_ch_hh_common1, by="part_id", all =TRUE)
#CoMix_ch_hh_common <- CoMix_ch_hh_common[!is.na(CoMix_ch_hh_common$cont_id),]
#remove(CoMix_ch_hh_common1)
CoMix_ch_hh_common$hh_id <- paste0("HH", CoMix_ch_hh_common$part_id)#cont_id
#CoMix_ch_hh_common$hh_id[!is.na(CoMix_ch_hh_common$cnt_relationship) & CoMix_ch_hh_common$cnt_relationship %in%"household_member"] <- paste0("HH",CoMix_ch_hh_common$hh_id[!is.na(CoMix_ch_hh_common$cnt_relationship) & CoMix_ch_hh_common$cnt_relationship %in%"household_member"]) 
#CoMix_ch_hh_common <- CoMix_ch_hh_common[grepl("HH", CoMix_ch_hh_common$hh_id),]
#CoMix_ch_hh_common$country <- "Switzerland"
CoMix_ch_hh_common <- CoMix_ch_hh_common[,c( "hh_id", "household_size")]#, "country"
#write.csv(CoMix_ch_hh_common,"./data/processed/zenodo/CoMix_ch_hh_common.csv",  quote = FALSE, sep = ";")# row.names = FALSE,
  write.table(CoMix_ch_hh_common, "./data/processed/zenodo/CoMix_ch_hh_common.csv", quote = FALSE, row.names = FALSE, sep = ";")
  
CoMix_ch_contact_common<- CoMix_ch_contact_common[,c("cont_id","part_id","cnt_age_exact","cnt_age_est_min","cnt_age_est_max", "cnt_gender","cnt_freq","cnt_dis_physical","cnt_place","cnt_duration")]
# write.csv(CoMix_ch_contact_common,"./data/processed/zenodo/CoMix_ch_contact_common.csv",  quote = FALSE, sep = ";")#, row.names = FALSE
write.table(CoMix_ch_contact_common, "./data/processed/zenodo/CoMix_ch_contact_common.csv", quote = FALSE, row.names = FALSE, sep = ";")

#CoMix_ch_participant_common.csv
CoMix_ch_participant_common<- part[,c("part_id","age_part","gender")]
CoMix_ch_participant_common$hh_id <- paste0("HH",CoMix_ch_participant_common$part_id)
colnames(CoMix_ch_participant_common) <- c("part_id", "part_age","part_gender","hh_id")
CoMix_ch_participant_common$part_gender <-  ifelse(CoMix_ch_participant_common$part_gender=="Male","M", as.character(CoMix_ch_participant_common$part_gender))
CoMix_ch_participant_common$part_gender <-  ifelse(CoMix_ch_participant_common$part_gender=="Female","F", as.character(CoMix_ch_participant_common$part_gender))
CoMix_ch_participant_common <- CoMix_ch_participant_common[!duplicated(CoMix_ch_participant_common$part_id),]
CoMix_ch_participant_common$country <- "Switzerland"
  #write.csv(CoMix_ch_participant_common,"./data/processed/zenodo/CoMix_ch_participant_common.csv",  quote = FALSE, sep = ";")
  write.table(CoMix_ch_participant_common, "./data/processed/zenodo/CoMix_ch_participant_common.csv", quote = FALSE, row.names = FALSE, sep = ";")
  

#CoMix_ch_participant_extra.csv
CoMix_ch_participant_extra <- part[,c("part_id", "panel","panel_wave","panel_wave_id", "survey_group","canton")]


 # write.csv(CoMix_ch_participant_extra,"./data/processed/zenodo/CoMix_ch_participant_extra.csv",  quote = FALSE, sep = ";")#)
  write.table(CoMix_ch_participant_extra, "./data/processed/zenodo/CoMix_ch_participant_extra.csv", quote = FALSE, row.names = FALSE, sep = ";")
  
#CoMix_ch_sday.csv
CoMix_ch_sday <- part[,c("part_id", "date","panel_wave")]
CoMix_ch_sday$wave <- gsub("[[:alpha:]]","", CoMix_ch_sday$panel_wave)
colnames(CoMix_ch_sday) <- c("part_id","sday_id","panel_wave","wave")
CoMix_ch_sday <- CoMix_ch_sday[,c("part_id", "sday_id","wave")]
CoMix_ch_sday$dayofweek <- weekdays(CoMix_ch_sday$sday_id)
CoMix_ch_sday$month <- month(CoMix_ch_sday$sday_id)
CoMix_ch_sday$year <- year(CoMix_ch_sday$sday_id)
CoMix_ch_sday$day <- day(CoMix_ch_sday$sday_id)
CoMix_ch_sday$sday_id <- as_date(CoMix_ch_sday$sday_id)

 #write.csv(CoMix_ch_sday,"./data/processed/zenodo/CoMix_ch_sday.csv",  quote = FALSE)
 write.table(CoMix_ch_sday, "./data/processed/zenodo/CoMix_ch_sday.csv", quote = FALSE, row.names = FALSE, sep = ";")
 
}


