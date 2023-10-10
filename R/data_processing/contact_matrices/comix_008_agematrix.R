#' ---
#' title: "Age contact pattern matrix "
#' author: "Martina Reichmuth"
#' date: "26/04/2023"
#' ---

comix_008_agematrix = function() {
  
  ## get Swiss CoMix data:
  # comix <- get_survey("https://doi.org/10.5281/zenodo.6542656")   # from zenodo
  comix <- load_survey(list.files(here("data/processed/zenodo"), full.names = TRUE))
  comix <- clean(comix)
  
  # truncate at 50 contacts 
  comix$contacts <- comix$contacts %>% group_by(part_id) %>% slice_sample(n=50,replace = FALSE)
  
  #prepare enviroment
  prob_95 <- c(0.5, 0.025, 0.975)
  dir_create <- paste0("./output/figures/age_matrices/sensitivity/")
  if (!dir.exists(dir_create)){
    dir.create(dir_create)
  } 
  
  # define age groups "age_band" (as age_sentinella) for participants
  age_sentinella <- c(0, 5, 15, 30, 65) # Age groups for Sentinella ie https://www.sentinella.ch/de/info
  age_sentinella_cat <- recode(c(0, 5, 15, 30, 65), "0"= "0-4", "5"= "5-14","15"="15-29", "30"="30-64","65"="65+")
  age_sentinella_matrice <- recode(c(0, 5, 15, 30, 65), "0"= "[0,5)", "5"= "[5,15)","15"="[15,30)", "30"="[30,65)","65"="65+")
  age_groups <- c(0, 5, 15, 18, 30, 65) # Age groups for Sentinella ie https://www.sentinella.ch/de/info
  age_groups_cat <- recode(age_groups, "0"= "0-4", "5"= "5-14","15"="15-17", "18"="18-29", "30"="30-64","65"="65+")
  #get age bands
  comix$participants <- comix$participants %>%
    group_by(part_id) %>%
    mutate(age_band =  max(age_sentinella[part_age >= age_sentinella]),
           age_band_fig1 =  max(age_groups[part_age >= age_groups]))
  
  # prepare 'contact file'
  comix$contacts <- merge(comix$contacts, comix$participants[,c("part_id","panel_wave","age_band", "sday_id")],by="part_id", all =T)
  # define location of contacts: Home, School, Work, Other, NA:
  comix$contacts$cnt_location <- ifelse(comix$contacts$cnt_place %in% c("school", "work", "home", "not specified"),comix$contacts$cnt_place,"other") # school, work, home, other, not specified
  comix$contacts$cnt_location <- factor(comix$contacts$cnt_location, levels= c("home","school", "work","other", "not specified"))
  comix$contacts$cnt_location <- recode(comix$contacts$cnt_location, home  = 'Home', school = 'School', work  = 'Work', other = 'Other','not specified'='Not specified')
  #number of contacts
  unadj_mean_contacts_location <- comix$contacts %>% group_by(part_id,cnt_location) %>% reframe(sum_contacts_location = length(part_id), panel_wave= panel_wave)
  unadj_mean_contacts_location$sum_contacts_location[is.na(unadj_mean_contacts_location$cnt_location)] <- 0
  unadj_mean_contacts_location$sum_contacts_location[is.na(unadj_mean_contacts_location$cnt_location)] <- 0
  unadj_mean_contacts_location <- unadj_mean_contacts_location[!is.na(unadj_mean_contacts_location$cnt_location),]
  comix$contacts <- comix$contacts[!is.na(comix[["contacts"]][["cnt_place"]]),]# get only 'real' contacts in 'contact file' due to data manippulation (lines above)
  # get sum of contacts for each participant
  comix$participants <- merge(comix$participants, merge(comix$contacts %>% group_by(part_id) %>% reframe(sum_contacts = length(part_id)),comix$participants[,c("part_id")], by="part_id", all.y =T) %>% mutate(sum_contacts = replace_na(sum_contacts, 0)), all.x =T)
  # get mean contacts for participants
  paste0(round(mean( comix$participants$sum_contacts),1), " (95% CI: ",
         round(mean( comix$participants$sum_contacts)-qnorm(0.975)*sd( comix$participants$sum_contacts)/sqrt(length( comix$participants$part_id)),1),"-",
         round(mean( comix$participants$sum_contacts)+qnorm(0.975)*sd( comix$participants$sum_contacts)/sqrt(length( comix$participants$part_id)),1), ")")
  # age groups with fewest and most contacts
  as.data.frame(comix$participants %>% group_by(age_band_fig1) %>%  reframe(mean_contacts = paste0(round(mean(sum_contacts),1), " (95% CI:",  round(mean(sum_contacts)+qnorm(0.975)*sd(sum_contacts)/sqrt(length(part_id)),1),"-", round(mean(sum_contacts)-qnorm(0.975)*sd(sum_contacts)/sqrt(length(part_id)),1), ")")))[order(as.data.frame(comix$participants %>% group_by(age_band_fig1) %>% reframe(mean(sum_contacts)))[,2]),]
  # get mean contacts per survey wave
  unadj_mean_contacts_all <-  comix$participants %>% group_by(panel_wave) %>%  # all ages combined
    reframe(mean_contacts = round(mean(sum_contacts),1),
            CI_up = round(mean(sum_contacts)+qnorm(0.975)*sd(sum_contacts)/sqrt(length(part_id)),1),
            CI_low = round(mean(sum_contacts)-qnorm(0.975)*sd(sum_contacts)/sqrt(length(part_id)),1))
  # get mean contacts per age groups
  unadj_mean_contacts_age_sentinella <- comix$participants %>% group_by(panel_wave,age_band_fig1) %>%   # by ages groups
    reframe(mean_contacts = round(mean(sum_contacts),1),
            CI_up = round(mean(sum_contacts)+qnorm(0.975)*sd(sum_contacts)/sqrt(length(part_id)),1),
            CI_low = round(mean(sum_contacts)-qnorm(0.975)*sd(sum_contacts)/sqrt(length(part_id)),1))
  unadj_mean_contacts_age_sentinella$mCI <- paste0(sprintf("%0.2f", round(unadj_mean_contacts_age_sentinella$mean_contacts,1)), " (95%-CI: ",unadj_mean_contacts_age_sentinella$CI_low,"-",unadj_mean_contacts_age_sentinella$CI_up,")")
  # get mean contacts per by gender
  unadj_mean_contacts_gender <- comix$participants %>% group_by(panel_wave,part_gender) %>%   # by ages groups
    reframe(mean_contacts = round(mean(sum_contacts),1),
            CI_up = round(mean(sum_contacts)+qnorm(0.975)*sd(sum_contacts)/sqrt(length(part_id)),1),
            CI_low = round(mean(sum_contacts)-qnorm(0.975)*sd(sum_contacts)/sqrt(length(part_id)),1))
  unadj_mean_contacts_gender <- unadj_mean_contacts_gender[!unadj_mean_contacts_gender$part_gender %in% c("In another way","Prefer not to answer"),]
  unadj_mean_contacts_gender$mCI <- paste0(sprintf("%0.2f", round(unadj_mean_contacts_gender$mean_contacts,1)), " (95%-CI: ",unadj_mean_contacts_gender$CI_low,"-",unadj_mean_contacts_gender$CI_up,")")
  unadj_mean_contacts_gender$part_gender <- factor(unadj_mean_contacts_gender$part_gender, levels= c("F","M"))
  unadj_mean_contacts_gender$part_gender <- recode(unadj_mean_contacts_gender$part_gender, F  = 'Female', M = 'Male')
  # get mean contacts per place of contact
  unadj_mean_contacts_location <- unadj_mean_contacts_location %>% group_by(panel_wave,cnt_location) %>% 
   summarise(mean_contacts = mean(sum_contacts_location), #length(part_id)/length(unique(part_id)),
            CI_up = round(mean_contacts+qnorm(0.975)*sd(sum_contacts_location)/sqrt(length(part_id)),1),
            CI_low = round(mean_contacts-qnorm(0.975)*sd(sum_contacts_location)/sqrt(length(part_id)),1),
            mean_contacts = round(mean_contacts,1))
  unadj_mean_contacts_location$mCI <- paste0(sprintf("%0.2f", round(unadj_mean_contacts_location$mean_contacts,1)), " (95%-CI: ",unadj_mean_contacts_location$CI_low,"-",unadj_mean_contacts_location$CI_up,")")
  unadj_mean_contacts_location$cnt_location <- factor(unadj_mean_contacts_location$cnt_location, levels= c("Home","School", "Work","Other", "Not specified"))
  
  # create data frame for overview of the survey waves
  panel_wave_df <- comix$participants %>% group_by(panel_wave) %>%
    summarise(panel=unique(panel),
              time_panel_wave = paste0(format(as_date(min(sday_id)),format = "%d %B %Y")," to ", format(as_date(max(sday_id)),format = "%d %B %Y")),
              max_time = as_date(max(sday_id)),
              min_time = as_date(min(sday_id)),
              mid_time = median(seq(min_time,max_time,1)), # careful using
              n_part = length(unique(part_id)),
              n_cont = sum(sum_contacts))
  panel_wave_df$contactsCI <- paste0(sprintf("%0.2f", round(unadj_mean_contacts_all$mean_contacts,1))," (",unadj_mean_contacts_all$CI_low, "-", unadj_mean_contacts_all$CI_up,")")
  min_time <- min(panel_wave_df$min_time)
  max_time <- max(panel_wave_df$max_time)
  
  # get Table 1
  table1 <- panel_wave_df[,c("panel_wave","time_panel_wave", "n_part", "contactsCI")]
  colnames(table1) <- c("Panel wave","Time period", "Number of participants", "Mean number of contacts (95% CI)")
  write.csv(table1,"./output/tables/age_matrices/Table1.csv")
  
  # get Figure 1 (and Supfig 1)
  age_groups_recode <- setNames(age_groups_cat, age_groups)
  unadj_mean_contacts_age_sentinella$age_band_fig1 <- recode(unadj_mean_contacts_age_sentinella$age_band_fig1, !!!age_groups_recode)
  unadj_mean_contacts_age_sentinella$age_band_fig1 <- factor(unadj_mean_contacts_age_sentinella$age_band_fig1, levels= age_groups_cat)
  
  unadj_mean_contacts_age_sentinella <- merge(unadj_mean_contacts_age_sentinella, panel_wave_df[,c("panel_wave", "mid_time", "min_time", "max_time")], by="panel_wave")
  unadj_mean_contacts_all <- merge(unadj_mean_contacts_all, panel_wave_df[,c("panel_wave", "mid_time", "min_time", "max_time")], by="panel_wave")
  unadj_mean_contacts_gender <- merge(unadj_mean_contacts_gender, panel_wave_df[,c("panel_wave", "mid_time", "min_time", "max_time")], by="panel_wave")
  unadj_mean_contacts_location <- merge(unadj_mean_contacts_location, panel_wave_df[,c("panel_wave", "mid_time", "min_time", "max_time")], by="panel_wave")
  
  ### get data
  #hosp_ch <- swiss_pop_data[[12]]# # hospitalized cases in Switzerland
  #write.csv(hosp_ch,"./data/processed/swiss_data/hosp_ch.csv")
  hosp_ch <- read.csv("./data/processed/swiss_data/hosp_ch.csv")
  hosp_ch$date <- as_date(hosp_ch$date)
  hosp_ch <- hosp_ch[as_date(hosp_ch$date) %in% seq(min(panel_wave_df$min_time)-20,max(panel_wave_df$max_time)+20,1),]
  hosp_ch <- hosp_ch[hosp_ch$geoRegion== "CH",]
  hosp_ch$date <- as_date(parse_date_time(paste0(format(hosp_ch$date, "%Y-%W"),"-Mon"), "%Y-%W-%a"))
  hosp_ch  <- aggregate(hosp_ch["hosp_num"], by=hosp_ch[c("date")], sum)
  
  #BAG_data <- swiss_pop_data[[1]]# SARS-CoV-2 cases per week in Switzerland
  #write.csv(BAG_data,"./data/processed/swiss_data/BAG_data.csv")
  BAG_data <- read.csv("./data/processed/swiss_data/BAG_data.csv")
  BAG_data <- BAG_data[BAG_data$geoRegion== "CH",]
  BAG_data$date <- as_date(BAG_data$date)
  BAG_data <- BAG_data %>% arrange(date) %>% mutate(vacdo = replace_na(vac_dos_num, 0), cum_vacdo = cumsum(vacdo),cum_vacdo_pre =cum_vacdo/unique(pop)[1]*100)
  
  #kof_ch <- swiss_pop_data[[3]] #strigency index for Switzerland
  #write.csv(kof_ch,"./data/processed/swiss_data/kof_ch.csv")
  kof_ch <- read.csv("./data/processed/swiss_data/kof_ch.csv")
  kof_ch$date <- as_date(kof_ch$date)
  
  #reported_cases <- swiss_pop_data[[11]]# SARS-CoV-2 cases per day in Switzerland 
  #write.csv(reported_cases,"./data/processed/swiss_data/reported_cases.csv")
  #reported_cases <- read.csv("./data/processed/swiss_data/reported_cases.csv")
  
  # run only once (takes time)
  #inc_mean <- 5.2; inc_sd <- 2.8 # https://doi.org/10.1016/S1473-3099(20)30230-9
  #incubation_period <- list(mean = convert_to_logmean(inc_mean, inc_sd),
  #                          mean_sd = 0.1,
  #                          sd = convert_to_logsd(inc_mean, inc_sd),
  #                          sd_sd = 0.1,
  #                          max = 14)
  #gen_mean <- 5.2; gen_sd <- 1.72 # https://doi.org/10.2807/1560-7917.ES.2020.25.17.2000257
  #generation_time <- list(mean = gen_mean,
  #                        mean_sd = 0.1, # (6.78 - 3.78)/2/qnorm(0.975)
  #                        sd = gen_sd,
  #                        sd_sd = 0.1, # (3.93 - 0.91)/2/qnorm(0.975)
  #                        max = 14)
  #rep_mean <- 2; rep_sd <- 2 # Assumption
  #reporting_delay <- list(mean = convert_to_logmean(rep_mean, rep_sd),
  #                        mean_sd = 0.1,
  #                        sd = convert_to_logsd(rep_mean, rep_sd),
  #                        sd_sd = 0.1,
  #                        max = 14)
  #reported_cases$confirm <- reported_cases$cases_num
  #reported_cases <- reported_cases[reported_cases$geoRegion== "CH",]
  #reported_cases<- reported_cases[,c("date", "confirm")]
  #reported_cases$date <- as_date(reported_cases$date)
  #reported_cases <- reported_cases[as_date(reported_cases$date) %in% seq(min(panel_wave_df$min_time)-16,max(panel_wave_df$max_time)+30,1),]
  #fit_epinow2_all <- c()
  # run for every 6 weeks and overlap with one week
  #l <- 28
  
  #for(i in 0:round(length(reported_cases$date)/l)){
  #  if(i==0){
  #    fit_epinow2 <- epinow(reported_cases = reported_cases[((i*l+1)):(l+(i*l)+15),],generation_time = generation_time,delays = delay_opts(incubation_period, reporting_delay),horizon = 0,verbose = FALSE)
  #  }
  #  else{
  #    fit_epinow2 <- epinow(reported_cases = reported_cases[((i*l+1)-15):(l+(i*l)+15),],generation_time = generation_time,delays = delay_opts(incubation_period, reporting_delay),horizon = 0,verbose = FALSE)
  #  }
  #  fit_epinow2_all <- rbind(fit_epinow2$estimates$summarised[fit_epinow2$estimates$summarised$variable=="R",],fit_epinow2_all)
  #}
  #fit_epinow2_all <- fit_epinow2_all[fit_epinow2_all$variable%in%"R",]
  #fit_epinow2_all <- fit_epinow2_all[fit_epinow2_all$type %in% "estimate",]
  #fit_epinow2_all$date <- as_date(fit_epinow2_all$date)
  #fit_epinow2_all <- fit_epinow2_all[as_date(fit_epinow2_all$date) %in% seq(min(panel_wave_df$min_time),max(panel_wave_df$min_time),1),]
  #fit_epinow2_all <- fit_epinow2_all[order(fit_epinow2_all$date, decreasing = TRUE),]
  
  #for(i in reported_cases$date){
  #  reported_cases$R_median[reported_cases$date ==i] <- median(fit_epinow2_all$median[fit_epinow2_all$date ==i])
  #  reported_cases$R_lower_90[reported_cases$date ==i] <- median(fit_epinow2_all$lower_90[fit_epinow2_all$date ==i])
  #  reported_cases$R_upper_90[reported_cases$date ==i] <- median(fit_epinow2_all$upper_90[fit_epinow2_all$date ==i])
  #}
  #write.csv(reported_cases,"./data/processed/epinow2/reported_cases.csv")
  reported_cases <- read.csv("./data/processed/epinow2/reported_cases.csv")
  reported_cases$date <- as_date(reported_cases$date)
  
  #seq_ch <-  swiss_pop_data[[2]] # sequencing in Switzerland
  #write.csv(seq_ch,"./data/processed/swiss_data/seq_ch.csv")
  seq_ch <- read.csv("./data/processed/swiss_data/seq_ch.csv")
  
  seq_ch$date <- as_date(seq_ch$date)
  seq_ch <- seq_ch[as_date(seq_ch$date) %in% seq(min(panel_wave_df$min_time)-20,max(panel_wave_df$max_time)+20,1),]
  who_variant_names <- function(x){
    if(is.na(x)){return("undetermined")}
    else if(grepl("BA.2.75",x)){return("Omicron")}#,useBytes = FALSE
    else if(grepl("BA.2|BG|BL|BP|BJ|BL|BS|CH|BM|CJ|CM",x)){return("Omicron")}
    else if(grepl("BA.1",x)){return("Omicron")}#,useBytes = FALSE
    else if(grepl("BA.3",x)){return("Omicron")}#,useBytes = FALSE
    else if(grepl("BA.4",x)){return("Omicron")}#,useBytes = FALSE
    else if(grepl("BA.5|BQ|BQ.1|BQ.1.1|BQ.1.2|BQ.1.2|BQ.1.3|BQ.1.4|BW|BF|BE|BK|BT|BU|BV|CF|CG|CK|CL|CN|CY|DF|DJ|DL",x)){return("Omicron")}
    else if(grepl("XE",x)){return("Omicron")}#,useBytes = FALSE
    else if(grepl("BA.2.12.1",x)){return("Omicron")}#,useBytes = FALSE
    else if(grepl("BQ.1|BQ.1.1|BQ.1.2|BQ.1.2|BQ.1.3|BQ.1.4",x)){return("Omicron")}#,useBytes = FALSE
    else if(x %in% "XBB.1.5"){return("Omicron")}#,useBytes = FALSE
    else if(grepl("XBB|XBB.1",x)){return("Omicron")}#,useBytes = FALSE
    else if(x %in% c("Alpha","alpha","B.1.1.7","Q.1","Q.2","Q.3","Q.4","Q.6")){return("Alpha")}
    else if(grepl("Delta|delta|B.1.617.2|AY.1.1|AY.2|AY.3|AY.3.1|AY.4|AY.5|AY.5.1|AY.5.2|AY.6|AY.7|AY.7.1|AY.7.2|AY.8|AY.9|AY.10|AY.11|AY.12|AY.13|AY.14|AY.15|AY.16|AY.17|AY.18|AY.19|AY.20|AY.21|AY.22|AY.23|AY.24|AY.25|AY.26|AY.27|AY.28|AY.29|AY.30|AY.31|AY.32|AY.33|AY.34|AY.35|AY.36|AY.37",x,useBytes = TRUE)){return("Delta")}
    else if(grepl("Unassigned",x)){return("undetermined")}#,useBytes = FALSE
    else{return("others")}
  }
  seq_ch$variants  <- sapply(seq_ch$pangoLineage, who_variant_names)
  lev <- c("Alpha", "Delta","Omicron","others")
  seq_ch$variants <- factor(seq_ch$variants, levels = lev)
  seq_ch <- seq_ch[!is.na(seq_ch$variants),]
  seq_ch$date <- as_date(parse_date_time(paste0(format(seq_ch$date, "%Y-%W"),"-Mon"), "%Y-%W-%a"))
  seq_ch$num_seq_variant <- 1
  seq_ch <- seq_ch %>% group_by(date, variants) %>% summarise(num_seq_variant =sum(num_seq_variant)) %>% group_by(date) %>% mutate(num_seq =sum(num_seq_variant), freq_seq =num_seq_variant/num_seq)
  #relative 'transmissibility'/'growth rates': Alpha: 129%;  Delta: 197%; Omicron: 652;  'other' set 1
  growth_day <- c()
  growth_day$date <- unique(as_date(parse_date_time(paste0(format(seq(min(seq_ch$date),max(seq_ch$date),1), "%Y-%W"),"-Mon"), "%Y-%W-%a")))
  growth_day$relative_growth <- 1
  growth_day <- as.data.frame(growth_day)
  cum_relative_growth <- function(x){
    i <- seq_ch[seq_ch$date %in% x,]
    y <- 0
    if(sum(i$variants == "others")==1){y <- y+i$freq_seq[i$variants %in% "others"]*1}
    if(sum(i$variants == "Alpha")==1){y <- y+i$freq_seq[i$variants %in% "Alpha"]*1.29}
    if(sum(i$variants == "Delta")==1){y <- y+i$freq_seq[i$variants %in% "Delta"]*1.97}
    if(sum(i$variants == "Omicron")==1){y <- y+i$freq_seq[i$variants %in% "Omicron"]*6.52}
    return(y)
  }
  growth_day$relative_growth  <- sapply(growth_day$date, cum_relative_growth)
  # color cases due to sequencing (proportion of variants)
  seq_ch <- merge(hosp_ch[,c("hosp_num", "date")], seq_ch, by="date",all.x =TRUE)
  seq_ch$hosp_num_variant <- seq_ch$hosp_num*seq_ch$freq_seq
  seq_ch <- seq_ch[!is.na(seq_ch$hosp_num_variant),]
  
  # Figure 1
  y_max <- max(seq_ch$hosp_num_variant)+10
  fig1A<- ggplot()+ 
    theme_minimal()+
    geom_area(data = seq_ch, aes(x = date, y = hosp_num_variant, fill=variants, color=variants), position = "stack")+
    geom_rect(data=panel_wave_df,  aes(xmin=min_time, xmax=max_time, ymin=0, ymax=y_max), color="transparent", fill="grey", alpha=0.4)+
    geom_text(data=panel_wave_df[panel_wave_df$panel%in% c("A", "B", "F"),], aes(x=mid_time, y=1,vjust = -2, label= panel_wave), colour= "black", size=3) + 
    geom_text(data=panel_wave_df[!panel_wave_df$panel%in% c("A", "B", "F"),], aes(x=mid_time, y=1,vjust = -0.5, label= panel_wave), colour= "black", size=3) + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y")+
    coord_cartesian(xlim=c(min_time-1,max_time+1))+
    scale_fill_manual(values= cols25(n=25)[8:(8+length(levels(seq_ch$variants)))],name="") +
    scale_color_manual(values= cols25(n=25)[8:(8+length(levels(seq_ch$variants)))],name="") +
    guides(color = guide_legend(override.aes = list(size = 5)))+
    theme(panel.grid.minor = element_line(colour = "transparent"),
          panel.grid.major.x = element_line(colour = "transparent"))+
    labs(tag=bquote(.("")),subtitle = "", x = "", y =bquote("Number of confirmed \nCOVID-19 hospitalization"))
  fig1B <- ggplot()+
    theme_minimal()+
    geom_rect(data=unadj_mean_contacts_age_sentinella,  aes(xmin=min_time, xmax=max_time, ymin=0, ymax=max(CI_up)+1), color="transparent", fill="grey", alpha=0.2)+
    scale_x_date(date_breaks= "1 month", date_labels = "%b\n%Y")+
    geom_point(data=unadj_mean_contacts_age_sentinella, aes(y = mean_contacts,x=mid_time, fill=age_band_fig1,color=age_band_fig1))+
    geom_errorbar(data=unadj_mean_contacts_age_sentinella, aes(xmin=min_time,xmax=max_time, ymin = CI_low, ymax = CI_up, y = mean_contacts,x=mid_time, color=age_band_fig1))+
    geom_linerange(data=unadj_mean_contacts_age_sentinella, aes(xmin=min_time,xmax=max_time, y=mean_contacts, color=age_band_fig1),linewidth=3) +
    geom_text(data=panel_wave_df[panel_wave_df$panel%in% c("A", "B", "F"),], aes(x=mid_time, y=1,vjust = 0, label= panel_wave), colour= "black", size=3) + 
    geom_text(data=panel_wave_df[!panel_wave_df$panel%in% c("A", "B", "F"),], aes(x=mid_time, y=1,vjust = 1.5, label= panel_wave), colour= "black", size=3) + 
    coord_cartesian(xlim =c(min(unadj_mean_contacts_age_sentinella$min_time)-1,max(unadj_mean_contacts_age_sentinella$max_time)+1),
                    ylim =c(0,max(unadj_mean_contacts_age_sentinella$mean_contacts)+1),expand = TRUE)+
    scale_color_manual(values=cols25(n=25)[15:(15+length(levels(unadj_mean_contacts_age_sentinella$age_band_fig1)))], name="")+
    scale_fill_manual(values=cols25(n=25)[15:(15+length(levels(unadj_mean_contacts_age_sentinella$age_band_fig1)))], name="")+
    theme(panel.grid.minor = element_line(colour = "transparent"),
          panel.grid.major.x = element_line(colour = "transparent"))+
    labs(tag="",x = "", y =bquote("Crude number of \ncontacts per day"))
  fig1<-ggarrange(fig1A,fig1B,ncol=1, nrow=2, align = "hv", labels=c("A", "B"))
  ggsave(fig1, filename = paste0("./output/figures/age_matrices/Figure1.png"), height = 6, width = 12,  bg = "transparent")
  
  # supplementary figure 1: by type gender and type of contact
  supfig1a <- ggplot()+
    theme_minimal()+
    geom_rect(data=unadj_mean_contacts_all,  aes(xmin=min_time, xmax=max_time, ymin=0, ymax=10), color="transparent", fill="grey", alpha=0.2)+
    scale_x_date(date_breaks= "1 month", date_labels = "%b\n%Y")+
    geom_point(data=unadj_mean_contacts_gender, aes(y = mean_contacts,x=mid_time, fill=part_gender,color=part_gender))+
    geom_errorbar(data=unadj_mean_contacts_gender, aes(xmin=min_time,xmax=max_time, ymin = CI_low, ymax = CI_up, y = mean_contacts,x=mid_time, color=part_gender))+
    geom_linerange(data=unadj_mean_contacts_gender, aes(xmin=min_time,xmax=max_time, y=mean_contacts, color=part_gender),linewidth=3) +
    geom_text(data=panel_wave_df[panel_wave_df$panel%in% c("A", "B", "F"),], aes(x=mid_time, y=1,vjust = 0, label= panel_wave), colour= "black", size=3) + 
    geom_text(data=panel_wave_df[!panel_wave_df$panel%in% c("A", "B", "F"),], aes(x=mid_time, y=1,vjust = 1.5, label= panel_wave), colour= "black", size=3) + 
    coord_cartesian(xlim =c(min(unadj_mean_contacts_all$min_time),max(unadj_mean_contacts_all$max_time)),
                    ylim =c(0,10),expand = TRUE)+
    scale_color_manual(values=cols25(n=25)[7:(15+length(levels(unadj_mean_contacts_gender$part_gender)))], name="")+
    scale_fill_manual(values=cols25(n=25)[7:(15+length(levels(unadj_mean_contacts_gender$part_gender)))], name="")+
    theme(panel.grid.minor = element_line(colour = "transparent"),
          panel.grid.major.x = element_line(colour = "transparent"))+
    labs(tag="",x = "", y =bquote("Crude number of \ncontacts per day"))
  supfig1b <- ggplot()+
    theme_minimal()+
    geom_rect(data=unadj_mean_contacts_all,  aes(xmin=min_time, xmax=max_time, ymin=0, ymax=28), color="transparent", fill="grey", alpha=0.2)+
    scale_x_date(date_breaks= "1 month", date_labels = "%b\n%Y")+
    geom_point(data=unadj_mean_contacts_location, aes(y = mean_contacts,x=mid_time, fill=cnt_location,color=cnt_location))+
    geom_errorbar(data=unadj_mean_contacts_location, aes(xmin=min_time,xmax=max_time, ymin = CI_low, ymax = CI_up, y = mean_contacts,x=mid_time, color=cnt_location))+
    geom_linerange(data=unadj_mean_contacts_location, aes(xmin=min_time,xmax=max_time, y=mean_contacts, color=cnt_location),linewidth=3) +
    geom_text(data=panel_wave_df[panel_wave_df$panel%in% c("A", "B", "F"),], aes(x=mid_time, y=1,vjust = 0, label= panel_wave), colour= "black", size=3) + 
    geom_text(data=panel_wave_df[!panel_wave_df$panel%in% c("A", "B", "F"),], aes(x=mid_time, y=1,vjust = 1.5, label= panel_wave), colour= "black", size=3) + 
    coord_cartesian(xlim =c(min(unadj_mean_contacts_all$min_time),max(unadj_mean_contacts_all$max_time)),
                    ylim =c(0,28),expand = TRUE)+
    scale_color_manual(values=cols25(n=25)[10:(15+length(levels(unadj_mean_contacts_location$cnt_location)))], name="")+
    scale_fill_manual(values=cols25(n=25)[10:(15+length(levels(unadj_mean_contacts_location$cnt_location)))], name="")+
    theme(panel.grid.minor = element_line(colour = "transparent"),
          panel.grid.major.x = element_line(colour = "transparent"))+
    labs(tag="",x = "", y =bquote("Crude number of \ncontacts per day"))
  supfig1<-ggarrange(supfig1a,supfig1b,ncol=1, nrow=2, align = "hv", labels=c("A", "B"))
  ggsave(supfig1, filename = paste0("./output/figures/age_matrices/sensitvity/SF1.png"), height = 6, width = 12,  bg = "transparent")
  # Supfig2: Show distribution of contacts
  test <- as.data.frame(comix$contacts)
  supfig2 <- ggplot(comix$contacts, aes( x=panel_wave, fill=cnt_location))+
    theme_minimal()+
    geom_bar( position="fill")+
    scale_fill_manual(values=cols25(n=25)[10:(15+length(levels(comix$contacts$cnt_location)))], name="")+
    #guides(fill = guide_legend(override.aes = list(size = 10, shape=22)))+
    theme(panel.grid.minor = element_line(colour = "transparent"),
          panel.border = element_blank(),
          panel.grid.major.x = element_line(colour = "transparent"))+
    labs(tag="",x = "", y =bquote("Crude number of \ncontacts per day"))
  ggsave(supfig2, filename = paste0("./output/figures/age_matrices/sensitvity/SF2.png"), height = 3, width = 8,  bg = "transparent")
  
  
  ### Swiss demographic data
  #url_ch_pop <- "https://dam-api.bfs.admin.ch/hub/api/dam/assets/23064766/master" # Source: https://www.bfs.admin.ch/bfs/de/home/statistiken/bevoelkerung/stand-entwicklung/bevoelkerung.assetdetail.23064766.html
  year_i <- "2021"
  #options(timeout=120)
  #GET(url_ch_pop, write_disk(tf <- tempfile(fileext = ".xlsx")))
  #pop_year_i <- readxl::read_excel(tf, sheet = year_i)
  #write.xlsx(pop_year_i"./data/raw/su-d-01.02.03.06.xlsx")
  pop_year_i <- readxl::read_excel("./data/raw/su-d-01.02.03.06.xlsx", sheet = year_i)
  pop_year_i <- data.frame(lower.age.limit = 0:100, population = as.numeric(pop_year_i[2, 3:103]))
  pop_year_i$year <- year_i
  pop_year_i$country <- "Switzerland"
  #pop_year_i$year <- as.character(pop_year_i$year)
  pop_year_i <- pop_year_i[,c("lower.age.limit", "population","country", "year")]
  
  pop <- numeric(length(age_sentinella))
  age_range <- c(age_sentinella, 200)
  for(i in 1:length(pop)) {
    pop[i] <- sum(pop_year_i$population[pop_year_i$lower.age.limit >= age_range[i] & pop_year_i$lower.age.limit < age_range[i + 1]])
  }
  pop_sentinella <- data.frame(lower.age.limit = age_sentinella, population = pop)$population
  
  ### Social contact matrix from Prem et al. (2021)
  # Source: "https://github.com/kieshaprem/synthetic-contact-matrices/generate_synthetic_matrices/output/syntheticmatrices/contact_all.rdata
  load("./data/raw/contact_all.rdata")
  prem_2021 <- contact_all$CHE
  rm(contact_all)
  age_prem <- seq(0, 75, 5)
  rownames(prem_2021) <- colnames(prem_2021) <- age_prem
  age_prem_range <- c(age_prem, 200)
  # Convert contact matrix from Prem et al. (2021) to Sentinella age groups
  pop <- numeric(length(age_prem))
  for(i in 1:length(pop)) {
    pop[i] <- sum(pop_year_i$population[pop_year_i$lower.age.limit >= age_prem_range[i] & pop_year_i$lower.age.limit < age_prem_range[i + 1]])
  }
  pop_prem_2021 <- data.frame(lower.age.limit = age_prem, population = pop)$population
  
  prem_2021_adapted <- matrix(NA, nrow = length(age_sentinella), ncol = length(age_sentinella))
  colnames(prem_2021_adapted) <- rownames(prem_2021_adapted) <- age_sentinella

  for(i in 1:length(age_sentinella)) {
    for(j in 1:length(age_sentinella)) {
      lower_i <- age_range[i]
      upper_i <- age_range[i + 1]
      w_i <- which(age_prem >= lower_i & age_prem < upper_i)
      lower_j <- age_range[j]
      upper_j <- age_range[j + 1]
      w_j <- which(age_prem >= lower_j & age_prem < upper_j)
      prem_2021_adapted[i, j] <- sum(pop_prem_2021[w_i]*prem_2021[w_i, w_j])/sum(pop_prem_2021[w_i])#sum(contact1[w_i, w_j])/length(contact1[w_i, w_j])
    } 
  }
  
  #change matrices to be reciprocal
  reciprocal <- function(m, N) {
    m_rec <- m
    for(i in 1:length(N)) {
      for(j in 1:length(N)) {
        m_rec[i, j] <- (m[i, j]*N[i] + m[j, i]*N[j])/(2*N[i])
      }
    }
    return(m_rec)
  }
  prem_2021 <- reciprocal(prem_2021_adapted, pop_sentinella)
  colnames(prem_2021) <- rownames(prem_2021) <- age_sentinella_matrice
  
  # largest eigenvalue of matrices:
  ev_syn <-abs(eigen(prem_2021)$values[1])
  round(ev_syn, 2) # synthetic contact matrix
  
  prem_2021_df <- as.data.frame(melt(prem_2021, varnames = c("participant_age","contact_age"), value.name = "contacts"))
  prem_2021_df$participant_age <- factor(prem_2021_df$participant_age, levels=age_sentinella_matrice)
  prem_2021_df$contact_age <- factor(prem_2021_df$contact_age, levels=age_sentinella_matrice)

  prem_2021_sum <- as.data.frame(rowSums(prem_2021)) # get sum of contacts per day 
  colnames(prem_2021_sum) <- "contact_sum"
  
  prem_2021_sum$participant_age <- rownames(prem_2021_sum)
  prem_2021_sum$participant_age <- factor(prem_2021_sum$participant_age, levels=  age_sentinella_matrice)
  
  
  # Figure 2
  prem_2021_sum$participant_age <- recode(prem_2021_sum$participant_age, "[0,5)"= "0-4", "[5,15)"= "5-14","[15,30)"="15-29", "[30,65)"="30-64","65+"="65+")
  contact_syn <- ggplot(prem_2021_sum, aes(x=participant_age, y = contact_sum))+
    theme_minimal()+
    ylim(c(0,20))+
    geom_bar(stat = "identity", fill="steelblue", linewidth=2, alpha=0.7)+#geom_point( color="steelblue")+
    theme(panel.grid.minor = element_line(colour = "transparent"),
          panel.grid.major.x = element_line(colour = "transparent"))+
    labs( y = "Number of contacts \n(per day)",
          x = "Age of participant (years)",
          fill = "", subtitle = "Synthetic (for 2021 in Switzerland)")
  prem_2021_df$participant_age <- recode(prem_2021_df$participant_age, "[0,5)"= "0-4", "[5,15)"= "5-14","[15,30)"="15-29", "[30,65)"="30-64","65+"="65+")
  prem_2021_df$contact_age <- recode(prem_2021_df$contact_age, "[0,5)"= "0-4", "[5,15)"= "5-14","[15,30)"="15-29", "[30,65)"="30-64","65+"="65+")
  contact_m_syn <- ggplot(prem_2021_df, aes(y = contact_age, x = participant_age, fill = contacts)) + 
    theme_minimal()+
    geom_tile()+
    geom_text(aes(label = sprintf("%0.2f", round(contacts,2)))) +
    scale_fill_gradientn(limits=c(0,10),
                         colors = c( "white","steelblue"),
                         na.value = col_9[9])+
    labs(subtitle = "",#Synthetic (for 2021 in Switzerland)
         x = "Age of participant (years)",
         y = "Age of contact (years)",
         fill = "Number of contacts \n(per day)")
  fig2_syn <- ggarrange(contact_syn,contact_m_syn,ncol=2, nrow=1,common.legend = TRUE, legend="top")
  
  #take sero data from here: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9985433/table/Tab2/?report=objectonly
  #median(seq(as_date("2020-05-01"),as_date("2020-10-31"),1))
  #median(seq(as_date("2020-11-01"),as_date("2021-05-15"),1))
  #median(seq(as_date("2021-05-16"),as_date("2021-09-30"),1))
  #take sero data from here: https://www.eurosurveillance.org/content/table/10.2807/1560-7917.ES.2022.27.31.2200561.t2?fmt=ahah&fullscreen=true Rebecca Amati et al 2022
  #median(seq(as_date("2022-03-01"),as_date("2022-03-31"),1))
  seroprevalence <- cbind(c(as_date("2020-01-01"), as_date("2020-07-31"),as_date("2021-02-06"), as_date("2021-07-23"), as_date("2022-03-16")),
                          c(0,0.37,0.162,0.72,0.975))
  colnames(seroprevalence) <- c("date", "value")
  seroprevalence<- as.data.frame(seroprevalence)
  seroprev <- c()
  seroprev$date <- seq(as_date("2020-01-01"),as_date("2022-06-01"),1)
  seroprevalence <- glm(value~as_date(date),data=seroprevalence, family=binomial(link="logit"))
  seroprev <- predict(seroprevalence,seroprev)
  seroprev <- 1 / (1 + exp(-seroprev))
  seroprev <- cbind(unlist(seroprev),seq(as_date("2020-01-01"),as_date("2022-06-01"),1))
  seroprev <- as.data.frame(seroprev)
  sf1 <- ggplot()+ 
    theme_minimal()+
    geom_line(data = seroprev, aes(x = as_date(V2), y = V1), color= "steelblue", linewidth=2)+
    geom_area()+
    coord_cartesian(ylim =c(0,1),expand = TRUE)+
    scale_x_date(date_breaks = "3 month", date_labels = "%b\n%Y")+
    theme(panel.grid.minor = element_line(colour = "transparent"),
          panel.grid.major.x = element_line(colour = "transparent"))+
    labs(tag=bquote(.("")),subtitle = "", x = "", y =bquote("Seroprevalence estimates \nin Switzerland"))
  ggsave(sf1, filename = paste0("./output/figures/age_matrices/sensitivity/SF3.png"), height = 4, width = 4,  bg = "transparent")
  remove(seroprev)
  
  ## get contact matrices, reduction, largest eigenvalue
  #get matrices per panel ('paired' child panel: A-C, B-D, E-F) 
  #get per wave (closest child wave)
  #get per wave (child waves together)
  contacts_function = function(comix, p, pc){
    l_ev1 <- c()
    l_ev2 <- c()
    l_ev3 <- c()
    l_ev4 <- c()
    l_ev5 <- c()
    l_ev6 <- c()
    contact_red <-c()
    comix_survey <- comix # get survey data
    
    if(z %in% c(1,2)){# if panel to filter
      comix_survey$participants <- comix_survey$participants %>% filter(panel %in% c(p, pc))# select/filter the survey 
    }
    if(!z %in% c(1,2)){# if (panel-) wave to filter
      comix_survey$participants <- comix_survey$participants %>% filter(panel_wave %in% c(p, pc))# select/filter the survey 
    }
    levels(comix_survey$participants$part_age) <- age_sentinella
    dates <- paste0(format(as_date(min(comix_survey$participants$sday_id[comix_survey$participants$survey_group=="adults"])),format = "%B %Y")," to ", format(as_date(max(comix_survey$participants$sday_id[comix_survey$participants$survey_group=="adults"])),format = "%B %Y"))

    if(z %in% c(2,4)){ # pooled children
      pc <- "all children waves"
    }
    # get contact matrices
    comix_matrix_sym <- replicate(n = n_sample,contact_matrix(comix_survey,
                                                              survey.pop =pop_year_i,
                                                              age.limits = age_sentinella,
                                                              symmetric = T,
                                                              estimated.contact.age = "sample",
                                                              missing.contact.age = "sample",
                                                              sample.participants = TRUE,
                                                              weigh.dayofweek = T) )
    
    # get reduction of contacts compared with synthetic contact matrix:
    m_r <- matrix(NA, nrow = n_sample, ncol = length(age_sentinella))
    for(i in 1:n_sample) m_r[i, ] <- rowSums(comix_matrix_sym[, i]$matrix)/prem_2021_sum$contact_sum
    contact_red <- matrix(nrow=length(age_sentinella),ncol=4)
    contact_red[,1] <- c(paste0(paste(p, collapse=", ")," and ", pc))
    for(i in 1:length(age_sentinella_matrice)) contact_red[i,c(2:4)] <- quantile(m_r[,i], probs = prob_95)
    contact_red <- as.data.frame(contact_red)
    colnames(contact_red)<- c("survey","reduction_median", "reduction_low", "reduction_up")
    remove(m_r)
    
    #get df do summarize panel/wave
    df <- cbind(comix_survey$participants %>%
      summarise(
        survey= paste(p, pc, collapse =" "),
        n_part = length(unique(part_id)),
                n_cont = sum(sum_contacts),
                mean_contacts = sum(sum_contacts)/length(unique(part_id))),
      comix_survey$participants %>% filter(survey_group=="adults")%>%
      summarise(
        time_panel_wave = paste0(format(as_date(min(sday_id)),format = "%d %B %Y")," to ", format(as_date(max(sday_id)),format = "%d %B %Y")),
                max_time = as_date(max(sday_id)),
                min_time = as_date(min(sday_id)),
                mid_time = median(seq(min_time,max_time,1)))) # careful using
        
    #comparison with R_e estimates:
    #seroprevalence/immunity data preparation
    new_data<- as.data.frame(unique(comix_survey$participants$sday_id)[order(unique(comix_survey$participants$sday_id))])
    colnames(new_data) <- c("date")
    
  
    # get largest eigenvalue from contact age matrix
    # for comparison with the effective reproduction number
    ev_b <- numeric(n_sample)
    #'largest eigenvalue' direct from matrices
    for(i in 1:n_sample) ev_b[i] <- abs(eigen(comix_matrix_sym[, i]$matrix)$values[1])
    l_ev1 <- t(quantile(ev_b, probs = prob_95))
    l_ev1 <- c(df, l_ev1)
    l_ev1 <- as.data.frame(l_ev1)
    l_ev1[c(9:11)] <- sapply(l_ev1[c(9:11)], as.numeric)
    colnames(l_ev1) <- c(colnames(df),"value", "value_low", "value_up")
    #'largest eigenvalue' * susceptibility and infectiousness (infectiousness =1, can be ignored)
    infectiousness <- c(1,1,1,1,1)
    susceptibility <- (1-rep(mean(predict(seroprevalence, new_data, type="response")),length(age_sentinella)))
    susceptibility <- c(0.5,0.5,(length(15:17)*0.5+length(18:29)*1)/length(15:29),1,1)*susceptibility# children:0.5 adults 1 from ONS see Munday et al 2021
    for(i in 1:n_sample) ev_b[i] <- abs(eigen(comix_matrix_sym[, i]$matrix*infectiousness*susceptibility)$values[1])
    l_ev2 <- t(quantile(ev_b, probs = prob_95))
    l_ev2 <- c(df, l_ev2)
    l_ev2 <- as.data.frame(t(l_ev2))
    l_ev2[c(9:11)] <- sapply(l_ev2[c(9:11)], as.numeric)
    colnames(l_ev2) <- c(colnames(df),"value", "value_low", "value_up")
    # 'largest eigenvalue' * susceptibility with 50% protection
    susceptibility <- (1-.5*rep(mean(predict(seroprevalence, new_data, type="response")),length(age_sentinella)))
    susceptibility <- c(0.5,0.5,(length(15:17)*0.5+length(18:29)*1)/length(15:29),1,1)*susceptibility# children:0.5 adults 1 from ONS see Munday et al 2021
    for(i in 1:n_sample) ev_b[i] <- abs(eigen(comix_matrix_sym[, i]$matrix*infectiousness*susceptibility)$values[1])
    l_ev3 <- t(quantile(ev_b, probs = prob_95))
    l_ev3 <- c(df, l_ev3)
    l_ev3 <- as.data.frame(t(l_ev3))
    l_ev3[c(9:11)] <- sapply(l_ev3[c(9:11)], as.numeric)
    colnames(l_ev3) <- c(colnames(df),"value", "value_low", "value_up")
    # 'largest eigenvalue' * relative 'growth rates'
    #relative 'transmissibility'/'growth rates': Alpha: 129%;  Delta: 197%; Omicron: 652;  'other' set 1
    relative_growth <- growth_day$relative_growth[growth_day$date %in% seq(min(comix_survey$participants$sday_id[comix_survey$participants$survey_group=="adults"]),max(comix_survey$participants$sday_id[comix_survey$participants$survey_group=="adults"]),1)]
    relative_growth <- sum(relative_growth/length(relative_growth))
    for(i in 1:n_sample) ev_b[i] <- abs(eigen(comix_matrix_sym[, i]$matrix*relative_growth)$values[1])
    l_ev4 <- t(quantile(ev_b, probs = prob_95))
    l_ev4 <- c(df, l_ev4)
    l_ev4 <- as.data.frame(t(l_ev4))
    l_ev4[c(9:11)] <- sapply(l_ev4[c(9:11)], as.numeric)
    colnames(l_ev4) <- c(colnames(df),"value", "value_low", "value_up")
    # 'largest eigenvalue' * relative 'growth rates' *susceptibility
    susceptibility <- (1-rep(mean(predict(seroprevalence, new_data, type="response")),length(age_sentinella)))
    susceptibility <- c(0.5,0.5,(length(15:17)*0.5+length(18:29)*1)/length(15:29),1,1)*susceptibility# children:0.5 adults 1 from ONS see Munday et al 2021
    for(i in 1:n_sample) ev_b[i] <- abs(eigen(comix_matrix_sym[, i]$matrix*infectiousness*susceptibility*relative_growth)$values[1])
    l_ev5 <- t(quantile(ev_b, probs = prob_95))
    l_ev5 <- c(df, l_ev5)
    l_ev5 <- as.data.frame(t(l_ev5))
    l_ev5[c(9:11)] <- sapply(l_ev5[c(9:11)], as.numeric)
    colnames(l_ev5) <- c(colnames(df),"value", "value_low", "value_up")
    # 'largest eigenvalue' * relative 'growth rates' *susceptibility * 50% protection
    susceptibility <- (1-.5*rep(mean(predict(seroprevalence, new_data, type="response")),length(age_sentinella)))
    susceptibility <- c(0.5,0.5,(length(15:17)*0.5+length(18:29)*1)/length(15:29),1,1)*susceptibility# children:0.5 adults 1 from ONS see Munday et al 2021
    for(i in 1:n_sample) ev_b[i] <- abs(eigen(comix_matrix_sym[, i]$matrix*infectiousness*susceptibility*relative_growth)$values[1])
    l_ev6 <- t(quantile(ev_b, probs = prob_95))
    l_ev6 <- c(df, l_ev6)
    l_ev6 <- as.data.frame(t(l_ev6))
    l_ev6[c(9:11)] <- sapply(l_ev6[c(9:11)], as.numeric)
    colnames(l_ev6) <- c(colnames(df),"value", "value_low", "value_up")
    remove(ev_b)
    
    
    # get number of contacts per participants age group
    c_sum <- matrix(ncol=length(age_sentinella_matrice),nrow=n_sample)
    for(i in 1:n_sample) c_sum[i,] <- rowSums(comix_matrix_sym[, i]$matrix)
    contact_sum <- matrix(nrow=length(age_sentinella_matrice),ncol=4)
    contact_sum[,1] <- c(paste0(paste(p, collapse=", ")," and ", pc))
    for(i in 1:length(age_sentinella_matrice)) contact_sum[i,c(2:4)] <- quantile(c_sum[,i], probs = prob_95)
    contact_sum <- as.data.frame(contact_sum)
    colnames(contact_sum)<- c("survey","contact_sum", "contact_sum_low", "contact_sum_up")
    contact_sum[c(2:4)] <- sapply(contact_sum[c(2:4)], as.numeric)
    remove(c_sum)
    
    # clean matrix data:
    comix_matrix_sym$matrix <- apply(abind(comix_matrix_sym["matrix", ], along=3), 1:2, median) # median from bootstraps
    comix_matrix_sym$matrix <- melt(comix_matrix_sym$matrix, varnames = c("participant_age","contact_age"), value.name = "contacts")
    comix_matrix_sym$matrix[,1] <- rep(levels(comix_matrix_sym$matrix[,2]),length(levels(comix_matrix_sym$matrix[,2])))
    comix_matrix_sym$matrix[,1] <- factor(comix_matrix_sym$matrix[,1], levels=levels(comix_matrix_sym$matrix[,2]))
    comix_matrix_sym$matrix$panels <- paste0(paste(p, collapse=", ")," and ", pc)
    
    contact_sum$participant_age <- levels(comix_matrix_sym$matrix$contact_age)
    contact_sum$participant_age <- factor(contact_sum$participant_age, levels=levels(comix_matrix_sym$matrix$contact_age))
    contact_sum <- contact_sum %>% mutate_at(c("contact_sum", "contact_sum_low", "contact_sum_up"), as.numeric)
  
    contact_red$participant_age <- levels(comix_matrix_sym$matrix$contact_age)
    contact_red$participant_age <- factor(contact_red$participant_age, levels=levels(comix_matrix_sym$matrix$contact_age))
    contact_red <- contact_red %>% mutate_at(c("reduction_median", "reduction_low", "reduction_up"), as.numeric)
    
    contact_red$participant_age <- recode(contact_red$participant_age, "[0,5)"= "0-4", "[5,15)"= "5-14","[15,30)"="15-29", "[30,65)"="30-64","65+"="65+")
    contact_sum$participant_age <- recode(contact_sum$participant_age, "[0,5)"= "0-4", "[5,15)"= "5-14","[15,30)"="15-29", "[30,65)"="30-64","65+"="65+")
    
    # Figure 2 (SF4)
    contact_s <- ggplot(contact_sum, aes(x=participant_age, y = contact_sum))+# figure 2 (and SF4)
      theme_minimal()+
      theme(panel.grid.minor = element_line(colour = "transparent"),
            panel.grid.major.x = element_line(colour = "transparent"))+
      ylim(c(0,20))+
      geom_bar(stat = "identity", fill="steelblue", linewidth=2, alpha=0.7)+#geom_point(color="steelblue", size=2)+
      geom_errorbar(aes(ymax=contact_sum_up, ymin=contact_sum_low),color="steelblue", width=0)+
      labs( y = "Number of contacts \n(per day)",
            x = "Age of participant (years)",
            fill = "", subtitle = dates)
    
    comix_matrix_sym$matrix$participant_age <- recode(comix_matrix_sym$matrix$participant_age, "[0,5)"= "0-4", "[5,15)"= "5-14","[15,30)"="15-29", "[30,65)"="30-64","65+"="65+")
    comix_matrix_sym$matrix$contact_age <- recode(comix_matrix_sym$matrix$contact_age, "[0,5)"= "0-4", "[5,15)"= "5-14","[15,30)"="15-29", "[30,65)"="30-64","65+"="65+")
    contact_m_sym <- ggplot(comix_matrix_sym$matrix, aes(y = contact_age, x = participant_age, fill = contacts)) + # figure 2 (and SF4)
      theme_minimal()+
      geom_tile()+
      geom_text(aes(label = sprintf("%0.2f", round(contacts,2)))) +
      scale_fill_gradientn(limits=c(0,10),
                           colors = c( "white","steelblue"),
                           na.value = col_9[9])+
      labs(subtitle = " ",
           x = "Age of participant (years)",
           y = "Age of contact (years)",
           fill = "Number of contacts \n(per day)")
    
    # Figure 3 (SF5)
    if(runs[z]%in% c("run_waves_pooledchildren","run_panels_pooledchildren")){
      contact_red$survey <- gsub("and all children waves","",contact_red$survey)
      contact_red <- contact_red[order(contact_red$survey),]
    }
    contact_red_fig3 <- ggplot(contact_red)+
      theme_minimal()+
      facet_wrap(~survey, ncol= 3)+
      geom_errorbar(aes(ymin=reduction_low,ymax=reduction_up, x=participant_age), color="steelblue", width=0)+
      geom_point(aes(y=reduction_median, x=participant_age), color="steelblue")+
      geom_hline(yintercept=1)+
      coord_cartesian(ylim=c(0,1))+
      theme(axis.text.x =element_text(angle = 45,vjust = 0.5),
            panel.grid.minor = element_line(colour = "transparent"),
            panel.grid.major.x = element_line(colour = "transparent"))+
      labs(subtitle = " ",
           y = "Relative number of contacts",
           x = "Age of participant (years)"
           )
    
  
    plots <- list(contact_s, contact_m_sym)
    return(list(plots, comix_matrix_sym, contact_red, contact_red_fig3, l_ev1,l_ev2,l_ev3,l_ev4,l_ev5,l_ev6, comix_matrix_sym$matrix,contact_sum))
  }
  
  n_sample <- 100 # number of bootstrapes (as using sampling method)

  # create file to save output from different scenarios/sensitivity analysis
  runs <- c("run_panels","run_panels_pooledchildren", "run_waves","run_waves_pooledchildren")
  correlation_df <- c()
  fig5_i <- c()
  for(z in 1:length(runs)){ 
    contact_red <- c()
    #comix_matrix_all <- c()
    #comix_contact_sum_all <- c()
    #comix_matrix_sym <- NULL
    l_ev1 <-c()
    l_ev2 <-c()
    l_ev3 <-c()
    l_ev4 <-c()
    l_ev5 <-c()
    l_ev6 <-c()
    fig2_i <- c()
    fig3_i <- c()

    if(z==1){  #get matrices per panel ('paired' child panel: A-C, B-D, E-F) 
      for(p in unique(comix$participants$panel[comix$participants$survey_group=="adults"])){# get adult panel
        if(p%in%"A"){
          pc <- "C"# choosing children panel
        }
        else if(p%in% "B"){
          pc <- "D"
        }
        else if(p%in%"F"){
          pc <- "E"
        }
        contacts_data <- contacts_function(comix, p, pc)
        contact_red <- rbind(contact_red, contacts_data[[3]])
        # SF4
        if(class(fig2_i)=="NULL"){ fig2_i[[1]] <- fig2_syn}
        fig2_i[[(length(fig2_i)+1)]] <- ggarrange(contacts_data[[1]][[1]], contacts_data[[1]][[2]],ncol=2, nrow=1, common.legend = TRUE, legend="none")
        fig2 <- grid.arrange(grobs = fig2_i,ncol=1, nrow=length(fig2_i), common.legend = TRUE, legend="top")
        ggsave(fig2, filename = paste0("./output/figures/age_matrices/sensitivity/SF4.png"), height = 3.5*length(fig2_i), width = 8,  bg = "transparent")
        # SF5
        if(class(fig3_i)!="NULL"){contacts_data[[4]] <-contacts_data[[4]]+labs(y="")}
        fig3_i[[(length(fig3_i)+1)]] <- ggarrange(contacts_data[[4]],ncol=1, nrow=1, common.legend = TRUE, legend="none")
        fig3 <- grid.arrange(grobs = fig3_i, ncol=length(fig3_i), common.legend = TRUE, legend="top")
        ggsave(fig3, filename = paste0("./output/figures/age_matrices/sensitivity/SF5.png"),  height = 4, width = 12, bg = "transparent")
      }
      } #get matrices per panel ('paired' child panel: A-C, B-D, E-F) 
    
    if(z==2){ #three adult panels with pooled children waves
      for(p in unique(comix$participants$panel[comix$participants$survey_group=="adults"])){# get adult panel
        pc <- unique(comix$participants$panel[comix$participants$survey_group=="children"])
        contacts_data <- contacts_function(comix, p, pc)
        contact_red <- rbind(contact_red, contacts_data[[3]])
        # Figure 2
        if(class(fig2_i)=="NULL"){ fig2_i[[1]] <- fig2_syn}
        fig2_i[[(length(fig2_i)+1)]] <- ggarrange(contacts_data[[1]][[1]], contacts_data[[1]][[2]],ncol=2, nrow=1, common.legend = TRUE, legend="none")
        fig2 <- grid.arrange(grobs = fig2_i,ncol=1, nrow=length(fig2_i), common.legend = TRUE, legend="top")
        ggsave(fig2, filename = paste0("./output/figures/age_matrices/Figure2.png"), height = 3.5*length(fig2_i), width = 8,  bg = "transparent")
        # Figure 3
        fig3_i[[(length(fig3_i)+1)]] <- ggarrange(contacts_data[[4]],ncol=1, nrow=1, common.legend = TRUE, legend="none")
        fig3 <- grid.arrange(grobs = fig3_i, ncol=length(fig3_i), common.legend = TRUE, legend="top")
        ggsave(fig3, filename = paste0("./output/figures/age_matrices/Figure3.png"),  height = 4, width = 12, bg = "transparent")
      }
    }
    
    if(z==3){ #get per wave (closest child wave)
      for(p in unique(comix$participants$panel_wave[comix$participants$survey_group=="adults"])){
        date <- panel_wave_df$mid_time[panel_wave_df$panel_wave==p] #selecting data (waves)
        x <- min(abs(as.numeric(comix$participants$sday_id[comix$participants$survey_group=="children"]) - as.numeric(date)))
        pc <- unique(comix$participants$panel_wave[comix$participants$survey_group=="children" & abs(as.numeric(comix$participants$sday_id)-as.numeric(date))==x])
     contacts_data <- contacts_function(comix, p, pc)
     
     l_ev1 <- rbind(l_ev1, contacts_data[[5]])
     l_ev2 <- rbind(l_ev2, contacts_data[[6]])
     l_ev3 <- rbind(l_ev3, contacts_data[[7]])
     l_ev4 <- rbind(l_ev4, contacts_data[[8]])
     l_ev5 <- rbind(l_ev5, contacts_data[[9]])
     l_ev6 <- rbind(l_ev6, contacts_data[[10]])
     }
        } 
    if(z==4){# pooled children waves
      for(p in unique(comix$participants$panel_wave[comix$participants$survey_group=="adults"])){
        pc <- unique(comix$participants$panel_wave[comix$participants$survey_group=="children"])
        contacts_data <- contacts_function(comix, p, pc)
       
        l_ev1 <- rbind(l_ev1, contacts_data[[5]])
        l_ev2 <- rbind(l_ev2, contacts_data[[6]])
        l_ev3 <- rbind(l_ev3, contacts_data[[7]])
        l_ev4 <- rbind(l_ev4, contacts_data[[8]])
        l_ev5 <- rbind(l_ev5, contacts_data[[9]])
        l_ev6 <- rbind(l_ev6, contacts_data[[10]])
    }
      }#get per wave (child waves together)
  
  if(z %in% c(1,2)){print(paste0("Max number of reduction: ", round((1- min(contact_red$reduction_median))*100,1), " in ",z))}
      
      if(z %in% c(3,4)){ # largest eigenvalue comparison
        larg_eig <- c()
        for(n in 1:6){
          y_name <- "Transmission matrix"
          if(n==1){
            larg_eig <- l_ev1
            y_name <- "Social contact matrix"
          }
          if(n==2){
            larg_eig <- l_ev2
          }
          if(n==3){
            larg_eig <- l_ev3
          }
          if(n==4){
            larg_eig <- l_ev4
          }
          if(n==5){
            larg_eig <- l_ev5
          }
          if(n==6){
            larg_eig <- l_ev6
          }
  colnames(larg_eig)[c(1,9:11)] <- c("survey","value","value_low", "value_up")
  larg_eig$min_time <- as_date(unlist(larg_eig$min_time))
  larg_eig$max_time <- as_date(unlist(larg_eig$max_time))
  larg_eig$mid_time <- as_date(unlist(larg_eig$mid_time))
  larg_eig$value <- unlist(larg_eig$value)
  larg_eig$value_low <- unlist(larg_eig$value_low)
  larg_eig$value_up <- unlist(larg_eig$value_up)
  # Get data for comparison with largest eigenvalue (stringency (KOF), vaccination uptake, R_e)
  bag <- BAG_data[as_date(BAG_data$date) %in% seq(min(larg_eig$min_time)-10,max(larg_eig$min_time)+10,1),]
  for (pw in larg_eig$survey) {
    larg_eig$kof_median[larg_eig$survey %in% pw] <- median(kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date %in% seq(min(larg_eig$min_time[larg_eig$survey %in% pw]), max(larg_eig$max_time[larg_eig$survey %in% pw]), 1)])
    larg_eig$kof_mean[larg_eig$survey %in% pw] <- mean(kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date %in% seq(min(larg_eig$min_time[larg_eig$survey %in% pw]), max(larg_eig$max_time[larg_eig$survey %in% pw]), 1)])
    
    larg_eig$Re_median[larg_eig$survey %in% pw] <- median(na.omit(reported_cases$R_median[reported_cases$date %in% seq(min(larg_eig$min_time[larg_eig$survey %in% pw]), min(larg_eig$max_time[larg_eig$survey %in% pw]), 1)]))
    larg_eig$Re_mean[larg_eig$survey %in% pw] <- median(na.omit(reported_cases$R_median[reported_cases$date %in% seq(min(larg_eig$min_time[larg_eig$survey %in% pw]), min(larg_eig$max_time[larg_eig$survey %in% pw]), 1)]))
  
    larg_eig$vac_median[larg_eig$survey %in% pw] <- median(bag$cum_vacdo_pre[bag$date %in% seq(min(larg_eig$min_time[larg_eig$survey %in% pw]), min(larg_eig$max_time[larg_eig$survey %in% pw]), 1)])
    larg_eig$vac_mean[larg_eig$survey %in% pw] <- median(bag$cum_vacdo_pre[bag$date %in% seq(min(larg_eig$min_time[larg_eig$survey %in% pw]), min(larg_eig$max_time[larg_eig$survey %in% pw]), 1)])
    
    }
  larg_eig[1:17] <- lapply(larg_eig[1:17], function(x) unlist(x))
  if(runs[z]%in% c("run_waves_pooledchildren","run_panels_pooledchildren")){
    larg_eig$survey <- gsub(" all children waves","",larg_eig$survey)
    larg_eig <- larg_eig[order(larg_eig$survey),]
  }
  #https://www.bag.admin.ch/bag/en/home/krankheiten/ausbrueche-epidemien-pandemien/aktuelle-ausbrueche-epidemien/novel-cov/massnahmen-des-bundes.html (Access: 2021-07-19)
  fig4a <- ggplot(data= kof_ch)+
    theme_minimal()+
    geom_line(data= kof_ch,aes(x= date, y=ch.kof.stringency.ch.stringency_plus), col="steelblue",linewidth =2) +
    scale_x_date(date_breaks = "3 month", 
                 date_labels = "%b\n%Y",
                 limits = as_date(c(min_time-2,max_time+2)))+
    
    annotate("pointrange", x = as_date("2021-01-18"), y = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-01-18")], ymin = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-01-18")], ymax = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-01-18")]+17,colour = col_9[9], linewidth =0.8)+
    annotate("text",hjust =0.5,vjust =0, x=as_date("2021-01-18"), y=kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-01-18")]+20, label= "Mandatory home-office, \nmax 5 people, closures, etc.",size=1.8,colour = "black") + 
    
    annotate("pointrange", x = as_date("2021-01-28"), y = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-01-28")], ymin = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-01-28")], ymax = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-01-28")]-7,colour = col_9[9], linewidth =0.8)+
    annotate("text",hjust =0.5,vjust =1, x=as_date("2021-01-28"), y=kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-01-28")]-10, label= "Free testing",size=1.8,colour = "black") + 
    
    annotate("pointrange", x = as_date("2021-03-01"), y = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-03-01")], ymin = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-03-01")], ymax = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-03-01")]-7 ,colour = col_9[9], linewidth =0.8)+
    annotate("text",hjust =0.5,vjust =1, x=as_date("2021-03-01"), y=kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-03-01")]-10, label= "Max of 15 people outside, \nre-opening of all shops, \nleisure and sport activities \nbut mandatory mask wearing",size=1.8,colour = "black") + 
    
    annotate("pointrange", x = as_date("2021-04-07"), y = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-04-07")], ymin = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-04-07")], ymax = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-04-07")]+29,colour = col_9[9], linewidth =0.8)+
    annotate("text",hjust =0.5,vjust =0, x=as_date("2021-04-07"), y=kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-04-07")]+32, label= "Free self-tests",size=1.8,colour = "black") + 
    
    annotate("pointrange", x = as_date("2021-04-19"), y = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-04-19")], ymin = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-04-19")], ymax = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-04-19")]-35,colour = col_9[9], linewidth =0.8)+
    annotate("text",hjust =0.5,vjust =1, x=as_date("2021-04-19"), y=kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-04-19")]-38, label= "Stepwise re-opening",size=1.8,colour = "black") + #Further re-openings, \ne.g., restaurants outdoors \nbut with capacity limits
    
    annotate("pointrange", x = as_date("2021-05-31"), y = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-05-31")], ymin = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-05-31")], ymax = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-05-31")]+37,colour = col_9[9], linewidth =0.8)+
    annotate("text",hjust =0.5,vjust =0, x=as_date("2021-05-31"), y=kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-05-31")]+40, label= "Stepwise re-opening",size=1.8,colour = "black") + #"Further re-openings, \ne.g., restaurants opened indoors
    
    annotate("pointrange", x = as_date("2021-06-22"), y = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-06-22")], ymin = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-06-22")], ymax = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-06-22")]+10,colour = col_9[9], linewidth =0.8)+
    annotate("text",hjust =0.5,vjust =1, x=as_date("2021-06-22"), y=kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-06-22")]+25, label= "Vaccination\n(>11 years)",size=1.8,colour = "black") + 
    
    annotate("pointrange", x = as_date("2021-06-26"), y = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-06-26")], ymin = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-06-26")], ymax = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-06-26")]-15,colour = col_9[9], linewidth =0.8)+
    annotate("text",hjust =0.5,vjust =1, x=as_date("2021-06-26"), y=kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-06-26")]-20, label= "Wide re-opening \nwith COVID certificate",size=1.8,colour = "black") + 
    
    annotate("pointrange", x = as_date("2021-12-20"), y = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-12-20")], ymin = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-12-20")], ymax = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-12-20")]-15,colour = col_9[9], linewidth =0.8)+
    annotate("text",hjust =0.5,vjust =1, x=as_date("2021-12-20"), y=kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-12-20")]-18, label= "Stricter measures (2G rule)",size=1.8,colour = "black") + 
    
    annotate("pointrange", x = as_date("2022-01-12"), y = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2022-01-12")], ymin = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2022-01-12")], ymax = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2022-01-12")]+47,colour = col_9[9], linewidth =0.8)+
    annotate("text",hjust =0.5,vjust =1, x=as_date("2022-01-12"), y=kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2022-01-12")]+52, label= "Isolation and quarantine shortened to 5d",size=1.8,colour = "black") + 
    
    #annotate("pointrange", x = as_date("2022-02-03"), y = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2022-02-03")], ymin = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2022-02-03")], ymax = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2022-02-03")]+37,colour = col_9[9], linewidth =0.8)+
    #annotate("text",hjust =0.5,vjust =1, x=as_date("2022-02-03"), y=kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2022-02-03")]+40, label= "Working from home obligation and \nquarantine was lifted",size=1.8,colour = "black") + 
    
    annotate("pointrange", x = as_date("2022-02-17"), y = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2022-02-17")], ymin = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2022-02-17")], ymax = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2022-02-17")]-7,colour = col_9[9], linewidth =0.8)+
    annotate("text",hjust =0.5,vjust =1, x=as_date("2022-02-17"), y=kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2022-02-17")]-10, label= "Most measures lifted",size=1.8,colour = "black") + 
    
    annotate("pointrange", x = as_date("2022-04-01"), y = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2022-04-01")], ymin = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2022-04-01")], ymax = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2022-04-01")]+27,colour = col_9[9], linewidth =0.8)+
    annotate("text",hjust =0.5,vjust =1, x=as_date("2022-04-01"), y=kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2022-04-01")]+40, label= "All covid-19 \nmeasures lifted",size=1.8,colour = "black") + 
    theme(axis.title=element_text(size=10),legend.position ="none" ,
          panel.grid.minor = element_line(colour = "transparent"), 
          panel.grid.major.x = element_line(colour = "transparent"))+
    scale_y_continuous(limits = c(0,100))+
    labs(x = "", y =bquote("KOF Stringency-\nPlus Index"))

  # largest eigenvalue from Swiss CoMix data
  fig4b <- ggplot(larg_eig)+
    theme_minimal()+
    geom_linerange(aes(xmin=min_time,xmax=max_time, y=value, color=survey),linewidth=1) +#col="steelblue"
    geom_errorbar(aes(x=mid_time, ymin=value_low, ymax=value_up, color=survey),width=0)+
    scale_x_date(date_breaks = "3 month", 
                 date_labels = "%b\n%Y",
                 limits = as_date(c(min_time-2,max_time+2)))+
    theme(axis.title=element_text(size=10),legend.position ="none" ,
          panel.grid.minor = element_line(colour = "transparent"), 
          panel.grid.major.x = element_line(colour = "transparent"))+
    scale_color_manual(values=cols25(n=length(larg_eig$survey)), name="Survey panels")+
    scale_fill_manual(values=cols25(n=length(larg_eig$survey)), name="Survey panels")+
    coord_cartesian(ylim=c(0,(round(max(larg_eig$value_up))+1)))+
    labs(x = "", y = y_name, color = "") 
  
  # stringency and largest eigenvalue
  fig4c <- ggplot(data= larg_eig,aes(y=value, x=kof_median))+
    theme_minimal()+
    coord_cartesian(ylim=c(round(min(larg_eig$value_low))-1,round(max(larg_eig$value_up))+1))+
    scale_x_continuous(limits = c(0,100))+
    geom_smooth( method="lm", formula = y ~ x, col="steelblue",fullrange=TRUE,  se=TRUE)+
    #theme(axis.title=element_text(size=10))+
    geom_point(aes(color=survey), alpha=0.6, size=2)+
    geom_errorbar(aes(ymin=value_low, ymax=value_up, color=survey),alpha=0.6,width=0)+
    theme(axis.title=element_text(size=10),
          panel.grid.minor = element_line(colour = "transparent"), 
          panel.grid.major.x = element_line(colour = "transparent"),
          legend.direction = "horizontal")+
    scale_color_manual(values=cols25(n=length(larg_eig$survey)), name="Survey panels")+
    scale_fill_manual(values=cols25(n=length(larg_eig$survey)), name="Survey panels")+
    labs(x = "Stringency", y = y_name, color = "") 
  
  #Vaccine uptake( firstdose coverage)
  fig4d<-ggplot()+
    theme_minimal()+
    geom_area(data= BAG_data,aes(x= date, y=cum_vacdo_pre),fill="steelblue", col="steelblue") +
    scale_x_date(date_breaks = "3 month", 
                 date_labels = "%b\n%Y",
                 limits = as_date(c(min_time-2,max_time+2)))+
    scale_y_continuous(limits=c(0,100),labels = function(x) format(x, nsmall = 0))+
    theme(axis.title=element_text(size=10),legend.position ="none" ,
          panel.grid.minor = element_line(colour = "transparent"), 
          panel.grid.major.x = element_line(colour = "transparent"))+
    labs(x = "",  y = "Vaccine coverage", color = "") 
  #Vaccine uptake and largest eigenvalue
  fig4e <-ggplot(data= larg_eig,aes(y=value, x=vac_median))+
    theme_minimal()+
    geom_smooth( method='lm', formula = y ~ x, col="steelblue",fullrange=TRUE, se=TRUE)+
    theme(axis.title=element_text(size=10),legend.position="none",
          panel.grid.minor = element_line(colour = "transparent"),
                panel.grid.major.x = element_line(colour = "transparent"))+
    geom_point(aes(color=survey), alpha=0.6, size=2)+
    geom_errorbar(aes(ymin=value_low, ymax=value_up, color=survey),alpha=0.6,width=0)+
    scale_color_manual(values=cols25(n=length(larg_eig$survey)), name="Survey panels")+
    scale_fill_manual(values=cols25(n=length(larg_eig$survey)), name="Survey panels")+
    scale_x_continuous(limits = c(0,100))+
    scale_y_continuous(labels = function(x) format(x, nsmall = 0))+
    theme(axis.title=element_text(size=10),legend.position ="none" ,
          panel.grid.minor = element_line(colour = "transparent"), 
          panel.grid.major.x = element_line(colour = "transparent"))+
    coord_cartesian(ylim=c(round(min(larg_eig$value_low))-1,round(max(larg_eig$value_up))+1))+
    labs(x = "Vaccine coverage", y = y_name, color = "") 
  
  g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}
  
  mylegend<-g_legend(fig4c)
  fig4c <- fig4c + theme(legend.position="none")
  
  #Re
  fig5a<-ggplot()+
    theme_minimal()+
    geom_line(data=reported_cases, aes(x=date, y=R_median), color="steelblue", linewidth=1)+
    geom_ribbon(data= reported_cases, aes(x= date,ymin = R_lower_90, ymax =R_upper_90), fill="steelblue", alpha=0.6)+
    geom_hline(yintercept=1)+
    scale_x_date(date_breaks = "3 month", 
                 date_labels = "%b\n%Y",
                 limits = as_date(c(min_time-2,max_time+2)))+
    theme(axis.title=element_text(size=10),legend.position ="none" ,
          panel.grid.minor = element_line(colour = "transparent"), 
          panel.grid.major.x = element_line(colour = "transparent"))+
    scale_color_manual(values=cols25(n=length(larg_eig$survey)), name="Survey panels")+
    scale_fill_manual(values=cols25(n=length(larg_eig$survey)), name="Survey panels")+
    scale_y_continuous(limits = c(0,2))+
    labs(x = "", y = bquote(italic("R"["e"])), color = "") 
  fig4b5a<-ggplot()+
    geom_line(data=reported_cases, aes(x=date, y=R_median*min(larg_eig$value_low)), color="steelblue", linewidth=1, alpha=0.6)+
    geom_ribbon(data= reported_cases, aes(x= date,ymin = R_lower_90*min(larg_eig$value_low), ymax =R_upper_90*min(larg_eig$value_low)), fill="steelblue", alpha=0.4)+
    theme_minimal()+
    geom_linerange(data=larg_eig, aes(xmin=min_time,xmax=max_time, y=value, color=survey),linewidth=1) +#col="steelblue"
    geom_errorbar(data=larg_eig, aes(x=mid_time, ymin=value_low, ymax=value_up, color=survey),width=0)+
    scale_x_date(date_breaks = "3 month", 
                 date_labels = "%b\n%Y",
                 limits = as_date(c(min_time-2,max_time+2)))+
    theme(axis.title=element_text(size=10),legend.position ="none" ,
          panel.grid.minor = element_line(colour = "transparent"), 
          panel.grid.major.x = element_line(colour = "transparent"))+
    scale_color_manual(values=cols25(n=length(larg_eig$survey)), name="Survey panels")+
    scale_fill_manual(values=cols25(n=length(larg_eig$survey)), name="Survey panels")+
    coord_cartesian(ylim=c(0,(round(max(larg_eig$value_up))+1)))+
    labs(x = "", y = y_name, color = "") 
  
  
  #Re and Largest Eigen-value
  fig5b<-ggplot(data= larg_eig, aes(y=value, x=Re_median))+
    theme_minimal()+
    geom_smooth( method='lm', formula = y ~ x, col="steelblue",fullrange=TRUE,  se=TRUE)+
    geom_point(aes(color=survey), alpha=0.6, size=2)+
    geom_errorbar(aes(ymin=value_low, ymax=value_up, color=survey),alpha=0.6,width=0)+
    theme(axis.title=element_text(size=10),legend.position ="none" ,
          panel.grid.minor = element_line(colour = "transparent"), 
          panel.grid.major.x = element_line(colour = "transparent"))+
    scale_color_manual(values=cols25(n=length(larg_eig$survey)), name="Survey panels")+
    scale_fill_manual(values=cols25(n=length(larg_eig$survey)), name="Survey panels")+
    scale_x_continuous(limits = c(0,2))+
    coord_cartesian(ylim=c(round(min(larg_eig$value_low))-1,round(max(larg_eig$value_up))+1))+
    labs(x = bquote(italic("R"["e"])), y = y_name, color = "") 
  

  #Figure4
  if(n==1&z==3){
      fig4 <-  ggarrange(fig4b,ggarrange(fig4c, fig4e,labels=LETTERS[2:3]),labels=LETTERS[1], nrow =2)
      ggsave(fig4, filename = paste0("./output/figures/age_matrices/sensitivity/SF5.png"), height = 6, width = 6,  bg = "transparent")
  }
  if(n==1&z==4){
    fig4 <-  ggarrange(fig4a,ggarrange(fig4b, fig4c, labels=LETTERS[2:3]),ggarrange(fig4d, fig4e,labels=LETTERS[4:5]),labels=LETTERS[1], nrow =3)
    ggsave(fig4, filename = paste0("./output/figures/age_matrices/Figure4.png"), height = 8, width = 6,  bg = "transparent")
  }

  #Figure5
  if(n==6&z==4){
    fig5 <-  ggarrange(fig5a,ggarrange(fig4b, fig5b, labels=LETTERS[2:3]),labels=LETTERS[1], nrow =2)
    ggsave(fig5, filename = paste0("./output/figures/age_matrices/Figure5.png"), height = 6, width = 6,  bg = "transparent")
  }
  fig5_i[[(length(fig5_i)+1)]] <-  ggarrange(fig4b5a,fig5b)
  
  correlation_coef <- rbind(c(coef(glm(value ~ kof_median, data=larg_eig))[2],  confint(glm(value ~ kof_median, data=larg_eig))[2,]),
                            c(coef(glm(value ~ Re_median, data=larg_eig))[2],  confint(glm(value ~ Re_median, data=larg_eig))[2,]),
                            c(coef(glm(value ~ vac_median, data=larg_eig))[2],  confint(glm(value ~ vac_median, data=larg_eig))[2,]))
  correlation_coef <- as.data.frame(correlation_coef)
  colnames(correlation_coef) <- c("value","value_low", "value_up")
  correlation_coef$comparision <- c("kof_median", "Re_median", "vac_median")
  correlation_coef$run <- runs[z]
  correlation_coef$n <- n
  correlation_df <- rbind(correlation_df, correlation_coef)
        }
      }
    }#end
  evens <- function(x) subset(x, x %% 2 == 0)
  unevens <- function(x) subset(x, x %% 2 == 1)
  
  for(i in 1: length(fig5_i)){
    if(i %in% c(1:6)){
      fig5_i[[i]]<- ggarrange(fig5_i[[i]], labels=LETTERS[unevens(1:12)[i]])
    }
    if(i %in% c(7:12)){
      l <- i-6
      fig5_i[[i]]<- ggarrange(fig5_i[[i]], labels=LETTERS[evens(1:12)[l]])
    }
  }
 
  fig5_l <- grid.arrange(grobs = fig5_i[1:6],labels=LETTERS[evens(1:12)],ncol=1, common.legend = TRUE, legend="none")
 fig5_r <- grid.arrange(grobs = fig5_i[7:12],labels=LETTERS[evens(1:12)],ncol=1, common.legend = TRUE, legend="none")
  fig5 <- ggarrange(fig5_l,fig5_r, ncol=2)
  ggsave(fig5, filename = paste0("./output/figures/age_matrices/sensitivity/SF6.png"),  height = 16, width = 12, bg = "transparent")
    

  
  write.csv(correlation_df,"./output/tables/age_matrices/correlation_df.csv")
  
  }

