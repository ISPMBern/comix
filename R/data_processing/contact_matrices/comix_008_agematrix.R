#' ---
#' title: "Age contact pattern matrix "
#' author: "Martina Reichmuth"
#' date: "26/04/2023"
#' ---

comix_008_agematrix = function() {
  
  # Data preparation
  age_sentinella <- c(0, 5, 15, 30, 65) # Age groups for Sentinella ie https://www.sentinella.ch/de/info
  age_sentinella_cat <- recode(c(0, 5, 15, 30, 65), "0"= "[0,5)", "5"= "[5,15)","15"="[15,30)", "30"="[30,65)","65"="65+")
  
  ### Swiss demographic data
  url_ch_pop <- "https://dam-api.bfs.admin.ch/hub/api/dam/assets/23064766/master" # Source: https://www.bfs.admin.ch/bfs/de/home/statistiken/bevoelkerung/stand-entwicklung/bevoelkerung.assetdetail.23064766.html
  year_i <- "2021"
  options(timeout=120)
  GET(url_ch_pop, write_disk(tf <- tempfile(fileext = ".xlsx")))
  pop_year_i <- readxl::read_excel(tf, sheet = year_i)
  #pop_year_i <- readxl::read_excel("./data/raw/su-d-01.02.03.06.xlsx", sheet = year_i)
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
  colnames(prem_2021) <- rownames(prem_2021) <- age_sentinella_cat
  
  prem_2021_df <- as.data.frame(melt(prem_2021, varnames = c("participant_age","contact_age"), value.name = "contacts"))
  prem_2021_df$participant_age <- factor(prem_2021_df$participant_age, levels=age_sentinella_cat)
  prem_2021_df$contact_age <- factor(prem_2021_df$contact_age, levels=age_sentinella_cat)

  prem_2021_sum <- as.data.frame(rowSums(prem_2021)) # get sum of contacts per day 
  colnames(prem_2021_sum) <- "contact_sum"
  
  prem_2021_sum$participant_age <- rownames(prem_2021_sum)
  prem_2021_sum$participant_age <- factor(prem_2021_sum$participant_age, levels=  age_sentinella_cat)
  
  
  ## Get Swiss CoMix data:
  # comix <- get_survey("https://doi.org/10.5281/zenodo.6542656")   # from zenodo
  part_df <- read.csv("./data/processed/zenodo/CoMix_ch_participant_common.csv",row.names = 1)
  part_df1 <- read.csv("./data/processed/zenodo/CoMix_ch_participant_extra.csv",row.names = 1)
  part_df2 <- read.csv("./data/processed/zenodo/CoMix_ch_sday.csv",row.names=1)
  part_df2$sday_id <- as_date(part_df2$sday_id)
  
  participants <- cbind(part_df1,part_df2[,-c(1)])
  participants <- merge(part_df,participants, by="part_id")
  
  cont_df <- read.csv("./data/processed/zenodo/CoMix_ch_contact_common.csv",row.names = 1)
  cont_df1 <- read.csv("./data/processed/zenodo/CoMix_ch_contact_extra.csv",row.names = 1)
  cont_df2 <- read.csv("./data/processed/zenodo/CoMix_ch_hh_common.csv",row.names = 1)
  contacts <- merge(cont_df,cont_df1, by=c("cont_id"))
  
  comix <- socialmixr::survey(
    participants =   participants,
    contacts =   contacts)
  comix <- clean(comix)
  
  # truncate at 50 contacts 
  comix$contacts <- comix$contacts %>% group_by(part_id) %>% slice_sample(n=50,replace = FALSE)
  
  # define age groups "age_band" (as age_sentinella) for participants
  age_sentinella_fig1 <- c(0, 5, 15, 18, 30, 65) # for crude estimates as CoMix separated age and adult waves
  comix$participants <- comix$participants %>%
    group_by(part_id) %>%
    mutate(age_band =  max(age_sentinella[part_age >= age_sentinella]),
           age_band_fig1 =  max(age_sentinella_fig1[part_age >= age_sentinella_fig1]))
  comix$contacts <- merge(comix$contacts, comix$participants[,c("part_id","panel_wave","age_band", "sday_id")],by="part_id", all.x = T)
  comix$participants <- merge(comix$participants, merge(comix$contacts %>% group_by(part_id) %>% reframe(sum_contacts = length(part_id)),comix$participants[,c("part_id")], by="part_id", all.y =T) %>% mutate(sum_contacts = replace_na(sum_contacts, 0)), all.x =T)
  
  # get mean contacts for participants
  unadj_mean_contacts_all <-  comix$participants %>% group_by(panel_wave) %>%  # all ages combined
    reframe(mean_contacts = round(mean(sum_contacts),1),
            CI_up = round(mean(sum_contacts)+qnorm(0.975)*sd(sum_contacts)/sqrt(length(part_id)),1),
            CI_low = round(mean(sum_contacts)-qnorm(0.975)*sd(sum_contacts)/sqrt(length(part_id)),1))
  
  unadj_mean_contacts_age_sentinella <- comix$participants %>% group_by(panel_wave,age_band_fig1) %>%   # by ages groups
    reframe(mean_contacts = round(mean(sum_contacts),1),
            CI_up = round(mean(sum_contacts)+qnorm(0.975)*sd(sum_contacts)/sqrt(length(part_id)),1),
            CI_low = round(mean(sum_contacts)-qnorm(0.975)*sd(sum_contacts)/sqrt(length(part_id)),1))
  unadj_mean_contacts_age_sentinella$mCI <- paste0(sprintf("%0.2f", round(unadj_mean_contacts_age_sentinella$mean_contacts,1)), " (95%-CI: ",unadj_mean_contacts_age_sentinella$CI_low,"-",unadj_mean_contacts_age_sentinella$CI_up,")")
  unadj_mean_contacts_age_sentinella$age_band_fig1 <- recode(unadj_mean_contacts_age_sentinella$age_band_fig1, "0"= "[0,5)", "5"= "[5,15)","15"="[15,18)","18"="[18,30)", "30"="[30,65)","65"="65+")
  unadj_mean_contacts_age_sentinella$age_band_fig1 <- factor(unadj_mean_contacts_age_sentinella$age_band_fig1, levels= c("[0,5)", "[5,15)","[15,18)","[18,30)","[30,65)","65+"))
  
  # creat data frame for overview of the survey waves
  panel_wave_df <- comix$participants %>% group_by(panel_wave) %>%
                                 summarise(time_panel_wave = paste0(format(as_date(min(sday_id)),format = "%d %B %Y")," to ", format(as_date(max(sday_id)),format = "%d %B %Y")),
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
  colnames(table1) <- c("Panel wave","Time period", "Number of participants", "Mean number of contacts")
  write.csv(table1,"./output/tables/age_matrices/Table1.csv")
  
  # get Figure 1
  unadj_mean_contacts_age_sentinella <- merge(unadj_mean_contacts_age_sentinella, panel_wave_df[,c("panel_wave", "mid_time", "min_time", "max_time")], by="panel_wave")
  unadj_mean_contacts_all <- merge(unadj_mean_contacts_all, panel_wave_df[,c("panel_wave", "mid_time", "min_time", "max_time")], by="panel_wave")

  fig1 <- ggplot()+
    theme_minimal()+
    geom_rect(data=unadj_mean_contacts_age_sentinella,  aes(xmin=min_time, xmax=max_time, ymin=0, ymax=max(mean_contacts)+1), color="transparent", fill="grey", alpha=0.3)+
    scale_x_date(date_breaks= "1 month", date_labels = "%d-%b")+
    geom_point(data=unadj_mean_contacts_age_sentinella, aes(y = mean_contacts,x=mid_time, color=age_band_fig1),size = 2)+
    geom_errorbar(data=unadj_mean_contacts_age_sentinella, aes(xmin=min_time,xmax=max_time, ymin = CI_low, ymax = CI_up, y = mean_contacts,x=mid_time, color=age_band_fig1))+
    geom_linerange(data=unadj_mean_contacts_age_sentinella, aes(xmin=min_time,xmax=max_time, y=mean_contacts, color=age_band_fig1),linewidth=3) +
    geom_line(data=unadj_mean_contacts_age_sentinella, aes(x=mid_time,  y=mean_contacts, color=age_band_fig1))+
    geom_text(data=unadj_mean_contacts_age_sentinella, aes(x=mid_time, y=0.1, label= panel_wave), colour= col_9[9], size=3) + 
    coord_cartesian(xlim =c(min(unadj_mean_contacts_age_sentinella$min_time),max(unadj_mean_contacts_age_sentinella$max_time)),
                    ylim =c(0,max(unadj_mean_contacts_age_sentinella$mean_contacts)+1),expand = TRUE)+
    scale_colour_manual(values=c(col_9[c(1:5,7)]), 
                        name="Age group")+
    labs(tag="",x = "", y =bquote("Crude number of contacts \nper age group"))
  ggsave(fig1, filename = paste0("./output/figures/age_matrices/Figure1.png"), height = 5, width = 15,  bg = "transparent")
  
  # generate age-contact matrices
  contact_red <- c()
  contact_red_sd <- c()
  n_sample <- 100 # number of bootstrapes (as using sampling method)
  for(p in unique(participants$panel[participants$survey_group=="adults"])){
    l <- grep(p,unique(participants$panel[participants$survey_group=="adults"])) # chosing children waves
    if(p%in%"A"){
      pc <- c(p,"C")
    }
    else if(p%in% "B"){
      pc <- c(p,"D")
    }
    else if(p%in%"F"){
      pc <- c(p,"E")
    }
   
  comix_survey <- comix # select the survey data
  comix_survey$participants <- comix_survey$participants %>% group_by %>% filter(panel %in% pc)
  levels(comix_survey$participants$part_age) <- age_sentinella
  dates <- paste0(format(as_date(min(comix_survey$participants$sday_id)),format = "%B %Y")," to ", format(as_date(max(comix_survey$participants$sday_id)),format = "%B %Y"))

 comix_matrix_sym <- replicate(n = n_sample,contact_matrix(comix_survey,
                                                           survey.pop =pop_year_i,
                                                           age.limits = age_sentinella,
                                                           symmetric = T,
                                                           estimated.contact.age = "sample",
                                                           missing.contact.age = "sample",
                                                           sample.participants = TRUE,
                                                           weigh.dayofweek = T) )
 m_r <- matrix(NA, nrow = n_sample, ncol = length(age_sentinella))
 for(i in 1:n_sample) m_r[i, ] <- rowSums(comix_matrix_sym[, i]$matrix)/prem_2021_sum$contact_sum
 
  comix_matrix_sym_sd <- apply(abind(comix_matrix_sym["matrix", ], along=3), 1:2, sd) # sd from bootstraps
  comix_matrix_sym_mean <- apply(abind(comix_matrix_sym["matrix", ], along=3), 1:2, mean) # mean from bootstraps
  comix_matrix_sym <-  comix_matrix_sym[,1]
  comix_matrix_sym$matrix <- comix_matrix_sym_mean
  comix_matrix_sym$matrix_sd <- comix_matrix_sym_sd
  comix_matrix_sym$reduction_sd <- apply(m_r, 2, sd)
  comix_matrix_sym$reduction_mean <- apply(m_r, 2, mean)
  contact_red <- rbind(contact_red,comix_matrix_sym$reduction_mean)
  suppressWarnings(rownames(contact_red)[length(contact_red[,1])] <- p)
  contact_red_sd <- rbind(contact_red_sd,comix_matrix_sym$reduction_sd)
  suppressWarnings(rownames(contact_red_sd)[length(contact_red_sd[,1])] <- p)
  comix_matrix_sym$contact_sum <- rowSums(comix_matrix_sym$matrix)
  remove(m_r)
  
  comix_matrix_sym$matrix <- melt(comix_matrix_sym$matrix, varnames = c("participant_age","contact_age"), value.name = "contacts")
  comix_matrix_sym$matrix[,1] <- rep(levels(comix_matrix_sym$matrix[,2]),length(levels(comix_matrix_sym$matrix[,2])))
  comix_matrix_sym$matrix[,1] <- factor(comix_matrix_sym$matrix[,1], levels=levels(comix_matrix_sym$matrix[,2]))
  
  comix_matrix_sym$matrix_sd <- melt(comix_matrix_sym$matrix_sd, varnames = c("participant_age","contact_age"), value.name = "contacts")
  comix_matrix_sym$matrix_sd[,1] <- rep(levels(comix_matrix_sym$matrix_sd[,2]),length(levels(comix_matrix_sym$matrix_sd[,2])))
  comix_matrix_sym$matrix_sd[,1] <- factor(comix_matrix_sym$matrix_sd[,1], levels=levels(comix_matrix_sym$matrix_sd[,2]))
  
  contact_sum <- as.data.frame(comix_matrix_sym$contact_sum)
  colnames(contact_sum)<- c("contact_sum")
  contact_sum$participant_age <- levels(comix_matrix_sym$matrix$contact_age)
  contact_sum$participant_age <- factor(contact_sum$participant_age, levels=levels(comix_matrix_sym$matrix$contact_age))
  
  contact_s <- ggplot(contact_sum, aes(x=participant_age, y = contact_sum))+
    theme_minimal()+
    ylim(c(0,20))+
    geom_bar(stat = "identity", fill="steelblue")+
    labs( y = "Number of contacts (per day)",
          x = "Age of participant (years)",
          fill = "", subtitle = dates)
  
  contact_m_sym <- ggplot(comix_matrix_sym$matrix, aes(y = contact_age, x = participant_age, fill = contacts)) + 
    theme_minimal()+
    geom_tile()+
    geom_text(aes(label = sprintf("%0.2f", round(contacts,2)))) +
    scale_fill_gradientn(limits=c(0,10),
      colors = c( "white","steelblue"),
      na.value = col_9[9])+
    labs(subtitle = " ",
         x = "Age of participant (years)",
         y = "Age of contact (years)",
          fill = "Number of contacts (per day)")
  
  
 if("A" %in% p){
   fig_panelA <- ggarrange(contact_s,contact_m_sym,ncol=2, nrow=1, common.legend = TRUE, legend="none")
   }
  else if("B" %in% p){
    fig_panelB <- ggarrange(contact_s,contact_m_sym,ncol=2, nrow=1, common.legend = TRUE, legend="none")
   }
  else if("F" %in% p){
    fig_panelF <- ggarrange(contact_s,contact_m_sym,ncol=2, nrow=1, common.legend = TRUE, legend="none")
  }
 
  }
  contact_syn <- ggplot(prem_2021_sum, aes(x=participant_age, y = contact_sum))+
    theme_minimal()+
    ylim(c(0,20))+
    geom_bar(stat = "identity", fill="steelblue")+
    labs( y = "Number of contacts (per day)",
          x = "Age of participant (years)",
          fill = "", subtitle = "Normalized to the Swiss population in 2021")
  
  contact_m_syn <- ggplot(prem_2021_df, aes(y = contact_age, x = participant_age, fill = contacts)) + 
    theme_minimal()+
    geom_tile()+
    geom_text(aes(label = sprintf("%0.2f", round(contacts,2)))) +
    scale_fill_gradientn(limits=c(0,10),
                         colors = c( "white","steelblue"),
                         na.value = col_9[9])+
    labs(subtitle = "Normalized to the Swiss population in 2021",
         x = "Age of participant (years)",
         y = "Age of contact (years)",
         fill = "Number of contacts (per day)")
  fig_syn <- ggarrange(contact_syn,contact_m_syn,ncol=2, nrow=1,common.legend = TRUE, legend="top")
  
  fig2 <- ggarrange(fig_syn, fig_panelA,fig_panelB,fig_panelF,ncol=1, nrow=4, common.legend = TRUE, legend="top")
  ggsave(fig2, filename = paste0("./output/figures/age_matrices/Figure2.png"), height = 15, width = 10,  bg = "transparent")
  
  colnames(contact_red) <- age_sentinella
  colnames(contact_red) <- recode(colnames(contact_red), "0"= "[0,5)", "5"= "[5,15)","15"="[15,30)", "30"="[30,65)","65"="65+")
  contact_red_df <-  melt(contact_red, varnames = c("panel", "age_sentinella"),value.name = "reduction") # contact reduction
  contact_red_df$reduction_sd <-  melt(contact_red_sd, varnames = c("panel", "age_sentinella"),value.name = "reduction_sd")[,3]
  contact_red_df$reduction_up <- contact_red_df$reduction + qnorm(0.975)*contact_red_df$reduction_sd/sqrt(n_sample)
  contact_red_df$reduction_low <- contact_red_df$reduction - qnorm(0.975)*contact_red_df$reduction_sd/sqrt(n_sample)
  
  fig3A <- ggplot(contact_red_df)+
    theme_minimal()+
    facet_wrap(~panel, ncol= 3)+
    geom_errorbar(aes(ymin=reduction_low,ymax=reduction_up, x=age_sentinella), color="steelblue")+
    geom_point(aes(y=reduction, x=age_sentinella), color="steelblue")+
    geom_hline(yintercept=1)+
    labs(subtitle = " ",
         y = "Relative number of contacts",
         x = " ",
         color = "Age group")
  
  # largest eigenvalue of matrices:
  sr_prem_2021 <- abs(eigen(prem_reci)$values[1])
  round(sr_prem_2021, 2)
  sr_syn_m_reci <- abs(eigen(syn_m_reci)$values[1])
  round(sr_syn_m_reci, 2)

  comix_matrix <-NULL
  contact_sum <- NULL
  leig <- as.data.frame(matrix("-",ncol=2,nrow=length(unique(comix$participants$panel_wave[comix$participants$survey_group=="adults"]))))
  colnames(larg_eig) <- c("panel_wave", "value")
  for(p in unique(comix$participants$panel_wave[comix$participants$survey_group=="adults"])){
    date<- panel_wave_df$mid_time[panel_wave_df$panel_wave==p]
    x<- min(abs(date-comix$participants$sday_id[comix$participants$survey_group=="children"]))
    pc <- unique(comix$participants$panel_wave[comix$participants$survey_group=="children" & abs(participants$sday_id-date)==x])
    
    comix_survey <- comix
    comix_survey$participants <- comix_survey$participants %>% filter(panel_wave %in% c(p,pc))
    levels(comix_survey$participants$part_age) <- age_sentinella
    dates <- paste0(format(as_date(min(comix_survey$participants$sday_id)),format = "%B %Y")," to ", format(as_date(max(comix_survey$participants$sday_id)),format = "%B %Y"))
  
    comix_matrix_sym<- replicate(n = n_sample,contact_matrix(comix_survey,
                                      survey.pop =pop_year_i,
                                      age.limits = age_sentinella,
                                      symmetric = T,
                                      missing.contact.age = "sample",
                                      estimated.contact.age = "sample",
                                      sample.participants = TRUE,
                                      weigh.dayofweek = T) )
    comix_matrix_sym_sd <- apply(abind(comix_matrix_sym["matrix", ], along=3), 1:2, sd)
    comix_matrix_sym_mean <- apply(abind(comix_matrix_sym["matrix", ], along=3), 1:2, mean)
    comix_matrix_sym <-  comix_matrix_sym[,1]
    comix_matrix_sym$matrix <- comix_matrix_sym_mean
    comix_matrix_sym$sd <- comix_matrix_sym_sd
    
    leig$panel_wave[min(grep("-",leig$value))] <- p
    leig$value[min(grep("-",leig$value))] <- abs(eigen(comix_matrix_sym$matrix)$values[1])
    
    comix_matrix_sym$contact_sum <- rowSums(comix_matrix_sym$matrix)
    comix_matrix_sym$matrix <- melt(comix_matrix_sym$matrix, varnames = c("participant_age", "contact_age"), value.name = "contacts")
    ages <- levels(comix_matrix_sym$matrix$contact_age)
    comix_matrix_sym$matrix$participant_age <- rep(ages,length(ages))
    comix_matrix_sym$matrix$participant_age <- factor(comix_matrix_sym$matrix$participant_age, levels=ages)
    
    comix_matrix1 <- comix_matrix_sym$matrix
    comix_matrix1$panels <- paste0(p," and ", pc)
    comix_matrix <- rbind(comix_matrix1, comix_matrix)
    rm(comix_matrix1)
    contact_sum1 <- as.data.frame(comix_matrix_sym$contact_sum)
    colnames(contact_sum1)<- c("contact_sum")
    contact_sum1$participant_age <- ages
    contact_sum1$participant_age <- factor(contact_sum1$participant_age, levels=ages)
    contact_sum1$panels <- paste0(p," and ", pc)
    contact_sum <- rbind(contact_sum1, contact_sum)
    rm(contact_sum1)
  }
  larg_eig$value <- as.numeric(larg_eig$value)
  
  
  # plot overview of 
  #geom_rect(data=unadj_mean_contacts_age_sentinella,  aes(xmin=min_time, xmax=max_time, ymin=0, ymax=max(mean_contacts)+1), color="transparent", fill="grey", alpha=0.3)+
  
  
  # comparisons between largest eigen values and other values:
  
  # Plot largest Eigenvalue and KOF
  larg_eig<- merge(larg_eig, panel_wave_df, by="panel_wave")
  kof_ch <- swiss_pop_data[[3]] #strigency index
  BAG_data <- swiss_pop_data[[1]]# Swiss covid data per week
  BAG_cases_canton_d <- swiss_pop_data[[11]]# Swiss covid data per day
  BAG_data <- BAG_data[BAG_data$geoRegion== "CH",]
  BAG_data$date <- as_date(BAG_data$date)
  BAG_data <- BAG_data %>% arrange(date) %>% mutate(vacdo = replace_na(vac_dos_num, 0), cum_vacdo = cumsum(vacdo),cum_vacdo_pre =cum_vacdo/unique(pop)[1]*100)
  BAG_data <- BAG_data[as_date(BAG_data$date) %in% seq(min(panel_wave_df$min_time),max(panel_wave_df$min_time),1),]
 
  ggsave(fig3A, filename = paste0("./output/figures/age_matrices/Figure3.png"), height = 4, width = 8,  bg = "transparent")
  
  
  # change makes no sense to take only the mid 
  larg_eig$date <- larg_eig$mid_time
  
  
  larg_eig <- merge(larg_eig, kof_ch, by="date")
  
  # Fit EpiNow2
  # Set parameters (for Wuhan-strain/Alpha variant)
  inc_mean <- 5.2; inc_sd <- 2.8 # https://doi.org/10.1016/S1473-3099(20)30230-9
  incubation_period <- list(mean = convert_to_logmean(inc_mean, inc_sd),
                            mean_sd = 0.1,
                            sd = convert_to_logsd(inc_mean, inc_sd),
                            sd_sd = 0.1,
                            max = 14)
  gen_mean <- 5.2; gen_sd <- 1.72 # https://doi.org/10.2807/1560-7917.ES.2020.25.17.2000257
  generation_time <- list(mean = gen_mean,
                          mean_sd = 0.1, # (6.78 - 3.78)/2/qnorm(0.975)
                          sd = gen_sd,
                          sd_sd = 0.1, # (3.93 - 0.91)/2/qnorm(0.975)
                          max = 14)
  rep_mean <- 2; rep_sd <- 2 # Assumption
  reporting_delay <- list(mean = convert_to_logmean(rep_mean, rep_sd),
                          mean_sd = 0.1,
                          sd = convert_to_logsd(rep_mean, rep_sd),
                          sd_sd = 0.1,
                          max = 14)
  #BAG_data$confirm <- BAG_data$cases_num
  #reported_cases<- BAG_data[,c("date", "confirm")]
  BAG_cases_canton_d$confirm <- BAG_cases_canton_d$cases_num
  BAG_cases_canton_d <- BAG_cases_canton_d[BAG_cases_canton_d$geoRegion== "CH",]
  reported_cases<- BAG_cases_canton_d[,c("date", "confirm")]
  reported_cases$date <- as_date(reported_cases$date)
  reported_cases <- reported_cases[as_date(reported_cases$date) %in% seq(min(panel_wave_df$min_time)-10,max(panel_wave_df$min_time),1),]
  
  # run only once (takes time)
  #fit_epinow2_all <- c()
  #for(i in 1:length(reported_cases[,1])){
  #  fit_epinow2 <- epinow(reported_cases = reported_cases[i:(30+i),],generation_time = generation_time,delays = delay_opts(incubation_period, reporting_delay),horizon = 0,verbose = FALSE)
  #  fit_epinow2_all <- rbind(fit_epinow2$estimates$summarised[fit_epinow2$estimates$summarised$variable=="R",],fit_epinow2_all)
  #}
  #write.csv(fit_epinow2_all,"./data/processed/epinow2/fit_comix_epinow2.csv")
  fit_epinow2 <- read.csv("./data/processed/epinow2/fit_comix_epinow2.csv")
  
  Re_epinow2 <- fit_epinow2[fit_epinow2$variable%in%"R",]
  Re_epinow2 <- Re_epinow2[Re_epinow2$type %in% "estimate",]
  Re_epinow2$date <- as_date(Re_epinow2$date)
  Re_epinow2 <- Re_epinow2[as_date(Re_epinow2$date) %in% seq(min(panel_wave_df$min_time),max(panel_wave_df$min_time),1),]
  
  for(i in reported_cases$date){
    reported_cases$R_median[reported_cases$date ==i] <- median(Re_epinow2$median[Re_epinow2$date ==i])
    reported_cases$R_lower_90[reported_cases$date ==i] <- median(Re_epinow2$lower_90[Re_epinow2$date ==i])
    reported_cases$R_upper_90[reported_cases$date ==i] <- median(Re_epinow2$upper_90[Re_epinow2$date ==i])
  }
  
  
  
  larg_eig$date <- as_date(parse_date_time(paste(year(larg_eig$date),week(larg_eig$date), '-Mon'), "%Y-%W-%a"))
  larg_eig <- merge(larg_eig, reported_cases, by="date", all.x = T)
  larg_eig <- merge(larg_eig, BAG_data[,c("date", "cum_vacdo_pre")], by="date", all.x = T)
  
  
  #covid cases
  seq_ch <-  swiss_pop_data[[2]]
  # level data
  variants_melt$who_variants <- factor(variants_melt$who_variants, levels = lev)
  seq_ch$who_variants <- factor(seq_ch$who_variants, levels = lev)
  
  # color cases due to sequencing (proportion of variants)
  variants_extra <- aggregate(seq_ch["num_seq_variant"], by=seq_ch[c("date", "who_variants")], sum)
  variants_extra <- merge(swiss_cov[,c("weigthed_cases", "date")], variants_extra, by="date",all=TRUE)
  variants_extra <- merge(seq_ch_date, variants_extra, by="date",y.all=TRUE)
  variants_extra$who_variants <- factor(variants_extra$who_variants, levels = lev)
  
  variants_extra$num_variant_extrapolated <- variants_extra$weigthed_cases*variants_extra$num_seq_variant/variants_extra$num_seq
  cov_ch_plot <- ggplot(data = variants_extra, aes(x = date, y = num_variant_extrapolated, color = who_variants,fill = who_variants))+ 
    geom_bar(stat='identity')+
    scale_y_continuous(limits = c(0,1e4))+
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b", limits = c(time_window_plots[1],time_window_plots[2]))+
    scale_fill_manual(values= col_variants,name="") +#SARS-CoV-2 variants
    scale_color_manual(values= col_variants,name="") +#SARS-CoV-2 variants
    theme_minimal()+
    theme(legend.position=c(.4,.9),legend.direction="horizontal")+
    labs(tag=bquote(.("")),subtitle = "", x = "", y =bquote("Number of reported \nSARS-CoV-2 cases"))

  
  fig4a<-ggplot()+
    theme_minimal()+
    geom_area(data= BAG_data,aes(x= date, y=cases_num),fill="steelblue", col="steelblue") +
    scale_x_date(date_breaks = "3 month", 
                 date_labels = "%b\n%Y",
                 limits = as_date(c(min_time,max_time)))+
    theme(axis.title=element_text(size=10))+
    labs(x = "", y = "SARS-CoV-2 cases", color = "") 
  
  
  # largest eigen value from Swiss CoMix data
  test <- as.data.frame(c(larg_eig$min_time, larg_eig$max_time))
  colnames(test) <- c("min_time")
  test <- merge(test, larg_eig[c("min_time","value")],by="min_time",all=TRUE)
  colnames(test) <- c("max_time")
  test <- merge(test, larg_eig[c("max_time","value")],by="max_time", x.all=TRUE)
  
  fig4b<- ggplot(test)+
    theme_minimal()+
    geom_line(data= test,aes(x= max_time, y=value),col="steelblue",linewidth = 2) +
    scale_x_date(date_breaks = "3 month", 
                 date_labels = "%b\n%Y",
                 limits = as_date(c(min_time,max_time)))+
    theme(axis.title=element_text(size=10))+
    labs(x = "", y = "Largest Eigen-value", color = "") 
  #Re
  fig4c<-ggplot()+
    theme_minimal()+
    geom_line(data=reported_cases, aes(x=date, y=R_median), color="steelblue", linewidth=1)+
    geom_ribbon(data= reported_cases, aes(x= date,ymin = R_lower_90, ymax =R_upper_90), fill="steelblue", alpha=0.6)+
    geom_hline(yintercept=1)+
    scale_x_date(date_breaks = "3 month", 
                 date_labels = "%b\n%Y",
                 limits = as_date(c(min_time,max_time)))+
    labs(x = "", y = bquote(italic("R"["e"])), color = "") 
  #Re and Largest Eigen-value
  fig4d<-ggplot(data= larg_eig,aes(y=value, x=R_median))+
    theme_minimal()+
    geom_smooth( method='lm', formula = y ~ x, col=col_9[2])+
    theme(axis.title=element_text(size=10))+
    geom_point(col=col_9[2], size=2)+
    labs(x = bquote(italic("R"["e"])), y = "Largest Eigen-value", color = "") 
  
  # stringency
  fig4e<-ggplot()+
    theme_minimal()+
    geom_line(data= kof_ch,aes(x= date, y=ch.kof.stringency.ch.stringency_plus), col="steelblue",linewidth = 2) +
    scale_x_date(date_breaks = "3 month", 
                 date_labels = "%b\n%Y",
                 limits = as_date(c(min_time,max_time)))+
    theme(axis.title=element_text(size=10))+
    scale_y_continuous(limits=c(0,100))+
    labs(x = "", y = "Strigency index", color = "") 
  #NPI
  #https://www.bag.admin.ch/bag/en/home/krankheiten/ausbrueche-epidemien-pandemien/aktuelle-ausbrueche-epidemien/novel-cov/massnahmen-des-bundes.html (Access: 2021-07-19)
  kof_plot <- ggplot(data= kof_ch)+
    theme_minimal()+
    geom_line(data= kof_ch,aes(x= date, y=ch.kof.stringency.ch.stringency_plus), col=col_9[9],size = 2) +
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b",
                 limits = as_date(c(time_window_plots[1],time_window_plots[2])))+
    
    annotate("pointrange", x = as_date("2020-10-19"), y = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2020-10-19")], ymin = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2020-10-19")], ymax = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2020-10-19")]+17,colour = col_9[9], size = 0.8)+
    annotate("text",hjust =0.5,vjust =0, x=as_date("2020-10-19"), y=kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2020-10-19")]+20, label= "max 15 people",size=1.8,colour = "black") + 
    
    annotate("pointrange", x = as_date("2020-10-29"), y = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2020-10-29")], ymin = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2020-10-29")], ymax = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2020-10-29")]-42,colour = col_9[9], size = 0.8)+
    annotate("text",hjust =0.5,vjust =1, x=as_date("2020-10-29"), y=kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2020-10-29")]-45, label= "max 10 people if private \notherwise 15 with mask",size=1.8,colour = "black") + 
    
    annotate("pointrange", x = as_date("2020-11-02"), y = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2020-11-02")], ymin = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2020-11-02")], ymax = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2020-11-02")]+32,colour = col_9[9], size = 0.8)+
    annotate("text",hjust =0.5,vjust =0, x=as_date("2020-11-02"), y=kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2020-11-02")]+35, label= "Online learning \nat higher educational institutes",size=1.8,colour = "black") + 
    
    annotate("pointrange", x = as_date("2020-12-12"), y = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2020-12-12")], ymin = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2020-12-12")], ymax = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2020-12-12")]+12,colour = col_9[9], size = 0.8)+
    annotate("text",hjust =0.5,vjust =0, x=as_date("2020-12-12"), y=kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2020-12-12")]+15, label= "Sperrstunde",size=1.8,colour = "black") + 
    
    annotate("pointrange", x = as_date("2020-12-21"), y = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2020-12-21")], ymin = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2020-12-21")], ymax = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2020-12-21")]-32,colour = col_9[9], size = 0.8)+
    annotate("text",hjust =0.5,vjust =1, x=as_date("2020-12-21"), y=kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2020-12-21")]-35, label= "quarantine restrictions \non countries with Alpha",size=1.8,colour = "black") + 
    
    annotate("pointrange", x = as_date("2021-01-18"), y = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-01-18")], ymin = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-01-18")], ymax = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-01-18")]+17,colour = col_9[9], size = 0.8)+
    annotate("text",hjust =0.5,vjust =0, x=as_date("2021-01-18"), y=kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-01-18")]+20, label= "mandatory home-office, \nmax 5 people, closures, etc.",size=1.8,colour = "black") + 
    
    annotate("pointrange", x = as_date("2021-01-28"), y = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-01-28")], ymin = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-01-28")], ymax = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-01-28")]-7,colour = col_9[9], size = 0.8)+
    annotate("text",hjust =0.5,vjust =1, x=as_date("2021-01-28"), y=kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-01-28")]-10, label= "free testing",size=1.8,colour = "black") + 
    
    annotate("pointrange", x = as_date("2021-03-01"), y = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-03-01")], ymin = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-03-01")], ymax = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-03-01")]-12,colour = col_9[9], size = 0.8)+
    annotate("text",hjust =0.5,vjust =1, x=as_date("2021-03-01"), y=kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-03-01")]-15, label= "max of 15 people outside, \nre-opening of all shops, \nleisure and sport activities \nbut mandatory mask wearing",size=1.8,colour = "black") + 
    
    annotate("pointrange", x = as_date("2021-04-07"), y = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-04-07")], ymin = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-04-07")], ymax = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-04-07")]+27,colour = col_9[9], size = 0.8)+
    annotate("text",hjust =0.5,vjust =0, x=as_date("2021-04-07"), y=kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-04-07")]+30, label= "free self-tests",size=1.8,colour = "black") + 
    
    annotate("pointrange", x = as_date("2021-04-19"), y = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-04-19")], ymin = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-04-19")], ymax = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-04-19")]-32,colour = col_9[9], size = 0.8)+
    annotate("text",hjust =0.5,vjust =1, x=as_date("2021-04-19"), y=kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-04-19")]-35, label= "further re-openings, \ne.g., restaurants outdoors \nbut with capacity limits",size=1.8,colour = "black") + 
    
    annotate("pointrange", x = as_date("2021-05-31"), y = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-05-31")], ymin = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-05-31")], ymax = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-05-31")]+37,colour = col_9[9], size = 0.8)+
    annotate("text",hjust =0.5,vjust =0, x=as_date("2021-05-31"), y=kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-05-31")]+40, label= "further re-openings, \ne.g., restaurants opened indoors",size=1.8,colour = "black") + 
    
    annotate("pointrange", x = as_date("2021-06-26"), y = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-06-26")], ymin = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-06-26")], ymax = kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-06-26")]-22,colour = col_9[9], size = 0.8)+
    annotate("text",hjust =0.5,vjust =1, x=as_date("2021-06-26"), y=kof_ch$ch.kof.stringency.ch.stringency_plus[kof_ch$date==as_date("2021-06-26")]-25, label= "wide re-opening, \nbut with COVID certificate",size=1.8,colour = "black") + 
    
    scale_y_continuous(limits = c(0,100))+
    labs(tag="",x = "", y =bquote("KOF Stringency-\nPlus Index"))
  
  #Strigency index and Largest Eigen-value
  fig4f <- ggplot(data= larg_eig,aes(y=value, x=ch.kof.stringency.ch.stringency_plus))+
    theme_minimal()+
    geom_smooth( method='lm', formula = y ~ x, col=col_9[2])+
    theme(axis.title=element_text(size=10))+
    geom_point(col=col_9[2], size=2)+
    labs(x = "Strigency index", y = "Largest Eigen-value", color = "") 
  cor(larg_eig$value, larg_eig$ch.kof.stringency.ch.stringency_plus, method = "pearson")
  lm_kof_eig <- lm(value ~ ch.kof.stringency.ch.stringency_plus, data=larg_eig)
  summary(lm_kof_eig)
  #1 dose coverage
  fig4g<-ggplot()+
    theme_minimal()+
    geom_area(data= BAG_data,aes(x= date, y=cum_vacdo_pre),fill="steelblue", col="steelblue") +
    scale_x_date(date_breaks = "3 month", 
                 date_labels = "%b\n%Y",
                 limits = as_date(c(min_time,max_time)))+
    scale_y_continuous(limits=c(0,100))+
    theme(axis.title=element_text(size=10))+
    labs(x = "", y = "Vaccine coverage", color = "") 
 #Vaccine and Largest Eigen-value
  fig4h<-ggplot(data= larg_eig,aes(y=value, x=cum_vacdo_pre))+
    theme_minimal()+
    geom_smooth( method='lm', formula = y ~ x, col=col_9[2])+
    theme(axis.title=element_text(size=10))+
    geom_point(col=col_9[2], size=2)+
    labs(x = "Vaccine coverage", y = "Largest Eigen-value", color = "") 
  
  fig4<- ggarrange(fig4a, fig4b,fig4c,fig4d,fig4e,fig4f,fig4g,fig4h,
                   labels = c("A", "B","C","D", "E", "F", "G", "H"),
                   ncol = 2, nrow = 4)
  
  ggsave(fig4, filename = paste0("./output/figures/age_matrices/Figure4.png"), height = 10, width = 6,  bg = "transparent")
  
  
  
  
  # supplementary Figures
  contact_s <- ggplot(contact_sum, aes(x=participant_age, y = contact_sum))+
    theme_minimal()+
    facet_wrap(~panels)+
    ylim(c(0,20))+
    geom_bar(stat = "identity", fill="steelblue")+
    labs( y = "Number of contacts (per day)",
          x = "Age of participant (years)",
          fill = "", subtitle = dates)
  
  contact_m<- ggplot(comix_matrix, aes(x = participant_age, y = contact_age, fill = contacts)) + 
    theme_minimal()+
    geom_tile()+
    facet_wrap(~panels)+
    geom_text(aes(label = sprintf("%0.2f", round(contacts,2)))) +
    scale_fill_gradientn(
      colors = c( "white","steelblue"),
      na.value = col_9[9])+
    labs(subtitle = " ",
         x = "Age of participant (years)",
         y = "Age of contact (years)",
         fill = "Number of contacts (per day)")
  
  ggsave(contact_m, filename = paste0("./output/figures/age_matrices/Figure_matices.png"), height = 15, width = 15,  bg = "transparent")
  ggsave(contact_s, filename = paste0("./output/figures/age_matrices/Figure_contacts.png"), height = 15, width = 15,  bg = "transparent")
}