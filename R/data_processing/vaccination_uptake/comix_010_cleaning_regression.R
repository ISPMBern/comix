#' ---
#' title: "Regression model to find socio-demographic factor associated with vaccination uptake and the CoMix study in Switzerland"
#' author: "Martina Reichmuth"
#' date: "'01/12/2021"
#' ---

comix_010_cleaning_regression = function(comixdata_part) {
  ##############################################
  # cleaning comix data (only panel B) for regression modeling
  ##############################################
  
  # define data set (period)
  data_reg <- comixdata_part %>% filter(grepl("B", comixdata_part$panel_wave) & date< as_date("2021-10-01"))
  data_reg$part_id<- as.character(data_reg$part_id)
  
  data_reg$vac_date <- ifelse(is.na(data_reg$vaccine_dose1), data_reg$vaccine_dose2, data_reg$vaccine_dose1)
  data_reg$vac_date <- ifelse(is.na(data_reg$vac_date), data_reg$vaccine_dose3, data_reg$vac_date)
  
  # overview of study population in B
  paste0("Number of data points in panel B: ", length((data_reg$part_id)))
  paste0("Number of paricipants in panel B: ", length(unique(data_reg$part_id)))
  paste0("Female ",length(unique(data_reg$part_id[data_reg$sex%in%"Female"])), "(", round(length(unique(data_reg$part_id[data_reg$sex%in%"Female"]))/length(unique(data_reg$part_id))*100),
         "%); Male ",length(unique(data_reg$part_id[data_reg$sex%in%"Male"])),"(",round(length(unique(data_reg$part_id[data_reg$sex%in%"Male"]))/length(unique(data_reg$part_id))*100),"%)")
  paste0("Number of pregnant (pregnant/female%) in panel B: ", table(data_reg$sex_pluspregnant[!duplicated(data_reg$part_id)])[2], " (", round(table(data_reg$sex_pluspregnant[!duplicated(data_reg$part_id)])[2]*100/table(data_reg$sex[!duplicated(data_reg$part_id)])[2]),"%)")
  paste0("Age of paricipants in panel B: ", paste(quantile(as.numeric(data_reg$age_part[!duplicated(data_reg$part_id)])),collapse = "; "))
  paste0("Number of paricipants that : ",paste0 (names(table(data_reg$drops[!duplicated(data_reg$part_id)])), " ",
                                                 table(data_reg$drops[!duplicated(data_reg$part_id)])," (",
                                                 round(table(data_reg$drops[!duplicated(data_reg$part_id)])/length(data_reg$drops[!duplicated(data_reg$part_id)])*100),
                                                 "%)",collapse="; "))
  paste0("Number missigness: ", table(data_reg$missing[!duplicated(data_reg$part_id)])[2]," (",
         round(table(data_reg$missing[!duplicated(data_reg$part_id)])[2]/sum(table(data_reg$missing[!duplicated(data_reg$part_id)]))*100),
         "%)")
  data_reg$wave<- as.numeric(gsub(".*?([0-9]+).*", "\\1",data_reg$panel_wave)) 
  data_reg$panel_wave <- factor(data_reg$panel_wave, levels=unique(data_reg$panel_wave)[order(gsub("B","",unique(data_reg$panel_wave)))])
  fut_id <- quantile(as.data.frame(data_reg %>%  group_by(part_id) %>% summarise(range_fut = max(date) -min(date)) %>% ungroup())[,2])
  paste0("Following up time: ", fut_id[3], " (", fut_id[2],"-",fut_id[4],")")
  fut_mean <- mean(as.data.frame(data_reg %>%  group_by(part_id) %>% summarise(range_fut = max(date) -min(date)) %>% ungroup())[,2])
  paste0("Following up time on average: ", fut_mean)
  
  
  panel_wave <- data_reg %>%  group_by(wave) %>% 
    summarise(min_date = as_date(min(date)), 
              max_date = as_date(max(date)),
              num_part = n()) %>% ungroup()
  panel_wave<- as.data.frame(panel_wave)
  recruitment_fun<- function(x){
    wave_id <- data_reg$part_id[data_reg$wave==x]
    pre_wave_id <- unique(data_reg$part_id[data_reg$wave<x])
    new <- sum(!wave_id %in% pre_wave_id)
    missed <- sum(!data_reg$part_id[data_reg$wave==x-1] %in% data_reg$part_id[data_reg$wave==x])
    if(x!=1){
      new_again <- sum(!data_reg$part_id[data_reg$wave==x] %in% data_reg$part_id[data_reg$wave==x-1]&data_reg$part_id[data_reg$wave==x] %in% data_reg$part_id[data_reg$wave<x-1])
    }
    else(new_again <- 0)
    return(list(new,missed,new_again))
  }
  panel_wave[,c("new","missed","new_again")] <- as.data.frame(t(sapply(panel_wave$wave, recruitment_fun)))
 
  #Visualize missingness
  missings_matrix<- table(data_reg$part_id,data_reg$panel_wave)
  missings_matrix<-as.data.frame(missings_matrix)
  supfig1_missing<- ggplot(missings_matrix, aes(Var2, Var1, fill = as.character(Freq))) +
    geom_raster(alpha=0.8) +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
    scale_fill_manual(name = "",
                      values = c( 'tomato3','steelblue'),
                      labels = c( "Missing","Present")) +
    labs(tag="",x = "Survey waves", y =" Participants")
  
  missings_matrix$row <- rownames(missings_matrix)
  fun <- function(x){
    id <- missings_matrix$Var1[missings_matrix$row == x]
    wave_current <- as.numeric(gsub("[[:alpha:]]","",missings_matrix$Var2[missings_matrix$row == x]))
    participation <- missings_matrix$Freq[missings_matrix$Var1 == id]
    
    category <- participation[wave_current]
    if(wave_current %in% 1 & participation[1] ==0){
      category <- 2
    }
    else if(wave_current %in% 2 & participation[1] ==0 & participation[2] ==0){
        category <- 2
    }
  
    return(category)
  }
  missings_matrix$category <- sapply(missings_matrix$row, fun)
  
  supfig1_missing<- ggplot(missings_matrix, aes(Var2, Var1, fill = as.character(category))) +
    geom_raster(alpha=0.8) +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
    scale_fill_manual(name = "",
                      values = c('tomato3','steelblue', 'grey'),
                      labels = c( "Missing","Present", "Not recruited")) +
    labs(tag="",x = "Survey waves", y =" Participants")
  ggsave(supfig1_missing, filename = paste0("./output/figures/vaccination_uptake/SupFig1.png"), height =3, width = 8,  bg = "transparent")
  
  # vaccination
  interval = 0.95
  interval <- qnorm(1 - (1 - interval)/2)
  vacc_part <- data_reg %>% arrange(as.numeric(gsub("[^0-9]","", data_reg$panel_wave))) %>% group_by(panel_wave) %>%
    summarise("Number participants" = length(part_id),
              "Number vaccinated" = sum(na.omit(vaccinated)),
              "Precent vaccinated" = sum(na.omit(vaccinated))/length(na.omit(vaccinated))*100,
              CI_mean = sum(na.omit(vaccinated))/length(na.omit(vaccinated)),
              mean=mean(na.omit(vaccinated)),
              sample.se = sd(na.omit(vaccinated))/sqrt(length(na.omit(vaccinated))),
              CI_low = CI_mean - interval*sample.se,
              CI_up = CI_mean + interval*sample.se,
              min_date = as_date(min(date)), 
              max_date = as_date(max(date)),
              mid_date = as_date(median(as.numeric(min_date:max_date))))%>% ungroup
  vacc_part$"Number vaccinated (precent)" <- paste0(vacc_part$`Number vaccinated`," (", round(vacc_part$`Precent vaccinated`,1),"%)")
  colnames(vacc_part)[1] <- "Panel wave"
  data_reg$lower.age.limit <- as.factor(data_reg$lower.age.limit)
  data_reg$age_bands <- recode(data_reg$lower.age.limit, 
                               "0"= "0-4", "5"="5-11", "12"="12-17", "18"="18-29", "30"="30-39", "40"="40-49","50"="50-59", "60"="60-69", "70"="70+")
  #                              "0"= "[0,5)", "5"="[5,11)", "12"="[12,18)", "18"="[18,30)", "30"="[30,40)", "40"="[40,50)","50"="[50,60)", "60"="[60,70)", "70"="70+")
  vacc_part_age <- data_reg %>% arrange(as.numeric(gsub("[^0-9]","", data_reg$panel_wave))) %>% group_by(panel_wave,age_bands) %>%
    summarise("Number participants" = length(part_id),
              "Number vaccinated" = sum(na.omit(vaccinated)),
              "Precent vaccinated" = sum(na.omit(vaccinated))/length(na.omit(vaccinated))*100,
              CI_mean = sum(na.omit(vaccinated))/length(na.omit(vaccinated)),
              mean=mean(na.omit(vaccinated)),
              sample.se = sd(na.omit(vaccinated))/sqrt(length(na.omit(vaccinated))),
              CI_low = CI_mean - interval*sample.se,
              CI_up = CI_mean + interval*sample.se,
              min_date = as_date(min(date)), 
              max_date = as_date(max(date)),
              mid_date = as_date(median(as.numeric(min_date:max_date))))%>% ungroup
  vacc_part_age$"Number vaccinated (precent)" <- paste0(vacc_part_age$`Number vaccinated`," (", round(vacc_part_age$`Precent vaccinated`,1),"%)")
 
  
  
  table_vaccination <- data_reg %>% group_by(lower.age.limit) %>%
    summarise(table_vaccination = sum(na.omit(vaccinated))) %>% 
    ungroup 
  #table_vaccination$part_age_group <- c("[18,30)", "[30,40)", "[40,50)","[50,60)", "[60,70)", "70+")
  table_vaccination$part_age_group <- c("18-29", "30-39", "40-49","50-59", "60-69", "70+")
  colnames(table_vaccination) <- c("Age group", "Number of vaccinated")
  
  bag_weekly <- swiss_pop_data[[1]]#swiss_pop_data[[2]]
  pop_size <- swiss_pop_data[[4]]
  pop_size <- pop_size[pop_size$year=="2021",]
  
  bag_weekly <- bag_weekly[bag_weekly$geoRegion=="CH",]
  bag_weekly <- bag_weekly[!is.na(bag_weekly$fulvacc_num),]
  
  bag_weekly <- subset(bag_weekly, week %in% seq(202052,max(bag_weekly$week),1)) #first vaccination on 23 Dec 2020; vaccination program started on 4 Jan 2021
  bag_weekly <- within(bag_weekly, cum_fulvacc_num <- cumsum(fulvacc_num))
  bag_weekly$cum_fulvacc_prec_total <- as.numeric(bag_weekly$cum_fulvacc_num/as.numeric(sum(pop_size$population))*100)# percentage fully vaccinated in Swiss pop
  bag_weekly$cum_fulvacc_prec_older17 <- as.numeric(bag_weekly$cum_fulvacc_num/as.numeric( sum(pop_size$population[pop_size$lower.age.limit>17]))*100)# percentage fully vaccinated in Swiss pop
  
  bag_weekly <- within(bag_weekly, cum_vac_dos_num <- cumsum(vac_dos_num))
  bag_weekly$cum_vac_dos_prec_total <- as.numeric(bag_weekly$cum_vac_dos_num/as.numeric(sum(pop_size$population))*100)# percentage vaccination doses in Swiss pop
  bag_weekly$cum_vac_dos_prec_older17 <- as.numeric(bag_weekly$cum_vac_dos_num/as.numeric( sum(pop_size$population[pop_size$lower.age.limit>17]))*100)# percentage fully vaccinated in Swiss pop
  
  bag_weekly$year_week <- suppressWarnings(as.Date(paste(gsub('.{2}$', '', bag_weekly$week), gsub('^.{4}', '', bag_weekly$week), 1, sep="-"), "%Y-%U-%u"))
  bag_weekly$year_week[2] <- bag_weekly$year_week[1]
  date_limits <- as_date(c(panel_wave[1,2],panel_wave[6,3]))
  bag_weekly <- bag_weekly[bag_weekly$year_week %in% c(as_date("2021-01-01"):(date_limits[2]+20)),]
  
  panel_date <- data_reg %>%  group_by(panel_wave) %>% 
    summarise(min_date = as_date(min(date)), 
              max_date = as_date(max(date)),
              mid_date = as_date(median(as.numeric(min_date:max_date)))) %>% ungroup()
  
  paste0("Full vaccination in Swiss population: ",bag_weekly$cum_fulvacc_num[month(bag_weekly$year_week)==6][1], " (", round(bag_weekly$cum_fulvacc_prec_total[month(bag_weekly$year_week)==6][1],1),"%)")
  paste0("Full vaccination in Swiss population in adults: ", round(bag_weekly$cum_fulvacc_prec_older17[month(bag_weekly$year_week)==6][1],1),"%")
  paste0("Full vaccination in Swiss population: ",bag_weekly$cum_fulvacc_num[month(bag_weekly$year_week)==9][sum(month(bag_weekly$year_week)==9)], " (", round(bag_weekly$cum_fulvacc_prec_total[month(bag_weekly$year_week)==9][sum(month(bag_weekly$year_week)==9)],1),"%)")
  paste0("Full vaccination in Swiss population in adults: ", round(bag_weekly$cum_fulvacc_prec_older17[month(bag_weekly$year_week)==9][sum(month(bag_weekly$year_week)==9)],1),"%")
  
  paste0("1. dose vaccination in Swiss population: ",bag_weekly$cum_vac_dos_num[month(bag_weekly$year_week)==6][1], " (", round(bag_weekly$cum_vac_dos_prec_total[month(bag_weekly$year_week)==6][1],1),"%)")
  paste0("1. dose vaccination in Swiss population in adults: ", round(bag_weekly$cum_vac_dos_prec_older17[month(bag_weekly$year_week)==6][1],1),"%")
  paste0("1. dose vaccination in Swiss population: ",bag_weekly$cum_vac_dos_num[month(bag_weekly$year_week)==9][sum(month(bag_weekly$year_week)==9)], " (", round(bag_weekly$cum_vac_dos_prec_total[month(bag_weekly$year_week)==9][sum(month(bag_weekly$year_week)==9)],1),"%)")
  paste0("1. dose vaccination in Swiss population in adults: ", round(bag_weekly$cum_vac_dos_prec_older17[month(bag_weekly$year_week)==9][sum(month(bag_weekly$year_week)==9)],1),"%")
  
  paste0("Vaccination in CoMix: ",vacc_part$`Number vaccinated`[vacc_part$`Panel wave`=="B1"], " (", round(vacc_part$`Precent vaccinated`[vacc_part$`Panel wave`=="B1"],1),"%)")
  paste0("Vaccination in CoMix: ",vacc_part$`Number vaccinated`[vacc_part$`Panel wave`=="B6"], " (", round(vacc_part$`Precent vaccinated`[vacc_part$`Panel wave`=="B6"],1),"%)")
  
  lables_fig1 <- c("First dose in CoMix study (proportion of \u226518 years)","Fully vaccinated in Switzerland \n(percentage of total population)","Fully vaccinated in Switzerland \n(percentage of \u226518 years)","First dose in Switzerland \n(percentage of total population)","First dose in Switzerland \n(percentage of \u226518 years)")
  lables_fig1 <- factor(lables_fig1,levels=lables_fig1)
  lables_fig1_shape = (c("solid", "solid", "dashed","dotdash","dotted"))
  lables_fig1_color = col_9[c(1:5)]
  figure1_vac <- ggplot()+
    theme_minimal()+
    geom_point(data=vacc_part, aes(y = CI_mean*100,x=mid_date, color=lables_fig1[1]),size = 2)+
    geom_errorbar(data=vacc_part, aes(xmin=min_date, ymin = CI_low*100, ymax = CI_up*100, y = CI_mean*100,x=mid_date, color=lables_fig1[1], linetype=lables_fig1[1],width=(as.numeric(max_date-min_date))),linewidth = 1)+
    #geom_point(data=vacc_part_age, aes(y = CI_mean*100,x=mid_date),size = 2)+
    #geom_errorbar(data=vacc_part_age, aes(xmin=min_date, ymin = CI_low*100, ymax = CI_up*100, y = CI_mean*100,x=mid_date,width=(as.numeric(max_date-min_date))),linewidth = 1)+
    geom_rect(data=panel_date,  aes(xmin=min_date, xmax=max_date, ymin=0, ymax=100), color="transparent", fill="#999999", alpha=0.3)+
    geom_line(data= bag_weekly,aes(x=as_date(year_week), y=cum_fulvacc_prec_total, color=lables_fig1[2],linetype=lables_fig1[2]),linewidth = 1)+
    geom_line(data= bag_weekly,aes(x=as_date(year_week), y=cum_fulvacc_prec_older17, color=lables_fig1[3],linetype=lables_fig1[3]),linewidth = 1)+
    geom_line(data= bag_weekly,aes(x=as_date(year_week), y=cum_vac_dos_prec_total, color=lables_fig1[4],linetype=lables_fig1[4]),linewidth = 1)+
    geom_line(data= bag_weekly,aes(x=as_date(year_week), y=cum_vac_dos_prec_older17, color=lables_fig1[5],linetype=lables_fig1[5]), linewidth = 1)+
    annotate("text", vjust = 0, x=panel_date$mid_date, y=0, label= panel_date$panel_wave, colour= "#999999", size=3) + 
    scale_color_manual(name=" ",values = lables_fig1_color)+
    scale_linetype_manual(name=" ",values = lables_fig1_shape)+
    theme(legend.position = 'top')+ 
    scale_y_continuous(labels = function(x) paste0(x, "%"))+
    guides(color = guide_legend(nrow=2,byrow=TRUE,override.aes = list( linetype = lables_fig1_shape)))+
    labs(tag="",x = "", y =bquote("Vaccination uptake (%)"))
  
  sf3_vac <- ggplot(data = vacc_part_age) +
    theme_minimal()+
    geom_point(aes(x=panel_wave,y=CI_mean*100, color=age_bands), position=position_dodge(width=0.8), size=2) +
    geom_errorbar(aes(x = panel_wave, y = CI_mean*100, ymin=CI_low*100, ymax=CI_up*100, color=age_bands),position=position_dodge(width=0.8))+
    theme(legend.position = 'bottom')+ 
    scale_color_manual(name=" ",values = c(col_9[c(3:5,7,8)], "aquamarine3"))+
    scale_y_continuous(limits=c(0,100), labels = function(x) paste0(x, "%"))+
    labs(tag="",subtitle = bquote(), x = "", y ="Overall vaccination uptake \n(data)")
  
  

  paste0("Vaccination attitude in CoMix (B1): ",paste0(names(table(data_reg$vaccine_want_cat_plus[data_reg$panel_wave=="B1"])), ": ",table(data_reg$vaccine_want_cat_plus[data_reg$panel_wave=="B1"]), " (",round(table(data_reg$vaccine_want_cat_plus[data_reg$panel_wave=="B1"])/sum(table(data_reg$vaccine_want_cat_plus[data_reg$panel_wave=="B1"]))*100,1),"%)",collapse = "; "))
  paste0("Vaccination attitude in CoMix (B6): ",paste0(names(table(data_reg$vaccine_want_cat_plus[data_reg$panel_wave=="B6"])), ": ",table(data_reg$vaccine_want_cat_plus[data_reg$panel_wave=="B6"]), " (",round(table(data_reg$vaccine_want_cat_plus[data_reg$panel_wave=="B6"])/sum(table(data_reg$vaccine_want_cat_plus[data_reg$panel_wave=="B6"]))*100,1),"%)",collapse = "; "))
  
  data_part <- data.frame(reshape2::melt(data_reg[,c("panel_wave","vaccine_want_cat_plus")], id.vars="panel_wave"))
  data_part <- data_part[!is.na(data_part$value)&data_part$value>0,]
  data_part <- data.frame(table(data_part$panel_wave, data_part$value))
  data_part$Var2 <- ifelse(data_part$Var2 %in% "Prefer not to say","Preferred not to answer",as.character(data_part$Var2))
  data_part$Var2 <- factor(data_part$Var2, levels = c("Vaccinated","Yes","Don't know","No","Preferred not to answer"))
  num_colors <- nlevels(data_part$Var2)
  label_colors <- rev(col_9[1:num_colors])
  data_part$Var2 <- recode_factor(data_part$Var2, Vaccinated="Already vaccinated", Yes="Intention to be vaccinated","Don't know"="Hesitate to be vaccinated",No="No intention to be vaccinated", "Preferred not to answer"="Preferred not to answer")
  label_colors <-setNames(label_colors, levels(data_part$Var2))# Name your color vector
  
  figure2_vac<-ggplot()+
    theme_minimal()+
    geom_bar(data=data_part, aes(y=Freq, x=Var1, fill= Var2), position="fill", stat="identity")+
    scale_y_continuous(labels = scales::percent_format())+
    scale_fill_manual(values = label_colors, name="") +
    guides(fill=guide_legend(ncol=2,byrow=TRUE,shape = guide_legend(override.aes =  list(shape=22))))+
    theme(legend.position = 'left')+
    labs(tag="",x = "Survey waves", y ="Proportion (%)", subtitle = "")
 figure1 <- suppressWarnings(ggarrange(figure1_vac, figure2_vac, ncol = 1, nrow = 2,labels = c("A","B")))
  ggsave(figure1, filename = paste0("./output/figures/vaccination_uptake/Figure1.png"), height =8, width = 8,  bg = "transparent")
  
  

   # prepare variables of interest:
#quantile(na.omit(data_reg$no_contacts), probs = c(0.33,0.66))
fun <- function(x){
  if(is.na(x)){return(NA)}
  else if(x<3){return("0-2")}
  else if(x>=3 &x<6){return("3-5")}
  else if(x>=6){return("6+")}
}
data_reg$contact_cat <- sapply(data_reg$no_contacts, fun)
#data_reg$contact_cat <- factor(data_reg$contact_cat, order = FALSE, levels = c("[0,3)","[3,6)","6+"))
data_reg$contact_cat <- factor(data_reg$contact_cat, order = FALSE, levels = c("0-2","3-5","6+"))
data_reg$agreement_measures_3cat <- gsub("[1-9] ","", data_reg$agreement_measures_3cat)
data_reg$agreement_measures_3cat <- factor(data_reg$agreement_measures_3cat, levels=gsub("[1-9] ","", levels(comixdata_part$agreement_measures_3cat)))
data_reg$grossregion <- factor(data_reg$grossregion, order = FALSE, levels = c(names(table(data_reg$grossregion))[rev(order(table(data_reg$grossregion)))]))
data_reg$panel_wave <- factor(data_reg$panel_wave, levels=unique(data_reg$panel_wave)[order(gsub("B","",unique(data_reg$panel_wave)))])

Fun_prehistory <- function(x){
  panel_wave_id <- data_reg$pos[data_reg$panel_wave_id ==x]
  wave <- data_reg$wave[data_reg$panel_wave_id ==x]
  id <- data_reg$part_id[data_reg$panel_wave_id ==x]
  waves <- data_reg$wave[data_reg$part_id ==id]
  waves <- waves[waves <= wave]
  tests <- data_reg$pos[data_reg$wave %in% waves & data_reg$part_id %in% id]
  if(sum(tests[length(tests)] %in% "Not tested")>0 & sum(tests %in% "Tested")>0){return("Tested")}
  if(sum(tests[length(tests)] %in% "Not tested")>0 & sum(tests %in% "Negative")>0){return("Negative")}
  if(sum(tests %in% "Positive")>0){return("Positive")}
  else return(as.character(tests[length(tests)]))
  
}
data_reg$prehistory <- sapply(data_reg$panel_wave_id, Fun_prehistory)
#data_reg$prehistory <- data_reg$pos 
data_reg$prehistory <- ifelse(data_reg$prehistory=="Prefer not to answer","Preferred not to answer",as.character(data_reg$prehistory))
data_reg$prehistory <- recode(data_reg$prehistory, Positive = "Tested positive", Tested = "Tested", 
                              Negative = "Tested", `Not tested`="Never tested", `Preferred not to answer`="Preferred not to answer")
data_reg$prehistory <- factor(data_reg$prehistory, levels=c("Tested positive","Tested","Never tested","Preferred not to answer"))

data_reg$date_num <-as.numeric(data_reg$date)
data_reg <-  data_reg[(order(data_reg$date_num)),]
Fun_risk <- function(x){
  if(is.na(x)){return(NA)}
  else if(x>0){return("One or more person in a risk group")}
  else if(x==0){return("No person in a risk group")}
}
data_reg$household_riskgroup <- sapply(data_reg$household_riskgroup, Fun_risk)
data_reg$household_riskgroup <- factor(data_reg$household_riskgroup, levels = c("No person in a risk group", "One or more person in a risk group"))

#  time numerical when vaccination was possible:
data_reg$date_num <- data_reg$date_num - as.numeric(as_date("2021-01-01"))
#  Add time of event (Vaccination)
vac <- data_reg %>% group_by(part_id) %>% 
  filter(vaccinated == 1) %>% 
  summarise(timetoevent=min(date_num))
unvac <- data_reg %>% group_by(part_id) %>% 
  filter(vaccinated == 0) %>% 
  summarise(timetoevent=NA) 
vac$part_id<- as.character(vac$part_id)
vac<- vac[!duplicated(vac$part_id),]

data_reg$grossregion <- sapply(data_reg$canton, from_cantons_to_great_regions)
data_reg$grossregion <- factor(data_reg$grossregion, order = FALSE, levels = c(names(table(data_reg$grossregion))[rev(order(table(data_reg$grossregion)))]))



## Comparison of study population with CH population
## age (comix) classes comparing Swiss population with CoMix data:
age_table <- data.frame(matrix(ncol=4,nrow=length(unique(data_reg$age_bands))))
age_table[,2] <-  unique(na.omit(data_reg$lower.age.limit))[order(unique(na.omit(data_reg$lower.age.limit)))]
colnames(age_table)<- c(c("Name","Age group","pop_num","part_num"))
for (n in 1:length(age_table[,2])) {
  i <- as.numeric(as.character(age_table[n,2]))
  if(i<70 & !is.na(i)){
    j <- as.numeric(as.character(age_table[(n+1),2]))
  }
  else{
    j <- 120
  }
  if(n <= length(na.omit(age_table[,2]))){
    age_table[!is.na(age_table$`Age group`) & age_table$`Age group`==i,3] <- sum(swiss_pop_data[[4]][as.numeric(swiss_pop_data[[4]][,1])>=i & as.numeric(swiss_pop_data[[4]][,1]) < j,3])
  }
}

age_table <- as.data.frame(age_table)
age_table[c(2,4)] <- data_reg %>% #[!duplicated(data_reg$part_id),]
  group_by(lower.age.limit) %>% 
  summarise("part_num" = length(lower.age.limit))%>%
  ungroup()
for (w in unique(data_reg$panel_wave)) {
  x <- data_reg[data_reg$panel_wave==w,]
  x<- x %>%
    group_by(lower.age.limit) %>% 
    summarise("part_num" = length(lower.age.limit)
    )%>%
    ungroup()
  x$precent <- round(x$part_num/sum(x$part_num)*100,1)
  if(sum(!is.na(x$lower.age.limit))>0){
    for (a in x$lower.age.limit) {
      age_table[!is.na(age_table$`Age group`) & age_table$`Age group`%in%a,w] <- paste0(x[x$lower.age.limit %in% a,c(2)]," (",x[x$lower.age.limit %in% a,c(3)],"%)")
    }
  }
  if(sum(is.na(x$lower.age.limit))>0){
    for (a in x$lower.age.limit) {
      age_table[ age_table$`Age group` %in%a,w] <- paste0(format(x[x$lower.age.limit %in% a,c(2)], nsmall=0, big.mark=",")," (",x[x$lower.age.limit %in% a,c(3)],"%)")
    }
  }
}
age_table$pop_num <- paste0(format(age_table$pop_num,nsmall=0, big.mark=","), " (", round(as.numeric(age_table$pop_num)/sum(as.numeric(age_table$pop_num))*100,1), "%)")
age_table[,4] <- paste0(format(age_table[,4],nsmall=0, big.mark=",")," (", round(age_table[,4]/sum(age_table[,4])*100,1),"%)")
age_table[is.na(age_table)] <- " "
age_table[,2] <- levels(data_reg$age_bands)
age_table[1,1] <- "Age group"
colnames(age_table) <- c("Category","Name", "Swiss population, n (%)","Study participants, n (%)",   levels(data_reg$panel_wave))

# gender, 
## gender comparing Swiss population with CoMix data:
gender_table <- data.frame(matrix(ncol=4,nrow=length(unique(data_reg$sex))))
gender_table[,2] <-  unique(data_reg$sex)
colnames(gender_table)<- c(c("Name","Gender","pop_num","part_num"))
swiss_pop_data[[5]]$gender <- factor(swiss_pop_data[[5]]$gender,level=levels(data_reg$sex))
gender_table[,3] <- swiss_pop_data[[5]][c(1,2,3),3]
gender_table[,4] <- as.data.frame(data_reg %>% group_by(sex) %>% 
                                    summarise(n = length(sex)) %>% 
                                    `colnames<-`(c("Gender", "part_num")))[,2]
for (w in unique(data_reg$panel_wave)) {
  x <- data_reg[data_reg$panel_wave==w,]
  x<- x %>%
    group_by(sex) %>% 
    summarise(part_num = length(sex)
    )%>%
    ungroup()
  x$precent <- round(x$part_num/sum(x$part_num)*100,1)
  if(!is.na(x[3,3])){
  }
  for (r in x$sex) {
    gender_table[gender_table$Gender==r,w] <- paste0(format(as.numeric(x[x$sex == r,c(2)]), nsmall=0, big.mark=",")," (",x[x$sex == r,c(3)],"%)")
  }
}
gender_table[c(1,2),3] <- paste0(format(gender_table[c(1,2),3],nsmall=0, big.mark=",")," (", round(gender_table[c(1,2),3]/sum(gender_table[c(1,2),3])*100,1),"%)")
gender_table[,4] <- paste0(format(gender_table[,4],nsmall=0, big.mark=",")," (", round(gender_table[,4]/sum(gender_table[,4])*100,1),"%)")
gender_table[is.na(gender_table)] <- " "
gender_table[1,1] <- "Gender"
colnames(gender_table) <- c("Category","Name", "Swiss population, n (%)","Study participants, n (%)", levels(data_reg$panel_wave))


## income:
income_table <- data.frame(matrix(ncol=4,nrow=length(levels(data_reg$household_income_3cat))))
income_table[,2] <-  levels(unique(data_reg$household_income_3cat))
colnames(income_table)<- c(c("Name","Household income","pop_num","part_num"))
income_table[,4] <- as.data.frame(data_reg %>% group_by(household_income_3cat) %>% 
                                    summarise(n = length(household_income_3cat)) %>% 
                                    `colnames<-`(c("Household income", "part_num")))[,2]
y <- as.numeric(unique(data_reg$household_income_min_3cat)[-4])
y <- y[order(y)]
fun <- function(x) {
  for(i in length(y):1){
    if(x>=y[i]){return(y[i])}
  }
}
swiss_pop_data[[7]][,11]<- sapply(swiss_pop_data[[7]][,10], fun)
income_table[1:length(y),3] <- as.data.frame(swiss_pop_data[[7]] %>% group_by(V11) %>% 
                                               summarise(n = sum(as.numeric(total_precent))) %>% 
                                               `colnames<-`(c("Household income", "pop_num")))[,2]

for (w in unique(data_reg$panel_wave)) {
  x <- data_reg[data_reg$panel_wave==w,]
  x$household_income_3cat <- as.character(x$household_income_3cat)
  x<- as.data.frame(x %>%
                      group_by(household_income_3cat) %>% 
                      summarise(part_num = length(household_income_3cat))%>%ungroup())
  x$precent <- round(x$part_num/sum(x$part_num)*100,1)
  for (r in x$household_income_3cat) {
    income_table[income_table$`Household income` ==r,w] <- paste0(format(as.numeric(x[x$household_income_3cat == r,c(2)]), nsmall=0, big.mark=",")," (",x[x$household_income_3cat == r,c(3)],"%)")
  }
}
income_table[1:length(y),3] <- paste0(income_table[1:length(y),3], "%")
income_table[,4] <- paste0(format(income_table[,4],nsmall=0, big.mark=",")," (", round(income_table[,4]/sum(income_table[,4])*100,1),"%)")

income_table[is.na(income_table)] <- " "
income_table[1,1] <- "Household income"
colnames(income_table) <- c("Category","Name", "Swiss population, n (%)","Study participants, n (%)", levels(data_reg$panel_wave))

# region of residence
## residence comparing Swiss population with CoMix data:
swiss_pop_data[[6]]$grossregion  <- sapply(swiss_pop_data[[6]]$residence, from_cantons_to_great_regions)
data_reg$grossregion  <- sapply(data_reg$canton, from_cantons_to_great_regions)
data_reg$grossregion <- factor(data_reg$grossregion, order = FALSE, levels = c(names(table(data_reg$grossregion))[rev(order(table(data_reg$grossregion)))]))
swiss_pop_data[[6]]$grossregion <- factor(swiss_pop_data[[6]]$grossregion,level=levels(unique(data_reg$grossregion)))
residence_table <- data.frame(matrix(ncol=4,nrow=length(unique(data_reg$grossregion))))
residence_table[,2] <-  levels(unique(data_reg$grossregion))
colnames(residence_table)<- c(c("Name","Residence","pop_num","part_num"))


residence_table[,c(2,3)] <- as.data.frame(swiss_pop_data[[6]][,c(5,3)] %>% group_by(grossregion) %>% 
                                            summarise(n = sum(population)) %>% 
                                            `colnames<-`(c("Residence", "pop_num")))
residence_table[c(2,4)] <- as.data.frame(data_reg %>% group_by(grossregion) %>% 
                                           summarise(n = length(grossregion)) %>% 
                                           `colnames<-`(c("Residence", "part_num")))
for (w in unique(data_reg$panel_wave)) {
  x <- data_reg[data_reg$panel_wave==w,]
  x<- x %>%
    group_by(grossregion) %>% 
    summarise(part_num = length(grossregion)
    )%>%
    ungroup()
  x$precent <- round(x$part_num/sum(x$part_num)*100,1)
  for (r in x$grossregion) {
    residence_table[residence_table$Residence==r,w] <- paste0(format(as.numeric(x[x$grossregion == r,2]), nsmall=0, big.mark=",")," (",x[x$grossregion == r,3],"%)")
  }
}
residence_table[,3] <- paste0(format(residence_table[,3], nsmall=0, big.mark=",")," (", round(residence_table[,3]/sum(residence_table[,3])*100,1),"%)")
residence_table[,4] <- paste0(format(residence_table[,4], nsmall=0, big.mark=",")," (", round(residence_table[,4]/sum(residence_table[,4])*100,1),"%)")
residence_table[is.na(residence_table)] <- " "
residence_table[1,1] <- "Residence"
colnames(residence_table) <- c("Category","Name", "Swiss population, n (%)","Study participants, n (%)", levels(data_reg$panel_wave))

table2_pop_part <-rbind(gender_table, age_table, income_table, residence_table)
write.csv(table2_pop_part, "./output/tables/vaccination_uptake/SupTable2.csv")


# variables in the model
model_variables <- c("part_id","panel_wave","wave", "date_num","age_bands","age_part","sex", 
                     "region","grossregion", "country_cat_birth","education_level3","employment_cat", 
                     "household_income_3cat","household_size","household_riskgroup",
                     "prehistory","contact_cat","no_contacts","agreement_measures_3cat", 
                     "vaccinated", "missing", "drops", "vac_date") 
data_reg <- data_reg[, colnames(data_reg) %in% model_variables]
data_reg <- data_reg[ , model_variables]


comixdata_ipw<- merge(vac, data_reg, by="part_id", all.y = TRUE)
comixdata_ipw <- comixdata_ipw %>% group_by(part_id) %>% filter(date_num<=timetoevent | is.na(timetoevent)) # new
comixdata_ipw <-  comixdata_ipw[(order(comixdata_ipw$date_num)),]

# inverse probability weighting (IPW) denominator
#comixdata_ipw <- as.data.frame(na.omit(data_reg[,c("missing","part_id","age_bands","sex","panel_wave")]))
#mod_missing <- glm(missing ~ age_bands+sex+panel_wave,family=binomial(link="logit"), data=comixdata_ipw)


comixdata_ipw <- as.data.frame(na.omit(comixdata_ipw[,c("missing","part_id","age_bands","sex","panel_wave",
                                                   "region", "grossregion", "employment_cat",
                                                     "household_income_3cat", "education_level3", "country_cat_birth", "prehistory",
                                                     "household_size", "household_riskgroup", "contact_cat", "agreement_measures_3cat")]))

mod_missing <- glm(missing ~ age_bands*panel_wave+sex+region+grossregion+employment_cat+
  household_income_3cat+education_level3+country_cat_birth+prehistory+
  household_size+household_riskgroup+contact_cat+agreement_measures_3cat,
family=binomial(link="logit"), data=comixdata_ipw)

comixdata_ipw$ipw <- NA
# Probability of missigness
comixdata_ipw$ipw <- predict(mod_missing, data=comixdata_ipw, type="response")
# Probability of non-missigness
comixdata_ipw$ipw[comixdata_ipw$missing==0] <- 1-predict(mod_missing, data=comixdata_ipw, type="response")[comixdata_ipw$missing==0]
comixdata_ipw <- comixdata_ipw %>% group_by(part_id) %>% mutate(ipw = cumsum(ipw))

# Stabilized weights
mod0 <- glm(missing ~ 1, family=binomial(link="logit"), data=comixdata_ipw)
comixdata_ipw$ipw0 <- predict(mod0, data=comixdata_ipw, type="response")
comixdata_ipw$ipw0[comixdata_ipw$missing==0] <- 1-predict(mod0, data=comixdata_ipw, type="response")[comixdata_ipw$missing==0]

comixdata_ipw$ipw <- comixdata_ipw$ipw0/comixdata_ipw$ipw
quantile(comixdata_ipw$ipw )
data_reg$part_id<- as.character(data_reg$part_id)
comixdata_ipw$part_id<- as.character(comixdata_ipw$part_id)
data_reg <- merge(data_reg, comixdata_ipw[,c("ipw0", "ipw","part_id","panel_wave")], by =c("part_id","panel_wave"), all.x = T)
apply(data_reg,2,function(x) sum(is.na(x))) # Any missing variables?


paste0("Number of participants: ", length(unique(data_reg$part_id)))
data_reg1 <- na.omit(data_reg[,!grepl("vac_date|ipw",colnames(data_reg))])
data_reg <- merge(data_reg1, data_reg[,c("part_id","panel_wave","vac_date","ipw")],by =c("part_id","panel_wave"))
paste0("Number of participants: ", length(unique(data_reg$part_id)))

data_reg <-  data_reg[(order(data_reg$date_num)),]
data_reg$household_size <- as.numeric(data_reg$household_size)
data_reg$age_part <- as.numeric(data_reg$age_part)

data_reg<- merge(vac, data_reg, by="part_id", all.y = TRUE)
remove(vac)

## Overview of study population in regression model
paste0("Number of data points in panel B: ", length((data_reg$part_id)))
paste0("Number of paricipants in panel B: ", length(unique(data_reg$part_id)))
paste0("Female ",length(unique(data_reg$part_id[data_reg$sex%in%"Female"])), "(", round(length(unique(data_reg$part_id[data_reg$sex%in%"Female"]))/length(unique(data_reg$part_id))*100),
       "%); Male ",length(unique(data_reg$part_id[data_reg$sex%in%"Male"])),"(",round(length(unique(data_reg$part_id[data_reg$sex%in%"Male"]))/length(unique(data_reg$part_id))*100),"%)")
paste0("Age (quantilies) of paricipants in panel B: ", paste(quantile(as.numeric(data_reg$age_part[!duplicated(data_reg$part_id)])),collapse = "; "))
paste0("Number of paricipants that : ",paste0 (names(table(data_reg$drops[!duplicated(data_reg$part_id)])), " ",
                                               table(data_reg$drops[!duplicated(data_reg$part_id)])," (",
                                               round(table(data_reg$drops[!duplicated(data_reg$part_id)])/length(data_reg$drops[!duplicated(data_reg$part_id)])*100),
                                               "%)",collapse="; "))

panel_wave_reg <- data_reg %>%  group_by(wave) %>% 
  summarise(num_part = n()) %>% ungroup()
panel_wave_reg<- as.data.frame(panel_wave_reg)
panel_wave <- cbind(panel_wave,panel_wave_reg[,2])

panel_wave$num_part <- format(panel_wave$num_part, nsmall=0, big.mark=",")

colnames(panel_wave) <- c("Survey wave", "Start date, year-month-day", "End date, year-month-day", "Number of participants", "Number of newly enrolled participants","Number of missing participants \nwho had been previously enrolled","Number of returning participants \nafter missing at least one wave", "Number of participants \nwith no missing variables")
panel_wave = data.frame(lapply(panel_wave, as.character), stringsAsFactors=FALSE)
write.csv(panel_wave, "./output/tables/vaccination_uptake/Table1.csv")


data_reg1 <- data_reg
# describe study population
data_reg <- data_reg[order(data_reg$date_num, decreasing = TRUE),]
data_reg <- data_reg[!duplicated(data_reg$part_id),]

table_study_pop <- data.frame(matrix(NA,  ncol = 3))
colnames(table_study_pop) <- c("Category", "All participants, N (%)", "Vaccinated participants, N (%)")
table_study_pop <- table_study_pop[-1,]
categories <- c("Total","age_bands",output_tte[[1]])
for(j in 1:length(categories)){
  if(j ==1){
    data_reg_table <- as.data.frame(data_reg[,c("part_id","panel_wave","vaccinated","sex")])
    l <- c(length(table_study_pop[,1])+1)
    table_levels <- 1
  }
  if(j !=1){
    c <- categories[j]
    data_reg_table <- as.data.frame(data_reg[,c("part_id","panel_wave","vaccinated",c)])
    l <- c(length(table_study_pop[,1])+1)
    if(is.numeric(data_reg_table[,4])){
      table_levels <- c
      table_study_pop[l,1] <- c
      table_study_pop[(l+1),1] <- paste0("Median ", "(Range)")
    }
    if(!is.numeric(data_reg_table[,4])){
      table_levels <- levels(data_reg_table[,4])
      table_study_pop[l,1] <- c
      table_study_pop[c((l+1):((l)+length(table_levels))),1] <- table_levels 
    }
  }
  for(i in (1+l):(l+length(table_levels))){
    
    if(sum(is.numeric(data_reg_table[,4]))>0){
      table_study_pop[i,2] <- paste0(quantile(data_reg_table[,4])[3]," (range: ",quantile(data_reg_table[,4])[1],"-",quantile(data_reg_table[,4])[5],")")
      table_study_pop[i,3] <- paste0(quantile(data_reg_table[,4][data_reg_table$vaccinated %in% 1])[3]," (range: ",quantile(data_reg_table[,4][data_reg_table$vaccinated %in% 1])[1],"-",quantile(data_reg_table[,4][data_reg_table$vaccinated %in% 1])[5],")")
    }
    
    if(sum(!is.numeric(data_reg_table[,4]))>0){
      table_levels <- levels(data_reg_table[,4])
      if(j ==1){
        i <- i-1
        k <- table_levels
      }
      if(j !=1){
        k <- table_levels[i-l]
      }
      
      table_study_pop[i,2] <- paste0(format(length(data_reg_table$part_id[data_reg_table[,4]%in% k]), nsmall=0, big.mark=","), " (",round(length(data_reg_table$part_id[data_reg_table[,4]%in%k])/length(data_reg_table$part_id)*100), "%)" )
      table_study_pop[i,3] <- paste0(format(length(data_reg_table$part_id[data_reg_table$vaccinated %in% 1 & data_reg_table[,4]%in%k]), nsmall=0, big.mark=","), " (",round(length(data_reg_table$part_id[data_reg_table$vaccinated %in% 1 & data_reg_table[,4]%in%k])/length(data_reg_table$part_id[data_reg_table$vaccinated %in% 1])*100), "%)" )
    }      
  }
}

table_study_pop[1,1] <- "Total"
table_study_pop[,1][table_study_pop[,1]==categories[2]] <- "Age groups, years"
table_study_pop[,1][table_study_pop[,1]==categories[3]] <- "Gender"
table_study_pop[,1][table_study_pop[,1]==categories[4]] <- "Region"
table_study_pop[,1][table_study_pop[,1]==categories[5]] <- "Swiss region of residence"
table_study_pop[,1][table_study_pop[,1]==categories[6]] <- "Country of birth"
table_study_pop[,1][table_study_pop[,1]==categories[7]] <- "Education level"
table_study_pop[,1][table_study_pop[,1]==categories[8]] <- "Employment status"
table_study_pop[,1][table_study_pop[,1]==categories[9]] <- "Household income, net"
table_study_pop[,1][table_study_pop[,1]==categories[10]] <- "Household size"
table_study_pop[,1][table_study_pop[,1]==categories[11]] <- "Household with medically vulnerability"
table_study_pop[,1][table_study_pop[,1]==categories[12]] <- "Testing for SARS-Cov-2"
table_study_pop[,1][table_study_pop[,1]==categories[13]] <- "Number of contacts per day"
table_study_pop[,1][table_study_pop[,1]==categories[14]] <- "Attitudes towards COVID-19 measures"

table_study_pop[41:43,1] <- paste0(table_study_pop[41:43,1]," CHF")

table_study_pop <- sapply(table_study_pop, function(x) gsub(" (0%)"," (<1%)", x, useBytes = TRUE,fixed = TRUE))
table_study_pop[is.na(table_study_pop)] <- " "

write.csv(table_study_pop[,c(1:3)], "./output/tables/vaccination_uptake/Table2.csv")



data_reg <- data_reg1

#return(data_reg)
return(list(data_reg,sf3_vac))
}
