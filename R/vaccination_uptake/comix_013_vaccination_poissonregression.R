#' ---
#' title: "Regression model to find socio-demographic factor associated with vaccination uptake and the CoMix study in Switzerland"
#' author: "Martina Reichmuth"
#' date: "'01/12/2021"
#' ---


comix_013_vaccination_poissonregression = function(data_reg) {
  ##############################################
  # point process using poisson regression 
  ##############################################
  wave_reg<- data_reg %>% 
    group_by(panel_wave) %>% 
    summarise(mean=mean(vaccinated))
  
  data_reg$unvaccinated <- ifelse(data_reg$vaccinated==1, 0, 1)
  
  sensitivity_date <- as.numeric(as_date("2021-06-01") - as_date("2021-01-01"))
  data_reg <- data_reg[order(data_reg$part_id ),]
  data_reg <- data_reg[order(data_reg$date_num ),]
  panel_date <- data_reg %>%  group_by(panel_wave) %>% 
    summarise(min_date = min(date_num), 
              max_date = max(date_num),
              mid_date = median(as.numeric(min_date:max_date))) %>% ungroup()
  
  # describe study population
  table_study_pop <- data.frame(matrix(NA,  ncol = 15))
  colnames(table_study_pop) <- c("Category", "All participants, n (%)", "Vaccinated participants, n (%)", "Observations in B1","Observations from participants that have been  vaccinated in B1", "Observations in B2","Observations from participants that have been vaccinated in B2","Observations in B3", "Observations from participants that have beenvaccinated in B3","Observations in B4","Observations from participants that have been  vaccinated in B4", "Observations in B5", "Observations from participants that have been vaccinated in B5", "Observations in B6","Observations from participants that have been  vaccinated in B6")
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
        
        #B1
        table_study_pop[i,4] <- paste0(quantile(data_reg_table[,4][data_reg_table[,2]%in%"B1"])[3]," (range: ",quantile(data_reg_table[,4][data_reg_table[,2]%in%"B1"])[1],"-",quantile(data_reg_table[,4][data_reg_table[,2]%in%"B1"])[5],")")
        table_study_pop[i,5] <- paste0(quantile(data_reg_table[,4][data_reg_table$vaccinated %in% 1 & data_reg_table[,2]%in%"B1"])[3]," (range: ",quantile(data_reg_table[,4][data_reg_table$vaccinated %in% 1 & data_reg_table[,2]%in%"B1"])[1],"-",quantile(data_reg_table[,4][data_reg_table$vaccinated %in% 1 & data_reg_table[,2]%in%"B1"])[5],")")
        #B2
        table_study_pop[i,6] <- paste0(quantile(data_reg_table[,4][data_reg_table[,2]%in%"B2"])[3]," (range: ",quantile(data_reg_table[,4][data_reg_table[,2]%in%"B2"])[1],"-",quantile(data_reg_table[,4][data_reg_table[,2]%in%"B2"])[5],")")
        table_study_pop[i,7] <- paste0(quantile(data_reg_table[,4][data_reg_table$vaccinated %in% 1 & data_reg_table[,2]%in%"B2"])[3]," (range: ",quantile(data_reg_table[,4][data_reg_table$vaccinated %in% 1 & data_reg_table[,2]%in%"B2"])[1],"-",quantile(data_reg_table[,4][data_reg_table$vaccinated %in% 1 & data_reg_table[,2]%in%"B2"])[5],")")
        #B3
        table_study_pop[i,8] <- paste0(quantile(data_reg_table[,4][data_reg_table[,2]%in%"B3"])[3]," (range: ",quantile(data_reg_table[,4][data_reg_table[,2]%in%"B3"])[1],"-",quantile(data_reg_table[,4][data_reg_table[,2]%in%"B3"])[5],")")
        table_study_pop[i,9] <- paste0(quantile(data_reg_table[,4][data_reg_table$vaccinated %in% 1 & data_reg_table[,2]%in%"B3"])[3]," (range: ",quantile(data_reg_table[,4][data_reg_table$vaccinated %in% 1 & data_reg_table[,2]%in%"B3"])[1],"-",quantile(data_reg_table[,4][data_reg_table$vaccinated %in% 1 & data_reg_table[,2]%in%"B3"])[5],")")
        #B4
        table_study_pop[i,10] <- paste0(quantile(data_reg_table[,4][data_reg_table[,2]%in%"B4"])[3]," (range: ",quantile(data_reg_table[,4][data_reg_table[,2]%in%"B4"])[1],"-",quantile(data_reg_table[,4][data_reg_table[,2]%in%"B4"])[5],")")
        table_study_pop[i,11] <- paste0(quantile(data_reg_table[,4][data_reg_table$vaccinated %in% 1 & data_reg_table[,2]%in%"B4"])[3]," (range: ",quantile(data_reg_table[,4][data_reg_table$vaccinated %in% 1 & data_reg_table[,2]%in%"B4"])[1],"-",quantile(data_reg_table[,4][data_reg_table$vaccinated %in% 1 & data_reg_table[,2]%in%"B4"])[5],")")
        #B5
        table_study_pop[i,12] <- paste0(quantile(data_reg_table[,4][data_reg_table[,2]%in%"B5"])[3]," (range: ",quantile(data_reg_table[,4][data_reg_table[,2]%in%"B5"])[1],"-",quantile(data_reg_table[,4][data_reg_table[,2]%in%"B5"])[5],")")
        table_study_pop[i,13] <- paste0(quantile(data_reg_table[,4][data_reg_table$vaccinated %in% 1 & data_reg_table[,2]%in%"B5"])[3]," (range: ",quantile(data_reg_table[,4][data_reg_table$vaccinated %in% 1 & data_reg_table[,2]%in%"B5"])[1],"-",quantile(data_reg_table[,4][data_reg_table$vaccinated %in% 1 & data_reg_table[,2]%in%"B5"])[5],")")
        #B6
        table_study_pop[i,14] <- paste0(quantile(data_reg_table[,4][data_reg_table[,2]%in%"B6"])[3]," (range: ",quantile(data_reg_table[,4][data_reg_table[,2]%in%"B6"])[1],"-",quantile(data_reg_table[,4][data_reg_table[,2]%in%"B6"])[5],")")
        table_study_pop[i,15] <- paste0(quantile(data_reg_table[,4][data_reg_table$vaccinated %in% 1 & data_reg_table[,2]%in%"B6"])[3]," (range: ",quantile(data_reg_table[,4][data_reg_table$vaccinated %in% 1 & data_reg_table[,2]%in%"B6"])[1],"-",quantile(data_reg_table[,4][data_reg_table$vaccinated %in% 1 & data_reg_table[,2]%in%"B6"])[5],")")
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
        
        #B1
        table_study_pop[i,4] <- paste0(length(data_reg_table$part_id[data_reg_table[,4]%in%k & data_reg_table[,2]%in%"B1"]), " (",round(length(data_reg_table$part_id[data_reg_table[,4]%in%k & data_reg_table[,2]%in%"B1"])/length(data_reg_table$part_id[data_reg_table[,2]%in%"B1"])*100), "%)" )
        table_study_pop[i,5] <- paste0(length(data_reg_table$part_id[data_reg_table$vaccinated %in% 1 & data_reg_table[,4]%in%k & data_reg_table[,2]%in%"B1"]), " (",round(length(data_reg_table$part_id[data_reg_table$vaccinated %in% 1 & data_reg_table[,4]%in%k & data_reg_table[,2]%in%"B1"])/length(data_reg_table$part_id[data_reg_table$vaccinated %in% 1 & data_reg_table[,2]%in%"B1"])*100), "%)" )
        #B2
        table_study_pop[i,6] <- paste0(length(data_reg_table$part_id[data_reg_table[,4]%in%k & data_reg_table[,2]%in%"B2"]), " (",round(length(data_reg_table$part_id[data_reg_table[,4]%in%k & data_reg_table[,2]%in%"B2"])/length(data_reg_table$part_id[data_reg_table[,2]%in%"B2"])*100), "%)" )
        table_study_pop[i,7] <- paste0(length(data_reg_table$part_id[data_reg_table$vaccinated %in% 1 & data_reg_table[,4]%in%k & data_reg_table[,2]%in%"B2"]), " (",round(length(data_reg_table$part_id[data_reg_table$vaccinated %in% 1 & data_reg_table[,4]%in%k & data_reg_table[,2]%in%"B2"])/length(data_reg_table$part_id[data_reg_table$vaccinated %in% 1 & data_reg_table[,2]%in%"B2"])*100), "%)" )
        #B3
        table_study_pop[i,8] <- paste0(length(data_reg_table$part_id[data_reg_table[,4]%in%k & data_reg_table[,2]%in%"B3"]), " (",round(length(data_reg_table$part_id[data_reg_table[,4]%in%k & data_reg_table[,2]%in%"B3"])/length(data_reg_table$part_id[data_reg_table[,2]%in%"B3"])*100), "%)" )
        table_study_pop[i,9] <- paste0(length(data_reg_table$part_id[data_reg_table$vaccinated %in% 1 & data_reg_table[,4]%in%k & data_reg_table[,2]%in%"B3"]), " (",round(length(data_reg_table$part_id[data_reg_table$vaccinated %in% 1 & data_reg_table[,4]%in%k & data_reg_table[,2]%in%"B3"])/length(data_reg_table$part_id[data_reg_table$vaccinated %in% 1 & data_reg_table[,2]%in%"B3"])*100), "%)" )
        #B4
        table_study_pop[i,10] <- paste0(length(data_reg_table$part_id[data_reg_table[,4]%in%k & data_reg_table[,2]%in%"B4"]), " (",round(length(data_reg_table$part_id[data_reg_table[,4]%in%k & data_reg_table[,2]%in%"B4"])/length(data_reg_table$part_id[data_reg_table[,2]%in%"B4"])*100), "%)" )
        table_study_pop[i,11] <- paste0(length(data_reg_table$part_id[data_reg_table$vaccinated %in% 1 & data_reg_table[,4]%in%k & data_reg_table[,2]%in%"B4"]), " (",round(length(data_reg_table$part_id[data_reg_table$vaccinated %in% 1 & data_reg_table[,4]%in%k & data_reg_table[,2]%in%"B4"])/length(data_reg_table$part_id[data_reg_table$vaccinated %in% 1 & data_reg_table[,2]%in%"B4"])*100), "%)" )
        #B5
        table_study_pop[i,12] <- paste0(length(data_reg_table$part_id[data_reg_table[,4]%in%k & data_reg_table[,2]%in%"B5"]), " (",round(length(data_reg_table$part_id[data_reg_table[,4]%in%k & data_reg_table[,2]%in%"B5"])/length(data_reg_table$part_id[data_reg_table[,2]%in%"B5"])*100), "%)" )
        table_study_pop[i,13] <- paste0(length(data_reg_table$part_id[data_reg_table$vaccinated %in% 1 & data_reg_table[,4]%in%k & data_reg_table[,2]%in%"B5"]), " (",round(length(data_reg_table$part_id[data_reg_table$vaccinated %in% 1 & data_reg_table[,4]%in%k & data_reg_table[,2]%in%"B5"])/length(data_reg_table$part_id[data_reg_table$vaccinated %in% 1 & data_reg_table[,2]%in%"B5"])*100), "%)" )
        #B6
        table_study_pop[i,14] <- paste0(length(data_reg_table$part_id[data_reg_table[,4]%in%k & data_reg_table[,2]%in%"B6"]), " (",round(length(data_reg_table$part_id[data_reg_table[,4]%in%k & data_reg_table[,2]%in%"B6"])/length(data_reg_table$part_id[data_reg_table[,2]%in%"B6"])*100), "%)" )
        table_study_pop[i,15] <- paste0(length(data_reg_table$part_id[data_reg_table$vaccinated %in% 1 & data_reg_table[,4]%in%k & data_reg_table[,2]%in%"B6"]), " (",round(length(data_reg_table$part_id[data_reg_table$vaccinated %in% 1 & data_reg_table[,4]%in%k & data_reg_table[,2]%in%"B6"])/length(data_reg_table$part_id[data_reg_table$vaccinated %in% 1 & data_reg_table[,2]%in%"B6"])*100), "%)" )
      }      
    }
  }
  
  table_study_pop[1,1] <- "Total"
  table_study_pop[,1][table_study_pop[,1]==categories[2]] <- "Age groups, years"
  table_study_pop[,1][table_study_pop[,1]==categories[3]] <- "Gender"
  table_study_pop[,1][table_study_pop[,1]==categories[4]] <- "Region"
  table_study_pop[,1][table_study_pop[,1]==categories[5]] <- "Swiss regions"
  table_study_pop[,1][table_study_pop[,1]==categories[6]] <- "Country of birth"
  table_study_pop[,1][table_study_pop[,1]==categories[7]] <- "Education level"
  table_study_pop[,1][table_study_pop[,1]==categories[8]] <- "Employment status"
  table_study_pop[,1][table_study_pop[,1]==categories[9]] <- "Household income"
  table_study_pop[,1][table_study_pop[,1]==categories[10]] <- "Household size"
  table_study_pop[,1][table_study_pop[,1]==categories[11]] <- "Household with vulnerability"
  table_study_pop[,1][table_study_pop[,1]==categories[12]] <- "Testing for SARS-Cov-2"
  table_study_pop[,1][table_study_pop[,1]==categories[13]] <- "Number of contacts"
  table_study_pop[,1][table_study_pop[,1]==categories[14]] <- "Perception of COVID-19 measures"
  
  table_study_pop[41:43,1] <- paste0(table_study_pop[41:43,1]," CHF")
  table_study_pop[48,1] <- paste0("No person in a risk group")
  table_study_pop[49,1] <- paste0("One or more person in a risk group")
  
  table_study_pop <- sapply(table_study_pop, function(x) gsub(" (0%)"," (<1%)", x, useBytes = TRUE,fixed = TRUE))
  table_study_pop[is.na(table_study_pop)] <- " "
  
  write.csv(table_study_pop[,c(1:3)], "../../tables/vaccination_uptake/Table2.csv")
  
  #Vaccination in study population
  data_reg <- data_reg %>% group_by(part_id) %>% arrange(date_num)  %>% mutate(t_dif_log = log(c(min(date_num),diff(date_num))), t_dif_log_sensitivity = log(c(min(date_num-sensitivity_date),diff(date_num-sensitivity_date))))
  data_reg <- data_reg %>% group_by(part_id) %>% filter(date_num<=timetoevent | is.na(timetoevent)) 
  ## Overview of study population in point process (panel B)
  paste0("Number of data points in point process: ", length((data_reg$part_id)))
  paste0("Number of paricipants in point process: ", length(unique(data_reg$part_id)))
  paste0("Female ",length(unique(data_reg$part_id[data_reg$sex%in%"Female"])), "(", round(length(unique(data_reg$part_id[data_reg$sex%in%"Female"]))/length(unique(data_reg$part_id))*100),
         "%); Male ",length(unique(data_reg$part_id[data_reg$sex%in%"Male"])),"(",round(length(unique(data_reg$part_id[data_reg$sex%in%"Male"]))/length(unique(data_reg$part_id))*100),"%)")
  paste0("Age of paricipants in point process: ", paste(quantile(as.numeric(data_reg$age_part[!duplicated(data_reg$part_id)])),collapse = "; "))
  paste0("Number of paricipants that : ",paste0 (names(table(data_reg$drops[!duplicated(data_reg$part_id)])), " ",
                                                 table(data_reg$drops[!duplicated(data_reg$part_id)])," (",
                                                 round(table(data_reg$drops[!duplicated(data_reg$part_id)])/length(data_reg$drops[!duplicated(data_reg$part_id)])*100),
                                                 "%)",collapse="; "))
  for (n in 1:2) {
    output_x <- c()
    for(i in c("panel_wave","age_bands",output_tte[[1]])){
      if(n==1){
        sub_set <- data_reg[c(grep("vaccinated|t_dif_log|panel_wave|id",colnames(data_reg)),grep(paste0("\\b",i),colnames(data_reg)))]
        colnames(sub_set)[grep(i,colnames(sub_set))] <- "variable"
        mod_uni <- glm(
          formula = vaccinated ~ variable,offset(t_dif_log),
          family = poisson(link = "log"),
          data = sub_set)
      }
      if(n==2){
        if(i%in%c("panel_wave")){
          sub_set <- data_reg[c(grep("vaccinated|t_dif_log|age_bands|id",colnames(data_reg)),grep(paste0("\\b",i),colnames(data_reg)))]
          colnames(sub_set)[grep(i,colnames(sub_set))] <- "variable"
          
          mod_uni <- glm(
            formula = vaccinated ~ age_bands*variable,offset(t_dif_log),
            family = poisson(link = "log"),
            data = sub_set)
        }
        if(i%in%c("age_bands")){
          sub_set <- data_reg[c(grep("vaccinated|t_dif_log|panel_wave|id",colnames(data_reg)),grep(paste0("\\b",i),colnames(data_reg)))]
          colnames(sub_set)[grep(i,colnames(sub_set))] <- "variable"
          
          mod_uni <- glm(
            formula = vaccinated ~ panel_wave*variable,offset(t_dif_log),
            family = poisson(link = "log"),
            data = sub_set)
        }
        if(!i %in% c("panel_wave","age_bands")){
          sub_set <- data_reg[c(grep("vaccinated|t_dif_log|panel_wave|age_bands|id",colnames(data_reg)),grep(paste0("\\b",i),colnames(data_reg)))]
          colnames(sub_set)[grep(i,colnames(sub_set))] <- "variable"
          mod_uni <- glm(
            formula = vaccinated ~  panel_wave*age_bands+variable,offset(t_dif_log),
            family = poisson(link = "log"),
            data = sub_set)
        }
      }
      output <- c()
      output <- data.frame(matrix(0, ncol = 8, nrow = length(names(mod_uni$coefficients))))
      colnames(output) <- c("Categories","Variables","names", "RR","CI.2.5.", "CI.97.5.", "p.value", "significants")
      sum_reg <- summary(mod_uni)
      output$names <- names(coef(mod_uni))
      output$RR <- coef(mod_uni)
      output$CI.2.5. <-  coef(mod_uni) - qnorm(0.975) * sqrt(diag(vcov(mod_uni)))  #ci(mod_uni)[-c(1),3]
      output$CI.97.5. <- coef(mod_uni) + qnorm(0.975) * sqrt(diag(vcov(mod_uni))) # ci(mod_uni)[-c(1),4]
      output[,c(4:6)] <- exp(output[,c(4:6)])
      
      output$p.value <- sum_reg$coefficients[,"Pr(>|z|)"]
      if(n==1){
        output<- output[!grepl("ntercept",output$names),]# only variable
      }
      output$significants <- sapply(output$p.value, FUN_sig)
      output$RRCI <- paste0(format(round(output$RR,2), nsmall = 2), " (", format(round(output$CI.2.5.,2), nsmall = 2), "-", format(round(output$CI.97.5.,2), nsmall = 2), ")")
      
      if(n==2){
        if(i %in% c("panel_wave","age_bands")){
          output<- output[!grepl("variable",output$names),]# only variable
        }
        output<- output[!grepl("ntercept|panel_wave|age_bands",output$names),]# only variable
      }
      output_x <-rbind(output_x, output)
    }
    if(n==1){
      output_unadj <- output_x
    }
    if(n==2){
      output_unadj_interaction <- output_x
    }
  }
  
  
  for (n in 1:2) {
    if(n==1){
      
      mod_multi <- glm(
        formula = vaccinated ~  panel_wave+age_bands+ sex + region + grossregion + 
          country_cat_birth + education_level3 + employment_cat + 
          household_income_3cat +  household_size+ household_riskgroup + 
          prehistory + contact_cat + agreement_measures_3cat,offset(t_dif_log),
        family = poisson(link = "log"),
        data = data_reg)
    }
    if(n==2){
      mod_multi <- glm(
        formula = vaccinated ~ panel_wave*age_bands+ sex + region + grossregion + 
          country_cat_birth + education_level3 + employment_cat + 
          household_income_3cat +  household_size+ household_riskgroup + 
          prehistory + contact_cat + agreement_measures_3cat,offset(t_dif_log),
        family = poisson(link = "log"),
        data = data_reg)
    }
    
    sum_reg <- summary(mod_multi)
    output <- data.frame(matrix(0, ncol = 8, nrow = length(names(mod_multi$coefficients))))
    colnames(output) <- c("Categories","Variables","names", "RR","CI.2.5.", "CI.97.5.", "p.value", "significants")
    
    output$names <- names(coef(mod_multi))
    output$RR <- coef(mod_multi)
    output$CI.2.5. <-  coef(mod_multi) - qnorm(0.975) * sqrt(diag(vcov(mod_multi)))  
    output$CI.97.5. <- coef(mod_multi) + qnorm(0.975) * sqrt(diag(vcov(mod_multi))) 
    output[,c(4:6)] <- exp(output[,c(4:6)])
    output$p.value <- sum_reg$coefficients[,"Pr(>|z|)"]
    output<- output[!grepl("ntercept|panel_wave|age_bands",output$names),]# only variable
    output$significants <- sapply(output$p.value, FUN_sig)
    output$RRCI <- paste0(format(round(output$RR,2), nsmall = 2), " (", format(round(output$CI.2.5.,2), nsmall = 2), "-", format(round(output$CI.97.5.,2), nsmall = 2), ")")
    if(n==1){
      output_adj <- output
    }
    if(n==2){
      output_adj_interaction <- output
      
    }
  }
  
  # sensitivity analysis changing start of study to 1 June
  mod_multi_sensitivity <- glm(formula = vaccinated ~ panel_wave:age_bands + .  - t_dif_log_sensitivity, offset(t_dif_log_sensitivity),
                               family = poisson(link = "log"),data = data_reg[,c("vaccinated", "t_dif_log_sensitivity", dependend_variables)])
  lrtest(mod_multi, mod_multi_sensitivity)$`Pr(>Chisq)`[2]
  sum_reg <- summary(mod_multi_sensitivity)
  output <- data.frame(matrix(0, ncol = 8, nrow = length(names(mod_multi_sensitivity$coefficients))))
  colnames(output) <- c("Categories","Variables","names", "RR","CI.2.5.", "CI.97.5.", "p.value", "significants")
  
  output$names <- names(coef(mod_multi_sensitivity))
  output$RR <- coef(mod_multi_sensitivity)
  output$CI.2.5. <-  coef(mod_multi_sensitivity) - qnorm(0.975) * sqrt(diag(vcov(mod_multi_sensitivity)))  
  output$CI.97.5. <- coef(mod_multi_sensitivity) + qnorm(0.975) * sqrt(diag(vcov(mod_multi_sensitivity))) 
  output[,c(4:6)] <- exp(output[,c(4:6)])
  output$p.value <- sum_reg$coefficients[,"Pr(>|z|)"]
  output<- output[!grepl("ntercept|panel_wave|age_bands",output$names),]# only variable
  output$significants <- sapply(output$p.value, FUN_sig)
  output$RRCI <- paste0(format(round(output$RR,2), nsmall = 2), " (", format(round(output$CI.2.5.,2), nsmall = 2), "-", format(round(output$CI.97.5.,2), nsmall = 2), ")")
  output_jun <- output
  
  mod_interaction <- glm(
    formula = vaccinated ~ panel_wave*age_bands,offset(t_dif_log),
    family = poisson(link = "log"),
    data = data_reg)
  output <- summary(mod_interaction)
  output <- as.data.frame(mod_interaction$coefficients)
  output$RR <- exp(coef(mod_interaction))
  output$se <- qnorm(0.975) * sqrt(diag(vcov(mod_interaction))) 
  mod_interaction <-output
  mod_interaction$rates_panel <- c(exp(mod_interaction[1,1]),exp(mod_interaction[1,1]+mod_interaction[-1,1]))
  mod_interaction$rates_CI2.5 <- c(exp(mod_interaction[1,1]-mod_interaction$se[1]),exp(mod_interaction[1,1]+mod_interaction[-1,1]-mod_interaction$se[-1]))
  mod_interaction$rates_CI97.5 <- c(exp(mod_interaction[1,1]+mod_interaction$se[1]),exp(mod_interaction[1,1]+mod_interaction[-1,1]+mod_interaction$se[-1]))
  
  output <- summary(mod_multi)
  output <- as.data.frame(mod_multi$coefficients)
  output$RR <- exp(coef(mod_multi))
  output$se <- qnorm(0.975) * sqrt(diag(vcov(mod_multi))) 
  mod_multi <- output[grepl("ntercept|age_bands|panel_wave",rownames(output)),]
  mod_multi$rates_panel <- c(exp(mod_multi[1,1]),exp(mod_multi[1,1]+mod_multi[-1,1]))
  mod_multi$rates_CI2.5 <- c(exp(mod_multi[1,1]-mod_multi$se[1]),exp(mod_multi[1,1]+mod_multi[-1,1]-mod_multi$se[-1]))
  mod_multi$rates_CI97.5 <- c(exp(mod_multi[1,1]+mod_multi$se[1]),exp(mod_multi[1,1]+mod_multi[-1,1]+mod_multi$se[-1]))
  remove(output)
  for(i in unlist(panel_date[-1,1])){
    mod_interaction[grepl(i,rownames(mod_interaction)),"mid_date"] <- panel_date$mid_date[panel_date$panel_wave==i]
  }
  mod_interaction$mid_date[is.na(mod_interaction$mid_date)] <- as_date(unlist(panel_date[1,"mid_date"]))
  mod_interaction$age_bands <- gsub("^.*\\[", "", rownames(mod_interaction))
  mod_interaction$age_bands <- sub(".*(70\\+)", "\\1", mod_interaction$age_bands )
  mod_interaction$age_bands[grepl("panel_wave",mod_interaction$age_bands)] <- "18,30)"
  mod_interaction$age_bands[1] <- "18,30)"
  
  
  mod_multi$mid_date <- mod_interaction$mid_date 
  mod_multi$age_bands <- mod_interaction$age_bands 
  mod_interaction$model <- "Unadjusted*"
  mod_multi$model <- "Adjusted"
  mod_interaction<- rbind(mod_interaction[,c("mid_date","age_bands", "rates_panel","rates_CI2.5", "rates_CI97.5","model")],mod_multi[,c("mid_date","age_bands", "rates_panel","rates_CI2.5", "rates_CI97.5","model")])
  
  mod_interaction$model <- factor(mod_interaction$model, levels=c("Unadjusted*", "Adjusted"))
  mod_interaction$age_bands[!grepl("\\b[+]",mod_interaction$age_bands)] <- paste0("[",mod_interaction$age_bands[!grepl("\\b[+]",mod_interaction$age_bands)])
  mod_interaction$age_bands <- factor(mod_interaction$age_bands, levels(data_reg$age_bands))
  
  plot_time<- ggplot(data = mod_interaction) +
    theme_minimal()+
    facet_wrap(vars(age_bands))+#  facet_wrap(vars(model))+
    geom_line(aes(x=as_date(mid_date+as_date("2022-01-01")),y=rates_panel*100, group=model, color=model), linewidth=0.6) +#http://sape.inf.usi.ch/quick-reference/ggplot2/linetype #linetype="dashed"
    geom_point(aes(x=as_date(mid_date+as_date("2022-01-01")),y=rates_panel*100, group=model, color=model), size=1) +
    geom_ribbon(aes(x = as_date(mid_date+as_date("2022-01-01")), y = rates_panel*100, ymin=rates_CI2.5*100, ymax=rates_CI97.5*100, group=model, fill=model),alpha=0.2)+
    scale_color_manual(name=" ",values = c(col_9[1:2]))+#c(col_9[1:6]))+
    scale_fill_manual(name=" ",values = c(col_9[1:2]))+#c(col_9[1:6]))+
    theme(legend.position = 'bottom')+ 
    scale_y_continuous(labels = function(x) paste0(x, "%"))+
    guides(fill=guide_legend(nrow=2,byrow=TRUE),color=guide_legend(nrow=2,byrow=TRUE))+
    labs(tag="",subtitle = bquote(), x = "", y ="Vaccination rate")
  print(plot_time)
  ggsave(plot_time, filename = paste0("../../figures/vaccination_uptake/Figure3.png"), height =4, width = 6,  bg = "transparent")
  
  
  # Due to interaction age and wave outcome in adjusted models not meaningful: Adapt tables accordingly:
  age_wave_NA <- as.data.frame(matrix(nrow=length(unique(data_reg$age_bands)[-1])+length(unique(data_reg$panel_wave)[-1]),ncol=length(output_unadj))) # to match Unadjusted model
  colnames(age_wave_NA) <- colnames(output_unadj)
  output_unadj_interaction <-rbind(age_wave_NA,output_unadj_interaction)
  output_adj <-rbind(age_wave_NA,output_adj)
  output_adj_interaction <-rbind(age_wave_NA,output_adj_interaction)
  output_jun <-rbind(age_wave_NA,output_jun)
  
  output_unadj[1,1] <- paste0("Survey wave\nReference: ",names(table(data_reg$panel_wave)[1]))
  output_unadj[1,2] <- paste0(names(table(data_reg$panel_wave)[2]))
  output_unadj[1,"N answers"] <- paste0(table(data_reg$panel_wave)[2])
  
  output_unadj[2,1] <- " "
  output_unadj[2,2] <- paste0(names(table(data_reg$panel_wave)[3]))
  output_unadj[2,"N answers"] <- paste0(table(data_reg$panel_wave)[3])
  
  output_unadj[3,1] <- " "
  output_unadj[3,2] <- paste0(names(table(data_reg$panel_wave)[4]))
  output_unadj[3,"N answers"] <- paste0(table(data_reg$panel_wave)[4])
  
  output_unadj[4,1] <- " "
  output_unadj[4,2] <- paste0(names(table(data_reg$panel_wave)[5]))
  output_unadj[4,"N answers"] <- paste0(table(data_reg$panel_wave)[5])
  
  output_unadj[5,1] <- " "
  output_unadj[5,2] <- paste0(names(table(data_reg$panel_wave)[6]))
  output_unadj[5,"N answers"] <- paste0(table(data_reg$panel_wave)[6])
  
  output_unadj[6,1] <- paste0("Age groups, years \nReference: ",names(table(data_reg$age_bands)[1]))
  output_unadj[6,2] <- paste0(names(table(data_reg$age_bands)[2]))
  output_unadj[6,"N participants"] <- paste0(table(data_reg$age_bands[!duplicated(data_reg$part_id)])[2])
  output_unadj[6,"N answers"] <- paste0(table(data_reg$age_bands)[2])
  
  output_unadj[7,1] <- " "
  output_unadj[7,2] <- paste0(names(table(data_reg$age_bands)[3]))
  output_unadj[7,"N participants"] <- paste0(table(data_reg$age_bands[!duplicated(data_reg$part_id)])[3])
  output_unadj[7,"N answers"] <- paste0(table(data_reg$age_bands)[3])
  
  output_unadj[8,1] <- " "
  output_unadj[8,2] <- paste0(names(table(data_reg$age_bands)[4]))
  output_unadj[8,"N participants"] <- paste0(table(data_reg$age_bands[!duplicated(data_reg$part_id)])[4])
  output_unadj[8,"N answers"] <- paste0(table(data_reg$age_bands)[4])
  
  output_unadj[9,1] <- " "
  output_unadj[9,2] <- paste0(names(table(data_reg$age_bands)[5]))
  output_unadj[9,"N participants"] <- paste0(table(data_reg$age_bands[!duplicated(data_reg$part_id)])[5])
  output_unadj[9,"N answers"] <- paste0(table(data_reg$age_bands)[5])
  
  output_unadj[10,1] <- " "
  output_unadj[10,2] <- paste0(names(table(data_reg$age_bands)[6]))
  output_unadj[10,"N participants"] <- paste0(table(data_reg$age_bands[!duplicated(data_reg$part_id)])[6])
  output_unadj[10,"N answers"] <- paste0(table(data_reg$age_bands)[6])
  
  
  output_unadj[11,1] <- "Gender\nReference: Female"
  output_unadj[11,2] <- paste0(names(table(data_reg$sex)[2]))
  output_unadj[11,"N participants"] <- paste0(table(data_reg$sex[!duplicated(data_reg$part_id)])[2])
  output_unadj[11,"N answers"] <- paste0(table(data_reg$sex)[2])
  
  output_unadj[12,1] <- " "
  output_unadj[12,2] <- "Others"
  output_unadj[12,"N participants"] <- paste0(table(data_reg$sex[!duplicated(data_reg$part_id)])[3])
  output_unadj[12,"N answers"] <- paste0(table(data_reg$sex)[3])
  
  output_unadj[13,1] <- "Region\nReference: Urban"
  output_unadj[13,2] <-  paste0(names(table(data_reg$region)[2]))
  output_unadj[13,"N participants"] <-  paste0(table(data_reg$region[!duplicated(data_reg$part_id)])[2])
  output_unadj[13,"N answers"] <-  paste0(table(data_reg$region)[2])
  
  output_unadj[14,1] <-paste0("Swiss regions","\nReference: ",names(table(data_reg$grossregion)[1]))
  output_unadj[14,2] <-  paste0(names(table(data_reg$grossregion)[2]))
  output_unadj[14,"N participants"] <-  paste0(table(data_reg$grossregion[!duplicated(data_reg$part_id)])[2])
  output_unadj[14,"N answers"] <-  paste0(table(data_reg$grossregion)[2])
  
  output_unadj[15,1] <- " "
  output_unadj[15,2] <- paste0(names(table(data_reg$grossregion)[3]))
  output_unadj[15,"N participants"] <-  paste0(table(data_reg$grossregion[!duplicated(data_reg$part_id)])[3])
  output_unadj[15,"N answers"] <-  paste0(table(data_reg$grossregion)[3])
  
  output_unadj[16,1] <- " "
  output_unadj[16,2] <- paste0(names(table(data_reg$grossregion)[4]))
  output_unadj[16,"N participants"] <-  paste0(table(data_reg$grossregion[!duplicated(data_reg$part_id)])[4])
  output_unadj[16,"N answers"] <-  paste0(table(data_reg$grossregion)[4])
  
  output_unadj[17,1] <- " "
  output_unadj[17,2] <- paste0(names(table(data_reg$grossregion)[5]))
  output_unadj[17,"N participants"] <-  paste0(table(data_reg$grossregion[!duplicated(data_reg$part_id)])[5])
  output_unadj[17,"N answers"] <-  paste0(table(data_reg$grossregion[!duplicated(data_reg$part_id)])[5])
  
  output_unadj[18,1] <- " "
  output_unadj[18,2] <- paste0(names(table(data_reg$grossregion)[6]))
  output_unadj[18,"N participants"] <-  paste0(table(data_reg$grossregion[!duplicated(data_reg$part_id)])[6])
  output_unadj[18,"N answers"] <-  paste0(table(data_reg$grossregion)[6])
  
  output_unadj[19,1] <- " "
  output_unadj[19,2] <- paste0(names(table(data_reg$grossregion)[7]))
  output_unadj[19,"N participants"] <- paste0(table(data_reg$grossregion[!duplicated(data_reg$part_id)])[7])
  output_unadj[19,"N answers"] <- paste0(table(data_reg$grossregion)[7])
  
  output_unadj[20,1] <- paste0("Country of birth\nReference: ",names(table(data_reg$country_cat_birth)[1]))
  output_unadj[20,2] <- paste0(names(table(data_reg$country_cat_birth)[2]))
  output_unadj[20,"N participants"] <- paste0(table(data_reg$country_cat_birth[!duplicated(data_reg$part_id)])[2])
  output_unadj[20,"N answers"] <- paste0(table(data_reg$country_cat_birth)[2])
  
  output_unadj[21,1] <- " "
  output_unadj[21,2] <- paste0(names(table(data_reg$country_cat_birth)[3]))
  output_unadj[21,"N participants"] <- paste0(table(data_reg$country_cat_birth[!duplicated(data_reg$part_id)])[3])
  output_unadj[21,"N answers"] <- paste0(table(data_reg$country_cat_birth)[3])
  
  output_unadj[22,1] <- " "
  output_unadj[22,2] <- paste0(names(table(data_reg$country_cat_birth)[4]))
  output_unadj[22,"N participants"] <- paste0(table(data_reg$country_cat_birth[!duplicated(data_reg$part_id)])[4])
  output_unadj[22,"N answers"] <- paste0(table(data_reg$country_cat_birth)[4])
  
  output_unadj[23,1] <- paste0("Education level\nReference: Lowest level")
  output_unadj[23,2] <- paste0("Middle level of education")
  output_unadj[23,"N participants"] <- paste0(table(data_reg$education_level3[!duplicated(data_reg$part_id)])[2])
  output_unadj[23,"N answers"] <- paste0(table(data_reg$education_level3)[2])
  
  output_unadj[24,1] <- " "
  output_unadj[24,2] <- paste0("Highest level of education")
  output_unadj[24,"N participants"] <- paste0(table(data_reg$education_level3[!duplicated(data_reg$part_id)])[3])
  output_unadj[24,"N answers"] <- paste0(table(data_reg$education_level3)[3])
  
  output_unadj[25,1] <- paste0("Employment status\nReference: ",names(table(data_reg$employment_cat)[1]))
  output_unadj[25,2] <- paste0(names(table(data_reg$employment_cat)[2]))
  output_unadj[25,"N participants"] <- paste0(table(data_reg$employment_cat[!duplicated(data_reg$part_id)])[2])
  output_unadj[25,"N answers"] <- paste0(table(data_reg$employment_cat)[2])
  
  output_unadj[26,1] <- " "
  output_unadj[26,2] <- paste0(names(table(data_reg$employment_cat)[3]))
  output_unadj[26,"N participants"] <- paste0(table(data_reg$employment_cat[!duplicated(data_reg$part_id)])[3])
  output_unadj[26,"N answers"] <- paste0(table(data_reg$employment_cat)[3])
  
  output_unadj[27,1] <- ""
  output_unadj[27,2] <- paste0(names(table(data_reg$employment_cat)[4]))
  output_unadj[27,"N participants"] <- paste0(table(data_reg$employment_cat[!duplicated(data_reg$part_id)])[4])
  output_unadj[27,"N answers"] <- paste0(table(data_reg$employment_cat)[4])
  
  output_unadj[28,1] <- " "
  output_unadj[28,2] <- paste0(names(table(data_reg$employment_cat)[5]))
  output_unadj[28,"N participants"] <- paste0(table(data_reg$employment_cat[!duplicated(data_reg$part_id)])[5])
  output_unadj[28,"N answers"] <- paste0(table(data_reg$employment_cat)[5])
  
  output_unadj[29,1] <- " "
  output_unadj[29,2] <- paste0(names(table(data_reg$employment_cat)[6]))
  output_unadj[29,"N participants"] <- paste0(table(data_reg$employment_cat[!duplicated(data_reg$part_id)])[6])
  output_unadj[29,"N answers"] <- paste0(table(data_reg$employment_cat)[6])
  
  output_unadj[30,1] <- paste0("Household income, net\nReference: ",names(table(data_reg$household_income_3cat)[1])," CHF")
  output_unadj[30,2] <- paste0(names(table(data_reg$household_income_3cat)[2])," CHF")
  #output_unadj[30,2] <- paste0("Income of ", names(table(data_reg$household_income_3cat)[2])," compared to ",names(table(data_reg$household_income_3cat)[1]))
  output_unadj[30,"N participants"] <- paste0(table(data_reg$household_income_3cat[!duplicated(data_reg$part_id)])[2])
  output_unadj[30,"N answers"] <- paste0(table(data_reg$household_income_3cat)[2])
  
  output_unadj[31,1] <- " "
  output_unadj[31,2] <- paste0(names(table(data_reg$household_income_3cat)[3])," CHF")
  output_unadj[31,"N participants"] <- paste0(table(data_reg$household_income_3cat[!duplicated(data_reg$part_id)])[3])
  output_unadj[31,"N answers"] <- paste0(table(data_reg$household_income_3cat)[3])
  
  output_unadj[32,1] <- " "
  output_unadj[32,2] <- paste0("", names(table(data_reg$household_income_3cat)[4]))
  output_unadj[32,"N participants"] <- paste0(table(data_reg$household_income_3cat[!duplicated(data_reg$part_id)])[4])
  output_unadj[32,"N answers"] <- paste0(table(data_reg$household_income_3cat)[4])
  
  output_unadj[33,1] <- " "
  output_unadj[33,2] <- paste0("Household size")
  output_unadj[33,"N participants"] <- paste0(round(mean(data_reg$household_size[!duplicated(data_reg$part_id)]))," (",min(data_reg$household_size[!duplicated(data_reg$part_id)])," - ",max(data_reg$household_size[!duplicated(data_reg$part_id)]),")")
  output_unadj[33,"N answers"] <- paste0(round(mean(data_reg$household_size))," (",min(data_reg$household_size)," - ",max(data_reg$household_size),")")
  
  output_unadj[34,1] <- "Vulnerability  \nReference: No person in a risk group"
  output_unadj[34,2] <- "One or more person in a risk group"
  output_unadj[34,"N participants"] <- paste0(table(data_reg$household_riskgroup[!duplicated(data_reg$part_id)])[2])
  output_unadj[34,"N answers"] <- paste0(table(data_reg$household_riskgroup)[2])
  
  output_unadj[35,1] <- paste0("Testing for SARS-CoV-2\nReference: ",names(table(data_reg$prehistory)[1]))
  output_unadj[35,2] <- paste0(names(table(data_reg$prehistory)[2]))
  output_unadj[35,"N answers"] <- paste0(table(data_reg$prehistory)[2])
  
  output_unadj[36,1] <- " "
  output_unadj[36,2] <- paste0(names(table(data_reg$prehistory)[3]))
  output_unadj[36,"N answers"] <- paste0(table(data_reg$prehistory)[3])
  
  output_unadj[37,1] <- " "
  output_unadj[37,2] <- paste0(names(table(data_reg$prehistory)[4]))
  output_unadj[37,"N answers"] <- paste0(table(data_reg$prehistory)[4])
  
  output_unadj[38,1] <- paste0("Number of contacts\nReference: ",names(table(data_reg$contact_cat)[1]))
  output_unadj[38,2] <- paste0(names(table(data_reg$contact_cat)[2]))
  output_unadj[38,"N answers"] <- paste0(table(data_reg$contact_cat)[2])
  
  output_unadj[39,1] <- " "
  output_unadj[39,2] <- paste0(names(table(data_reg$contact_cat)[3]))
  output_unadj[39,"N answers"] <- paste0(table(data_reg$contact_cat)[3])
  
  output_unadj[40,1] <- paste0("Perception of COVID-19 measures \nReference: ",names(table(data_reg$agreement_measures_3cat)[1]))
  output_unadj[40,2] <- paste0(names(table(data_reg$agreement_measures_3cat)[2]))
  output_unadj[40,"N answers"] <- paste0(table(data_reg$agreement_measures_3cat)[2])
  
  output_unadj[41,1] <- "   "
  output_unadj[41,2] <- paste0(names(table(data_reg$agreement_measures_3cat)[3]))
  output_unadj[41,"N answers"] <- paste0(table(data_reg$agreement_measures_3cat)[3])
  
  output_unadj[42,1] <- " "
  output_unadj[42,2] <- paste0(names(table(data_reg$agreement_measures_3cat)[4]))
  output_unadj[42,"N answers"] <- paste0(table(data_reg$agreement_measures_3cat)[4])
  output_unadj[is.na(output_unadj)] <- "-"
  output_unadj[-c(33),c(10)] <- format(as.numeric(output_unadj[-c(33),c(10)]), nsmall=0, big.mark=",")
  output_unadj[-c(1:5,33,35:42),c(11)] <- format(as.numeric(output_unadj[-c(1:5,33,35:42),c(11)]), nsmall=0, big.mark=",")
  
  
  #Overall table
  output_adj_interaction[,c(1,2,10,11)] <- output_unadj[,c(1,2,10,11)]
  output_poissonreg<- cbind(output_unadj[,c(1,2,10,11)],output_unadj[,c(8,9)],output_unadj_interaction[,c(8,9)],output_adj[,c(8,9)],output_adj_interaction[,c(8,9)],output_jun[,c(8,9)])
  
  
  colnames(output_poissonreg) <- c("Categories","Variables","N answers","N participants","P-value of unadjusted model","Unadjusted RR","P-value of unadjusted model with age as interaction","Unadjusted RR with age as interaction","P-value of adjusted RR","Adjusted RR","P-value of adjusted RR with age as interaction","Adjusted RR with age as interaction","P-value of adjusted RR with age as interaction (1 June)","Adjusted RR with age as interaction (1 June)")
  output_poissonreg[is.na(output_poissonreg)] <- "-"
  write.csv(output_poissonreg, "../../tables/vaccination_uptake/Table2.csv")
  # table of unadjusted and adjusted
  
  #Figure (forestplot) of RR
  output_unadj$model <- "Unadjusted"
  output_adj_interaction$model <- "Adjusted"
  binded_regression <- rbind(output_unadj, output_adj_interaction)
  binded_regression$model <- factor(binded_regression$model, levels=c("Unadjusted", "Adjusted"))
  binded_regression$names <- factor(binded_regression$Variables, levels=rev(unique(binded_regression$Variables)))
  
  FUN_noNa <- function(x){
    if(is.na(x)) {return("-")}
    else {return(as.character(x))}
  }
  output_adj_interaction$RRCI  <- sapply(output_adj_interaction$RRCI, FUN_noNa)
  binded_regression <- cbind(output_unadj[-c(1:5),], output_adj_interaction[-c(1:5),])
  binded_regression$cbindRRCI <- paste0(output_unadj$RRCI[-c(1:5)],"\n", output_adj_interaction$RRCI[-c(1:5)])
  binded_regression$cbindRRCI<- gsub(" ", "", binded_regression$cbindRRCI, fixed = TRUE)
  binded_regression$cbindRRCI<- gsub("(", " (", binded_regression$cbindRRCI, fixed = TRUE)
  
  
  tabletext <- cbind(c("Categories    ",as.character(output_adj_interaction[-c(1:5),1])),
                     c("Factors    ",as.character(output_adj_interaction[-c(1:5),2])), 
                     c(paste0("°n=",format(length(data_reg$part_id), nsmall=0, big.mark=",") ), output_adj_interaction[-c(1:5),10]),
                     c(paste0("*N=",format(length(unique(data_reg$part_id)), nsmall=0, big.mark=",") ), output_adj_interaction[-c(1:5),11]),
                     c("RR (95%-CI)", output_unadj[-c(1:5),9]),c("aRR (95%-CI)", output_adj_interaction[-c(1:5),9]))
  
  ticks <- c(0.5,1,2) # xaxis labeling
  attr(ticks, "labels") <- as.character(ticks) # # xaxis labeling  ADDITION
  
  
  
  own <- fpTxtGp(label = gpar(cex=2),ticks = gpar(cex=2),summary=gpar(cex=2), xlab =gpar(cex=2), legend = gpar(cex=2.5),legend.title = gpar(cex=2), title = gpar(cex=2))# adjust text size of forestplot
  own <- fpTxtGp(label = gpar(cex=1),ticks = gpar(cex=1),summary=gpar(cex=1), xlab =gpar(cex=1), legend = gpar(cex=1.5),legend.title = gpar(cex=1), title = gpar(cex=1))# adjust text size of forestplot
  # adjust text size of forestplot
  setEPS()
  pdf(paste0("../../figures/vaccination_uptake/Figure2.pdf"), width = 14, height = 18 )
  forestplot(tabletext, graph.pos=5, align = c("l","l","l","l","r","l","l"),
             legend =  c( "Unadjusted", "Adjusted"),
             legend_args = fpLegend(pos = list(x=0.5, y=1), 
                                    gp=gpar(col="#CCCCCC", fill="#F9F9F9")),
             mean = cbind(c(NA,output_unadj$RR[-c(1:5)]), c(NA,output_adj_interaction$RR[-c(1:5)])),
             lower = cbind( c(NA,output_unadj$CI.2.5.[-c(1:5)]), c(NA,output_adj_interaction$CI.2.5.[-c(1:5)])),
             upper = cbind(c(NA,output_unadj$CI.97.5.[-c(1:5)]), c(NA,output_adj_interaction$CI.97.5.[-c(1:5)])),
             txt_gp = own,  
             hrzl_lines = list("2" = gpar(lty=1), 
                               "7" = gpar(lty=2), 
                               "9" = gpar(lty=2), 
                               "10" = gpar(lty=2),
                               "16" = gpar(lty=2), 
                               "19" = gpar(lty=2), 
                               "21" = gpar(lty=2),
                               "26" = gpar(lty=2),
                               "29" = gpar(lty=3),
                               "30" = gpar(lty=3),
                               "31" = gpar(lty=2), 
                               "34" = gpar(lty=2), 
                               "36" = gpar(lty=2),
                               "39" = gpar(lty=1)),
             title="",
             xlab="Outcome: getting vaccinated",
             graphwidth=unit (70, "mm"), 
             colgap=unit(3,"mm"),new_page = F,
             xlog=TRUE, 
             xticks=ticks, 
             clip =c(0.5, 1.5),
             col=fpColors(box=c(col_9[1:2]),lines = c(col_9[1:2])),
             boxsize= c(0.12),lwd.ci=4, ci.vertices=TRUE, ci.vertices.height = 0.1)
  
  dev.off()
  
  return(output_poissonreg)
}

