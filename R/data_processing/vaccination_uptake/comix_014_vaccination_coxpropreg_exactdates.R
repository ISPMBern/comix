#' ---
#' title: "Initialize"
#' author: "mreichmuth"
#' date: "19/10/2021"
#' ---

comix_014_vaccination_coxpropreg_exactdates = function(data_reg) {
  ##############################################
  # Cox proportional hazard regression (with IPWC) and exact dates of vaccination
  ##############################################
  
  # keep participants that have not been vaccinated or have exact vaccination date
  print(length(unique(data_reg$part_id[!is.na(data_reg$vac_date)& data_reg$vaccinated==1]))/
          length(unique(data_reg$part_id[data_reg$vaccinated==1])))
  data_reg <- data_reg %>% group_by(part_id) %>% filter(!is.na(vac_date) & vaccinated==1|is.na(vac_date) & vaccinated==0)
  #filter everything after vaccination and keep then earliest date
  data_reg <- data_reg %>% group_by(part_id) %>% filter(date>=vac_date | vaccinated==0) 
  
  data_reg <- data_reg %>% group_by(part_id) %>% filter(date_num == min(date_num)) 
  
  #replace vaccination date for those we have it
  data_reg$date_num <- ifelse(is.na(data_reg$vac_date), data_reg$date_num, data_reg$vac_date)
  
  
  print(length(data_reg$part_id))
  print(length(data_reg$part_id[data_reg$vaccinated==1]))
  
  for(n in 1:2){
    if(n==2){
      cox_adj <- coxph(Surv(date_num, vaccinated) ~ strata(age_bands) + sex + region + grossregion + 
                         country_cat_birth + education_level3 + employment_cat + 
                         household_income_3cat +  household_size+ household_riskgroup + 
                         prehistory + contact_cat + agreement_measures_3cat, data = data_reg)
      
      
    }
    if(n==1){
      cox_adj <- coxph(Surv(date_num, vaccinated) ~ strata(age_bands) + sex + region + grossregion + 
                         country_cat_birth + education_level3 + employment_cat + 
                         household_income_3cat +  household_size+ household_riskgroup + 
                         prehistory + contact_cat + agreement_measures_3cat,weights = ipw,data = data_reg)
    }
    output <-  data.frame(matrix(0, ncol = 0, nrow =length(names(cox_adj$coefficients))))
    output$names <- names(cox_adj$coefficients)
    sum_reg <- summary(cox_adj)
    output$HR <- cox_adj$coefficients
    output$CI.2.5. <-  output$HR - qnorm(0.975) * sqrt(diag(vcov(cox_adj)))# robust se
    output$CI.97.5. <-  output$HR + qnorm(0.975) * sqrt(diag(vcov(cox_adj)))# robust se
    output[,c(2:4)] <- exp(output[,c(2:4)])
    output$p.value <- sum_reg$coefficients[,grepl("Pr(>|z|)", colnames(sum_reg$coefficients))]
    output$significants <- sapply(output$p.value, FUN_sig)
    output$'HR (95% CI)' <- paste0(format(round(output$HR,2), nsmall = 2), " (", format(round(output$CI.2.5.,2), nsmall = 2), "-", format(round(output$CI.97.5.,2), nsmall = 2), ")")
    output_adj <- output
    if(n==1){
      output_adj_ipw <- output_adj
    }
  }
  
  model_variables<- coxVariableName(cox_adj, data_reg)$lpvars.original
  for(n in 1:2){
    output_unadj <- c()
    for(i in c("age_bands",unlist(model_variables))){
      
      sub_set <- data_reg[c(grep("vaccinated|date_num|ipw",colnames(data_reg)),grep(paste0("\\b",i),colnames(data_reg)))]
      colnames(sub_set)[grep(i,colnames(sub_set))] <- "variable"
      if(n==2){
        cox_unadj <- coxph(Surv(date_num, vaccinated) ~ variable, data=sub_set)
      }  
      if(n==1){
        cox_unadj <- coxph(Surv(date_num, vaccinated) ~ variable, weights = ipw, data=sub_set)
      } 
      output <-  data.frame(matrix(0, ncol = 0, nrow =length(names(cox_unadj$coefficients))))
      sum_reg <- summary(cox_unadj)
      output$names <- names(cox_unadj$coefficients)
      output$HR <- cox_unadj$coefficients
      output$CI.2.5. <-  output$HR - qnorm(0.975) * sqrt(diag(vcov(cox_unadj)))# robust se
      output$CI.97.5. <-  output$HR + qnorm(0.975) * sqrt(diag(vcov(cox_unadj)))# robust se
      output[,c(2:4)] <- exp(output[,c(2:4)])
      
      output$p.value <- sum_reg$coefficients[,grepl("Pr(>|z|)", colnames(sum_reg$coefficients))]
      
      output$significants <- sapply(output$p.value, FUN_sig)
      output$'HR (95% CI)' <- paste0(format(round(output$HR,2), nsmall = 2), " (", format(round(output$CI.2.5.,2), nsmall = 2), "-", format(round(output$CI.97.5.,2), nsmall = 2), ")")
      output_unadj <-rbind(output_unadj, output)
      
    }
    if(n==1){
      output_unadj_ipw <- output_unadj
    }
  }
  
  age_wave_NA <- as.data.frame(matrix(nrow=length(unique(data_reg$age_bands)[-1])+length(unique(data_reg$panel_wave)[-1]),ncol=length(output_unadj))) # to match univariable model
  colnames(age_wave_NA) <- colnames(output)
  output_adj <-rbind(age_wave_NA,output_adj)
  output_adj_ipw <-rbind(age_wave_NA,output_adj_ipw)
  
  waves_NA <- as.data.frame(matrix(nrow=length(unique(data_reg$panel_wave)[-1]),ncol=length(output_unadj))) # Cox hazard regression was stratified by age (bands)
  colnames(waves_NA) <- colnames(output_unadj)
  output_unadj <-rbind(waves_NA,output_unadj)
  output_unadj_ipw <-rbind(waves_NA,output_unadj_ipw)
  
  
  
  output_unadj[6,1] <- paste0("Age groups, years \nReference: ",names(table(data_reg$age_bands)[1]))
  output_unadj[6,"Factor"] <- paste0(names(table(data_reg$age_bands)[2]))
  output_unadj[6,"N answers"] <- paste0(table(data_reg$age_bands)[2])
  
  output_unadj[7,1] <- " "
  output_unadj[7,"Factor"] <- paste0(names(table(data_reg$age_bands)[3]))
  output_unadj[7,"N answers"] <- paste0(table(data_reg$age_bands)[3])
  
  output_unadj[8,1] <- " "
  output_unadj[8,"Factor"] <- paste0(names(table(data_reg$age_bands)[4]))
  output_unadj[8,"N answers"] <- paste0(table(data_reg$age_bands)[4])
  
  output_unadj[9,1] <- " "
  output_unadj[9,"Factor"] <- paste0(names(table(data_reg$age_bands)[5]))
  output_unadj[9,"N answers"] <- paste0(table(data_reg$age_bands)[5])
  
  output_unadj[10,1] <- " "
  output_unadj[10,"Factor"] <- paste0(names(table(data_reg$age_bands)[6]))
  output_unadj[10,"N answers"] <- paste0(table(data_reg$age_bands)[6])
  
  
  output_unadj[11,1] <- "Gender\nReference: Female"
  output_unadj[11,"Factor"] <- paste0(names(table(data_reg$sex)[2]))
  output_unadj[11,"N answers"] <- paste0(table(data_reg$sex)[2])
  
  output_unadj[12,1] <- " "
  output_unadj[12,"Factor"] <- "Others"
  output_unadj[12,"N answers"] <- paste0(table(data_reg$sex)[3])
  
  output_unadj[13,1] <- "Region\nReference: Urban"
  output_unadj[13,"Factor"] <-  paste0(names(table(data_reg$region)[2]))
  output_unadj[13,"N answers"] <-  paste0(table(data_reg$region)[2])
  
  output_unadj[14,1] <-paste0("Swiss regions of residence","\nReference: ",names(table(data_reg$grossregion)[1]))
  output_unadj[14,"Factor"] <-  paste0(names(table(data_reg$grossregion)[2]))
  output_unadj[14,"N answers"] <-  paste0(table(data_reg$grossregion)[2])
  
  output_unadj[15,1] <- " "
  output_unadj[15,"Factor"] <- paste0(names(table(data_reg$grossregion)[3]))
  output_unadj[15,"N answers"] <-  paste0(table(data_reg$grossregion)[3])
  
  output_unadj[16,1] <- " "
  output_unadj[16,"Factor"] <- paste0(names(table(data_reg$grossregion)[4]))
  output_unadj[16,"N answers"] <-  paste0(table(data_reg$grossregion)[4])
  
  output_unadj[17,1] <- " "
  output_unadj[17,"Factor"] <- paste0(names(table(data_reg$grossregion)[5]))
  output_unadj[17,"N answers"] <-  paste0(table(data_reg$grossregion[!duplicated(data_reg$part_id)])[5])
  
  output_unadj[18,1] <- " "
  output_unadj[18,"Factor"] <- paste0(names(table(data_reg$grossregion)[6]))
  output_unadj[18,"N answers"] <-  paste0(table(data_reg$grossregion)[6])
  
  output_unadj[19,1] <- " "
  output_unadj[19,"Factor"] <- paste0(names(table(data_reg$grossregion)[7]))
  output_unadj[19,"N answers"] <- paste0(table(data_reg$grossregion)[7])
  
  output_unadj[20,1] <- paste0("Country of birth\nReference: ",names(table(data_reg$country_cat_birth)[1]))
  output_unadj[20,"Factor"] <- paste0(names(table(data_reg$country_cat_birth)[2]))
  output_unadj[20,"N answers"] <- paste0(table(data_reg$country_cat_birth)[2])
  
  output_unadj[21,1] <- " "
  output_unadj[21,"Factor"] <- paste0(names(table(data_reg$country_cat_birth)[3]))
  output_unadj[21,"N answers"] <- paste0(table(data_reg$country_cat_birth)[3])
  
  output_unadj[22,1] <- " "
  output_unadj[22,"Factor"] <- paste0(names(table(data_reg$country_cat_birth)[4]))
  output_unadj[22,"N answers"] <- paste0(table(data_reg$country_cat_birth)[4])
  
  output_unadj[23,1] <- paste0("Education level\nReference: Lowest level")
  output_unadj[23,"Factor"] <- paste0("Middle level of education")
  output_unadj[23,"N answers"] <- paste0(table(data_reg$education_level3)[2])
  
  output_unadj[24,1] <- " "
  output_unadj[24,"Factor"] <- paste0("Highest level of education")
  output_unadj[24,"N answers"] <- paste0(table(data_reg$education_level3)[3])
  
  output_unadj[25,1] <- paste0("Employment status\nReference: ",names(table(data_reg$employment_cat)[1]))
  output_unadj[25,"Factor"] <- paste0(names(table(data_reg$employment_cat)[2]))
  output_unadj[25,"N answers"] <- paste0(table(data_reg$employment_cat)[2])
  
  output_unadj[26,1] <- " "
  output_unadj[26,"Factor"] <- paste0(names(table(data_reg$employment_cat)[3]))
  output_unadj[26,"N answers"] <- paste0(table(data_reg$employment_cat)[3])
  
  output_unadj[27,1] <- ""
  output_unadj[27,"Factor"] <- paste0(names(table(data_reg$employment_cat)[4]))
  output_unadj[27,"N answers"] <- paste0(table(data_reg$employment_cat)[4])
  
  output_unadj[28,1] <- " "
  output_unadj[28,"Factor"] <- paste0(names(table(data_reg$employment_cat)[5]))
  output_unadj[28,"N answers"] <- paste0(table(data_reg$employment_cat)[5])
  
  output_unadj[29,1] <- " "
  output_unadj[29,"Factor"] <- paste0(names(table(data_reg$employment_cat)[6]))
  output_unadj[29,"N answers"] <- paste0(table(data_reg$employment_cat)[6])
  
  output_unadj[30,1] <- paste0("Household income, net\nReference: ",names(table(data_reg$household_income_3cat)[1])," CHF")
  output_unadj[30,"Factor"] <- paste0(names(table(data_reg$household_income_3cat)[2])," CHF")
  output_unadj[30,"N answers"] <- paste0(table(data_reg$household_income_3cat)[2])
  
  output_unadj[31,1] <- " "
  output_unadj[31,"Factor"] <- paste0(names(table(data_reg$household_income_3cat)[3])," CHF")
  output_unadj[31,"N answers"] <- paste0(table(data_reg$household_income_3cat)[3])
  
  output_unadj[32,1] <- " "
  output_unadj[32,"Factor"] <- paste0("", names(table(data_reg$household_income_3cat)[4]))
  output_unadj[32,"N answers"] <- paste0(table(data_reg$household_income_3cat)[4])
  
  output_unadj[33,1] <- "Household size "
  output_unadj[33,"Factor"] <- paste0("Mean (range)")
  output_unadj[33,"N answers"] <- paste0(round(mean(data_reg$household_size))," (",min(data_reg$household_size)," - ",max(data_reg$household_size),")")
  
  output_unadj[34,1] <- paste0("Household with medically vulnerability \nReference: ",names(table(data_reg$household_riskgroup)[1]))
  output_unadj[34,"Factor"] <- paste0("", names(table(data_reg$household_riskgroup)[2]))#"One or more person in a risk group"
  output_unadj[34,"N answers"] <- paste0(table(data_reg$household_riskgroup)[2])
  
  output_unadj[35,1] <- paste0("Testing for SARS-CoV-2\nReference: ",names(table(data_reg$prehistory)[1]))
  output_unadj[35,"Factor"] <- paste0(names(table(data_reg$prehistory)[2]))
  output_unadj[35,"N answers"] <- paste0(table(data_reg$prehistory)[2])
  
  output_unadj[36,1] <- " "
  output_unadj[36,"Factor"] <- paste0(names(table(data_reg$prehistory)[3]))
  output_unadj[36,"N answers"] <- paste0(table(data_reg$prehistory)[3])
  
  output_unadj[37,1] <- " "
  output_unadj[37,"Factor"] <- paste0(names(table(data_reg$prehistory)[4]))
  output_unadj[37,"N answers"] <- paste0(table(data_reg$prehistory)[4])
  
  output_unadj[38,1] <- paste0("Number of contacts per day\nReference: ",names(table(data_reg$contact_cat)[1]))
  output_unadj[38,"Factor"] <- paste0(names(table(data_reg$contact_cat)[2]))
  output_unadj[38,"N answers"] <- paste0(table(data_reg$contact_cat)[2])
  
  output_unadj[39,1] <- " "
  output_unadj[39,"Factor"] <- paste0(names(table(data_reg$contact_cat)[3]))
  output_unadj[39,"N answers"] <- paste0(table(data_reg$contact_cat)[3])
  
  output_unadj[40,1] <- paste0("Attitudes towards COVID-19 measures \nReference: ",names(table(data_reg$agreement_measures_3cat)[1]))
  output_unadj[40,"Factor"] <- paste0(names(table(data_reg$agreement_measures_3cat)[2]))
  output_unadj[40,"N answers"] <- paste0(table(data_reg$agreement_measures_3cat)[2])
  
  output_unadj[41,1] <- "   "
  output_unadj[41,"Factor"] <- paste0(names(table(data_reg$agreement_measures_3cat)[3]))
  output_unadj[41,"N answers"] <- paste0(table(data_reg$agreement_measures_3cat)[3])
  
  output_unadj[42,1] <- " "
  output_unadj[42,"Factor"] <- paste0(names(table(data_reg$agreement_measures_3cat)[4]))
  output_unadj[42,"N answers"] <- paste0(table(data_reg$agreement_measures_3cat)[4])
  
  output_tte<- cbind(output_unadj[,c(1,8,9,7)],output_unadj_ipw[,c(7)],output_adj[,c(7)],output_adj_ipw[,c(7)])
  output_tte[is.na(output_tte)] <- " "
  colnames(output_tte) <- c("Name","Factor","N answers", "Unadjusted HR","Unadjusted weighted HR","Adjusted HR","Adjusted weighted HR")
  cox_regressions <-cbind(output_tte[c(1:3, 4,7)])[-c(1:5),]# Combine cox regression models
  write.csv(cox_regressions, "./output/tables/vaccination_uptake/SupTable4.csv")
  
  return(output_tte)
  
}
