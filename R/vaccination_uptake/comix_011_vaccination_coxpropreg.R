#' ---
#' title: "Initialize"
#' author: "mreichmuth"
#' date: "19/10/2021"
#' ---

comix_011_vaccination_coxpropreg = function(data_reg) {
  ##############################################
  # Cox proportional hazard regression (with IPWC)
  ##############################################
  
  # keep data when vaccinated or latest
  data_reg <- data_reg %>% group_by(part_id) %>% filter(date_num<=timetoevent | is.na(timetoevent)) 
  data_reg <- data_reg %>% group_by(part_id) %>% filter(date_num == max(date_num)) 
  
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
  
  output_tte1<- cbind(output_adj[,1],output_unadj[,c(6,7)],output_unadj_ipw[,c(6,7)],output_adj[,c(6,7)],output_adj_ipw[,c(6,7)])
  output_tte1[is.na(output_tte1)] <- " "
  colnames(output_tte1) <- c("Name", "P-value of unadjusted HR","Unadjusted HR","P-value of unadjusted weighted HR","Unadjusted weighted HR","P-value of adjusted HR", "Adjusted HR","P-value of adjusted weighted HR","Adjusted weighted HR")
  return(list(model_variables,output_tte1))
  
}
