#' ---
#' title: "Regression model to find socio-demographic factor associated with participation and the CoMix study in Switzerland"
#' author: "Martina Reichmuth"
#' date: "'01/12/2021"
#' ---

comix_012_missingparticipating = function(data_reg) {
  ##############################################
  # Association of missing surveys on primary outcome (vaccination uptake)
  ##############################################
  
  # get last observation of each participant incl in regression model
  data_reg_last_id <- data_reg %>% 
    group_by(part_id) %>% 
    arrange(date_num) %>%  
    slice(n())
  # variables used for regression modeling
  variables <- c("age_bands",output_tte[[1]], "vaccinated")
  variables <- variables[c(1:10,14,11,12,13)] 
  output_unadj <- c()
  for(i in variables){
    sub_set <- data_reg_last_id[c(grep("missing|",colnames(data_reg_last_id)),grep(i,colnames(data_reg_last_id)))]
    colnames(sub_set)[grep(i,colnames(sub_set))] <- "variable"
    
    log_reg <-  glm(missing ~ variable, data= sub_set, family = binomial)#glm(missing ~  (1|part_id)+variable , weights = ipw, data= sub_set)
    
    output <- data.frame(matrix(0, ncol = 8, nrow =length(names(coef(log_reg)))))
    colnames(output) <- c("group","subgroup","names", "OR","CI.2.5.", "CI.97.5.", "p.value", "significants")
    sum_reg <- summary(log_reg)
    output$names <- names(coef(log_reg))
    output$OR <- coef(log_reg)
    output$CI.2.5. <-  coef(log_reg) - qnorm(0.975) * sqrt(diag(vcov(log_reg)))
    output$CI.97.5. <- coef(log_reg) + qnorm(0.975) * sqrt(diag(vcov(log_reg)))
    output[,c(4:6)] <- exp(output[,c(4:6)])
    output$p.value <- sum_reg$coefficients[,"Pr(>|z|)"]
    output$significants <- sapply(output$p.value, FUN_sig)
    output<- output[!grepl("ntercept",output$names),]# only variable
    output$oddsCI <- paste0(format(round(output$OR,2), nsmall = 2), " (", format(round(output$CI.2.5.,2), 2), "-", format(round(output$CI.97.5.,2),  2), ")")
    output_unadj <-rbind(output_unadj, output)
  }
  
  
  for(n in 1:2){
    output_adj <- c()
    if(n==1){
      log_reg <-  glm(missing ~   . ,
                      data= data_reg_last_id[,c("missing",variables)])
    }
    if(n==2){
      log_reg <-  glm(missing ~   . ,
                      data= data_reg_last_id[,c("missing",variables[-c(11,13:14)])])
    }
    s <- summary(log_reg, corr = TRUE)
    output <- data.frame(matrix(0, ncol = 8, nrow =length(names(coef(log_reg)))))
    colnames(output) <- c("group","subgroup","names", "OR","CI.2.5.", "CI.97.5.", "p.value", "significants")
    sum_reg <- summary(log_reg)
    output$names <- names(coef(log_reg))
    output$OR <- coef(log_reg)
    output$CI.2.5. <-  coef(log_reg) - qnorm(0.975) * sqrt(diag(vcov(log_reg)))
    output$CI.97.5. <- coef(log_reg) + qnorm(0.975) * sqrt(diag(vcov(log_reg)))
    output[,c(4:6)] <- exp(output[,c(4:6)])
    output$p.value <- sum_reg$coefficients[,"Pr(>|t|)"]
    output$significants <- sapply(output$p.value, FUN_sig)
    output<- output[!grepl("ntercept",output$names),]# only variable
    
    if(n==1){
      output_adj <- output
      output_adj$oddsCI1 <- paste0(format(round(output_adj$OR,2), nsmall = 2), " (", format(round(output_adj$CI.2.5.,2), nsmall = 2), "-",format(round(output_adj$CI.97.5.,2), nsmall = 2), ")")
      output_adj_variablex <- output_adj
    }
    if(n==2){
      output_adj_novariablex <- output
      output_adj_novariablex$oddsCI1 <- paste0(format(round(output_adj_novariablex$OR,2), nsmall = 2), " (", format(round(output_adj_novariablex$CI.2.5.,2), nsmall = 2), "-",format(round(output_adj_novariablex$CI.97.5.,2), nsmall = 2), ")")
      output_adj_novariablex[length(output_adj_variablex[,1]),] <- NA
    }
  }
  output_adj<-  cbind(output_adj_novariablex,output_adj_variablex)
  
  
  
  output <- data.frame(matrix(NA, ncol = 7, nrow =length(output_adj)))
  colnames(output) <- c("Category","Variables","Number participants","Number observations", "Univariable OR (95%-CI)", "aOR (95% CI) without time varying variables", "aOR (95% CI) with time varying variables")
  
  output[1,1] <- paste0("Age groups (years) \nReference: ",names(table(data_reg_last_id$age_bands)[1]))
  output[1,2] <- paste0(names(table(data_reg_last_id$age_bands)[2]))
  output[1,"Number participants"] <- paste0(table(data_reg_last_id$age_bands[!duplicated(data_reg_last_id$part_id)])[2])
  output[1,"Number observations"] <- paste0(table(data_reg_last_id$age_bands)[2])
  
  output[2,1] <- " "
  output[2,2] <- paste0(names(table(data_reg_last_id$age_bands)[3]))
  output[2,"Number participants"] <- paste0(table(data_reg_last_id$age_bands[!duplicated(data_reg_last_id$part_id)])[3])
  output[2,"Number observations"] <- paste0(table(data_reg_last_id$age_bands)[3])
  
  output[3,1] <- " "
  output[3,2] <- paste0(names(table(data_reg_last_id$age_bands)[4]))
  output[3,"Number participants"] <- paste0(table(data_reg_last_id$age_bands[!duplicated(data_reg_last_id$part_id)])[4])
  output[3,"Number observations"] <- paste0(table(data_reg_last_id$age_bands)[4])
  
  output[4,1] <- " "
  output[4,2] <- paste0(names(table(data_reg_last_id$age_bands)[5]))
  output[4,"Number participants"] <- paste0(table(data_reg_last_id$age_bands[!duplicated(data_reg_last_id$part_id)])[5])
  output[4,"Number observations"] <- paste0(table(data_reg_last_id$age_bands)[5])
  
  output[5,1] <- " "
  output[5,2] <- paste0(names(table(data_reg_last_id$age_bands)[6]))
  output[5,"Number participants"] <- paste0(table(data_reg_last_id$age_bands[!duplicated(data_reg_last_id$part_id)])[6])
  output[5,"Number observations"] <- paste0(table(data_reg_last_id$age_bands)[6])
  
  
  output[6,1] <- "Gender\nReference: Female"
  output[6,2] <- paste0(names(table(data_reg_last_id$sex)[2]))
  output[6,"Number participants"] <- paste0(table(data_reg_last_id$sex[!duplicated(data_reg_last_id$part_id)])[2])
  output[6,"Number observations"] <- paste0(table(data_reg_last_id$sex)[2])
  
  output[7,1] <- " "
  output[7,2] <- "Others"
  output[7,"Number participants"] <- paste0(table(data_reg_last_id$sex[!duplicated(data_reg_last_id$part_id)])[3])
  output[7,"Number observations"] <- paste0(table(data_reg_last_id$sex)[3])
  
  output[8,1] <- "Region\nReference: Urban"
  output[8,2] <-  paste0(names(table(data_reg_last_id$region)[2]))
  output[8,"Number participants"] <-  paste0(table(data_reg_last_id$region[!duplicated(data_reg_last_id$part_id)])[2])
  output[8,"Number observations"] <-  paste0(table(data_reg_last_id$region)[2])
  
  output[9,1] <-paste0("Swiss region ","\nReference: ",names(table(data_reg_last_id$grossregion)[1]))
  output[9,2] <-  paste0(names(table(data_reg_last_id$grossregion)[2]))
  output[9,"Number participants"] <-  paste0(table(data_reg_last_id$grossregion[!duplicated(data_reg_last_id$part_id)])[2])
  output[9,"Number observations"] <-  paste0(table(data_reg_last_id$grossregion)[2])
  
  output[10,1] <- " "
  output[10,2] <- paste0(names(table(data_reg_last_id$grossregion)[3]))
  output[10,"Number participants"] <-  paste0(table(data_reg_last_id$grossregion[!duplicated(data_reg_last_id$part_id)])[3])
  output[10,"Number observations"] <-  paste0(table(data_reg_last_id$grossregion)[3])
  
  output[11,1] <- " "
  output[11,2] <- paste0(names(table(data_reg_last_id$grossregion)[4]))
  output[11,"Number participants"] <-  paste0(table(data_reg_last_id$grossregion[!duplicated(data_reg_last_id$part_id)])[4])
  output[11,"Number observations"] <-  paste0(table(data_reg_last_id$grossregion)[4])
  
  output[12,1] <- " "
  output[12,2] <- paste0(names(table(data_reg_last_id$grossregion)[5]))
  output[12,"Number participants"] <-  paste0(table(data_reg_last_id$grossregion[!duplicated(data_reg_last_id$part_id)])[5])
  output[12,"Number observations"] <-  paste0(table(data_reg_last_id$grossregion[!duplicated(data_reg_last_id$part_id)])[5])
  
  output[13,1] <- " "
  output[13,2] <- paste0(names(table(data_reg_last_id$grossregion)[6]))
  output[13,"Number participants"] <-  paste0(table(data_reg_last_id$grossregion[!duplicated(data_reg_last_id$part_id)])[6])
  output[13,"Number observations"] <-  paste0(table(data_reg_last_id$grossregion)[6])
  
  output[14,1] <- " "
  output[14,2] <- paste0(names(table(data_reg_last_id$grossregion)[7]))
  output[14,"Number participants"] <- paste0(table(data_reg_last_id$grossregion[!duplicated(data_reg_last_id$part_id)])[7])
  output[14,"Number observations"] <- paste0(table(data_reg_last_id$grossregion)[7])
  
  output[15,1] <- paste0("Country of birth\nReference: ",names(table(data_reg_last_id$country_cat_birth)[1]))
  output[15,2] <- paste0(names(table(data_reg_last_id$country_cat_birth)[2]))
  output[15,"Number participants"] <- paste0(table(data_reg_last_id$country_cat_birth[!duplicated(data_reg_last_id$part_id)])[2])
  output[15,"Number observations"] <- paste0(table(data_reg_last_id$country_cat_birth)[2])
  
  output[16,1] <- " "
  output[16,2] <- paste0(names(table(data_reg_last_id$country_cat_birth)[3]))
  output[16,"Number participants"] <- paste0(table(data_reg_last_id$country_cat_birth[!duplicated(data_reg_last_id$part_id)])[3])
  output[16,"Number observations"] <- paste0(table(data_reg_last_id$country_cat_birth)[3])
  
  output[17,1] <- " "
  output[17,2] <- paste0(names(table(data_reg_last_id$country_cat_birth)[4]))
  output[17,"Number participants"] <- paste0(table(data_reg_last_id$country_cat_birth[!duplicated(data_reg_last_id$part_id)])[4])
  output[17,"Number observations"] <- paste0(table(data_reg_last_id$country_cat_birth)[4])
  
  output[18,1] <- paste0("Education level\nReference: Lowest level")
  output[18,2] <- paste0("Middle level of education")
  output[18,"Number participants"] <- paste0(table(data_reg_last_id$education_level3[!duplicated(data_reg_last_id$part_id)])[2])
  output[18,"Number observations"] <- paste0(table(data_reg_last_id$education_level3)[2])
  
  output[19,1] <- " "
  output[19,2] <- paste0("Highest level of education")
  output[19,"Number participants"] <- paste0(table(data_reg_last_id$education_level3[!duplicated(data_reg_last_id$part_id)])[3])
  output[19,"Number observations"] <- paste0(table(data_reg_last_id$education_level3)[3])
  
  output[20,1] <- paste0("Employment status\nReference: ",names(table(data_reg_last_id$employment_cat)[1]))
  output[20,2] <- paste0(names(table(data_reg_last_id$employment_cat)[2]))
  output[20,"Number participants"] <- paste0(table(data_reg_last_id$employment_cat[!duplicated(data_reg_last_id$part_id)])[2])
  output[20,"Number observations"] <- paste0(table(data_reg_last_id$employment_cat)[2])
  
  output[21,1] <- " "
  output[21,2] <- paste0(names(table(data_reg_last_id$employment_cat)[3]))
  output[21,"Number participants"] <- paste0(table(data_reg_last_id$employment_cat[!duplicated(data_reg_last_id$part_id)])[3])
  output[21,"Number observations"] <- paste0(table(data_reg_last_id$employment_cat)[3])
  
  output[22,1] <- ""
  output[22,2] <- paste0(names(table(data_reg_last_id$employment_cat)[4]))
  output[22,"Number participants"] <- paste0(table(data_reg_last_id$employment_cat[!duplicated(data_reg_last_id$part_id)])[4])
  output[22,"Number observations"] <- paste0(table(data_reg_last_id$employment_cat)[4])
  
  output[23,1] <- " "
  output[23,2] <- paste0(names(table(data_reg_last_id$employment_cat)[5]))
  output[23,"Number participants"] <- paste0(table(data_reg_last_id$employment_cat[!duplicated(data_reg_last_id$part_id)])[5])
  output[23,"Number observations"] <- paste0(table(data_reg_last_id$employment_cat)[5])
  
  output[24,1] <- " "
  output[24,2] <- paste0(names(table(data_reg_last_id$employment_cat)[6]))
  output[24,"Number participants"] <- paste0(table(data_reg_last_id$employment_cat[!duplicated(data_reg_last_id$part_id)])[6])
  output[24,"Number observations"] <- paste0(table(data_reg_last_id$employment_cat)[6])
  
  output[25,1] <- paste0("Household\nReference: ",names(table(data_reg_last_id$household_income_3cat)[1])," CHF")
  output[25,2] <- paste0(names(table(data_reg_last_id$household_income_3cat)[2])," CHF")
  output[25,"Number participants"] <- paste0(table(data_reg_last_id$household_income_3cat[!duplicated(data_reg_last_id$part_id)])[2])
  output[25,"Number observations"] <- paste0(table(data_reg_last_id$household_income_3cat)[2])
  
  output[26,1] <- " "
  output[26,2] <- paste0(names(table(data_reg_last_id$household_income_3cat)[3])," CHF")
  output[26,"Number participants"] <- paste0(table(data_reg_last_id$household_income_3cat[!duplicated(data_reg_last_id$part_id)])[3])
  output[26,"Number observations"] <- paste0(table(data_reg_last_id$household_income_3cat)[3])
  
  output[27,1] <- " "
  output[27,2] <- paste0("", names(table(data_reg_last_id$household_income_3cat)[4]))
  output[27,"Number participants"] <- paste0(table(data_reg_last_id$household_income_3cat[!duplicated(data_reg_last_id$part_id)])[4])
  output[27,"Number observations"] <- paste0(table(data_reg_last_id$household_income_3cat)[4])
  
  output[28,1] <- " "
  output[28,2] <- paste0("Household size")
  output[28,"Number participants"] <- paste0(round(mean(data_reg_last_id$household_size[!duplicated(data_reg_last_id$part_id)]))," (",min(data_reg_last_id$household_size[!duplicated(data_reg_last_id$part_id)])," - ",max(data_reg_last_id$household_size[!duplicated(data_reg_last_id$part_id)]),")")
  output[28,"Number observations"] <- paste0(round(mean(data_reg_last_id$household_size))," (",min(data_reg_last_id$household_size)," - ",max(data_reg_last_id$household_size),")")
  
  output[29,1] <- "  \nReference: Nonvulnerable  population"
  output[29,2] <- "Vulnerable population"
  output[29,"Number participants"] <- paste0(table(data_reg_last_id$household_riskgroup[!duplicated(data_reg_last_id$part_id)])[2])
  output[29,"Number observations"] <- paste0(table(data_reg_last_id$household_riskgroup)[2])
  
  output[30,1] <- paste0("Testing for SARS-CoV-2\nReference: ",names(table(data_reg_last_id$prehistory)[1]))
  output[30,2] <- paste0(names(table(data_reg_last_id$prehistory)[2]))
  output[30,"Number observations"] <- paste0(table(data_reg_last_id$prehistory)[2])
  
  output[31,1] <- " "
  output[31,2] <- paste0(names(table(data_reg_last_id$prehistory)[3]))
  output[31,"Number observations"] <- paste0(table(data_reg_last_id$prehistory)[3])
  
  output[32,1] <- " "
  output[32,2] <- paste0(names(table(data_reg_last_id$prehistory)[4]))
  output[32,"Number observations"] <- paste0(table(data_reg_last_id$prehistory)[4])
  
  output[33,1] <- "Vaccination status\nReference: Not vaccinated "
  output[33,2] <- paste0("Vaccinated")
  output[33,"Number participants"] <- paste0(table(data_reg_last_id$vaccinated)[2])
  output[33,"Number observations"] <- paste0(table(data_reg_last_id$vaccinated)[2])
  
  
  output[34,1] <- paste0("Number of contacts\nReference: ",names(table(data_reg_last_id$contact_cat)[1]))
  output[34,2] <- paste0(names(table(data_reg_last_id$contact_cat)[2]))
  output[34,"Number observations"] <- paste0(table(data_reg_last_id$contact_cat)[2])
  
  output[35,1] <- " "
  output[35,2] <- paste0(names(table(data_reg_last_id$contact_cat)[3]))
  output[35,"Number observations"] <- paste0(table(data_reg_last_id$contact_cat)[3])
  
  output[36,1] <- paste0("Perception of COVID-19 measures \nReference: ",names(table(data_reg_last_id$agreement_measures_3cat)[1]))
  output[36,2] <- paste0(names(table(data_reg_last_id$agreement_measures_3cat)[2]))
  output[36,"Number observations"] <- paste0(table(data_reg_last_id$agreement_measures_3cat)[2])
  
  output[37,1] <- "   "
  output[37,2] <- paste0(names(table(data_reg_last_id$agreement_measures_3cat)[3]))
  output[37,"Number observations"] <- paste0(table(data_reg_last_id$agreement_measures_3cat)[3])
  
  output[38,1] <- " "
  output[38,2] <- paste0(names(table(data_reg_last_id$agreement_measures_3cat)[4]))
  output[38,"Number observations"] <- paste0(table(data_reg_last_id$agreement_measures_3cat)[4])
  
  output[is.na(output)] <- "-"
  output$`Number observations`[-28] <- format(as.numeric(output$`Number observations`[-28]), nsmall=0, big.mark=",")
  output[is.na(output)] <- "-"
  
  print(table(data_reg_last_id$drops))
  print(round(table(data_reg_last_id$drops)/sum(table(data_reg_last_id$drops))*100,2))
  print(table(data_reg_last_id$missing))
  print(round(table(data_reg_last_id$missing)/sum(table(data_reg_last_id$missing))*100,2))
  
  output[5] <- output_unadj$oddsCI
  output[6] <- output_adj_novariablex$oddsCI
  output[7] <- output_adj_variablex$oddsCI
  
  write.csv(output, "../../tables/vaccination_uptake/SupTable3.csv")
  return(output)
}
