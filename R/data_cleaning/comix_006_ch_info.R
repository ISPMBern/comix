#' ---
#' title: "Load Swiss population data"
#' author: "Martina Reichmuth"
#' date: "05/11/2021"
#' ---

 
comix_006_ch_info = function() {

  ## Swiss SARS-CoV-2  metadata:
  url <- readLines("https://www.covid19.admin.ch/api/data/context", warn=FALSE)
  url <- gsub("^(.*)https:", "https:", url[20])
  url <- gsub("\"", "",url)
  url <- gsub(",", "",url)
  download(url, dest=paste0("covid19_bag_", Sys.Date(),".zip"), mode="wb") 
  unzip(paste0("covid19_bag_", Sys.Date(),".zip"), exdir = "./temp_data")
  
  BAG_cases_canton <- read.csv("./temp_data/data/COVID19Cases_geoRegion_w.csv")
  colnames(BAG_cases_canton)[2] <- "week"
  BAG_cases_canton$week <- gsub(53,52,BAG_cases_canton$week)
  colnames(BAG_cases_canton)[3] <- "cases_num"
  BAG_cases_canton <- BAG_cases_canton[BAG_cases_canton$geoRegio %in% ch_cantons,]
  
  BAG_cases_canton_d <- read.csv("./temp_data/data/COVID19Cases_geoRegion.csv")
  colnames(BAG_cases_canton_d)[2] <- "date"
  colnames(BAG_cases_canton_d)[3] <- "cases_num"
  BAG_cases_canton_d <- BAG_cases_canton_d[BAG_cases_canton_d$geoRegio %in% ch_cantons,]
  
  
  BAG_test_canton <- read.csv("./temp_data/data/COVID19Test_geoRegion_PCR_Antigen_w.csv")
  colnames(BAG_test_canton)[2] <- "week"
  BAG_test_canton$week <- gsub(53,52,BAG_test_canton$week)
  BAG_test_canton <- BAG_test_canton[BAG_test_canton$geoRegio %in% ch_cantons,]
  
  BAG_test_canton_pcr <- BAG_test_canton[BAG_test_canton$nachweismethode=="PCR",]
  colnames(BAG_test_canton_pcr)[3] <- "pcrtests_num"
  colnames(BAG_test_canton_pcr)[4] <- "pcrtests_pos_num"
  
  BAG_test_canton_antig <- BAG_test_canton[BAG_test_canton$nachweismethode=="Antigen_Schnelltest",]
  colnames(BAG_test_canton_antig)[3] <- "antigtests_num"
  colnames(BAG_test_canton_antig)[4] <- "antigtests_pos_num"
  BAG_test_canton <- merge(BAG_test_canton_antig[,c(1,2,3,4)], BAG_test_canton_pcr[,c(1,2,3,4)],by=c("week","geoRegion"), all=TRUE )
  BAG_test_canton[is.na(BAG_test_canton)] <- 0
  BAG_test_canton<- BAG_test_canton %>% 
    rowwise() %>% #rowwise will make sure the sum operation will occur on each row
    mutate(tests_num = sum(antigtests_num,pcrtests_num, na.rm=TRUE))%>% 
    mutate(tests_pos_num = sum(antigtests_pos_num,pcrtests_pos_num, na.rm=TRUE))
  BAG_data <- merge(BAG_cases_canton[,c("week","geoRegion","cases_num", "pop")], BAG_test_canton[,c("week","geoRegion","tests_num", "tests_pos_num")],by=c("week","geoRegion"), all=TRUE )
  
  
  BAG_data <- BAG_data[!is.na(BAG_data$cases_num),]
  BAG_data$date <- parse_date_time(paste(BAG_data$week, '-Mon'), "%Y-%W-%a")
  
  
  BAG_data <- BAG_data %>% group_by(week, date, geoRegion) %>% reframe(cases_num = sum(cases_num),
                                                                       tests_num = sum(tests_num),
                                                                       tests_pos_num = sum(tests_pos_num),
                                                                       pop = pop)
  
  swiss_cov_vaccine <- read.csv("./temp_data/data/COVID19FullyVaccPersons_indication_w_v2.csv")
  colnames(swiss_cov_vaccine)[4] <- "fulvacc_num"
  swiss_cov_vaccine <- swiss_cov_vaccine[swiss_cov_vaccine$geoRegio %in% ch_cantons,]
  swiss_cov_vaccine$week <- swiss_cov_vaccine$date
  swiss_cov_vaccine$week <- gsub(53,52,swiss_cov_vaccine$week)
  swiss_cov_vaccine <- swiss_cov_vaccine[swiss_cov_vaccine$indication=="all",]
  swiss_cov_vaccine <- swiss_cov_vaccine %>% group_by(week,geoRegion) %>% summarise(fulvacc_num = sum(fulvacc_num))
  
  swiss_cov_vaccine_doses <- read.csv("./temp_data/data/COVID19VaccPersons_vaccine.csv")#COVID19VaccPersons_v2.csv")
  colnames(swiss_cov_vaccine_doses)[grepl("entries",colnames(swiss_cov_vaccine_doses))] <- "vac_1dos_num" #on 10Nov dedected - for entries?!
  swiss_cov_vaccine_doses <- swiss_cov_vaccine_doses[swiss_cov_vaccine_doses$type=="COVID19AtLeastOneDosePersons",]
  swiss_cov_vaccine_doses <- swiss_cov_vaccine_doses[swiss_cov_vaccine_doses$geoRegio %in% ch_cantons,]
  swiss_cov_vaccine_doses$week <- paste0(year(swiss_cov_vaccine_doses$date),sprintf("%02d",week(swiss_cov_vaccine_doses$date))) 
  swiss_cov_vaccine_doses$week <- gsub(53,52,swiss_cov_vaccine_doses$week)
  swiss_cov_vaccine_doses <- swiss_cov_vaccine_doses %>% group_by(week, geoRegion) %>% reframe(vac_dos_num = sum(vac_1dos_num),
                                                                                               pop = unique(pop))

  BAG_vaccine <- merge(swiss_cov_vaccine[,c("week","geoRegion","fulvacc_num")], swiss_cov_vaccine_doses[,c("week","geoRegion","vac_dos_num")],by=c("week","geoRegion"), all=TRUE )
  BAG_vaccine$date <- parse_date_time(paste(BAG_vaccine$week, '-Mon'), "%Y-%W-%a")
  BAG_vaccine$date <- as_date(BAG_vaccine$date)
  BAG_data <- merge(BAG_data[,c("date","week","geoRegion", "pop","cases_num","tests_num", "tests_pos_num")], BAG_vaccine[,c("date","week","geoRegion","fulvacc_num","vac_dos_num")],by=c("date","week","geoRegion"), all=TRUE )
  BAG_data$date <- as_date(BAG_data$date)

  
  unlink("temp_data", recursive = TRUE)
  unlink(paste0("covid19_bag_", Sys.Date(),".zip"), recursive = TRUE)
  remove(BAG_cases_canton)
  remove(BAG_vaccine)
  remove(swiss_cov_vaccine_doses)
  remove(swiss_cov_vaccine)
  remove(BAG_test_canton_antig)
  remove(BAG_test_canton_pcr)
  remove(BAG_test_canton)  


  
  ## Swiss SARS-CoV-2 sequencing metadata:
  url <- GET("https://lapis.cov-spectrum.org/open/v1/sample/aggregated?country=Switzerland&fields=date,division,pangoLineage")#Used since Oct 2022
  jsonRespParsed<- content(url,as="parsed", encoding="UTF-8") 
  seq_ch <- suppressWarnings(jsonRespParsed%>%bind_rows) #%>%select(date,division,pangolinLineage)# %>%subset(.,country %in% "Switzerland") #%>%
  seq_ch <- seq_ch[,c("date","division","pangoLineage","count")]
  
  seq_ch <- seq_ch[seq_ch$count>0&!is.na(seq_ch$count)&!is.na(seq_ch$date),]
  seq_ch$country <- "CH"
  seq_ch <- seq_ch[rep(row.names(seq_ch), seq_ch$count), c(1,2,3,5)]
  
  seq_ch$week <- paste0(year(seq_ch$date),gsub(53,52,strftime(seq_ch$date, format = "%V")))
  seq_ch$date <- parse_date_time(paste(seq_ch$week, '-Mon'), "%Y-%W-%a")
  seq_ch$geoRegion <- sapply(seq_ch$division,FUN=from_cantons_to_abrev)




## Swiss population information
FUN_gender<- function(x){
  if(is.na(x)){return(NA)}
  else if(x=="weiblich"){return("Female")}
  else if(x=="männlich"){return("Male")}
}
#use data from here: https://www.bfs.admin.ch/bfs/en/home/news/whats-new.assetdetail.23164064.html
swiss_population <- read.csv("./data/raw/CH_population_age_gender_192021_bsf.csv",sep=";")
colnames(swiss_population) <- c("year", "canton", "gender", "age", "population")
swiss_population <- swiss_population[grepl("year", swiss_population$age),]
swiss_population <- swiss_population[!grepl("No indication", swiss_population$canton),]
swiss_population$age <- gsub("[^0-9.-]+", "\\1",swiss_population$age)
#swiss_population$age <- sub("\\years.*","",swiss_population$age)
swiss_population$age <- as.numeric(swiss_population$age)
swiss_population <- swiss_population[swiss_population$year=="2021",]
canton_population <- swiss_population[swiss_population$canton!="Switzerland",]
swiss_population <- swiss_population[swiss_population$canton=="Switzerland",]

canton_population$canton <- sapply(canton_population$canton, from_cantons_to_abrev)

pop_data_age_gender <- as.data.frame(swiss_population %>% group_by(age, gender, year) %>% 
                                            summarise(n = sum(population)) %>% 
                                            `colnames<-`(c("lower.age.limit","gender","year","population")))
colnames(pop_data_age_gender)[grepl("canton", colnames(pop_data_age_gender) )] <- "country"

pop_data_age <- as.data.frame(swiss_population %>% group_by(age, year) %>% 
                                     summarise(n = sum(population)) %>% 
                                     `colnames<-`(c("lower.age.limit", "year", "population")))

colnames(pop_data_age)[grepl("canton", colnames(pop_data_age) )] <- "country"


pop_data_gender <- as.data.frame(swiss_population %>% group_by(gender, year) %>% 
                                        summarise(n = sum(population)) %>% 
                                        `colnames<-`(c("gender", "year","population")))
pop_data_gender[,1]  <- factor(pop_data_gender[,1] , levels = c("Female","Male"))
#pop_data_gender <- pop_data_gender[c(2,1),]

pop_data_residence <- as.data.frame(canton_population %>% group_by(canton, year) %>% 
                                           summarise(n = sum(population)) %>% 
                                           `colnames<-`(c("residence", "year", "population")))

pop_data_residence$pop_data_residence  <- sapply(pop_data_residence$residence, from_cantons_to_abrev)


## KOF stringency, gives information about the measures in place
KOF <- read.csv(paste0("https://datenservice.kof.ethz.ch/api/v1/public/sets/stringency_plus_web?mime=csv&df=Y-m-d.csv"))
KOF[,"date"] <- seq(as_date("2020-01-01"),(as_date("2020-01-01")+length(KOF[,"date"])-1),1)
KOF$date <- as_date(KOF$date)




## Salary of the Swiss population
url <- "https://www.bfs.admin.ch/bfsstatic/dam/assets/12488300/master"# accessed first time on 2021-09-16
GET(url, write_disk(tf <- tempfile(fileext = ".xls")))
salary_ch_bfs_2018 <- read.xlsx(tf,sheetIndex = 1, startRow = 9)
salary_ch_bfs_2018 <- salary_ch_bfs_2018[c(1:21),c(1:7)]
colnames(salary_ch_bfs_2018) <- c("salary_class","total_precent", "cum_precent", "women_total_precent", "women_cum_precent", "men_total_precent", "men_cum_precent")
salary_ch_bfs_2018$country <- "Switzerland"
salary_ch_bfs_2018$year <- "2018" 
salary_ch_bfs_2018$salary_class_min <- as.numeric(gsub("([0-9]+).*$", "\\1", salary_ch_bfs_2018$salary_class))


## level of employment in the Swiss population
#https://www.bfs.admin.ch/bfs/en/home/statistics/catalogues-databases/tables.assetdetail.15844773.html (access 18.10.2021)
#employment_ch_bfs <- read.xlsx("data/ch/employmentlevel_ch_bfs.xlsx",sheetIndex = 2)
url <- "https://www.bfs.admin.ch/bfsstatic/dam/assets/15844773/master"# accessed first time on 2021-09-16
GET(url, write_disk(tf <- tempfile(fileext = ".xls")))
employment_ch_bfs <- read.xlsx(tf,sheetIndex = 2)
employment_ch_bfs_2020 <- employment_ch_bfs[c(14,16,17),c(1,31)]
colnames(employment_ch_bfs_2020) <- c("category", "2020")
employment_ch_bfs_2020[,1] <- c("employmentlevel_90_100","employmentlevel_50_89","employmentlevel_1_49")
remove(employment_ch_bfs)

## education level in the Swiss population (only >=25 years)
#https://www.bfs.admin.ch/bfs/de/home/statistiken/bildung-wissenschaft/bildungsstand.assetdetail.15404053.html (access 18.10.2021)
#education_ch_bfs <- read.xlsx("data/ch/educationlevel_ch_bfs.xlsx",sheetIndex = 2)
url <- "https://www.bfs.admin.ch/bfsstatic/dam/assets/15404053/master"
GET(url, write_disk(tf <- tempfile(fileext = ".xls")))
education_ch_bfs <- read.xlsx(tf,sheetIndex = 2)
education_ch_bfs <- education_ch_bfs[c(3,5),c(4,6,8,10,12,14)] #2019
education_ch_bfs[1,] <- c("Total","Obligatory school", "Vocational education","Gymnasium", "Advanced vocational education", "Higher education (e.g., Bachelor, Master or PhD)")
education_ch_bfs <- as.data.frame(t(education_ch_bfs))
education_ch_bfs$total_25andolder <- round((as.numeric(education_ch_bfs[,2])/as.numeric(education_ch_bfs[1,2])*100),2)
education_ch_bfs$total_pop <- round((as.numeric(education_ch_bfs[,2])/sum(swiss_population$population)*100),2)


#key data 
##NPI
#https://de.wikipedia.org/wiki/COVID-19-Pandemie_in_der_Schweiz (Access: 2021-10-04)
#https://www.bag.admin.ch/bag/en/home/krankheiten/ausbrueche-epidemien-pandemien/aktuelle-ausbrueche-epidemien/novel-cov/massnahmen-des-bundes.html (Access: 2021-07-19)
#2020-10-19: max 15 people  #https://www.bag.admin.ch/bag/de/home/das-bag/aktuell/news/news-18-10-2020.html
#2020-10-29: max 10 people if private otherwise 15 with mask #https://www.srf.ch/news/schweiz/fragen-zu-coronaregeln-darf-mich-die-polizei-jetzt-buessen-wenn-ich-keine-maske-trage 
#2020-11-02: Online learning at higher educational institutes #https://www.srf.ch/news/schweiz/fragen-zu-coronaregeln-darf-mich-die-polizei-jetzt-buessen-wenn-ich-keine-maske-trage 
#2020-12-12: Sperrstunde #https://www.admin.ch/gov/de/start/dokumentation/medienmitteilungen.msg-id-81582.html
#2020-12-22: closure of restaurants outdoors, leisure and sport activities #https://www.bag.admin.ch/bag/de/home/das-bag/aktuell/medienmitteilungen.msg-id-81745.html
#2020-12-21:Entry ban and retroactive quarantine for persons from the UK and South Africa. All persons who have entered Switzerland from these two countries since 14 December must go into quarantine for 10 days.#https://www.admin.ch/gov/en/start/documentation/media-releases.msg-id-81777.html
#2021-01-18: mandatory home-office, max 5 people in- and outside, closure of not shops not for daily use, mandatory mask wearing if not working alone #https://www.bag.admin.ch/bag/de/home/das-bag/aktuell/medienmitteilungen.msg-id-81967.html
#2021-01-28: test are for free also for asymptomatic people #https://www.admin.ch/gov/de/start/dokumentation/medienmitteilungen.msg-id-82136.html
#2021-03-01: max. of 15 people outside, opening of all shops, museums, libraries, zoos, gardens, and outdoor sport and leisure places but with mandatory mask wearing #https://www.bag.admin.ch/bag/de/home/das-bag/aktuell/medienmitteilungen.msg-id-82371.html
#2021-03-22: max. 10 people inside #https://www.bag.admin.ch/bag/de/home/das-bag/aktuell/medienmitteilungen.msg-id-82762.html
#2021-04-07: 5 offered antigen self tests per month #https://www.srf.ch/news/schweiz/coronavirus-selbsttests-fuer-zuhause-ihre-fragen-unsere-antworten
#2021-04-19: openings of restaurants outdoors (4 people per table), leisure and sport activities, events with 15 people (up to 50 resp. 1/3 of capacity or outdoors up to 100 resp. 1/3 of capacity) #https://bag-coronavirus.ch/downloads/ (Access: 2021-05-04) #https://www.bag.admin.ch/bag/de/home/das-bag/aktuell/medienmitteilungen.msg-id-83106.html
#2021-05-31: meets up to 30 and 50 people inside resp. outside (incl. culture and sport activities, but private stays 10 or 15), opening of restaurants (in and outside, 4 people per table), no-mandatory home-office, presence lectures, events with 50 people (up to 100 resp. 1/2 of capacity or outdoors up to 300 resp. 1/2 of capacity), no quarantine for vaccinated individuals #https://www.bag.admin.ch/bag/de/home/das-bag/aktuell/medienmitteilungen.msg-id-83531.html  #https://bag-coronavirus.ch/downloads/ (Access: 2021-06-04)
#2021-06-26: wide re-opening, but covid-19 certificate for large event (with sitting indoor >1000, without sitting >250 or outdoor >500), private gathering (indoors <=30, outdoor <=50), mask wearing indoors #https://www.bag.admin.ch/bag/de/home/das-bag/aktuell/medienmitteilungen.msg-id-84127.html
# in August announced that test want be for free anymore after September
#2021-09-20: testing if coming from abroad if not vaccinated or recovered no matter where coming from #https://www.bag.admin.ch/bag/de/home/das-bag/aktuell/medienmitteilungen.msg-id-85168.html
#2021-10-10: test not payed by Bund anymore #https://www.admin.ch/gov/de/start/dokumentation/medienmitteilungen.msg-id-85254.html
#2021-12-17: 2G 
#2022-04-01: no SARS-CoV-2 restrictions in Switzerland
#2022-10-10: vaccination (booster) recommendation for >65y

#quarantine measures were not explored as not of relevance here.

## PI i.e. vaccine (https://www.bag.admin.ch/bag/en/home/krankheiten/ausbrueche-epidemien-pandemien/aktuelle-ausbrueche-epidemien/novel-cov/impfen.html (access 2021-05-06))
# 2021-01-04 Vaccination program has been started. Especially vulnerable people are being vaccinated first.
# But first vaccination on 23.12.2020 (https://www.swissinfo.ch/eng/first-vaccines-arrive-in-switzerland--first-jabs-on-wednesday/46240828 (access 2021-05-06))


swiss_pop_data <- list(BAG_data, seq_ch,KOF,pop_data_age,pop_data_gender,pop_data_residence,salary_ch_bfs_2018,employment_ch_bfs_2020,education_ch_bfs, pop_data_age_gender, BAG_cases_canton_d)

return(swiss_pop_data)
}
