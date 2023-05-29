
**First project: Vaccination uptake within the CoMix study in Switzerland**


#### Socio-demographic characteristics associated with COVID-19 vaccination uptake in Switzerland: longitudinal analysis as part of the CoMix study

**Authors:** Martina L Reichmuth, Leonie Heron, Julien Riou, André Moser, Anthony Hauser, Nicola Low, Christian L Althaus

**Correspondence:** martina.reichmuth@unibe.ch


**Abstract**

**Background:** Vaccination is an effective strategy to reduce morbidity and mortality from coronavirus disease 2019 (COVID-19). However, the uptake of COVID-19 vaccination has varied across and within countries. Switzerland has had lower levels of COVID-19 vaccination uptake in the general population than many other high-income countries. Understanding the socio-demographic factors associated with vaccination uptake can help to inform future vaccination strategies to increase uptake.

**Methods:** We conducted a longitudinal online survey in the Swiss population, consisting of six survey waves from June to September 2021. Participants provided information on socio-demographic characteristics, history of testing for severe acute respiratory syndrome coronavirus 2 (SARS-CoV-2), social contacts, willingness to be vaccinated, and vaccination status. We used a multivariable Poisson regression model to estimate the adjusted rate ratio (aRR) and 95\% confidence intervals (CI) of COVID-19 vaccine uptake.

**R Code:**

If you are interested in using the R code, please make sure:\\
**1.** you installed all packages from R/comix_000_initialize.R\\
**2.** You can run the code from R/01_data_mgmt.R\\
	**2.1** R/vaccination_uptake/comix_010_cleaning_regression.R\\
	Prepares the data for the regression models, includes the inverse probability weighting analysis, describes the data\\
	**2.1.1** R/vaccination_uptake/comix_011_vaccination_coxpropreg.R\\
	Includes the Cox proportional hazard regression model to analyse the vaccine uptake\\
	**2.1.2** R/vaccination_uptake/comix_012_missingparticipating.R\\
	Includes the logistic regression model to analyse the missigness (not attending in all survey waves after recruitment)\\
	**2.1.3** R/vaccination_uptake/comix_013_vaccination_poissonregression.R\\
	Includes the point process using Poisson regression model to analyse the vaccine uptake\\
	

**Results:** We recorded 6,758 observations from 1,884 adults. For the regression analysis, we included 3,513 observations from 1,883 participants. By September 2021, 600 (75\%) of 806 study participants had received at least one vaccine dose. Participants who were older, male, and students, had a higher education, household income, and number of social contacts, and lived in a household with a medically vulnerable person were more likely to have received at least one vaccine dose. Female participants, those who lived in rural areas and smaller households, and people who perceived COVID-19 measures as being too strict were less likely to be vaccinated. We found no significant association between previous SARS-CoV-2 infections and vaccination uptake.

**Conclusions:** Our results suggest that socio-demographic factors as well as individual behaviours and attitudes played an important role in COVID-19 vaccination uptake in Switzerland. Therefore, appropriate communication with the public is needed to ensure that public health interventions are accepted and implemented by the population. Tailored COVID-19 vaccination strategies in Switzerland that aim to improve uptake should target specific subgroups such as women, people from rural areas or people with lower socio-demographic status.

**Keywords:** Vaccine, COVID-19, contact survey, social contact, socio-demographic characteristics, Switzerland


Please cite: [Reichmuth et al. 2023]()
	
	