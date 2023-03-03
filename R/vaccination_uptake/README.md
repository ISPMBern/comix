
**First project: Vaccination uptake within the CoMix study in Switzerland**
####Sociodemographic characteristics associated with COVID-19 vaccination uptake in Switzerland: longitudinal analysis within the CoMix survey from June to September 2021
**Authors:** Martina L Reichmuth, Leonie Heron, Julien Riou, André Moser, Anthony Hauser, Nicola Low, Christian L Althaus
**Correspondence:**martina.reichmuth@ispm.unibe.ch


**Abstract**

**Background:** Vaccine hesitancy was high in the Swiss population before and during the COVID-19 pandemic and may have hampered control efforts. We studied socio-demographic characteristics, social contact behaviour and COVID-19 vaccine uptake in adults living in Switzerland between June 2021 and September 2021.

**Methods:** We conducted a longitudinal online survey in the Swiss population, sampled to represent national distributions of age, gender, and region of residence between June and September 2021. Participants provided sociodemographic information, history of testing for SARS-CoV-2, social contact behaviour, such as number of physical contacts, vaccination status, and willingness to be vaccinated. We calculated the adjusted rate ratio of vaccination using Poisson regression, and an interaction term for age and time.

**Results:** We recorded 6,758 observations from 1,884 adults. For the regression analysis, we included 3,513 observations from 1,883 participants. In September 2021, 75\% of the participants had at least one vaccine dose. Using a multivariable model, vaccination rate was higher in older age groups, individuals identifying as male, students, individuals with a high-economic and educational status, individuals that had many contacts, and individuals living in a household with medically vulnerable individuals. Vaccination rate was lower in individuals living in rural areas, in smaller households, and those who perceived COVID-19 measures as too strict.

**Conclusions:** In addition to population-related characteristics, individual attitudes affect the association to get vaccinated. The combination of population- and individual-level reasons for decision-making requires appropriate communication with the public to ensure that public health interventions are accepted and implemented by the public. Tailored COVID-19 vaccination strategies in Switzerland should target specific population subgroups such as women, people from rural areas or people with lower socio-economic status.

**Keywords:** Vaccine, COVID-19, contact survey, social contact, socio-demographic characteristics, Switzerland


**Method: R Code**
If you are interested in using the R code, please make sure:
**1.** you installed all packages from R/comix_000_initialize.R
**2.** You can run the code from R/01_data_mgmt.R
	**2.1** R/vaccination_uptake/comix_010_cleaning_regression.R
	Prepares the data for the regression models, includes the inverse probability weighting analysis, describes the data
	**2.1.1** R/vaccination_uptake/comix_011_vaccination_coxpropreg.R
	Includes the Cox proportional hazard regression model to analyse the vaccine uptake
	**2.1.2** R/vaccination_uptake/comix_012_missingparticipating.R
	Includes the logistic regression model to analyse the missigness (not attending in all survey waves after recruitment)
	**2.1.3** R/vaccination_uptake/comix_013_vaccination_poissonregression.R
	Includes the point process using Poisson regression model to analyse the vaccine uptake
	
	