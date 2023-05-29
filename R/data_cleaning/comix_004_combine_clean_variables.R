#' ---
#' title: "Descriptive output of CoMix in Switzerland"
#' author: "Martina Reichmuth"
#' date: "05/11/2021"
#' ---

comix_004_combine_clean_variables = function(comixdata_cont){


  comixdata_cont$cnt_othersetting2 <-  str_trim(comixdata_cont$cnt_othersetting2, "right")
  

  comixdata_cont$cnt_othersetting21[!is.na(comixdata_cont$cnt_othersetting2)]<- "other"#camping, auto, holidays?, garage?
  comixdata_cont$cnt_othersetting21[grepl("keine Angabe|Nul part|Maintenant la question est plus claire|Sulla strada di casa|Vom Spital zurück Fahrt|Weiss nicht|Wo|Runklar|.| |-",comixdata_cont$cnt_othersetting2,ignore.case = T)] <- "not specified"
  comixdata_cont$cnt_othersetting21[grepl("auto|car|voiture|Mezzo di trasporto privato|Mezzo privato|Fahrzeug|Mi ha dato un passaggio|macchina|Pkw",comixdata_cont$cnt_othersetting2,ignore.case = T)] <- "car"
  comixdata_cont$cnt_othersetting21[grepl("ospedale|Alternativmed.Zentrum|Altersheim|Alterszentrum|Altersheim|Arzt|veterinaire|vétérinaire|COVID|Hôpital|hopital|physio|Spital|Impftentrum|Aerztin|Arzt|Praxis|Zahnarzt|Pflegeheim|Klinik|Krankenhaus|medico|Ospedale|Pharmacie|oftalmologia|Therapie|Gesundheitseinrichtung|USZ|impfzentrum",comixdata_cont$cnt_othersetting2,ignore.case = T)] <- "health care"
  comixdata_cont$cnt_othersetting21[grepl("extérieur|exterieur|All’aperto|l’aperto|Gartenzaun|Seeufer|plain air|plein air|Wanderung|extérieur|exterieur|Parkplatz|Spazier|camminare|marche|rue|route|trotoir|Strasse|Natur|draussen|garten|Jardin|Rheinufer|Sur leur pelouse",comixdata_cont$cnt_othersetting2,ignore.case = T)]<- "outdoors"
  comixdata_cont$cnt_othersetting21[grepl("home|24 Stunden zusammen|maison|chez moi|Haustüre|Wohnungstür|Appartamento|Appartement|Bett|lit|auto|bei mir|Bei uns|Bett|boite aux lettres|C'est mon conjoint|c'est mon mari|Ehepartner|épouse|epouse|C’est moi|C’est un voisin|chez moi|chez nous|Corridor|entrée|notre maison|Den ganzen Tag|domicile|haushalt|en bas de chez moi|Entrata|Entree|Eingang|bei mir|Escalier domicile|Gemeinsamen Wohnung|gemeinsamer Haushalt|Geschlechtsverkehr|eigenen Garten|Haus|Treppenhaus|Wohnung|unserm Garten|unserem Garten|appartement|resté à la maison|vis ave|Lebenspartner|Frau|Mann|Mietwohnung|Mobilhome|vit avec moi|Privat|zu Hause|Zu haue|casa|Überall er ist mein Sohn|bei mir|wohnen zusammen|Zuhause",comixdata_cont$cnt_othersetting2,ignore.case = T)] <- "home"
  comixdata_cont$cnt_othersetting21[grepl("work|Arbeit|Vortrag|travail|lavoro|Archivraum|archive|Atelier|bureau|Baustelle|Kundentermim|Autogarage|Autowerkstatt|Klient|kunde|client|Büro|Buro|Bus|Besprechungsraum|Cantiere in costruzione|garagiste|Feuerwehr|Firma|Geschäft|betreue|Autowerkstatt|eigenen Praxis|Versicherungsagentur|Kantine|konferenz|Lager|Labo|Labor|Lavora|Pausenraum|Werkstatt",comixdata_cont$cnt_othersetting2,ignore.case = T)] <- "work"# also if visited someone at THEIR work? at a conference/Presentaions?
  comixdata_cont$cnt_othersetting21[grepl("église|eglise|Kirche",comixdata_cont$cnt_othersetting2,ignore.case = T)] <- "worship"
  comixdata_cont$cnt_othersetting21[grepl("mes parents|son domicile|Alphütte|Ascenseur|Aufzug|Lift|escaliers|ascensore|domicile de maman|zusanmen im Hotelzimmer|hôtel|hotel|Bei cousin zuhause|Freunden|ihm daheim|ihm zu Hause|Ihnen Zuhause|ihr daheim|ihr zu Hause|Bei Julia|Mutter zuhause|meiner Tochter|Mo zu Hause|Bei oma|Bei Tante|Bei V|BeibOma|Besuch|Cabinet|casa sua|chalet|Chez lui|Chetz lui|Chez lui|Chez  elle|Chez Adele|Chez elle|Chez eux|Chez FB|chez frère|Chez Jacqueline|Chez la personne|Chez ma|Chez mon|Chez Nathalie|Chez Princesse Macha|Chez s|Chez Tati|Chez un|Colonia|immeuble|immmeubje|bâtiment|sa maison|caabinet|cabinet|son jardin|Dans une salle|Devant chez elle|Devant sa maison|Ferienhaus|Ferienwohnung|Freund|Haus des Partners|Hausbesichtigung|Wohnung meiner Mutter|Alphütte|seiner wg|ihrer wg|vivons ensemble|Wohnung der Eltern|Wohnung Franz|Wohnung Strittmatter|Zuhause bei Alexander|Zuhause bei Bruno und Hanny|zuhause bei chrid|zuhause bei Hanny + Bruno|Zuhause bei Matthias und Julia|Zuhause von MT",comixdata_cont$cnt_othersetting2,ignore.case = T)] <- "house" #elevetor, if not specifited where?
  comixdata_cont$cnt_othersetting21[grepl("Restaurant-Terrasse|bibliothèque|cinéma|cinema|Kino|bar|Beim Malkurs|Biblootheque|Bildergalerie|RestaurantClub|Konzert|Fest|Europapark|Eventhalle|Festival|Freizeitpark|Museum|Ratshaus|Eventsaal|sagra|verein|Kulturveranstaltung|Lottosaal|musée|Musee|Museum|Pfadiheim|Rdv social|anlass|Spielhalle|spielplatz|Table extérieur|théatre|theatre|Theater|Zoo",comixdata_cont$cnt_othersetting2,ignore.case = T)] <- "entertainment place"
  comixdata_cont$cnt_othersetting21[grepl("school|crèche|creche|Ecole|schul|garderie|Kindergarten|Kita|Kurs|Scuola",comixdata_cont$cnt_othersetting2,ignore.case = T)] <- "school"
  comixdata_cont$cnt_othersetting21[grepl("ski de fond|local du matériel du rallye|équestre|Reit|Pferd|Maneggio|Formation|Fussball|Hockey|Piscina|piscine|BADEANSTALT|schwimm|Ski|Supermar|Supermercato|tennis",comixdata_cont$cnt_othersetting2,ignore.case = T)] <- "sport"
  comixdata_cont$cnt_othersetting21[grepl("Aereo|Flughafen|Flugzeug|aéroport|aeroport|Bahnhof|cff|sbb|car|train|Zug|Guichet|öv|ov|Tram|Transport mit andere Personen|Transport public",comixdata_cont$cnt_othersetting2,ignore.case = T)] <- "public tansport" # also air planes/aeroport? car auto (language problem?)?
  comixdata_cont$cnt_othersetting21[grepl("magazzino|magasin|Post|bank|banque|Amtsstelle|Notaire|Bäckerei|Baeckerei|boulangerie|forno|Einkaufen|Botschaft|caissi|Einkauf|Früchtestand|Denner|Lidl|laden|supermarkt|Metzgerei|migros|Passbüro|SCAN|administratif|Station essence|station service",comixdata_cont$cnt_othersetting2,ignore.case = T)] <- "shop" # post bank, Amtsstelle?
  comixdata_cont$cnt_othersetting21[grepl("Kiosk|Kisok|kiosque|Autobahnraststätte|distributore benzina|Tankstelle|vide greniers|Coop Center|Konditorei|Magasin d autoroute|Service auto|Souvenirladen|Station de beinzine|Swisscom Shop|Tabac|Telekomshop|Benzin|zollgebäude",comixdata_cont$cnt_othersetting2,ignore.case = T)] <- "shop but not essential"
  comixdata_cont$cnt_othersetting21[grepl("Bains therma|Nagelstudio",comixdata_cont$cnt_othersetting2,ignore.case = T)] <- "aesthetic care"  
  comixdata_cont$cnt_othersetting21[grepl("Telefon|telephone|Skype|aucun|C’était pas hier|erreur|keinen Kontakt|gar nicht|Garnicht|Harnicht|niemand|hier c'était dimanche je ne travaille pas le dimanche|pas vu|sbagliato|Fehler|Q21|n’ai vu personne|kein Kontakt|keine|nein|non|nix|Pas de contact|Pas encore|Pas eu|Pas vu|DA c'est moi !",comixdata_cont$cnt_othersetting2,ignore.case = T)] <- "DELETE"  #Nirgendwo, ich stehe sicher nicht um 5 uhr morgens auf|nirgens

  
  comixdata_cont <- comixdata_cont[comixdata_cont$cnt_othersetting21!="DELETE" | is.na(comixdata_cont$cnt_othersetting21),]
  
  comixdata_cont$cnt_place1[grepl("not specified",comixdata_cont$cnt_othersetting21)] <- "not specified"
  comixdata_cont$cnt_place1[grepl("other",comixdata_cont$cnt_othersetting21)] <- "other"
  comixdata_cont$cnt_place1[grepl("school",comixdata_cont$cnt_othersetting21)] <- "school"
  comixdata_cont$cnt_place1[grepl("Yes",comixdata_cont$cnt_school)] <- "school"
  comixdata_cont$cnt_place1[grepl("work",comixdata_cont$cnt_othersetting21)] <- "work"
  comixdata_cont$cnt_place1[grepl("Yes",comixdata_cont$cnt_work)] <- "work"
  comixdata_cont$cnt_place1[grepl("home",comixdata_cont$cnt_othersetting21)] <- "home"
  comixdata_cont$cnt_place1[grepl("Yes",comixdata_cont$cnt_home)] <- "home"
  comixdata_cont$cnt_place1[grepl("Yes",comixdata_cont$household_member)] <- "home"
  comixdata_cont$cnt_place1[grepl("house",comixdata_cont$cnt_othersetting21)] <- "house"
  comixdata_cont$cnt_place1[grepl("Yes",comixdata_cont$cnt_house)] <- "house"
  comixdata_cont$cnt_place1[grepl("worship",comixdata_cont$cnt_othersetting21)] <- "worship"
  comixdata_cont$cnt_place1[grepl("Yes",comixdata_cont$cnt_worship)] <- "worship"
  comixdata_cont$cnt_place1[grepl("public tansport",comixdata_cont$cnt_othersetting21)] <- "public tansport"
  comixdata_cont$cnt_place1[grepl("Yes",comixdata_cont$cnt_publictansport)] <- "public tansport"
  comixdata_cont$cnt_place1[grepl("shop",comixdata_cont$cnt_othersetting21)] <- "shop"
  comixdata_cont$cnt_place1[grepl("Yes",comixdata_cont$cnt_shop)] <- "shop"
  comixdata_cont$cnt_place1[grepl("shop but not essential",comixdata_cont$cnt_othersetting21)] <- "shop but not essential"
  comixdata_cont$cnt_place1[grepl("Yes",comixdata_cont$cnt_shop_notessential)] <- "shop but not essential"
  comixdata_cont$cnt_place1[grepl("entertainment place",comixdata_cont$cnt_othersetting21)] <- "entertainment place"
  comixdata_cont$cnt_place1[grepl("Yes",comixdata_cont$cnt_entertainmentplace)] <- "entertainment place"
  comixdata_cont$cnt_place1[grepl("shop",comixdata_cont$cnt_othersetting21)] <- "shop"
  comixdata_cont$cnt_place1[grepl("Yes",comixdata_cont$cnt_shop)] <- "shop"
  comixdata_cont$cnt_place1[grepl("sport",comixdata_cont$cnt_othersetting21)] <- "sport"
  comixdata_cont$cnt_place1[grepl("Yes",comixdata_cont$cnt_sport)] <- "sport"
  comixdata_cont$cnt_place1[grepl("aesthetic care",comixdata_cont$cnt_othersetting21)] <- "aesthetic care"
  comixdata_cont$cnt_place1[grepl("Yes",comixdata_cont$cnt_saloncare)] <- "aesthetic care"
  comixdata_cont$cnt_place1[grepl("health care",comixdata_cont$cnt_othersetting21)] <- "health care"
  comixdata_cont$cnt_place1[grepl("Yes",comixdata_cont$cnt_healthcare)] <- "health care"
  comixdata_cont$cnt_place1[grepl("outdoors",comixdata_cont$cnt_othersetting21)] <- "outdoors"
  comixdata_cont$cnt_place1[grepl("Yes",comixdata_cont$cnt_outside)] <- "outdoors"
  
  
  comixdata_cont$cnt_place <- comixdata_cont$cnt_place1
  comixdata_cont$cnt_place[grepl("not specified|car|other",comixdata_cont$cnt_outside)] <- "other"
  
  FUN_cnt_place<- function(x){
    if(is.na(x)){return(NA)}
    else if(x=="school"){return("school")}
    else if(x=="work"){return("work")}
    else if(x=="home"){return("home")}
  else return("other")}
  comixdata_cont$cnt_place_main <- sapply(comixdata_cont$cnt_place,FUN_cnt_place)
  comixdata_cont$cnt_place_main <- factor( comixdata_cont$cnt_place_main, levels = c("home","school","work","other"))
  
  
  ## distance measures 
  # distance measures in meters:
  comixdata_cont$cnt_dis_meters[grepl("Yes",comixdata_cont$household_member,ignore.case = T)] <- "houshold member"
  comixdata_cont$cnt_dis_meters[grepl("Yes",comixdata_cont$cnt_dis_2m_plus,ignore.case = T)] <- "more 2m"
  comixdata_cont$cnt_dis_meters[grepl("Yes",comixdata_cont$cnt_dis_1m_plus,ignore.case = T)] <- "more 1m"
  comixdata_cont$cnt_dis_meters[grepl("Yes",comixdata_cont$cnt_dis_1m_minus,ignore.case = T)] <- "less 1m"
  comixdata_cont$cnt_dis_meters[grepl("Yes",comixdata_cont$cnt_dis_mask,ignore.case = T)] <- "other"
  comixdata_cont$cnt_dis_meters[grepl("Yes",comixdata_cont$cnt_dis_wash_before,ignore.case = T)] <- "other"
  comixdata_cont$cnt_dis_meters[grepl("Yes",comixdata_cont$cnt_dis_wash_after,ignore.case = T)] <- "other"
  comixdata_cont$cnt_dis_meters[grepl("Yes",comixdata_cont$cnt_dis_dontknow,ignore.case = T)] <- "Don't know"
  comixdata_cont$cnt_dis_meters[grepl("Yes",comixdata_cont$cnt_dis_notsaid,ignore.case = T)] <- "Prefer not to say"
  comixdata_cont$cnt_dis_meters[grepl("Yes",comixdata_cont$cnt_dis_other,ignore.case = T)] <- "other"
  
  # hand washing:
  comixdata_cont$cnt_dis_hands[grepl("Yes",comixdata_cont$household_member,ignore.case = T)] <- "houshold member"
  comixdata_cont$cnt_dis_hands[grepl("Yes",comixdata_cont$cnt_dis_2m_plus,ignore.case = T)] <- "other"
  comixdata_cont$cnt_dis_hands[grepl("Yes",comixdata_cont$cnt_dis_1m_plus,ignore.case = T)] <- "other"
  comixdata_cont$cnt_dis_hands[grepl("Yes",comixdata_cont$cnt_dis_1m_minus,ignore.case = T)] <- "other"
  comixdata_cont$cnt_dis_hands[grepl("Yes",comixdata_cont$cnt_dis_mask,ignore.case = T)] <- "other"
  comixdata_cont$cnt_dis_hands[grepl("Yes",comixdata_cont$cnt_dis_wash_before,ignore.case = T)] <- "washed hands before"
  comixdata_cont$cnt_dis_hands[grepl("Yes",comixdata_cont$cnt_dis_wash_after,ignore.case = T)] <- "washed hands after"
  comixdata_cont$cnt_dis_hands[grepl("Yes",comixdata_cont$cnt_dis_dontknow,ignore.case = T)] <- "Don't know"
  comixdata_cont$cnt_dis_hands[grepl("Yes",comixdata_cont$cnt_dis_notsaid,ignore.case = T)] <- "Prefer not to say"
  comixdata_cont$cnt_dis_hands[grepl("Yes",comixdata_cont$cnt_dis_other,ignore.case = T)] <- "other"
  
  # mask wearing:
  comixdata_cont$cnt_dis_mask[grepl("Yes",comixdata_cont$household_member,ignore.case = T)] <- "houshold member"
  comixdata_cont$cnt_dis_mask[grepl("Yes",comixdata_cont$cnt_dis_2m_plus,ignore.case = T)] <- "other"
  comixdata_cont$cnt_dis_mask[grepl("Yes",comixdata_cont$cnt_dis_1m_plus,ignore.case = T)] <- "other"
  comixdata_cont$cnt_dis_mask[grepl("Yes",comixdata_cont$cnt_dis_1m_minus,ignore.case = T)] <- "other"
  comixdata_cont$cnt_dis_mask[grepl("Yes",comixdata_cont$cnt_dis_mask,ignore.case = T)] <- "mask"
  comixdata_cont$cnt_dis_mask[grepl("Yes",comixdata_cont$cnt_dis_wash_before,ignore.case = T)] <- "other"
  comixdata_cont$cnt_dis_mask[grepl("Yes",comixdata_cont$cnt_dis_wash_after,ignore.case = T)] <- "other"
  comixdata_cont$cnt_dis_mask[grepl("Yes",comixdata_cont$cnt_dis_dontknow,ignore.case = T)] <- "Don't know"
  comixdata_cont$cnt_dis_mask[grepl("Yes",comixdata_cont$cnt_dis_notsaid,ignore.case = T)] <- "Prefer not to say"
  comixdata_cont$cnt_dis_mask[grepl("Yes",comixdata_cont$cnt_dis_other,ignore.case = T)] <- "other"
  
  
  comixdata_cont$cnt_dis_measures <- as.character(comixdata_cont$cnt_dis_measures)
  comixdata_cont$cnt_dis_measures[grepl("Yes",comixdata_cont$household_member,ignore.case = T)] <- "houshold member"
  comixdata_cont$cnt_dis_measures[grepl("No",comixdata_cont$cnt_dis_wash_before,ignore.case = T)] <- "No"
  comixdata_cont$cnt_dis_measures[grepl("No",comixdata_cont$cnt_dis_wash_after,ignore.case = T)] <- "No"
  comixdata_cont$cnt_dis_measures[grepl("No",comixdata_cont$cnt_dis_2m_plus,ignore.case = T)] <- "No"
  comixdata_cont$cnt_dis_measures[grepl("Yes",comixdata_cont$cnt_dis_1m_plus,ignore.case = T)] <- "Sometimes"
  comixdata_cont$cnt_dis_measures[grepl("No",comixdata_cont$cnt_dis_mask,ignore.case = T)] <- "No"
  comixdata_cont$cnt_dis_measures[grepl("Yes",comixdata_cont$cnt_dis_other,ignore.case = T)] <- "Sometimes"
  comixdata_cont$cnt_dis_measures[grepl("Yes",comixdata_cont$cnt_dis_wash_before,ignore.case = T)] <- "Yes"
  comixdata_cont$cnt_dis_measures[grepl("Yes",comixdata_cont$cnt_dis_wash_after,ignore.case = T)] <- "Yes"
  comixdata_cont$cnt_dis_measures[grepl("Yes",comixdata_cont$cnt_dis_2m_plus,ignore.case = T)] <- "Yes"
  comixdata_cont$cnt_dis_measures[grepl("Yes",comixdata_cont$cnt_dis_1m_plus,ignore.case = T)] <- "Sometimes"
  comixdata_cont$cnt_dis_measures[grepl("Yes",comixdata_cont$cnt_dis_1m_minus,ignore.case = T)] <- "No"
  comixdata_cont$cnt_dis_measures[grepl("Yes",comixdata_cont$cnt_dis_mask,ignore.case = T)] <- "Yes"
  comixdata_cont$cnt_dis_measures[grepl("Yes",comixdata_cont$cnt_dis_dontknow,ignore.case = T)] <- "Don't know"
  comixdata_cont$cnt_dis_measures[grepl("Yes",comixdata_cont$cnt_dis_notsaid,ignore.case = T)] <- "Prefer not to say"

  
  comixdata_cont$cnt_duration <- factor(comixdata_cont$cnt_duration, levels = c("Less than 5 minutes","5 minutes or more, but less than 15 minutes","15 minutes or more, but less than 1 hour","1 hour or more, but less than 4 hours","4 hours or more","houshold member","Don’t know"))
  comixdata_cont$panel_wave<- gsub("_.*","", comixdata_cont$panel_wave_id)
  comixdata_cont$survey_group <-  ifelse(grepl("A|B|F",comixdata_cont$panel_wave_id),"adults", "children")

  comixdata_cont <- comixdata_cont[grepl("[[:alpha:]]",comixdata_cont$panel_wave),]
  comixdata_cont <- comixdata_cont[!is.na(comixdata_cont$panel_wave_id),]
  return(comixdata_cont[,c("panel_wave_id","panel_wave","cnt_gender","cnt_age","cnt_age_est_min","cnt_age_est_max","num_cnt_beforeCOVID19","cnt_relationship","hh_riskgroup","hh_education","hh_occupation","cnt_duration","cnt_dis_meters","cnt_dis_hands","cnt_dis_mask","cnt_dis_measures","cnt_place_main","cnt_place","cnt_freq","cnt_dis_physical", "contact")])
}
