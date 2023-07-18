  
  #2.1 verdacht binnen registratie
  
  
  #####################################
  #### 1. unstandardized variables ####
  #####################################
  
  #21: religion
  print_21 <- dyn_person[dyn_person$variable==3, ]
  print_21 <- print_21[!(trimws(tolower(print_21$value)) %in% tolower(religion$original)),]
  print_21$code <- 21
  print_21$melding <- "niet-gestandaardiseerde religie"
  print_21$entry_BR <- print_21$value
  print_21$entry_expected <- NA
  if(length(print_21$IDNR)==0){
    rm(print_21)
  } else{
    l$print_21 <- complete_other(print_21)
    rm(print_21)
  }
  
  #41: occupation
  print_41 <- dyn_person[dyn_person$variable==5,]
  print_41 <- print_41[!(trimws(tolower(print_41$value)) %in% tolower(occupation$original)),]
  print_41$code <- 41
  print_41$melding <- "niet-gestandaardiseerd beroep"
  print_41$entry_BR <- print_41$value
  print_41$entry_expected <- NA
  if(length(print_41$IDNR)==0){
    rm(print_41)
  } else{
    l$print_41 <- complete_other(print_41)
    rm(print_41)
  }
  
  #91: place of birth
  print_91 <- stat_person[!(trimws(tolower(stat_person$geboorteplaats)) %in% tolower(location$original)), ]
  print_91$code <- 91
  print_91$melding <- "niet-gestandaardiseerde geboorteplaats"
  print_91$entry_BR <- print_91$geboorteplaats
  print_91$entry_expected <- NA
  if(length(print_91$IDNR)==0){
    rm(print_91)
  } else{
    l$print_91 <- complete_other(print_91)
    rm(print_91)
  }
  
  #92: place of death
  print_92 <- stat_person[!(trimws(tolower(stat_person$overlijdensplaats)) %in% tolower(location$original)), ]
  print_92$code <- 92
  print_92$melding <- "niet-gestandaardiseerde overlijdensplaats"
  print_92$entry_BR <- print_92$overlijdensplaats
  print_92$entry_expected <- NA
  if(length(print_92$IDNR)==0){
    rm(print_92)
  } else{
    l$print_92 <- complete_other(print_92)
    rm(print_92)
  }
  
  #93: place of residence
  print_93 <- stat_person[!(trimws(tolower(stat_person$woonplaats)) %in% tolower(location$original)), ]
  print_93$code <- 93
  print_93$melding <- "niet-gestandaardiseerde plaats van domicilie"
  print_93$entry_BR <- print_93$woonplaats
  print_93$entry_expected <- NA
  if(length(print_93$IDNR)==0){
    rm(print_93)
  } else{
    l$print_93 <- complete_other(print_93)
    rm(print_93)
  }
  
  #94: address
  print_94 <- place[place$address_type=="PL", ]
  if(length(which(trimws(tolower(print_94$location)) %in% tolower(location$original)==F))==0){
    rm(print_94)
  } else{
    print_94 <- print_94[!(trimws(tolower(print_94$location)) %in% tolower(location$original)), ]
    print_94$code <- 94
    print_94$melding <- "niet-gestandaardiseerde woonplaats"
    print_94$entry_BR <- print_94$location
    print_94$entry_expected <- NA
    if(length(print_94$IDNR)==0){
      rm(print_94)
    } else{
      l$print_94 <- complete_other(print_94)
      rm(print_94)
    }
  }
  
  #101: Amsterdam street without neighbourhood
  Amsterdam <- AINB[AINB$GEMNAAM=="Amsterdam",]
  print_101 <- place[place$address_type=="WK",]
  print_101 <- place[place$ID_bron %in% Amsterdam$ID_bron & #in A'dam
                       !(place$temp_id_adres %in% print_101$temp_id_adres),] #en niet ingevoerd
  print_101 <- print_101[print_101$address_type=="ST" & grepl("@", print_101$location)==F,]
  print_101$code <- 101
  print_101$melding <- "adres in A'dam ingevoerd, wijk niet ingevoerd"
  print_101$entry_BR <- paste(print_101$location, print_101$nummer, sep=" ")
  print_101$entry_expected <- NA
  if(length(print_101$IDNR)==0){
    rm(print_101)
  } else{
    l$print_101 <- complete_other(print_101)
    rm(print_101)
  }
  
  #102: Amsterdam street without neighbourhood, address partly unintelligible
  Amsterdam <- AINB[AINB$GEMNAAM=="Amsterdam",]
  print_102 <- place[place$address_type=="WK",]
  print_102 <- place[place$ID_bron %in% Amsterdam$ID_bron & #in A'dam
                       !(place$temp_id_adres %in% print_102$temp_id_adres),] #en niet ingevoerd
  print_102 <- print_102[print_102$address_type=="ST" & grepl("@", print_102$location),]
  print_102$code <- 102
  print_102$melding <- "adres in A'dam deels onleesbaar, wijk niet ingevoerd"
  print_102$entry_BR <- paste(print_102$location, print_102$nummer, sep=" ")
  print_102$entry_expected <- NA
  if(length(print_102$IDNR)==0){
    rm(print_102)
  } else{
    l$print_102 <- complete_other(print_102)
    rm(print_102)
  }
  
  
  
  
  ##########################
  #### 2. sleutelfouten ####
  ##########################
  
  #unexpected entries 
  #1112a
  print_1112a <- AINB[AINB$source_type %in% LETTERS[1:4],] #A-D
  print_1112a <- cert[cert$ID_bron %in% print_1112a$ID_bron,]
  print_1112a <- print_1112a[print_1112a$page_number=="" |
                               print_1112a$page_number==0, ]
  print_1112a$code <- 1112
  print_1112a$melding <- "ontbrekend paginanummer in bevolksregister"
  print_1112a$entry_BR <- print_1112a$page_number
  print_1112a$entry_expected <- ""
  #1112b
  print_1112b <- AINB[AINB$source_type %in% LETTERS[1:4],] #A-D
  print_1112b <- cert[cert$ID_bron %in% print_1112b$ID_bron,]
  print_1112b <- print_1112b[print_1112b$household_number=="" |
                               print_1112b$household_number==0, ]
  print_1112b$code <- 1112
  print_1112b$entry_BR <- print_1112b$household_number
  print_1112b$entry_expected <- ""
  print_1112b$melding <- "ontbrekend huishoudnummer in bevolksregister"
  #bind
  print_1112 <- rbind(print_1112a, print_1112b)
  if(length(print_1112$IDNR)==0){
    rm(print_1112)
  } else{
    l$print_1112 <- complete_cert(print_1112)
    rm(print_1112)
  }
  rm(print_1112a, print_1112b)
  
  #1113a
  print_1113a <- AINB[AINB$source_type %in% LETTERS[7:9],] #G-I
  print_1113a <- cert[cert$ID_bron %in% print_1113a$ID_bron,]
  print_1113a <- print_1113a[!(print_1113a$page_number %in% c("", 0)), ]
  print_1113a$code <- 1113
  print_1113a$entry_BR <- print_1113a$page_number
  print_1113a$entry_expected <- ""
  print_1113a$melding <- "onverwacht paginanummer op familiekaart"
  #1113b
  print_1113b <- AINB[AINB$source_type %in% LETTERS[7:9],] #G-I
  print_1113b <- cert[cert$ID_bron %in% print_1113b$ID_bron,]
  print_1113b <- print_1113b[!(print_1113b$household_number %in% c("", 0)), ]
  print_1113b$code <- 1113
  print_1113b$entry_BR <- print_1113b$household_number
  print_1113b$entry_expected <- ""
  print_1113b$melding <- "onverwacht huishoudnummer op familiekaart"
  #bind
  print_1113 <- rbind(print_1113a, print_1113b)
  if(length(print_1113$IDNR)==0){
    rm(print_1113)
  } else{
    l$print_1113 <- complete_cert(print_1113)
    rm(print_1113)
  }
  rm(print_1113a, print_1113b)
  
  #1114
  print_1114 <- AINB[AINB$source_type %in% LETTERS[7:9],] #G-I
  print_1114 <- cert[cert$ID_bron %in% print_1114$ID_bron,]
  print_1114 <- print_1114[print_1114$GK_info=="", ]
  print_1114$code <- 1114
  print_1114$melding <- "ontbrekende informatie familiekaart"
  print_1114$entry_BR <- NA
  print_1114$entry_expected <- NA
  if(length(print_1114$IDNR)==0){
    rm(print_1114)
  } else{
    l$print_1114 <- complete_cert(print_1114)
    rm(print_1114)
  }
  
  #1115
  print_1115 <- AINB[AINB$source_type %in% LETTERS[1:4],] #A-D
  print_1115 <- cert[cert$ID_bron %in% print_1115$ID_bron,]
  print_1115 <- print_1115[print_1115$GK_info!="", ]
  print_1115$code <- 1115
  print_1115$entry_BR <- print_1115$GK_info
  print_1115$entry_expected <- NA
  print_1115$melding <- "GK-informatie op familiekaart"
  if(length(print_1115$IDNR)==0){
    rm(print_1115)
  } else{
    l$print_1115 <- complete_cert(print_1115)
    rm(print_1115)
  }
  
  #1118
  print_1118 <- AINB[AINB$source_type=="A" | AINB$source_type=="I",] #A or I
  print_1118 <- cert[cert$ID_bron %in% print_1118$ID_bron,]
  print_1118 <- print_1118[duplicated(print_1118[, c("IDNR", "ID_bron", "dag_inschrijving", "maand_inschrijving", "jaar_inschrijving"),]), ]
  print_1118$code <- 1118
  print_1118$melding <- "dubbele registratie alleenstaande"
  print_1118$entry_BR <- NA
  print_1118$entry_expected <- NA
  if(length(print_1118$IDNR)==0){
    rm(print_1118)
  } else{
    l$print_1118 <- complete_cert(print_1118)
    rm(print_1118)
  }
  
  #1178: andere achternaam op geboorteakte
  print_1178 <- stat_person[stat_person$HSN_RP==1 | stat_person$HSN_RP==5, ]
  print_1178 <- print_1178[!duplicated(print_1178[, c("IDNR", "achternaam")]),]
  print_1178 <- merge(print_1178, HSNRP, by="IDNR", all=F)
  print_1178 <- print_1178[tolower(gsub(",.*", "", print_1178$achternaam))!=tolower(print_1178$RP_family2),]
  print_1178$code <- 1178
  print_1178$melding <- "achternaam verschilt met geboorteakte"
  print_1178$entry_BR <- print_1178$achternaam
  print_1178$entry_expected <- print_1178$RP_family2
  if(length(print_1178$IDNR)==0){
    rm(print_1178)
  } else{
    l$print_1178 <- complete_other(print_1178)
    rm(print_1178)
  }
  
  #1179: andere voornaam op geboorteakte
  print_1179 <- stat_person[stat_person$HSN_RP==1 | stat_person$HSN_RP==5, ] 
  print_1179 <- print_1179[!duplicated(print_1179[, c("IDNR", "achternaam")]),]
  print_1179 <- merge(print_1179, HSNRP, by="IDNR", all=F)
  print_1179 <- print_1179[tolower(print_1179$voornaam)!=tolower(print_1179$RP_firstname),]
  print_1179$code <- 1179
  print_1179$melding <- "voornaam verschilt met geboorteakte"
  print_1179$entry_BR <- print_1179$voornaam
  print_1179$entry_expected <- print_1179$RP_firstname
  if(length(print_1179$IDNR)==0){
    rm(print_1179)
  } else{
    l$print_1179 <- complete_other(print_1179)
    rm(print_1179)
  }
  
  
  
  
  #########################
  #### 3. entry errors ####
  #########################
  
  #1064
  print_1064 <- place[duplicated(place$temp_id_adres2) & place$address_type=="AN",]
  print_1064$code <- 1064
  print_1064$melding <- "dubbele invoer of ontbrekend simultaannummer adresbestand (B6)"
  print_1064$entry_BR <- NA
  print_1064$entry_expected <- NA
  if(length(print_1064$IDNR)==0){
    rm(print_1064)
  } else{
    l$print_1064 <- complete_other(print_1064)
    rm(print_1064)
  }
  
  #afgekapte informatie
  #1086
  print_1086 <- place[grepl("#", place$location),]
  print_1086$code <- 1086
  print_1086$melding <- "adres afgekapt na #"
  print_1086$entry_BR <- print_1086$location
  print_1086$entry_expected <- ""
  if(length(print_1086$IDNR)==0){
    rm(print_1086)
  } else{
    l$print_1086 <- complete_other(print_1086)
    rm(print_1086)
  }
  
  #1093
  print_1093 <- place[grepl("[a-zA-Z]/", place$location),]
  print_1093$code <- 1093
  print_1093$melding <- "/ gebruikt als separator straat / wijk (t/m v4.01)"
  print_1093$entry_BR <- print_1093$location
  print_1093$entry_expected <- ""
  if(length(print_1093$IDNR)==0){
    rm(print_1093)
  } else{
    l$print_1093 <- complete_other(print_1093)
    rm(print_1093)
  }
  
  #1094
  print_1094 <- place[grepl("/[0-9][0-9]/", place$location),]
  print_1094$code <- 1094
  print_1094$melding <- "/../ gebruikt als aanduiding huisnummer (t/m v4.01)"
  print_1094$entry_BR <- print_1094$location
  print_1094$entry_expected <- ""
  if(length(print_1094$IDNR)==0){
    rm(print_1094)
  } else{
    l$print_1094 <- complete_other(print_1094)
    rm(print_1094)
  }
  
  #1077
  print_1077 <- place[duplicated(place[,c("IDNR", "ID_bron", "simultaannummer", "dag_inschrijving", "maand_inschrijving", "jaar_inschrijving", "address_type")]) & place$simultaannummer>0, ]
  print_1077 <- print_1077[print_1077$address_type=="AN",]
  print_1077$code <- 1077
  print_1077$melding <- "simultaannummer meermaals gebruikt"
  print_1077$entry_BR <- NA
  print_1077$entry_expected <- NA
  if(length(print_1077$IDNR)==0){
    rm(print_1077)
  } else{
    l$print_1077 <- complete_other(print_1077)
    rm(print_1077)
  }
  
  
  
  
  #########################
  #### 4. entry errors ####
  #########################
  
  #1120
  print_1120 <- merge(cert, AINB[,c("ID_bron", "source_type")], by="ID_bron", all.x=T)
  print_1120 <- print_1120[print_1120$source_type=="A" & print_1120$jaar_adres<1863 | 
                             print_1120$source_type=="B" & print_1120$jaar_adres<1863 | 
                             print_1120$source_type=="C" & print_1120$jaar_adres<1850 | 
                             print_1120$source_type=="D" & print_1120$jaar_adres<1812 | 
                             print_1120$source_type=="G" & print_1120$jaar_adres<1890 | 
                             print_1120$source_type=="I" & print_1120$jaar_adres<1890 | 
                             print_1120$source_type=="V" & print_1120$jaar_adres<1812, ]
  print_1120$code <- 1120
  print_1120$melding <- "hoofddatum voor start register"
  print_1120$entry_BR <- print_1120$jaar_adres
  print_1120$entry_expected <- ifelse(print_1120$source_type=="A", 1863,
                                      ifelse(print_1120$source_type=="B", 1863,
                                             ifelse(print_1120$source_type=="C", 1850,
                                                    ifelse(print_1120$source_type=="D", 1812,
                                                           ifelse(print_1120$source_type=="G", 1890,
                                                                  ifelse(print_1120$source_type=="I", 1890,
                                                                         ifelse(print_1120$source_type=="V", 1812, NA)))))))
  if(length(print_1120$IDNR)==0){
    rm(print_1120)
  } else{
    l$print_1120 <- complete_cert(print_1120)
    rm(print_1120)
  }
  
  #1121
  print_1121 <- merge(cert, AINB[,c("ID_bron", "source_type")], by="ID_bron", all.x=T)
  print_1121 <- print_1121[print_1121$source_type=="A" & print_1121$jaar_adres>1940 | 
                             print_1121$source_type=="B" & print_1121$jaar_adres>1940 | 
                             print_1121$source_type=="C" & print_1121$jaar_adres>1862 | 
                             print_1121$source_type=="D" & print_1121$jaar_adres>1850 | 
                             print_1121$source_type=="G" & print_1121$jaar_adres>1940 | 
                             print_1121$source_type=="I" & print_1121$jaar_adres>1940 | 
                             print_1121$source_type=="V" & print_1121$jaar_adres>1850, ]
  print_1121$code <- 1121
  print_1121$melding <- "hoofddatum na eind register"
  print_1121$entry_BR <- print_1121$jaar_adres
  print_1121$entry_expected <- ifelse(print_1121$source_type=="A", 1940,
                                      ifelse(print_1121$source_type=="B", 1940,
                                             ifelse(print_1121$source_type=="C", 1862,
                                                    ifelse(print_1121$source_type=="D", 1850,
                                                           ifelse(print_1121$source_type=="G", 1940,
                                                                  ifelse(print_1121$source_type=="I", 1940,
                                                                         ifelse(print_1121$source_type=="V", 1850, NA)))))))
  if(length(print_1121$IDNR)==0){
    rm(print_1121)
  } else{
    l$print_1121 <- complete_cert(print_1121)
    rm(print_1121)
  }
  
  #1122
  print_1122 <- dyn_person[dyn_person$variable==1 & dyn_person$code==1, ]
  print_1122 <- stat_person[stat_person$temp_id_person %in% print_1122$temp_id_person, ]
  print_1122 <- print_1122[!duplicated(print_1122[,c("IDNR", "ID_bron", "jaar_inschrijving", "maand_inschrijving", "dag_inschrijving")])]
  #1122a
  print_1122a <- print_1122[print_1122$jaar_inschrijving<print_1122$jaar_adres, ]
  print_1122a$code <- 1122
  print_1122a$melding <- "jaar hoofddatum valt voor herkomstdatum hoofd"
  print_1122a$entry_BR <- paste(print_1122a$dag_inschrijving, print_1122a$maand_inschrijving, print_1122a$jaar_inschrijving, sep="-")
  print_1122a$entry_expected <- paste(print_1122a$dag_adres, print_1122a$maand_adres, print_1122a$jaar_adres, sep="-")
  #1122b
  print_1122b <- print_1122[print_1122$jaar_inschrijving==print_1122$jaar_adres & 
                              print_1122$maand_inschrijving<print_1122$maand_adres, ]
  print_1122b$code <- 1122
  print_1122b$melding <- "maand hoofddatum valt voor herkomstdatum hoofd"
  print_1122b$entry_BR <- print_1122b$maand_inschrijving
  print_1122b$entry_expected <- print_1122b$maand_adres
  #bind
  print_1122 <- rbind(print_1122a, print_1122b)
  if(length(print_1122$IDNR)==0){
    rm(print_1122)
  } else{
    l$print_1122 <- complete_cert(print_1122)
    rm(print_1122)
  }
  rm(print_1122a, print_1122b)
  
  #1123
  print_1123 <- dyn_person[dyn_person$variable==1 & dyn_person$code==1, ]
  print_1123 <- stat_person[stat_person$temp_id_person %in% print_1123$temp_id_person,]
  print_1123 <- print_1123[!duplicated(print_1123[,c("IDNR", "ID_bron", "jaar_inschrijving", "maand_inschrijving", "dag_inschrijving")])]
  print_1123 <- print_1123[print_1123$jaar_inschrijving==print_1123$jaar_adres &
                             print_1123$maand_inschrijving==print_1123$maand_adres &
                             print_1123$dag_inschrijving<print_1123$dag_adres &
                             print_1123$dag_inschrijving>0, ]
  print_1123$code <- 1123
  print_1123$melding <- "dag hoofddatum valt voor datum inschrijving hoofd"
  print_1123$entry_BR <- print_1123$dag_adres
  print_1123$entry_expected <- print_1123$dag_inschrijving
  if(length(print_1123$IDNR)==0){
    rm(print_1123)
  } else{
    l$print_1123 <- complete_other(print_1123)
    rm(print_1123)
  }
  
  #1144
  print_1144 <- AINB[AINB$source_type!="A" & AINB$source_type!="I",] #A or I
  print_1144 <- dyn_person[dyn_person$ID_bron %in% print_1144$ID_bron & dyn_person$variable==1 & dyn_person$code==1,]
  print_1144 <- stat_person[stat_person$temp_id_person %in% print_1144$temp_id_person,]
  print_1144 <- print_1144[(print_1144$temp-print_1144$geboortejaar)<18 & (print_1144$temp-print_1144$geboortejaar)>=12, ]
  print_1144$code <- 1144
  print_1144$melding <- "hoofd <18 jaar"
  print_1144$entry_BR <- paste(print_1144$geboortedag, print_1144$geboortemaand, print_1144$geboortejaar, sep="-")
  print_1144$entry_expected <- NA
  if(length(print_1144$IDNR)==0){
    rm(print_1144)
  } else{
    l$print_1144 <- complete_cert(print_1144)
    rm(print_1144)
  }
  
  #1153a
  print_1153a <- AINB[AINB$source_type %in% c("V", "D"),]
  print_1153a <- stat_person[stat_person$ID_bron %in% print_1153a$ID_bron & stat_person$HSN_RP>0,]
  print_1153a <- print_1153a[print_1153a$jaar_inschrijving<1812,]
  print_1153a$entry_BR <- print_1153a$jaar_inschrijving
  print_1153a$entry_expected <- "1812-1850"
  #1153b
  print_1153b <- AINB[AINB$source_type=="C",]
  print_1153b <- stat_person[stat_person$ID_bron %in% print_1153b$ID_bron & stat_person$HSN_RP>0,]
  print_1153b <- print_1153b[print_1153b$jaar_inschrijving<1850,]
  print_1153b$entry_BR <- print_1153b$jaar_inschrijving
  print_1153b$entry_expected <- "1850-1862"
  #1153c
  print_1153c <- AINB[AINB$source_type %in% c("B", "A"),]
  print_1153c <- stat_person[stat_person$ID_bron %in% print_1153c$ID_bron & stat_person$HSN_RP>0,]
  print_1153c <- print_1153c[print_1153c$jaar_inschrijving<1863,]
  print_1153c$entry_BR <- print_1153c$jaar_inschrijving
  print_1153c$entry_expected <- "1863-1940"
  #1153d
  print_1153d <- AINB[AINB$source_type %in% c("G", "I"),]
  print_1153d <- stat_person[stat_person$ID_bron %in% print_1153d$ID_bron & stat_person$HSN_RP>0,]
  print_1153d <- print_1153d[print_1153d$jaar_inschrijving<1890,]
  print_1153d$entry_BR <- print_1153d$jaar_inschrijving
  print_1153d$entry_expected <- "1890-1940"
  #rbind
  print_1153 <- rbind(print_1153a, print_1153b, print_1153c, print_1153d)
  print_1153$code <- 1153
  print_1153$melding <- "OP-datum voor start register"
  if(length(print_1153$IDNR)==0){
    rm(print_1153)
  } else{
    l$print_1153 <- complete_cert(print_1153)
    rm(print_1153)
  }
  
  #1154a
  print_1154a <- AINB[AINB$source_type %in% c("V", "D"),]
  print_1154a <- stat_person[stat_person$ID_bron %in% print_1154a$ID_bron & stat_person$HSN_RP>0,]
  print_1154a <- print_1154a[print_1154a$jaar_inschrijving>1850,]
  print_1154a$entry_BR <- print_1154a$jaar_inschrijving
  print_1154a$entry_expected <- "1812-1850"
  #1154b
  print_1154b <- AINB[AINB$source_type=="C",]
  print_1154b <- stat_person[stat_person$ID_bron %in% print_1154b$ID_bron & stat_person$HSN_RP>0,]
  print_1154b <- print_1154b[print_1154b$jaar_inschrijving>1862,]
  print_1154b$entry_BR <- print_1154b$jaar_inschrijving
  print_1154b$entry_expected <- "1850-1862"
  #1154c
  print_1154c <- AINB[AINB$source_type %in% c("B", "A"),]
  print_1154c <- stat_person[stat_person$ID_bron %in% print_1154c$ID_bron & stat_person$HSN_RP>0,]
  print_1154c <- print_1154c[print_1154c$jaar_inschrijving>1940,]
  print_1154c$entry_BR <- print_1154c$jaar_inschrijving
  print_1154c$entry_expected <- "1863-1940"
  #1154d
  print_1154d <- AINB[AINB$source_type %in% c("G", "I"),]
  print_1154d <- stat_person[stat_person$ID_bron %in% print_1154d$ID_bron & stat_person$HSN_RP>0,]
  print_1154d <- print_1154d[print_1154d$jaar_inschrijving>1940,]
  print_1154d$entry_BR <- print_1154d$jaar_inschrijving
  print_1154d$entry_expected <- "1890-1940"
  #rbind
  print_1154 <- rbind(print_1154a, print_1154b, print_1154c, print_1154d)
  print_1154$code <- 1154
  print_1154$melding <- "OP-datum voor start register"
  if(length(print_1154$IDNR)==0){
    rm(print_1154)
  } else{
    l$print_1154 <- complete_cert(print_1154)
    rm(print_1154)
  }
  
  
  
  
  ##################
  #### 5. HSNRP ####
  ##################
  
  #1180
  print_1180 <- stat_person[stat_person$HSN_RP==1 | stat_person$HSN_RP==5, ]
  print_1180 <- print_1180[!duplicated(print_1180[, c("IDNR", "achternaam")]),]
  print_1180 <- print_1180[duplicated(print_1180$IDNR),]
  print_1180 <- stat_person[stat_person$HSN_RP==1 & stat_person$IDNR %in% print_1180$IDNR |
                              stat_person$HSN_RP==5 & stat_person$IDNR %in% print_1180$IDNR,]
  print_1180$code <- 1180
  print_1180$melding <- "achternaam OP inconsistent"
  print_1180$entry_BR <- print_1180$achternaam
  print_1180$entry_expected <- NA
  if(length(print_1180$IDNR)==0){
    rm(print_1180)
  } else{
    l$print_1180 <- complete_other(print_1180)
    rm(print_1180)
  }
  
  #1181
  print_1181 <- stat_person[stat_person$HSN_RP==1 | stat_person$HSN_RP==5, ]
  print_1181 <- print_1181[!duplicated(print_1181[, c("IDNR", "voornaam")]),]
  print_1181 <- print_1181[duplicated(print_1181$IDNR),]
  print_1181 <- stat_person[stat_person$HSN_RP==1 & stat_person$IDNR %in% print_1181$IDNR |
                              stat_person$HSN_RP==5 & stat_person$IDNR %in% print_1181$IDNR,]
  print_1181$code <- 1181
  print_1181$melding <- "voornaam OP inconsistent"
  print_1181$entry_BR <- print_1181$voornaam
  print_1181$entry_expected <- NA
  if(length(print_1181$IDNR)==0){
    rm(print_1181)
  } else{
    l$print_1181 <- complete_other(print_1181)
    rm(print_1181)
  }
  
  #1182a
  print_1182a <- stat_person[stat_person$HSN_RP==1 | stat_person$HSN_RP==5, ]
  print_1182a <- print_1182a[!duplicated(print_1182a[, c("IDNR", "geboortedag")]),]
  print_1182a <- print_1182a[duplicated(print_1182a$IDNR),]
  print_1182a <- stat_person[stat_person$HSN_RP==1 & stat_person$IDNR %in% print_1182a$IDNR |
                               stat_person$HSN_RP==5 & stat_person$IDNR %in% print_1182a$IDNR,]
  print_1182a$code <- 1182
  print_1182a$melding <- "geboortedag OP inconsistent"
  print_1182a$entry_BR <- print_1182a$geboortedag
  print_1182a$entry_expected <- NA
  #1182b
  print_1182b <- stat_person[stat_person$HSN_RP==1 | stat_person$HSN_RP==5, ]
  print_1182b <- print_1182b[!duplicated(print_1182b[, c("IDNR", "geboortemaand")]),]
  print_1182b <- print_1182b[duplicated(print_1182b$IDNR),]
  print_1182b <- stat_person[stat_person$HSN_RP==1 & stat_person$IDNR %in% print_1182b$IDNR |
                               stat_person$HSN_RP==5 & stat_person$IDNR %in% print_1182b$IDNR,]
  print_1182b$code <- 1182
  print_1182b$melding <- "geboortemaand OP inconsistent"
  print_1182b$entry_BR <- print_1182b$geboortemaand
  print_1182b$entry_expected <- NA
  #1182c
  print_1182c <- stat_person[stat_person$HSN_RP==1 | stat_person$HSN_RP==5, ]
  print_1182c <- print_1182c[!duplicated(print_1182c[, c("IDNR", "geboortejaar")]),]
  print_1182c <- print_1182c[duplicated(print_1182c$IDNR),]
  print_1182c <- stat_person[stat_person$HSN_RP==1 & stat_person$IDNR %in% print_1182c$IDNR |
                               stat_person$HSN_RP==5 & stat_person$IDNR %in% print_1182c$IDNR,]
  print_1182c$code <- 1182
  print_1182c$melding <- "geboortejaar OP inconsistent"
  print_1182c$entry_BR <- print_1182c$geboortejaar
  print_1182c$entry_expected <- NA
  #1182
  print_1182 <- rbind(print_1182a, print_1182b, print_1182c)
  if(length(print_1182$IDNR)==0){
    rm(print_1182)
  } else{
    l$print_1182 <- complete_other(print_1182)
    rm(print_1182)
  }
  rm(print_1182a, print_1182b, print_1182c)
  
  #1183
  print_1183 <- stat_person[stat_person$HSN_RP==1 | stat_person$HSN_RP==5, ]
  print_1183 <- print_1183[!duplicated(print_1183[, c("IDNR", "geboorteplaats")]),]
  print_1183 <- print_1183[duplicated(print_1183$IDNR),]
  print_1183 <- stat_person[stat_person$HSN_RP==1 & stat_person$IDNR %in% print_1183$IDNR |
                              stat_person$HSN_RP==5 & stat_person$IDNR %in% print_1183$IDNR,]
  print_1183$code <- 1183
  print_1183$melding <- "geboorteplaats OP mogelijk inconsistent"
  print_1183$entry_BR <- print_1183$geboorteplaats
  print_1183$entry_expected <- NA
  if(length(print_1183$IDNR)==0){
    rm(print_1183)
  } else{
    l$print_1183 <- complete_other(print_1183)
    rm(print_1183)
  }
  
  #1186
  print_1186 <- stat_person[stat_person$HSN_RP==1 | stat_person$HSN_RP==5, ] 
  print_1186 <- print_1186[!duplicated(print_1186),]
  print_1186 <- merge(print_1186, HSNRP, by="IDNR", all=F)
  print_1186 <- print_1186[(print_1186$geboortejaar-print_1186$RP_B_YEAR)>1 | (print_1186$geboortejaar-print_1186$RP_B_YEAR)<(-1),]
  print_1186$code <- 1186
  print_1186$melding <- "geboortejaar verschilt met HSNRP (gesamplede akte)"
  print_1186$entry_BR <- print_1186$geboortejaar
  print_1186$entry_expected <- print_1186$RP_B_YEAR
  #bind
  if(length(print_1186$IDNR)==0){
    rm(print_1186)
  } else{
    l$print_1186 <- complete_other(print_1186)
    rm(print_1186)
  }
  
  #1187
  print_1187 <- stat_person[stat_person$HSN_RP==1 & is.na(stat_person$geslacht) |
                              stat_person$HSN_RP==5 & is.na(stat_person$geslacht) |
                              stat_person$HSN_RP==1 & stat_person$geslacht!="m" & stat_person$geslacht!="v" |
                              stat_person$HSN_RP==5 & stat_person$geslacht!="m" & stat_person$geslacht!="v", ]
  print_1187$code <- 1187
  print_1187$melding <- "geslacht OP mist/ongeldig"
  print_1187$entry_BR <- NA
  print_1187$entry_expected <- NA
  #bind
  if(length(print_1187$IDNR)==0){
    rm(print_1187)
  } else{
    l$print_1187 <- complete_other(print_1187)
    rm(print_1187)
  }
  
  
  
  
  
  