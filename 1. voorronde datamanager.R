  
  #1. voorronde datamanager

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
  
  
  
  
  ##########################
  #### 2. sleutelfouten ####
  ##########################
  
  #unknown identifier
  #1053 
  print_1053 <- cert[!(cert$ID_bron %in% AINB$ID_bron),]
  print_1053$code <- 1053
  print_1053$melding <- "ID_bron niet in index (AINB)"
  print_1053$entry_BR <- NA
  print_1053$entry_expected <- NA
  if(length(print_1053$IDNR)==0){
    rm(print_1053)
  } else{
    l$print_1053 <- complete_cert(print_1053)
    rm(print_1053)
  }
  
  #1054
  print_1054 <- cert[!(cert$IDNR %in% HSNRP$IDNR),]
  print_1054$code <- 1054
  print_1054$melding <- "IDNR niet in geboorteakte (HSNRP)"
  print_1054$entry_BR <- NA
  print_1054$entry_expected <- NA
  if(length(print_1054$IDNR)==0){
    rm(print_1054)
  } else{
    l$print_1054 <- complete_cert(print_1054)
    rm(print_1054)
  }
  
  #double entry  
  #1028
  print_1028 <- cert[duplicated(cert[,c("IDNR", "ID_bron", "dag_inschrijving", "maand_inschrijving", "jaar_inschrijving")]),]
  print_1028$code <- 1028
  print_1028$melding <- "dubbele sleutel"
  print_1028$entry_BR <- NA
  print_1028$entry_expected <- NA
  if(length(print_1028$IDNR)==0){
    rm(print_1028)
  } else{
    l$print_1028 <- complete_cert(print_1028)
    rm(print_1028)
  }
  
  
  
  
  #########################
  #### 3. entry errors ####
  #########################
  
  #double entry
  #1111
  print_1111 <- cert[duplicated(cert[, c("IDNR", "ID_bron", "dag_inschrijving", "maand_inschrijving", "jaar_inschrijving", "page_number", "household_number"),]), ]
  print_1111$code <- 1111
  print_1111$melding <- "huishouden dubbel ingevoerd"
  print_1111$entry_BR <- NA
  print_1111$entry_expected <- NA
  if(length(print_1111$IDNR)==0){
    rm(print_1111)
  } else{
    l$print_1111 <- complete_cert(print_1111)
    rm(print_1111)
  }
  
  #Missing entries
  #1020
  print_1020 <- cert[!(cert$temp_id_cert %in% stat_person$temp_id_cert), ]
  print_1020$code <- 1020
  print_1020$melding <- "bron (B4) niet gekoppeld aan statische gegevens (B2)"
  print_1020$entry_BR <- NA
  print_1020$entry_expected <- NA
  if(length(print_1020$IDNR)==0){
    rm(print_1020)
  } else{
    l$print_1020 <- complete_cert(print_1020)
    rm(print_1020)
  }
  
  #1021
  print_1021 <- cert[!(cert$temp_id_cert %in% stat_person$temp_id_cert), ]
  print_1021$code <- 1021
  print_1021$melding <- "statische informatie (B2) niet gekoppeld aan bron (B4)"
  print_1021$entry_BR <- NA
  print_1021$entry_expected <- NA
  if(length(print_1021$IDNR)==0){
    rm(print_1021)
  } else{
    l$print_1021 <- complete_cert(print_1021)
    rm(print_1021)
  }
  
  #1022
  print_1022 <- cert[!(cert$temp_id_cert %in% stat_person$temp_id_cert), ]
  print_1022$code <- 1022
  print_1022$melding <- "dynamische informatie (B3) niet gekoppeld aan bron (B4)"
  print_1022$entry_BR <- NA
  print_1022$entry_expected <- NA
  if(length(print_1022$IDNR)==0){
    rm(print_1022)
  } else{
    l$print_1022 <- complete_cert(print_1022)
    rm(print_1022)
  }
  
  #1023
  print_1023 <- cert[!(cert$temp_id_cert %in% stat_person$temp_id_cert), ]
  print_1023$code <- 1023
  print_1023$melding <- "1023: adress (B6) niet gekoppeld aan bron (B4)"
  print_1023$entry_BR <- NA
  print_1023$entry_expected <- NA
  if(length(print_1023$IDNR)==0){
    rm(print_1023)
  } else{
    l$print_1023 <- complete_cert(print_1023)
    rm(print_1023)
  }
  
  #unlinked entries
  #1058
  print_1058 <- dyn_person[dyn_person$variable==1,]
  print_1058 <- stat_person[!(stat_person$temp_id_person %in% print_1058$temp_id_person), ]
  print_1058$code <- 1058
  print_1058$melding <- "statische gegevens (B2) niet gekoppeld aan relatie tot hoofd (B3)"
  print_1058$entry_BR <- NA
  print_1058$entry_expected <- NA
  if(length(print_1058$IDNR)==0){
    rm(print_1058)
  } else{
    l$print_1058 <- complete_other(print_1058)
    rm(print_1058)
  }
  
  #1059
  print_1059 <- dyn_person[dyn_person$variable==2,]
  print_1059 <- stat_person[!(stat_person$temp_id_person %in% print_1059$temp_id_person), ]
  print_1059$code <- 1059
  print_1059$melding <- "statische gegevens (B2) niet gekoppeld aan burgerlijke stand (B3)"
  print_1059$entry_BR <- NA
  print_1059$entry_expected <- NA
  print_1059$ronde <- "1. voorronde datamanager"
  if(length(print_1059$IDNR)==0){
    rm(print_1059)
  } else{
    l$print_1059 <- complete_other(print_1059)
    rm(print_1059)
  }
  
  #1060
  print_1060 <- dyn_person[dyn_person$variable==3,]
  print_1060 <- stat_person[!(stat_person$temp_id_person %in% print_1060$temp_id_person), ]
  print_1060$code <- 1060
  print_1060$melding <- "statische gegevens (B2) niet gekoppeld aan religie (B3)"
  print_1060$entry_BR <- NA
  print_1060$entry_expected <- NA
  if(length(print_1060$IDNR)==0){
    rm(print_1060)
  } else{
    l$print_1060 <- complete_other(print_1060)
    rm(print_1060)
  }
  
  #1061
  print_1061 <- dyn_person[dyn_person$variable>=3,]
  print_1061 <- print_1061[!(print_1061$temp_id_person %in% stat_person$temp_id_person), ]
  print_1061$code <- 1061
  print_1061$melding <- "dynamische gegevens (B3) niet gekoppeld aan statische gegevens (B2)"
  print_1061$entry_BR <- NA
  print_1061$entry_expected <- NA
  if(length(print_1061$IDNR)==0){
    rm(print_1061)
  } else{
    l$print_1061 <- complete_other(print_1061)
    rm(print_1061)
  }
  
  #1062
  dyn_person$temp_id2 <- paste(dyn_person$temp_id_person, 
                               dyn_person$variable, dyn_person$code, dyn_person$volgnummer_dyn, sep="_")
  print_1062 <- dyn_person[duplicated(dyn_person$temp_id2) & dyn_person$jaar_mutatie!="-3",]
  print_1062$code <- 1062
  print_1062$melding <- "dubbele invoer dynamisch bestand (B3)"
  print_1062$entry_BR <- NA
  print_1062$entry_expected <- NA
  if(length(print_1062$IDNR)==0){
    rm(print_1062)
  } else{
    l$print_1062 <- complete_other(print_1062)
    rm(print_1062)
  }
  
  #1063
  #1063a
  print_1063a <- place[place$volgnummer==0 & !(place$temp_id_cert %in% stat_person$temp_id_cert), ]
  print_1063a$code <- 1063
  print_1063a$melding <- "adresgegevens (B6) niet gekoppeld aan statische gegevens (B2)"
  #1063a
  print_1063b <- place[place$volgnummer>0 & !(place$temp_id_person %in% stat_person$temp_id_person), ]
  print_1063b$code <- 1063
  print_1063b$melding <- "adresgegevens (B6) niet gekoppeld aan statische gegevens (B2)"
  #bind
  print_1063 <- rbind(print_1063a, print_1063b)
  print_1063$entry_BR <- NA
  print_1063$entry_expected <- NA
  if(length(print_1063$IDNR)==0){
    rm(print_1063)
  } else{
    l$print_1063 <- complete_other(print_1063)
    rm(print_1063)
  }
  rm(print_1063a, print_1063b)
  
  #1064
  print_1064 <- place[duplicated(place$temp_id_adres2),]
  print_1064$code <- 1064
  print_1064$melding <- "dubbele invoer adresbestand (B6)"
  print_1064$entry_BR <- NA
  print_1064$entry_expected <- NA
  if(length(print_1064$IDNR)==0){
    rm(print_1064)
  } else{
    l$print_1064 <- complete_other(print_1064)
    rm(print_1064)
  }
  
  #incomplete administratie
  #1024a
  print_1024a <- stat_person[stat_person$ONDRZKO=="", ]
  print_1024a$code <- 1024
  print_1024a$melding <- "administratief veld ONDRZKO mist (B2)"
  #1024b
  print_1024b <- stat_person[stat_person$OPDRNRO=="", ]
  print_1024b$code <- 1024
  print_1024b$melding <- "administratief veld OPDRNRO mist (B2)"
  #1024c
  print_1024c <- stat_person[stat_person$DATUMO=="", ]
  print_1024c$code <- 1024
  print_1024c$melding <- "administratief veld DATUMO mist (B2)"
  #1024d
  print_1024d <- stat_person[stat_person$INITO=="", ]
  print_1024d$code <- 1024
  print_1024d$melding <- "administratief veld INITO mist (B2)"
  #1024e
  print_1024e <- stat_person[stat_person$VERSIEO=="", ]
  print_1024e$code <- 1024
  print_1024e$melding <- "administratief veld VERSIEO mist (B2)"
  #bind
  print_1024 <- rbind(print_1024a, print_1024b, print_1024c, print_1024d, print_1024e)
  print_1024$entry_BR <- NA
  print_1024$entry_expected <- NA
  if(length(print_1024$IDNR)==0){
    rm(print_1024)
  } else{
    l$print_1024 <- complete_other(print_1024)
    rm(print_1024)
  }
  rm(print_1024a, print_1024b, print_1024c, print_1024d, print_1024e)
  
  #1025a
  print_1025a <- dyn_person[dyn_person$ONDRZKO=="", ]
  print_1025a$code <- 1025
  print_1025a$melding <- "administratief veld ONDRZKO mist (B3)"
  #1025b
  print_1025b <- dyn_person[dyn_person$OPDRNRO=="", ]
  print_1025b$code <- 1025
  print_1025b$melding <- "administratief veld OPDRNRO mist (B3)"
  #1025c
  print_1025c <- dyn_person[dyn_person$DATUMO=="", ]
  print_1025c$code <- 1025
  print_1025c$melding <- "administratief veld DATUMO mist (B3)"
  #1025d
  print_1025d <- dyn_person[dyn_person$INITO=="", ]
  print_1025d$code <- 1025
  print_1025d$melding <- "administratief veld INITO mist (B3)"
  #1025e
  print_1025e <- dyn_person[dyn_person$VERSIEO=="", ]
  print_1025e$code <- 1025
  print_1025e$melding <- "administratief veld VERSIEO mist (B3)"
  #bind
  print_1025 <- rbind(print_1025a, print_1025b, print_1025c, print_1025d, print_1025e)
  print_1025$entry_BR <- NA
  print_1025$entry_expected <- NA
  if(length(print_1025$IDNR)==0){
    rm(print_1025)
  } else{
    l$print_1025 <- complete_other(print_1025)
    rm(print_1025)
  }
  rm(print_1025a, print_1025b, print_1025c, print_1025d, print_1025e)
  
  #1026a
  print_1026a <- cert[cert$ONDRZKO=="", ]
  print_1026a$code <- 1026
  print_1026a$melding <- "administratief veld ONDRZKO mist (B4)"
  #1026b
  print_1026b <- cert[cert$OPDRNRO=="", ]
  print_1026b$code <- 1026
  print_1026b$melding <- "administratief veld OPDRNRO mist (B4)"
  #1026c
  print_1026c <- cert[cert$DATUMO=="", ]
  print_1026c$code <- 1026
  print_1026c$melding <- "administratief veld DATUMO mist (B4)"
  #1026d
  print_1026d <- cert[cert$INITO=="", ]
  print_1026d$code <- 1026
  print_1026d$melding <- "administratief veld INITO mist (B4)"
  #1026e
  print_1026e <- cert[cert$VERSIEO=="", ]
  print_1026e$code <- 1026
  print_1026e$melding <- "administratief veld VERSIEO mist (B4)"
  #bind
  print_1026 <- rbind(print_1026a, print_1026b, print_1026c, print_1026d, print_1026e)
  print_1026$entry_BR <- NA
  print_1026$entry_expected <- NA
  if(length(print_1026$IDNR)==0){
    rm(print_1026)
  } else{
    l$print_1026 <- complete_cert(print_1026)
    rm(print_1026)
  }
  rm(print_1026a, print_1026b, print_1026c, print_1026d, print_1026e)
  
  #1027a
  print_1027a <- place[place$ONDRZKO=="", ]
  print_1027a$code <- 1027
  print_1027a$melding <- "administratief veld ONDRZKO mist (B4)"
  #1027b
  print_1027b <- place[place$OPDRNRO=="", ]
  print_1027b$code <- 1027
  print_1027b$melding <- "administratief veld OPDRNRO mist (B4)"
  #1027c
  print_1027c <- place[place$DATUMO=="", ]
  print_1027c$code <- 1027
  print_1027c$melding <- "administratief veld DATUMO mist (B4)"
  #1027d
  print_1027d <- place[place$INITO=="", ]
  print_1027d$code <- 1027
  print_1027d$melding <- "administratief veld INITO mist (B4)"
  #1027e
  print_1027e <- place[place$VERSIEO=="", ]
  print_1027e$code <- 1027
  print_1027e$melding <- "administratief veld VERSIEO mist (B4)"
  #bind
  print_1027 <- rbind(print_1027a, print_1027b, print_1027c, print_1027d, print_1027e)
  print_1027$entry_BR <- NA
  print_1027$entry_expected <- NA
  if(length(print_1027$IDNR)==0){
    rm(print_1027)
  } else{
    l$print_1027 <- complete_other(print_1027)
    rm(print_1027)
  }
  rm(print_1027a, print_1027b, print_1027c, print_1027d, print_1027e)
  
  #1092
  print_1092 <- place[grepl("\\}", place$location) & grepl("[0-9]\\}", place$location)==F,]
  print_1092$code <- 1092
  print_1092$melding <- "} gebruikt als aanduiding persoonlijk adres (t/m v4.01)"
  print_1092$entry_BR <- print_1092$location
  print_1092$entry_expected <- ""
  if(length(print_1092$IDNR)==0){
    rm(print_1092)
  } else{
    l$print_1092 <- complete_other(print_1092)
    rm(print_1092)
  }
  
  
  
  
  ##################
  #### 5. HSNRP ####
  ##################
  
  #1185
  print_1185 <- stat_person[stat_person$HSN_RP==1 | stat_person$HSN_RP==5, ]
  print_1185 <- merge(print_1185, HSNRP, by="IDNR", all=F)
  print_1185 <- print_1185[print_1185$geslacht!=print_1185$RP_B_SEX,]
  print_1185$code <- 1185
  print_1185$melding <- "geslacht verschilt met HSNRP (gesamplede akte)"
  print_1185$entry_BR <- print_1185$geslacht
  print_1185$entry_expected <- print_1185$RP_B_SEX
  if(length(print_1185$IDNR)==0){
    rm(print_1185)
  } else{
    l$print_1185 <- complete_other(print_1185)
    rm(print_1185)
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
  
  
  
  
  
  