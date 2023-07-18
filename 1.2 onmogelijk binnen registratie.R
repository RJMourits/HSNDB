  
  #2.2 onmogelijk binnen registratie
  
  #########################
  #### 3. entry errors ####
  #########################
  
  #double OP
  #1006
  print_1006 <- stat_person[stat_person$HSN_RP==1,]
  print_1006 <- print_1006[duplicated(print_1006[,c("IDNR", "ID_bron", "dag_inschrijving", "maand_inschrijving", "jaar_inschrijving", "dag_adres", "maand_adres", "jaar_adres")]),]
  print_1006$code <- 1006
  print_1006$melding <- "dubbele OP"
  print_1006$entry_BR <- NA
  print_1006$entry_expected <- NA
  if(length(print_1006$IDNR)==0){
    rm(print_1006)
  } else{
    l$print_1006 <- complete_other(print_1006)
    rm(print_1006)
  }
  
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
  print_1064 <- place[duplicated(place$temp_id_adres2) & place$address_type!="AN",]
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
  
  #ongeldige invoer
  #1089
  print_1089 <- place[grepl("\\*\\*", place$location) |
                        grepl("\\*.*\\*", place$location),]
  print_1089$code <- 1089
  print_1089$melding <- "2x * in straatnaam"
  print_1089$entry_BR <- print_1089$location
  print_1089$entry_expected <- ""
  if(length(print_1089$IDNR)==0){
    rm(print_1089)
  } else{
    l$print_1089 <- complete_other(print_1089)
    rm(print_1089)
  }
  
  #1090
  print_1090 <- place[grepl("//", place$location) |
                        grepl("/.*/", place$location),]
  print_1090$code <- 1090
  print_1090$melding <- "2x / in straatnaam"
  print_1090$entry_BR <- print_1090$location
  print_1090$entry_expected <- ""
  if(length(print_1090$IDNR)==0){
    rm(print_1090)
  } else{
    l$print_1090 <- complete_other(print_1090)
    rm(print_1090)
  }
  
  #1091
  
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
  
  #1075-1076: depricated
  #1077
  print_1077 <- place[duplicated(place[,c("IDNR", "ID_bron", "simultaannummer", "dag_inschrijving", "maand_inschrijving", "jaar_inschrijving", "address_type")]) & place$simultaannummer>0, ]
  print_1077 <- print_1077[print_1077$address_type!="AN",]
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
  
  #1002a
  print_1002a <- stat_person[stat_person$maand_inschrijving==1 & stat_person$dag_inschrijving>31 |
                               stat_person$maand_inschrijving==2 & stat_person$dag_inschrijving>29 |
                               stat_person$maand_inschrijving==3 & stat_person$dag_inschrijving>31 |
                               stat_person$maand_inschrijving==4 & stat_person$dag_inschrijving>30 |
                               stat_person$maand_inschrijving==5 & stat_person$dag_inschrijving>31 |
                               stat_person$maand_inschrijving==6 & stat_person$dag_inschrijving>30 |
                               stat_person$maand_inschrijving==7 & stat_person$dag_inschrijving>31 |
                               stat_person$maand_inschrijving==8 & stat_person$dag_inschrijving>31 |
                               stat_person$maand_inschrijving==9 & stat_person$dag_inschrijving>30 |
                               stat_person$maand_inschrijving==10 & stat_person$dag_inschrijving>31 |
                               stat_person$maand_inschrijving==11 & stat_person$dag_inschrijving>30 |
                               stat_person$maand_inschrijving==12 & stat_person$dag_inschrijving>31, ]
  print_1002a$code <- 1002
  print_1002a$melding <- "ongeldigde startdag"
  print_1002a$entry_BR <- print_1002a$dag_inschrijving
  print_1002a$entry_expected <- ""
  #1002b
  print_1002b <- stat_person[stat_person$maand_adres==1 & stat_person$dag_adres>31 |
                               stat_person$maand_adres==2 & stat_person$dag_adres>29 |
                               stat_person$maand_adres==3 & stat_person$dag_adres>31 |
                               stat_person$maand_adres==4 & stat_person$dag_adres>30 |
                               stat_person$maand_adres==5 & stat_person$dag_adres>31 |
                               stat_person$maand_adres==6 & stat_person$dag_adres>30 |
                               stat_person$maand_adres==7 & stat_person$dag_adres>31 |
                               stat_person$maand_adres==8 & stat_person$dag_adres>31 |
                               stat_person$maand_adres==9 & stat_person$dag_adres>30 |
                               stat_person$maand_adres==10 & stat_person$dag_adres>31 |
                               stat_person$maand_adres==11 & stat_person$dag_adres>30 |
                               stat_person$maand_adres==12 & stat_person$dag_adres>31, ]
  print_1002b$code <- 1002
  print_1002b$melding <- "ongeldigde mutatiedag"
  print_1002b$entry_BR <- print_1002b$dag_adres
  print_1002b$entry_expected <- ""
  #1002c
  print_1002c <- stat_person[stat_person$geboortemaand==1 & stat_person$geboortedag>31 |
                               stat_person$geboortemaand==2 & stat_person$geboortedag>29 |
                               stat_person$geboortemaand==3 & stat_person$geboortedag>31 |
                               stat_person$geboortemaand==4 & stat_person$geboortedag>30 |
                               stat_person$geboortemaand==5 & stat_person$geboortedag>31 |
                               stat_person$geboortemaand==6 & stat_person$geboortedag>30 |
                               stat_person$geboortemaand==7 & stat_person$geboortedag>31 |
                               stat_person$geboortemaand==8 & stat_person$geboortedag>31 |
                               stat_person$geboortemaand==9 & stat_person$geboortedag>30 |
                               stat_person$geboortemaand==10 & stat_person$geboortedag>31 |
                               stat_person$geboortemaand==11 & stat_person$geboortedag>30 |
                               stat_person$geboortemaand==12 & stat_person$geboortedag>31, ]
  print_1002c$code <- 1002
  print_1002c$melding <- "ongeldigde geboortedag"
  print_1002c$entry_BR <- print_1002c$geboortedag
  print_1002c$entry_expected <- ""
  #1002d
  print_1002d <- stat_person[stat_person$overlijdensmaand==1 & stat_person$overlijdensdag>31 |
                               stat_person$overlijdensmaand==2 & stat_person$overlijdensdag>29 |
                               stat_person$overlijdensmaand==3 & stat_person$overlijdensdag>31 |
                               stat_person$overlijdensmaand==4 & stat_person$overlijdensdag>30 |
                               stat_person$overlijdensmaand==5 & stat_person$overlijdensdag>31 |
                               stat_person$overlijdensmaand==6 & stat_person$overlijdensdag>30 |
                               stat_person$overlijdensmaand==7 & stat_person$overlijdensdag>31 |
                               stat_person$overlijdensmaand==8 & stat_person$overlijdensdag>31 |
                               stat_person$overlijdensmaand==9 & stat_person$overlijdensdag>30 |
                               stat_person$overlijdensmaand==10 & stat_person$overlijdensdag>31 |
                               stat_person$overlijdensmaand==11 & stat_person$overlijdensdag>30 |
                               stat_person$overlijdensmaand==12 & stat_person$overlijdensdag>31, ]
  print_1002d$code <- 1002
  print_1002d$melding <- "ongeldigde overlijdensdag"
  print_1002d$entry_BR <- print_1002d$overlijdensdag
  print_1002d$entry_expected <- ""
  #1002e
  print_1002e <- stat_person[stat_person$maand_inschrijving>12, ]
  print_1002e$code <- 1002
  print_1002e$melding <- "ongeldigde startmaand"
  print_1002e$entry_BR <- print_1002e$maand_inschrijving
  print_1002e$entry_expected <- ""
  #1002f
  print_1002f <- stat_person[stat_person$maand_adres>12, ]
  print_1002f$code <- 1002
  print_1002f$melding <- "ongeldigde mutatiemaand"
  print_1002f$entry_BR <- print_1002f$maand_adres
  print_1002f$entry_expected <- ""
  #1002g
  print_1002g <- stat_person[stat_person$geboortemaand>12, ]
  print_1002g$code <- 1002
  print_1002g$melding <- "ongeldigde geboortemaand"
  print_1002g$entry_BR <- print_1002g$geboortemaand
  print_1002g$entry_expected <- ""
  #1002h
  print_1002h <- stat_person[stat_person$overlijdensmaand>12, ]
  print_1002h$code <- 1002
  print_1002h$melding <- "ongeldigde overlijdensmaand"
  print_1002h$entry_BR <- print_1002h$overlijdensmaand
  print_1002h$entry_expected <- ""
  #1002i
  print_1002i <- stat_person[grepl("[0-9][0-9][0-9][0-9]", stat_person$jaar_inschrijving)==F & stat_person$jaar_inschrijving>0, ]
  print_1002i$code <- 1002
  print_1002i$melding <- "ongeldig startjaar"
  print_1002i$entry_BR <- print_1002i$jaar_inschrijving
  print_1002i$entry_expected <- ""
  #1002j
  print_1002j <- stat_person[grepl("[0-9][0-9][0-9][0-9]", stat_person$jaar_adres)==F & stat_person$jaar_adres>0, ]
  print_1002j$code <- 1002
  print_1002j$melding <- "ongeldig mutatiejaar"
  print_1002j$entry_BR <- print_1002j$jaar_adres
  print_1002j$entry_expected <- ""
  #1002k
  print_1002k <- stat_person[grepl("[0-9][0-9][0-9][0-9]", stat_person$geboortejaar)==F & stat_person$geboortejaar>0, ]
  print_1002k$code <- 1002
  print_1002k$melding <- "ongeldig geboortejaar"
  print_1002k$entry_BR <- print_1002k$geboortejaar
  print_1002k$entry_expected <- ""
  #1002l
  print_1002l <- stat_person[grepl("[0-9][0-9][0-9][0-9]", stat_person$overlijdensjaar)==F & stat_person$overlijdensjaar>0, ]
  print_1002l$code <- 1002
  print_1002l$melding <- "ongeldig overlijdensjaar"
  print_1002l$entry_BR <- print_1002l$overlijdensjaar
  print_1002l$entry_expected <- ""
  #bind
  print_1002 <- rbind(print_1002a, print_1002b, print_1002c, print_1002d, 
                      print_1002e, print_1002f, print_1002g, print_1002h,
                      print_1002i, print_1002j, print_1002k, print_1002l)
  if(length(print_1002$IDNR)==0){
    rm(print_1002)
  } else{
    l$print_1002 <- complete_other(print_1002)
    rm(print_1002)
  }
  rm(print_1002a, print_1002b, print_1002c, print_1002d, 
     print_1002e, print_1002f, print_1002g, print_1002h,
     print_1002i, print_1002j, print_1002k, print_1002l)
  
  #1003
  print_1003 <- stat_person[stat_person$HSN_RP==1 & stat_person$HSN_RP==5,]
  #1003a
  print_1003a <- print_1003[print_1003$dag_adres<=0 & print_1003$maand_adres!=0 & print_1003$jaar_adres!=0,]
  print_1003a$code <- 1003
  print_1003a$melding <- "dag inschrijving OP ongeldig"
  print_1003a$entry_BR <- print_1003a$dag_adres
  print_1003a$entry_expected <- NA
  #1003b
  print_1003b <- print_1003[print_1003$maand_adres<=0 & print_1003$dag_adres!=0 & print_1003$jaar_adres!=0,]
  print_1003b$code <- 1003
  print_1003b$melding <- "maand inschrijving OP ongeldig"
  print_1003b$entry_BR <- print_1003b$dag_adres
  print_1003b$entry_expected <- NA
  #bind
  print_1003 <- rbind(print_1003a, print_1003b)
  if(length(print_1003$IDNR)==0){
    rm(print_1003)
  } else{
    l$print_1003 <- complete_other(print_1003)
    rm(print_1003)
  }
  rm(print_1003a, print_1003b)
  
  #1004
  print_1004 <- stat_person[stat_person$HSN_RP==1 & stat_person$HSN_RP==5,]
  print_1004 <- print_1004[print_1004$dag_adres>0 & print_1004$maand_adres>0 & print_1004$jaar_adres>0,]
  print_1004 <- print_1004[print_1004$jaar_adres<print_1004$jaar_inschrijving |
                             print_1004$jaar_adres<print_1004$jaar_inschrijving & print_1004$maand_adres<print_1004$maand_inschrijving | 
                             print_1004$jaar_adres<print_1004$jaar_inschrijving & print_1004$maand_adres==print_1004$maand_inschrijving & print_1004$dag_adres<print_1004$dag_inschrijving,]
  print_1004$code <- 1004
  print_1004$melding <- "adresdatum OP voor hoofddatum"
  print_1004$entry_BR <- NA
  print_1004$entry_expected <- NA
  if(length(print_1004$IDNR)==0){
    rm(print_1004)
  } else{
    l$print_1004 <- complete_other(print_1004)
    rm(print_1004)
  }
  
  #1005
  print_1005 <- dyn_person[dyn_person$variable==1 & dyn_person$code==1 & dyn_person$HSN_RP==1,]
  print_1005 <- stat_person[stat_person$HSN_RP==1 & stat_person$temp_id_person %in% print_1005$temp_id_person |
                              stat_person$HSN_RP==5 & stat_person$temp_id_person %in% print_1005$temp_id_person,]
  #1005a
  print_1005a <- print_1005[print_1005$dag_adres>0 & print_1005$dag_inschrijving!=print_1005$dag_adres,]
  print_1005a$code <- 1005
  print_1005a$entry_BR <- print_1005a$dag_inschrijving
  print_1005a$entry_expected <- print_1005a$dag_adres
  print_1005a$melding <- "dag inschrijving en herkomstdatum verschillen, maar OP is hoofd"
  #1005b
  print_1005b <- print_1005[print_1005$dag_adres>0 & print_1005$maand_inschrijving!=print_1005$maand_adres,]
  print_1005b$code <- 1005
  print_1005b$entry_BR <- print_1005b$maand_inschrijving
  print_1005b$entry_expected <- print_1005b$maand_adres
  print_1005b$melding <- "maand inschrijving en herkomstdatum verschillen, maar OP is hoofd"
  #1005c
  print_1005c <- print_1005[print_1005$dag_adres>0 & print_1005$jaar_inschrijving!=print_1005$jaar_adres,]
  print_1005c$code <- 1005
  print_1005c$entry_BR <- print_1005c$jaar_inschrijving
  print_1005c$entry_expected <- print_1005c$jaar_adres
  print_1005c$melding <- "jaar inschrijving en herkomstdatum verschillen, maar OP is hoofd"
  #bind
  print_1005 <- rbind(print_1005a, print_1005b, print_1005c)
  if(length(print_1005$IDNR)==0){
    rm(print_1005)
  } else{
    l$print_1005 <- complete_other(print_1005)
    rm(print_1005)
  }
  rm(print_1005a, print_1005b, print_1005c)
  
  #1116, 1117: integrated into 1002
  
  #1124
  print_1124 <- dyn_person[dyn_person$variable==1 & dyn_person$code==1, ]
  print_1124 <- stat_person[stat_person$temp_id_person %in% print_1124$temp_id_person,]
  #1124a
  print_1124a <- print_1124[print_1124$jaar_inschrijving<print_1124$geboortejaar, ]
  print_1124a$code <- 1124
  print_1124a$melding <- "jaar hoofddatum valt voor geboortejaar"
  print_1124a$entry_BR <- paste(print_1124a$dag_inschrijving, print_1124a$maand_inschrijving, print_1124a$jaar_inschrijving, sep="-")
  print_1124a$entry_expected <- paste(print_1124a$geboortedag, print_1124a$geboortemaand, print_1124a$geboortejaar, sep="-")
  #1124b
  print_1124b <- print_1124[print_1124$jaar_inschrijving==print_1124$geboortejaar &
                              print_1124$maand_inschrijving<print_1124$geboortemaand, ]
  print_1124b$code <- 1124
  print_1124b$melding <- "maand hoofddatum valt voor geboortemaand"
  print_1124b$entry_BR <- print_1124b$maand_inschrijving
  print_1124b$entry_expected <- print_1124b$geboortemaand
  #1124c
  print_1124c <- print_1124[print_1124$jaar_inschrijving==print_1124$geboortejaar &
                              print_1124$maand_inschrijving==print_1124$geboortemaand &
                              print_1124$dag_inschrijving<print_1124$geboortedag, ]
  print_1124c$code <- 1124
  print_1124c$melding <- "dag hoofddatum valt voor geboortedag"
  print_1124c$entry_BR <- print_1124c$dag_inschrijving
  print_1124c$entry_expected <- print_1124c$geboortedag
  #rbind
  print_1124 <- rbind(print_1124a, print_1124b, print_1124c)
  if(length(print_1124$IDNR)==0){
    rm(print_1124)
  } else{
    l$print_1124 <- complete_other(print_1124)
    rm(print_1124)
  }
  rm(print_1124a, print_1124b, print_1124c)
  
  #1125
  print_1125 <- dyn_person[dyn_person$variable==1 & dyn_person$code==1, ]
  print_1125 <- stat_person[stat_person$temp_id_person %in% print_1125$temp_id_person, ]
  #1125a
  print_1125a <- print_1125[print_1125$jaar_inschrijving>print_1125$overlijdensjaar & print_1125$overlijdensjaar>0, ]
  print_1125a$code <- 1125
  print_1125a$melding <- "jaar hoofddatum valt na overlijdensjaar"
  print_1125a$entry_BR <- paste(print_1125a$dag_inschrijving, print_1125a$maand_inschrijving, print_1125a$jaar_inschrijving, sep="-")
  print_1125a$entry_expected <- paste(print_1125a$overlijdensdag, print_1125a$overlijdensmaand, print_1125a$overlijdensjaar, sep="-")
  #1125b
  print_1125b <- print_1125[print_1125$jaar_inschrijving==print_1125$overlijdensjaar & print_1125$overlijdensjaar>0 &
                              print_1125$maand_inschrijving>print_1125$overlijdensmaand, ]
  print_1125b$code <- 1125
  print_1125b$melding <- "maand hoofddatum valt na overlijdensmaand"
  print_1125b$entry_BR <- print_1125b$maand_inschrijving
  print_1125b$entry_expected <- print_1125b$overlijdensmaand
  #1125c
  print_1125c <- print_1125[print_1125$jaar_inschrijving==print_1125$overlijdensjaar & print_1125$overlijdensjaar>0 &
                              print_1125$maand_inschrijving==print_1125$overlijdensmaand &
                              print_1125$dag_inschrijving>print_1125$overlijdensdag, ]
  print_1125c$code <- 1125
  print_1125c$melding <- "dag hoofddatum valt na overlijdensdag"
  print_1125c$entry_BR <- print_1125c$dag_inschrijving
  print_1125c$entry_expected <- print_1125c$overlijdensdag
  #rbind
  print_1125 <- rbind(print_1125a, print_1125b, print_1125c)
  if(length(print_1125$IDNR)==0){
    rm(print_1125)
  } else{
    l$print_1125 <- complete_other(print_1125)
    rm(print_1125)
  }
  rm(print_1125a, print_1125b, print_1125c)
  
  #1126
  print_1126 <- dyn_person[dyn_person$variable==1 & dyn_person$code==1, ]
  print_1126 <- place[place$temp_id_person %in% print_1126$temp_id_person, ]
  #1126a
  print_1126a <- print_1126[!duplicated(print_1126$temp_id_cert) & 
                              print_1126$jaar_inschrijving<print_1126$jaar_adres,]
  print_1126a$code <- 1126
  print_1126a$melding <- "jaar hoofddatum valt voor aankomstdatum hoofd"
  print_1126a$entry_BR <- paste(print_1126a$dag_inschrijving, print_1126a$maand_inschrijving, print_1126a$jaar_inschrijving, sep="-")
  print_1126a$entry_expected <- paste(print_1126a$dag_adres, print_1126a$maand_adres, print_1126a$jaar_adres, sep="-")
  #1126b
  print_1126b <- print_1126[!duplicated(print_1126$temp_id_cert) & 
                              print_1126$jaar_inschrijving==print_1126$jaar_adres &
                              print_1126$maand_inschrijving<print_1126$maand_adres, ]
  print_1126b$code <- 1126
  print_1126b$melding <- "maand hoofddatum valt voor aankomstdatum hoofd"
  print_1126b$entry_BR <- print_1126b$maand_inschrijving
  print_1126b$entry_expected <- print_1126b$maand_adres
  #1126c
  print_1126c <- print_1126[!duplicated(print_1126$temp_id_cert) & 
                              print_1126$jaar_inschrijving==print_1126$jaar_adres &
                              print_1126$maand_inschrijving==print_1126$maand_adres &
                              print_1126$dag_inschrijving<print_1126$dag_adres, ]
  print_1126c$code <- 1126
  print_1126c$melding <- "dag hoofddatum valt voor aankomstdatum hoofd"
  print_1126c$entry_BR <- print_1126c$dag_inschrijving
  print_1126c$entry_expected <- print_1126c$dag_adres
  #rbind
  print_1126 <- rbind(print_1126a, print_1126b, print_1126c)
  if(length(print_1126$IDNR)==0){
    rm(print_1126)
  } else{
    l$print_1126 <- complete_other(print_1126)
    rm(print_1126)
  }
  rm(print_1126a, print_1126b, print_1126c)
  
  #1127
  print_1127 <- dyn_person[dyn_person$variable==1 & dyn_person$code==1, ]
  print_1127 <- place[place$temp_id_person %in% print_1127$temp_id_person, ]
  #1127a
  print_1127a <- print_1127[print_1127$jaar_inschrijving>print_1127$jaar_adres & print_1127$jaar_adres>0,]
  print_1127a$code <- 1127
  print_1127a$melding <- "jaar hoofddatum valt voor aankomstdatum hoofd"
  print_1127a$entry_BR <- paste(print_1127a$dag_inschrijving, print_1127a$maand_inschrijving, print_1127a$jaar_inschrijving, sep="-")
  print_1127a$entry_expected <- paste(print_1127a$dag_adres, print_1127a$maand_adres, print_1127a$jaar_adres, sep="-")
  #1127b
  print_1127b <- print_1127[print_1127$jaar_inschrijving==print_1127$jaar_adres & print_1127$jaar_adres>0 &
                              print_1127$maand_inschrijving>print_1127$maand_adres, ]
  print_1127b$code <- 1127
  print_1127b$melding <- "maand hoofddatum valt voor aankomstdatum hoofd"
  print_1127b$entry_BR <- print_1127b$maand_inschrijving
  print_1127b$entry_expected <- print_1127b$maand_adres
  #1127c
  print_1127c <- print_1127[print_1127$jaar_inschrijving==print_1127$jaar_adres & print_1127$jaar_adres>0 &
                              print_1127$maand_inschrijving==print_1127$maand_adres &
                              print_1127$dag_inschrijving<print_1127$dag_adres, ]
  print_1127c$code <- 1127
  print_1127c$melding <- "dag hoofddatum valt voor aankomstdatum hoofd"
  print_1127c$entry_BR <- print_1127c$dag_inschrijving
  print_1127c$entry_expected <- print_1127c$dag_adres
  #rbind
  print_1127 <- rbind(print_1127a, print_1127b, print_1127c)
  if(length(print_1127$IDNR)==0){
    rm(print_1127)
  } else{
    l$print_1127 <- complete_other(print_1127)
    rm(print_1127)
  }
  rm(print_1127a, print_1127b, print_1127c)
  
  #1130
  print_1130 <- place[place$volgnummer==0, ]
  #1130a
  print_1130a <- print_1130[!duplicated(print_1130$temp_id_cert) & 
                              print_1130$jaar_inschrijving<print_1130$jaar_adres,]
  print_1130a$code <- 1130
  print_1130a$melding <- "jaar hoofddatum valt voor adresdatum hoofd"
  print_1130a$entry_BR <- paste(print_1130a$dag_inschrijving, print_1130a$maand_inschrijving, print_1130a$jaar_inschrijving, sep="-")
  print_1130a$entry_expected <- paste(print_1130a$dag_adres, print_1130a$maand_adres, print_1130a$jaar_adres, sep="-")
  #1130b
  print_1130b <- print_1130[!duplicated(print_1130$temp_id_cert) & 
                              print_1130$jaar_inschrijving==print_1130$jaar_adres &
                              print_1130$maand_inschrijving<print_1130$maand_adres, ]
  print_1130b$code <- 1130
  print_1130b$melding <- "maand hoofddatum valt voor adresdatum hoofd"
  print_1130b$entry_BR <- print_1130b$maand_inschrijving
  print_1130b$entry_expected <- print_1130b$maand_adres
  #rbind
  print_1130 <- rbind(print_1130a, print_1130b)
  if(length(print_1130$IDNR)==0){
    rm(print_1130)
  } else{
    l$print_1130 <- complete_other(print_1130)
    rm(print_1130)
  }
  rm(print_1130a, print_1130b)
  
  #1128
  print_1128 <- place[place$volgnummer==0, ]
  #1128c
  print_1128 <- print_1128[!duplicated(print_1128$temp_id_cert) & 
                             print_1128$jaar_inschrijving==print_1128$jaar_adres &
                             print_1128$maand_inschrijving==print_1128$maand_adres &
                             print_1128$dag_inschrijving<print_1128$dag_adres, ]
  print_1128$code <- 1128
  print_1128$melding <- "dag hoofddatum valt voor adresdatum hoofd"
  print_1128$entry_BR <- print_1128$dag_inschrijving
  print_1128$entry_expected <- print_1128$dag_adres
  if(length(print_1128$IDNR)==0){
    rm(print_1128)
  } else{
    l$print_1128 <- complete_other(print_1128)
    rm(print_1128)
  }
  
  #1131
  print_1131 <- merge(place, AINB[,c("ID_bron", "source_type")], by="ID_bron", all.x=T)
  print_1131 <- print_1131[print_1131$source_type=="A" & print_1131$jaar_adres<1863 & print_1131$jaar_adres>0 | 
                             print_1131$source_type=="B" & print_1131$jaar_adres<1863 & print_1131$jaar_adres>0 | 
                             print_1131$source_type=="C" & print_1131$jaar_adres<1850 & print_1131$jaar_adres>0 | 
                             print_1131$source_type=="D" & print_1131$jaar_adres<1812 & print_1131$jaar_adres>0 | 
                             print_1131$source_type=="G" & print_1131$jaar_adres<1890 & print_1131$jaar_adres>0 | 
                             print_1131$source_type=="I" & print_1131$jaar_adres<1890 & print_1131$jaar_adres>0 | 
                             print_1131$source_type=="V" & print_1131$jaar_adres<1812 & print_1131$jaar_adres>0, ]
  print_1131$code <- 1131
  print_1131$melding <- "jaar adresdatum voor start register"
  print_1131$entry_BR <- print_1131$jaar_adres
  print_1131$entry_expected <- ifelse(print_1131$source_type=="A", 1863,
                                      ifelse(print_1131$source_type=="B", 1863,
                                             ifelse(print_1131$source_type=="C", 1850,
                                                    ifelse(print_1131$source_type=="D", 1812,
                                                           ifelse(print_1131$source_type=="G", 1890,
                                                                  ifelse(print_1131$source_type=="I", 1890,
                                                                         ifelse(print_1131$source_type=="V", 1812, NA)))))))
  if(length(print_1131$IDNR)==0){
    rm(print_1131)
  } else{
    l$print_1131 <- complete_other(print_1131)
    rm(print_1131)
  }
  
  #1132
  print_1132 <- merge(place, AINB[,c("ID_bron", "source_type")], by="ID_bron", all.x=T)
  print_1132 <- print_1132[print_1132$source_type=="A" & print_1132$jaar_adres>1940 | 
                             print_1132$source_type=="B" & print_1132$jaar_adres>1940 | 
                             print_1132$source_type=="C" & print_1132$jaar_adres>1862 | 
                             print_1132$source_type=="D" & print_1132$jaar_adres>1850 | 
                             print_1132$source_type=="G" & print_1132$jaar_adres>1940 | 
                             print_1132$source_type=="I" & print_1132$jaar_adres>1940 | 
                             print_1132$source_type=="V" & print_1132$jaar_adres>1850, ]
  print_1132$code <- 1132
  print_1132$melding <- "hoofddatum na eind register"
  print_1132$entry_BR <- print_1132$jaar_adres
  print_1132$entry_expected <- ifelse(print_1132$source_type=="A", 1940,
                                      ifelse(print_1132$source_type=="B", 1940,
                                             ifelse(print_1132$source_type=="C", 1862,
                                                    ifelse(print_1132$source_type=="D", 1850,
                                                           ifelse(print_1132$source_type=="G", 1940,
                                                                  ifelse(print_1132$source_type=="I", 1940,
                                                                         ifelse(print_1132$source_type=="V", 1850, NA)))))))
  if(length(print_1132$IDNR)==0){
    rm(print_1132)
  } else{
    l$print_1132 <- complete_cert(print_1132)
    rm(print_1132)
  }
  
  #1145
  print_1145 <- AINB[AINB$source_type!="A" & AINB$source_type!="I",] #A or I
  print_1145 <- dyn_person[dyn_person$ID_bron %in% print_1145$ID_bron & dyn_person$variable==1 & dyn_person$code==1,]
  print_1145 <- stat_person[stat_person$temp_id_person %in% print_1145$temp_id_person,]
  print_1145 <- print_1145[(print_1145$temp-print_1145$geboortejaar)<=12, ]
  print_1145$code <- 1145
  print_1145$melding <- "hoofd <12 jaar"
  print_1145$entry_BR <- paste(print_1145$geboortedag, print_1145$geboortemaand, print_1145$geboortejaar, sep="-")
  print_1145$entry_expected <- NA
  if(length(print_1145$IDNR)==0){
    rm(print_1145)
  } else{
    l$print_1145 <- complete_cert(print_1145)
    rm(print_1145)
  }
  
  
  
  
  ##################
  #### 5. HSNRP ####
  ##################
  
  #1184
  print_1184 <- stat_person[stat_person$HSN_RP==1 | stat_person$HSN_RP==5, ]
  print_1184 <- print_1184[!duplicated(print_1184[, c("IDNR", "geslacht")]),]
  print_1184 <- print_1184[duplicated(print_1184$IDNR),]
  print_1184 <- stat_person[stat_person$HSN_RP==1 & stat_person$IDNR %in% print_1184$IDNR | 
                              stat_person$HSN_RP==5 & stat_person$IDNR %in% print_1184$IDNR,]
  print_1184$code <- 1184
  print_1184$melding <- "geslacht OP inconsistent"
  print_1184$entry_BR <- print_1184$geslacht
  print_1184$entry_expected <- NA
  if(length(print_1184$IDNR)==0){
    rm(print_1184)
  } else{
    l$print_1184 <- complete_other(print_1184)
    rm(print_1184)
  }
  
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
  
  
  
  
