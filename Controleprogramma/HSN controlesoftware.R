  
  #load libraries
  library(data.table)
  library(openxlsx)
  library(dplyr)
  
  rm(list=ls())
  
  #set working directory
  setwd("C:/Surfdrive/HSNDB/Controlesoftware")
  
  
  
  ########################
  #### set parameters ####
  ########################
  
  #opdracht <- "KS3"
  opdracht <- "KQ6"
  #opdracht <- "KR0"
  #opdracht <- "KR3"
  #opdracht <- "KR7"
  
  #ronde <- 1
  ronde <- 2.1
  #ronde <- 2.2
  
  
  
  #######################
  #### 0. open files ####
  #######################
  
  b2 <- fread("b2.csv", encoding="UTF-8", na="NULL")
  b3 <- fread("b3.csv", encoding="UTF-8", na="NULL")
  b4 <- fread("b4.csv", encoding="UTF-8", na="NULL")
  b6 <- fread("b6.csv", encoding="UTF-8", na="NULL")
  
  b1 <- fread("AINB.csv", encoding="UTF-8")
  HSNRP <- fread("HSNRP.csv", encoding="UTF-8", na="NULL")
  
  #standardisation files
  location <- fread("C:/Surfdrive/Data/Standaardisering/ref_location.csv", encoding="UTF-8", na="NULL")
  occupation <- fread("C:/Surfdrive/Data/Standaardisering/ref_occupation.csv", encoding="UTF-8", na="NULL")
  religion <- fread("C:/Surfdrive/Data/Standaardisering/ref_religion.csv", encoding="UTF-8", na="NULL")
  
  
  ###############################
  #### 1. filter by opdracht ####
  ###############################
  
  #temp filter
  stat_person <- b2[b2$OPDRNRI==opdracht,]
  dyn_person  <- b3[b3$OPDRNRI==opdracht,]
  cert        <- b4[b4$OPDRNRI==opdracht,]
  place       <- b6[b6$OPDRNRI==opdracht,]
  AINB        <- b1
  
  
  ########################################
  #### 2. recode and rename variables ####
  ########################################
    
  #AINB
  #add interpretation to main variable & flag interpretation
   #if available, use interpreted start dates
    AINB$est_start <- ifelse(AINB$B1BJBGCR>0, 1, 0)
    AINB$B1BJBG <- ifelse(AINB$B1BJBGCR>0, AINB$B1BJBGCR, AINB$B1BJBG)
   #if available, use interpreted end dates
    AINB$est_end <- ifelse(AINB$B1EJBGCR>0, 1, 0)
    AINB$B1EJBG <- ifelse(AINB$B1EJBGCR>0, AINB$B1EJBGCR, AINB$B1EJBG)
    
    
  #b2
  #add interpretation to main variable & flag interpretation
   #if available, use interpreted registration dates
    stat_person$est_adres <- ifelse(stat_person$B2RDCR>0 | stat_person$B2RMCR>0 | stat_person$B2RJCR>0, 1, 0)
    stat_person$B2RDNR <- ifelse(stat_person$B2RDCR>0, stat_person$B2RDCR, stat_person$B2RDNR)
    stat_person$B2RMNR <- ifelse(stat_person$B2RMCR>0, stat_person$B2RMCR, stat_person$B2RMNR)
    stat_person$B2RJNR <- ifelse(stat_person$B2RJCR>0, stat_person$B2RJCR, stat_person$B2RJNR)
   #if available, use interpreted birth dates
    stat_person$est_geb <- ifelse(stat_person$B2GDCR>0 | stat_person$B2GMCR>0 | stat_person$B2GJCR>0, 1, 0)
    stat_person$B2GDNR <- ifelse(stat_person$B2GDCR>0, stat_person$B2GDCR, stat_person$B2GDNR)
    stat_person$B2GMNR <- ifelse(stat_person$B2GMCR>0, stat_person$B2GMCR, stat_person$B2GMNR)
    stat_person$B2GJNR <- ifelse(stat_person$B2GJCR>0, stat_person$B2GJCR, stat_person$B2GJNR)
   #if available, use interpreted death dates
    stat_person$est_ove <- ifelse(stat_person$B2ODCR>0 | stat_person$B2OMCR>0 | stat_person$B2OJCR>0, 1, 0)
    stat_person$B2ODNR <- ifelse(stat_person$B2ODCR>0, stat_person$B2ODCR, stat_person$B2ODNR)
    stat_person$B2OMNR <- ifelse(stat_person$B2OMCR>0, stat_person$B2OMCR, stat_person$B2OMNR)
    stat_person$B2OJNR <- ifelse(stat_person$B2OJCR>0, stat_person$B2OJCR, stat_person$B2OJNR)
    
  #recode B2FCBG
    stat_person$B2FCBG <- ifelse(stat_person$B2FCBG==2, 0, 
                                 ifelse(stat_person$B2FCBG==5, 5, stat_person$B2FCBG))
    
    
  #B3
  #add interpretation to main variable & flag interpretation
    #if available, use interpreted registration dates
    dyn_person$est_mutation <- ifelse(dyn_person$B3MDCR>0 | dyn_person$B3MMCR>0 | dyn_person$B3MJCR>0, 1, 0)
    dyn_person$B3MDNR <- ifelse(dyn_person$B3MDCR>0, dyn_person$B3MDCR, dyn_person$B3MDNR)
    dyn_person$B3MMNR <- ifelse(dyn_person$B3MMCR>0, dyn_person$B3MMCR, dyn_person$B3MMNR)
    dyn_person$B3MJNR <- ifelse(dyn_person$B3MJCR>0, dyn_person$B3MJCR, dyn_person$B3MJNR)
    
  #recode B2FCBG
    dyn_person$B2FCBG <- ifelse(dyn_person$B2FCBG==2, 0, dyn_person$B2FCBG)
    
    
  #B6
  #add interpretation to main variable & flag interpretation
    place$est_place <- ifelse(place$B6MDCR>0 | place$B6MMCR>0 | place$B6MJCR>0, 1, 0)
    place$B6MDNR <- ifelse(place$B6MDCR>0, place$B6MDCR, place$B6MDNR)
    place$B6MMNR <- ifelse(place$B6MMCR>0, place$B6MMCR, place$B6MMNR)
    place$B6MJNR <- ifelse(place$B6MJCR>0, place$B6MJCR, place$B6MJNR)
    
    
  #HSNRP
    HSNRP$RP_family2 <- ifelse(grepl("-", HSNRP$RP_family), gsub(".*-", "", HSNRP$RP_family), HSNRP$RP_family)
    HSNRP$RP_family2 <- gsub(",.*", "", HSNRP$RP_family2)
    
    
  #rename fields in b2
  stat_person <- stat_person[, c("IDNR", "B1IDBG", #IDNRs 
                                 "B2FCBG", #RP
                                 "B2DIBG", "B2MIBG", "B2JIBG", #start register
                                 "B2RDNR", "B2RMNR", "B2RJNR", #registration date
                                 "est_adres", #registration date interpreted
                                 "B2RNBG", #volgnummer
                                 "B2VNNR", "B2ANNR", #given & family name
                                 "B2GSNR", #sex
                                 "B2GDNR", "B2GMNR", "B2GJNR", #date of birth
                                 "est_geb", #registration date interpreted
                                 "B2GNNR", #place of birth
                                 "B2ODNR", "B2OMNR", "B2OJNR", #date of death
                                 "est_ove",
                                 "B2ONNR", #place of death
                                 "B2NANR", #nationality
                                 "B2WDNR", #place of residence
                                 "B2VWNR", #verblijfsstatus
                                 "ONDRZKO", "OPDRNRO", "DATUMO", "INITO", "VERSIEO")] #administrative info
  #rename variables
  colnames(stat_person) <- c("IDNR", "ID_bron", 
                             "HSN_RP",
                             "dag_inschrijving", "maand_inschrijving", "jaar_inschrijving", 
                             "dag_adres", "maand_adres", "jaar_adres", 
                             "est_adres",
                             "volgnummer", 
                             "voornaam", "achternaam", 
                             "geslacht", 
                             "geboortedag", "geboortemaand", "geboortejaar",
                             "est_geb",
                             "geboorteplaats",
                             "overlijdensdag", "overlijdensmaand", "overlijdensjaar",
                             "est_ov",
                             "overlijdensplaats",
                             "nationaliteit",
                             "woonplaats",
                             "verblijfsstatus",
                             "ONDRZKO", "OPDRNRO", "DATUMO", "INITO", "VERSIEO")
  
  dyn_person <- dyn_person[, c("IDNR", "B1IDBG", #IDNRs
                               "B2FCBG", #RP
                               "B2DIBG", "B2MIBG", "B2JIBG", #start register
                               "B3MDNR", "B3MMNR", "B3MJNR", #mutation date
                               "est_mutation", #mutation date was interpreted
                               "B2RNBG", "B3VRNR", #volgnummer
                               "B3TYPE", "B3KODE", "B3GEGEVEN", #variable + value
                               "ONDRZKO", "OPDRNRO", "DATUMO", "INITO", "VERSIEO")] #administrative info
  #rename variables
  colnames(dyn_person) <- c("IDNR", "ID_bron", 
                            "HSN_RP",
                            "dag_inschrijving", "maand_inschrijving", "jaar_inschrijving", 
                            "dag_mutatie", "maand_mutatie", "jaar_mutatie",
                            "est_mutation",
                            "volgnummer", "volgnummer_dyn",
                            "variable", "code", "value", 
                            "ONDRZKO", "OPDRNRO", "DATUMO", "INITO", "VERSIEO")
  
  #rename fields b4
  cert <- cert[, c("IDNR", "B1IDBG", #IDNRs 
                   "B2DIBG", "B2MIBG", "B2JIBG", #start register
                   "B2FDBG", "B2FMBG", "B2FJBG", #registration date
                   "B2PGBG", "B2VHBG", "B4GKBG", #source information
                   "ONDRZKO", "OPDRNRO", "DATUMO", "INITO", "VERSIEO")] #administrative fields
  #rename variables
  colnames(cert) <- c("IDNR", "ID_bron", 
                      "dag_inschrijving", "maand_inschrijving", "jaar_inschrijving", 
                      "dag_adres", "maand_adres", "jaar_adres",
                      "page_number", "household_number", "GK_info",
                      "ONDRZKO", "OPDRNRO", "DATUMO", "INITO", "VERSIEO")
  
  #rename fields b6
  place <- place[, c("IDNR", "B1IDBG", #IDNRs 
                   "B2DIBG", "B2MIBG", "B2JIBG", #start register
                   "B6MDNR", "B6MMNR", "B6MJNR", #start address
                   "est_place", #start address date interpreted
                   "B2RNBG", "B6SINR", "B6VRNR", #volgnummer
                   "B6TPNR", #address type 
                   "B6STNR", "B6NRNR", "B6TVNR", #address
                   "ONDRZKO", "OPDRNRO", "DATUMO", "INITO", "VERSIEO")] #administrative fields
  #rename variables
  colnames(place) <- c("IDNR", "ID_bron",
                       "dag_inschrijving", "maand_inschrijving", "jaar_inschrijving", 
                       "dag_adres", "maand_adres", "jaar_adres",
                       "est_place",
                       "volgnummer", "simultaannummer", "volgnummer_adres",
                       "address_type",
                       "location", "nummer", "addendum",
                       "ONDRZKO", "OPDRNRO", "DATUMO", "INITO", "VERSIEO")
  
  #rename fields b1 / AINB
  AINB <- AINB[, c("B1IDBG", #IDNR
                   "B1ABBG", #source type 
                   "B1BJBG", "est_start", 
                   "B1EJBG", "est_end",
                   "GEMNAAM")]
  #rename variables
  colnames(AINB) <- c("ID_bron",
                      "source_type",
                      "jaar_start", "est_start", 
                      "jaar_eind", "est_end",
                      "GEMNAAM")
  
  #rename fields HSNRP
  HSNRP <- HSNRP[, c("IDNR", "RP_B_SEX", 
                     "RP_firstname", "RP_family", "RP_family2", 
                     "RP_B_DAY", "RP_B_MONTH", "RP_B_YEAR", "RP_B_PLACE")]
  
  
  #combine IDNR, ID_bron, dag_inschrijving, maand_inschrijving, jaar_inschrijving,volgnummer into 1 temp_id.
  stat_person$temp_id_person <- paste(stat_person$IDNR, stat_person$ID_bron, stat_person$dag_inschrijving, stat_person$maand_inschrijving, stat_person$jaar_inschrijving, stat_person$volgnummer, sep="_")
  dyn_person$temp_id_person  <- paste(dyn_person$IDNR, dyn_person$ID_bron, dyn_person$dag_inschrijving, dyn_person$maand_inschrijving, dyn_person$jaar_inschrijving, dyn_person$volgnummer, sep="_")
  place$temp_id_person       <- paste(place$IDNR, place$ID_bron, place$dag_inschrijving, place$maand_inschrijving, place$jaar_inschrijving, place$volgnummer, sep="_")
  #combine IDNR, ID_bron, dag_inschrijving, maand_inschrijving, jaar_inschrijving into 1 temp_id.
  stat_person$temp_id_cert   <- paste(stat_person$IDNR, stat_person$ID_bron, stat_person$dag_inschrijving, stat_person$maand_inschrijving, stat_person$jaar_inschrijving, sep="_")
  dyn_person$temp_id_cert    <- paste(dyn_person$IDNR, dyn_person$ID_bron, dyn_person$dag_inschrijving, dyn_person$maand_inschrijving, dyn_person$jaar_inschrijving, sep="_")
  cert$temp_id_cert          <- paste(cert$IDNR, cert$ID_bron, cert$dag_inschrijving, cert$maand_inschrijving, cert$jaar_inschrijving, sep="_")
  place$temp_id_cert         <- paste(place$IDNR, place$ID_bron, place$dag_inschrijving, place$maand_inschrijving, place$jaar_inschrijving, sep="_")
  #combine IDNR, ID_bron, dag_inschrijving, maand_inschrijving, jaar_inschrijving, dag_adres, maand_adres, jaar_adres into 1 temp_id.
  place$temp_id_adres        <- paste(place$IDNR, place$ID_bron, 
                                      place$dag_inschrijving, place$maand_inschrijving, place$jaar_inschrijving, place$dag_adres, place$maand_adres, place$jaar_adres, 
                                      place$volgnummer, place$simultaannummer, sep="_")
  place$temp_id_adres2       <- paste(place$IDNR, place$ID_bron, 
                                      place$dag_inschrijving, place$maand_inschrijving, place$jaar_inschrijving, place$dag_adres, place$maand_adres, place$jaar_adres, 
                                      place$volgnummer, place$simultaannummer, place$address_type, sep="_")
  
  #add dag_adres, maand_adres, jaar_adres, voornaam, achternaam to dyn_person & place
  dyn_person <- merge(dyn_person, stat_person[,c("temp_id_person", "dag_adres", "maand_adres", "jaar_adres", "voornaam", "achternaam")], by="temp_id_person", all=F)
  place <- merge(place, stat_person[,c("temp_id_person", "voornaam", "achternaam", "HSN_RP")], by="temp_id_person", all.x=T)
  
  #order data
  stat_person <- stat_person <- stat_person[order(stat_person[,c("IDNR", "jaar_inschrijving", "maand_inschrijving", "dag_inschrijving", "volgnummer")]),]
  dyn_person <- dyn_person[order(dyn_person[,c("IDNR", "jaar_inschrijving", "maand_inschrijving", "dag_inschrijving", "volgnummer")]),]
  cert <- cert[order(cert[,c("IDNR", "jaar_inschrijving", "maand_inschrijving", "dag_inschrijving")]),]
  place <- place[order(place[,c("IDNR", "jaar_inschrijving", "maand_inschrijving", "dag_inschrijving", "volgnummer_adres")]),]
  
  
  
  ################################
  #### 3. generate bug report ####
  ################################
  
  #write function to add columns required to print b4 / cert
  complete_cert <- function(x){
    x[["volgnummer"]] <- x[["voornaam"]] <- x[["achternaam"]] <- x[["HSN_RP"]] <- NA
    x <- x[, c("IDNR", "ID_bron", "dag_inschrijving", "maand_inschrijving", "jaar_inschrijving", 
               "volgnummer", "voornaam", "achternaam", 
               "dag_adres", "maand_adres", "jaar_adres", 
               "code", "melding", "entry_BR", "entry_expected",
               "HSN_RP")]
    x
  }
  
  #write function to add columns required to print b3 & b6
  complete_other <- function(x){
    x <- x[, c("IDNR", "ID_bron", "dag_inschrijving", "maand_inschrijving", "jaar_inschrijving", 
               "volgnummer", "voornaam", "achternaam", 
               "dag_adres", "maand_adres", "jaar_adres", 
               "code", "melding", "entry_BR", "entry_expected",
               "HSN_RP")]
    x
  }
  
  
  
  ###########################
  #### 3. compile report ####
  ###########################
  
    l <- list()
  
    if(ronde==1){
      source("C:/Surfdrive/HSNDB/Controlesoftware/Programma/1. voorronde datamanager.R")
    }
  
    if(ronde==2.1){
      source("C:/Surfdrive/HSNDB/Controlesoftware/Programma/2.1 verdacht binnen registratie.R")
      source("C:/Surfdrive/HSNDB/Controlesoftware/Programma/2.2 onmogelijk binnen registratie.R")
    }
  
    if(ronde==2.2){
      source("C:/Surfdrive/HSNDB/Controlesoftware/Programma/2.2 onmogelijk binnen registratie.R")
    }
    
    
    #order into one data frame
    HSNRP_print <- do.call(rbind, l)
    #set day=0 to NA
    HSNRP_print$dag_inschrijving[HSNRP_print$dag_inschrijving==0] <- NA
    HSNRP_print$maand_inschrijving[HSNRP_print$maand_inschrijving==0] <- NA
    HSNRP_print$jaar_inschrijving[HSNRP_print$jaar_inschrijving==0] <- NA
    HSNRP_print$dag_adres[HSNRP_print$dag_adres==0] <- NA
    HSNRP_print$maand_adres[HSNRP_print$maand_adres==0] <- NA
    HSNRP_print$jaar_adres[HSNRP_print$jaar_adres==0] <- NA
    #sort
    HSNRP_print <- HSNRP_print[order(HSNRP_print[,c("IDNR", "jaar_inschrijving", "maand_inschrijving", "dag_inschrijving", "volgnummer", "code")]),]
    #remove duplicated IDs for easy reading
    HSNRP_print <- HSNRP_print %>% mutate(ID_bron=ifelse(row_number()==1, ID_bron,
                                                         ifelse(IDNR==lag(IDNR) & ID_bron==lag(ID_bron), NA, ID_bron)))
    HSNRP_print$IDNR <- ifelse(duplicated(HSNRP_print$IDNR), NA, HSNRP_print$IDNR)
    
    #write output to excel sheet
    wb <- createWorkbook()
    addWorksheet(wb, opdracht)
    writeData(wb, opdracht, HSNRP_print)
    addStyle(wb, sheet=1,
             style=createStyle(textDecoration="bold"), 
             rows=as.numeric(row.names(HSNRP_print)[!is.na(HSNRP_print$HSN_RP) & HSNRP_print$HSN_RP==1])+1,
             cols=7:8,
             gridExpand=T)
    deleteData(wb, opdracht, rows=1:length(HSNRP_print$IDNR), cols=16, gridExpand=T)
    saveWorkbook(wb, paste("Output/", opdracht, " controlerapport ronde ", ronde, " (", Sys.Date(), ").xlsx", sep="") , overwrite=T)
    
    
    
    
    write.table(as.data.frame(table(HSNRP_print$code)),
                paste("Output/Metadata/", 
                      opdracht, " overzicht meldingen ronde ", 
                      if(ronde==2.1){"2.1 + 2.2"} else{ronde}, " (", Sys.Date(), ").csv", sep=""), 
                sep ="\t", 
                col.names=T, 
                row.names=F, 
                quote=F,
                fileEncoding="UTF-8")
    
    
    
    
    
    
    
    
    
    
    
    
