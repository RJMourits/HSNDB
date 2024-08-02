 #load packages
  library("dplyr"); library("data.table")
  options(dplyr.print_max = 40)
  
 #clean environment
  rm(list=ls())

 #load required files
  #set working directory
  setwd("C:/ff/2024-07")
  #load data frames
  pkadres <- fread("pkadres.csv", quote="", encoding="UTF-8")
  pkbrp <- fread("pkbrp.csv", quote="", encoding="UTF-8")
  pkeigknd <- fread("pkeigknd.csv", quote="", encoding="UTF-8")
  pkhuw <- fread("pkhuw.csv", quote="", encoding="UTF-8")
  pkknd <- fread("pkknd.csv", quote="", encoding="UTF-8")
  #load standardisation tables
  ref_location <- fread("ref_location.csv", encoding="UTF-8")
  ref_occupation <- fread("HSN_HISCO_release_2020_02.csv", encoding="UTF-8")
  ref_religion <- fread("ref_religion.csv", encoding="UTF-8")
  
  #######################
  #### HSN_RP_SSD_Id ####
  #######################
  
 #01b. filter RPs born after 1882 from pkknd and select relevant variables
  HSN_RP_SSD_Id <- pkknd[pkknd$IDNR<300000 & pkknd$GJRPERP>1882, c("IDNR", 
                                                                   "CTRDGP", "CTRMDP", "CTRJRP",
                                                                   "NATPERP", "GDSPERP", "GSLPERP", 
                                                                   "VNM1PERP", "VNM2PERP", "VNM3PERP", "ANMPERP",
                                                                   "GDGPERP", "GMDPERP", "GJRPERP", "GPLPERP",
                                                                   "ODGPERP", "OMDPERP", "OJRPERP", "OPLPERP",
                                                                   "VNM1MDRP", "VNM2MDRP", "VNM3MDRP", "ANMMDRP",
                                                                   "GDGMDRP", "GMDMDRP", "GJRMDRP", "GPLMDRP",
                                                                   "VNM1VDRP", "VNM2VDRP", "VNM3VDRP", "ANMVDRP",
                                                                   "GDGVDRP", "GMDVDRP", "GJRVDRP", "GPLVDRP"
                                                                   )]
  #rename columns
  colnames(HSN_RP_SSD_Id) <- c("IDNR", 
                               "Start_PK_day", " Start_PK_month", "Start_PK_year",
                               "Nationality", "Religion", "Sex_RP", 
                               "Firstname1", "Firstname2", "Firstname3", "Lastname",
                               "B_day_RP", "B_month_RP", "B_year_RP", "B_place",
                               "D_day_RP", "D_month_RP", "D_year_RP", "D_place",
                               "Firstname1_mother", "Firstname2_mother", "Firstname3_mother", "Lastname_mother",
                               "B_day_RP_mother", "B_month_RP_mother", "B_year_RP_mother", "B_place_mother",
                               "Firstname1_father", "Firstname2_father", "Firstname3_father", "Lastname_father",
                               "B_day_RP_father", "B_month_RP_father", "B_year_RP_father", "B_place_father"
                               )
  
  
  #Q_02a update Birthday father if NA
  HSN_RP_SSD_Id <- merge(HSN_RP_SSD_Id, pkknd[pkknd$IDNR<300000 & pkknd$GJRVDRPCR>1, c("IDNR", "GDGVDRPCR", "GMDVDRPCR", "GJRVDRPCR")],
                         by="IDNR", all.x=T)
  #check replaceable data
  length(which(HSN_RP_SSD_Id$B_day_RP_father<1 & !is.na(HSN_RP_SSD_Id$GDGVDRPCR)))
  length(which(HSN_RP_SSD_Id$B_day_RP_father<1 & !is.na(HSN_RP_SSD_Id$GMDVDRPCR)))
  length(which(HSN_RP_SSD_Id$B_day_RP_father<1 & !is.na(HSN_RP_SSD_Id$GJRVDRPCR)))
  #replace if B_day_RP_father is NA & GDxxVDRPCR !NA
  HSN_RP_SSD_Id$B_day_RP_father <- ifelse(HSN_RP_SSD_Id$B_day_RP_father<1 & !is.na(HSN_RP_SSD_Id$GDGVDRPCR), HSN_RP_SSD_Id$GDGVDRPCR, HSN_RP_SSD_Id$B_day_RP_father) 
  HSN_RP_SSD_Id$B_month_RP_father <- ifelse(HSN_RP_SSD_Id$B_month_RP_father<1 & !is.na(HSN_RP_SSD_Id$GMDVDRPCR), HSN_RP_SSD_Id$GMDVDRPCR, HSN_RP_SSD_Id$B_month_RP_father) 
  HSN_RP_SSD_Id$B_year_RP_father <- ifelse(HSN_RP_SSD_Id$B_day_RP_father<1 & !is.na(HSN_RP_SSD_Id$GJRVDRPCR), HSN_RP_SSD_Id$GJRVDRPCR, HSN_RP_SSD_Id$B_year_RP_father) 
  HSN_RP_SSD_Id <- HSN_RP_SSD_Id[, -c("GDGVDRPCR", "GMDVDRPCR", "GJRVDRPCR")]
  
  
  #Q_02b update Birthday mother if NA
  HSN_RP_SSD_Id <- merge(HSN_RP_SSD_Id, pkknd[pkknd$IDNR<300000 & pkknd$GJRMDRPCR>1, c("IDNR", "GDGMDRPCR", "GMDMDRPCR", "GJRMDRPCR")],
                         by="IDNR", all.x=T)
  #check replaceable data
  length(which(HSN_RP_SSD_Id$B_day_RP_mother<1 & !is.na(HSN_RP_SSD_Id$GDGMDRPCR)))
  length(which(HSN_RP_SSD_Id$B_day_RP_mother<1 & !is.na(HSN_RP_SSD_Id$GMDMDRPCR)))
  length(which(HSN_RP_SSD_Id$B_day_RP_mother<1 & !is.na(HSN_RP_SSD_Id$GJRMDRPCR)))
  #replace if B_day_RP_mother is NA & GDxxMDRPCR !NA
  HSN_RP_SSD_Id$B_day_RP_mother <- ifelse(HSN_RP_SSD_Id$B_day_RP_mother<1 & !is.na(HSN_RP_SSD_Id$GDGMDRPCR), HSN_RP_SSD_Id$GDGMDRPCR, HSN_RP_SSD_Id$B_day_RP_mother) 
  HSN_RP_SSD_Id$B_month_RP_mother <- ifelse(HSN_RP_SSD_Id$B_month_RP_mother<1 & !is.na(HSN_RP_SSD_Id$GMDMDRPCR), HSN_RP_SSD_Id$GMDMDRPCR, HSN_RP_SSD_Id$B_month_RP_mother) 
  HSN_RP_SSD_Id$B_year_RP_mother <- ifelse(HSN_RP_SSD_Id$B_day_RP_mother<1 & !is.na(HSN_RP_SSD_Id$GJRMDRPCR), HSN_RP_SSD_Id$GJRMDRPCR, HSN_RP_SSD_Id$B_year_RP_mother) 
  HSN_RP_SSD_Id <- HSN_RP_SSD_Id[, -c("GDGMDRPCR", "GMDMDRPCR", "GJRMDRPCR")]
  
  
  #Q_03a_HSNRP_SSD_basis_hulpfile_huwjaar
  #select last known marriages
  info_LP <- pkhuw %>% filter(IDNR<300000) %>% group_by(IDNR) %>% filter(HJRHUWP==max(HJRHUWP)) %>% ungroup()
  #deal with duplicates (set relevant variables to 0 and drop duplicates)
  info_LP <- info_LP %>% group_by(IDNR) %>% filter(row_number()==max(row_number())) %>% ungroup()
  nrow(info_LP) #23949
  #filter relevant columns
  info_LP <- info_LP[, c("IDNR", 
                         "GDGHUWP", "GMDHUWP", "GJRHUWP", "GPLHUWP",
                         "HDGHUWP", "HMDHUWP", "HJRHUWP", "HPLHUWP",
                         "VN1HUWP", "VN2HUWP", "VN3HUWP", "ANMHUWP"
                         )]
  #rename columns
  colnames(info_LP) <- c("IDNR",
                         "B_day_last_partner", "B_month_last_partner", "B_year_last_partner", "B_place_last_partner",
                         "M_day_last_partner", "M_month_last_partner", "M_year_last_partner", "M_place_last_partner",
                         "Firstname1_last_partner", "Firstname2_last_partner", "Firstname3_last_partner", "Lastname_last_partner" 
                         )
  #Q_03b add marital information last partner
  #add last marriage info to HSN
  HSN_RP_SSD_Id <- merge(HSN_RP_SSD_Id, info_LP, by="IDNR", all.x=T)
  length(which(HSN_RP_SSD_Id$B_day_last_partner>0))
  rm(info_LP)
  
  
  #Q_04 set missing partner information to -3
  HSN_RP_SSD_Id$M_day_last_partner <- ifelse(is.na(HSN_RP_SSD_Id$M_day_last_partner), -3, HSN_RP_SSD_Id$M_day_last_partner)
  HSN_RP_SSD_Id$M_month_last_partner <- ifelse(is.na(HSN_RP_SSD_Id$M_month_last_partner), -3, HSN_RP_SSD_Id$M_month_last_partner)
  HSN_RP_SSD_Id$M_year_last_partner <- ifelse(is.na(HSN_RP_SSD_Id$M_year_last_partner), -3, HSN_RP_SSD_Id$M_year_last_partner)
  
  
  #Q_05 identify source
  HSN_RP_SSD_Id <- merge(HSN_RP_SSD_Id, pkknd[, c("IDNR", "PKTYPE")], by="IDNR", all.x=T)
  HSN_RP_SSD_Id$Source <- ifelse(HSN_RP_SSD_Id$PKTYPE==1, "PK", 
                                 ifelse(HSN_RP_SSD_Id$PKTYPE==8 | HSN_RP_SSD_Id$PKTYPE==9, "PL", "Else"))
  HSN_RP_SSD_Id$PKTYPE <- NULL
  
  
  #standardise religion
  HSN_RP_SSD_Id$Religion <- tolower(HSN_RP_SSD_Id$Religion)
  HSN_RP_SSD_Id$Religion <- gsub("<", "", HSN_RP_SSD_Id$Religion)
  HSN_RP_SSD_Id$Religion <- gsub(" \\(door.*", "", HSN_RP_SSD_Id$Religion)
  HSN_RP_SSD_Id$Religion <- ifelse(substr(HSN_RP_SSD_Id$Religion, nchar(HSN_RP_SSD_Id$Religion), nchar(HSN_RP_SSD_Id$Religion))=="*",
                                   substr(HSN_RP_SSD_Id$Religion, 1, nchar(HSN_RP_SSD_Id$Religion)-1),
                                   HSN_RP_SSD_Id$Religion)
  #split into separate variables
  HSN_RP_SSD_Id$Religion1 <- trimws(gsub("\\*.*", "", HSN_RP_SSD_Id$Religion))
  HSN_RP_SSD_Id$Religion2 <- ifelse(grepl("\\*", HSN_RP_SSD_Id$Religion), trimws(sub(".*?\\*", "", HSN_RP_SSD_Id$Religion)), NA)
  HSN_RP_SSD_Id$Religion3 <- ifelse(grepl("\\*", HSN_RP_SSD_Id$Religion2), trimws(sub(".*?\\*", "", HSN_RP_SSD_Id$Religion2)), NA)
  HSN_RP_SSD_Id$Religion4 <- ifelse(grepl("\\*", HSN_RP_SSD_Id$Religion3), trimws(sub(".*?\\*", "", HSN_RP_SSD_Id$Religion3)), NA)
  HSN_RP_SSD_Id$Religion5 <- ifelse(grepl("\\*", HSN_RP_SSD_Id$Religion4), trimws(sub(".*?\\*", "", HSN_RP_SSD_Id$Religion4)), NA)
  HSN_RP_SSD_Id$Religion2 <- trimws(gsub("\\*.*", "", HSN_RP_SSD_Id$Religion2))
  HSN_RP_SSD_Id$Religion3 <- trimws(gsub("\\*.*", "", HSN_RP_SSD_Id$Religion3))
  HSN_RP_SSD_Id$Religion4 <- trimws(gsub("\\*.*", "", HSN_RP_SSD_Id$Religion4))
  HSN_RP_SSD_Id$Religion5 <- trimws(gsub("\\*.*", "", HSN_RP_SSD_Id$Religion5))
  #make file with religions
  Religion <- rbind(data.frame(IDNR=HSN_RP_SSD_Id$IDNR, nr=1, Religion=HSN_RP_SSD_Id$Religion1),
                    data.frame(IDNR=HSN_RP_SSD_Id$IDNR, nr=2, Religion=HSN_RP_SSD_Id$Religion2),
                    data.frame(IDNR=HSN_RP_SSD_Id$IDNR, nr=3, Religion=HSN_RP_SSD_Id$Religion3),
                    data.frame(IDNR=HSN_RP_SSD_Id$IDNR, nr=4, Religion=HSN_RP_SSD_Id$Religion4),
                    data.frame(IDNR=HSN_RP_SSD_Id$IDNR, nr=5, Religion=HSN_RP_SSD_Id$Religion5)
                    )
  HSN_RP_SSD_Id$Religion <- HSN_RP_SSD_Id$Religion1 <- HSN_RP_SSD_Id$Religion2 <- HSN_RP_SSD_Id$Religion3 <- HSN_RP_SSD_Id$Religion4 <- HSN_RP_SSD_Id$Religion5 <- NULL
  #standardise religion list
  Religion <- merge(Religion, ref_religion, by.x="Religion", by.y="original", all.x=T)
  Religion$Religion <- Religion$standard
  Religion <- Religion[!is.na(Religion$Religion) & Religion$Religion!="", c("IDNR", "nr", "Religion")] %>% arrange(IDNR, nr)
  Religion <- Religion[!(Religion$IDNR==lag(Religion$IDNR) & Religion$Religion==lag(Religion$Religion)),]
  Religion <- Religion %>% group_by(IDNR) %>% mutate(nr=row_number())
  #add Religion back to HSN_RP_SSD_Id
  HSN_RP_SSD_Id <- merge(HSN_RP_SSD_Id, Religion[Religion$nr==1, c("IDNR", "Religion")], by="IDNR", all.x=T)
  HSN_RP_SSD_Id$Religion_first_known <- HSN_RP_SSD_Id$Religion
  HSN_RP_SSD_Id$Religion_changed <- ifelse(HSN_RP_SSD_Id$IDNR %in% Religion[Religion$nr==2,]$IDNR, 1, 
                                           ifelse(is.na(HSN_RP_SSD_Id$Religion), NA, 0))
  
  
  #standardise place names
  HSN_RP_SSD_Id2 <- merge(HSN_RP_SSD_Id, ref_location, by.x="B_place", by.y="original", all.x=T)
  length(which(!is.na(HSN_RP_SSD_Id2$B_place) & is.na(HSN_RP_SSD_Id2$location)))
  #829
  #View(as.data.frame(table(HSN_RP_SSD_Id2[!is.na(HSN_RP_SSD_Id2$B_place) & is.na(HSN_RP_SSD_Id2$location),]$B_place)))
  
  
  #HSN_RP_SSD_Id <- merge(HSN_RP_SSD_Id, ref_location, by.x="D_place", by.y="original", all.x=T)
  #HSN_RP_SSD_Id <- merge(HSN_RP_SSD_Id, ref_location, by.x="B_place_mother", by.y="original", all.x=T)
  #HSN_RP_SSD_Id <- merge(HSN_RP_SSD_Id, ref_location, by.x="B_place_father", by.y="original", all.x=T)
  
  
  #reorder
  HSN_RP_SSD_Id <- HSN_RP_SSD_Id[,c("IDNR", "Source",
                                    "Start_PK_day", " Start_PK_month", "Start_PK_year",
                                    "Nationality", "Religion_first_known", "Religion_changed", "Sex_RP", 
                                    "Firstname1", "Firstname2", "Firstname3", "Lastname",
                                    "B_day_RP", "B_month_RP", "B_year_RP", "B_place",
                                    "D_day_RP", "D_month_RP", "D_year_RP", "D_place",
                                    "Firstname1_mother", "Firstname2_mother", "Firstname3_mother", "Lastname_mother",
                                    "B_day_RP_mother", "B_month_RP_mother", "B_year_RP_mother", "B_place_mother",
                                    "Firstname1_father", "Firstname2_father", "Firstname3_father", "Lastname_father",
                                    "B_day_RP_father", "B_month_RP_father", "B_year_RP_father", "B_place_father",
                                    "Firstname1_last_partner", "Firstname2_last_partner", "Firstname3_last_partner", "Lastname_last_partner", 
                                    "B_day_last_partner", "B_month_last_partner", "B_year_last_partner", "B_place_last_partner",
                                    "M_day_last_partner", "M_month_last_partner", "M_year_last_partner", "M_place_last_partner"
                                    )]
  HSN_RP_SSD_Id_naamloos <- HSN_RP_SSD_Id[,c("IDNR", "Source",
                                             "Start_PK_day", " Start_PK_month", "Start_PK_year",
                                             "Nationality", "Religion_first_known", "Religion_changed", "Sex_RP", 
                                             "B_day_RP", "B_month_RP", "B_year_RP", "B_place",
                                             "D_day_RP", "D_month_RP", "D_year_RP", "D_place",
                                             "B_day_RP_mother", "B_month_RP_mother", "B_year_RP_mother", "B_place_mother",
                                             "B_day_RP_father", "B_month_RP_father", "B_year_RP_father", "B_place_father",
                                             "B_day_last_partner", "B_month_last_partner", "B_year_last_partner", "B_place_last_partner",
                                             "M_day_last_partner", "M_month_last_partner", "M_year_last_partner", "M_place_last_partner"
                                             )]
  
  
  #save outfiles
  write.table(HSN_RP_SSD_Id, file="Output/HSN_RP_SSD_Id_NAMED.csv", sep =",", col.names=T, row.names=F, fileEncoding="UTF-8")
  write.table(HSN_RP_SSD_Id_naamloos, file="Output/HSN_RP_SSD_Id.csv", quote=T, sep =",", col.names=T, row.names=F, fileEncoding="UTF-8")
  
  write.table(Religion, file="Output/Religion.csv", quote=T, sep =",", col.names=T, row.names=F, fileEncoding="UTF-8")
  
  
  
  #############################
  #### HSN_RP_child_SSD_Id ####
  #############################
  
  #Q_11b. filter children of RPs born after 1882 from pkeigknd and select relevant variables
  HSN_RP_child_SSD_Id <- pkknd[pkknd$IDNR<300000 & pkknd$GJRPERP>1882 |
                                 pkknd$IDNR>600000, c("IDNR", "IDNRP")]
  HSN_RP_child_SSD_Id <- merge(HSN_RP_child_SSD_Id, pkeigknd[ ,c("IDNR", "VGNRKDP",
                                                                 "RELKNDP", 
                                                                 "VN1KNDP", "VN2KNDP", "VN3KNDP", "ANMKNDP",  
                                                                 "GDGKNDP", "GMDKNDP", "GJRKNDP", "GPLKNDP",
                                                                 "ODGKNDP", "OMDKNDP", "OJRKNDP", "OPLKNDP",
                                                                 "VNMPTNP", "ANMPTNP",
                                                                 "HDGKNDP", "HMDKNDP", 'HJRKNDP', "HPLKNDP",
                                                                 "ADGKNDP", "AMDKNDP", "AJRKNDP", "APLKNDP"
                                                                 )], 
                               by="IDNR", all=F)
  #calculate IDNR_child
  HSN_RP_child_SSD_Id$IDNR_child <- HSN_RP_child_SSD_Id$IDNR*1000 + HSN_RP_child_SSD_Id$VGNRKDP
  HSN_RP_child_SSD_Id$VGNRKDP <- NULL
  #rename columns
  colnames(HSN_RP_child_SSD_Id) <- c("IDNR_first_gen", "IDNR_RP", 
                                     "Relation", 
                                     "Firstname1", "Firstname2", "Firstname3", "Lastname", 
                                     "B_day", "B_month", "B_year", "B_place",
                                     "D_day", "D_month", "D_year", "D_place",
                                     "Firstname_first_partner", "Lastname_first_partner",
                                     "M_day_first_partner", "M_month_first_partner", "M_year_first_partner", "M_place_first_partner",
                                     "LO_day", "LO_month", "LO_year", "LO_place",
                                     "IDNR_child"
                                     )
  
  
  #Q_11c-d: delete children
  HSN_RP_child_SSD_Id <- merge(HSN_RP_child_SSD_Id, pkknd[,c("IDNR", "GJRPERP")], by.x="IDNR_first_gen", by.y="IDNR", all.x=T)
  HSN_RP_child_SSD_Id <- HSN_RP_child_SSD_Id[!(HSN_RP_child_SSD_Id$IDNR_first_gen>600000 & HSN_RP_child_SSD_Id$GJRPERP<1883), -c("GJRPERP")]
  
  
  #Q_11e-g: voor dubbele kinderen, selecteer registratie met meeste informatie
  #count number of registrations 
  HSN_RP_child_SSD_Id <- HSN_RP_child_SSD_Id %>% group_by(Firstname1, Firstname2, Firstname3, Lastname, B_day, B_month, B_year) %>% mutate(n=n())
  #flag availability of death, marriage, and last observations
  HSN_RP_child_SSD_Id <- HSN_RP_child_SSD_Id %>% mutate(Death_known=ifelse(is.na(D_year) | D_year<=0, 0, 1)) %>%
                                                  group_by(Firstname1, Firstname2, Firstname3, Lastname, B_day, B_month, B_year) %>% mutate(Death_known=sum(Death_known, na.rm=T))
  HSN_RP_child_SSD_Id <- HSN_RP_child_SSD_Id %>% mutate(Marriage_known=ifelse(is.na(M_year_first_partner) | M_year_first_partner<=0, 0, 1)) %>% 
                                                  group_by(Firstname1, Firstname2, Firstname3, Lastname, B_day, B_month, B_year) %>% mutate(Marriage_known=sum(Marriage_known, na.rm=T))
  HSN_RP_child_SSD_Id <- HSN_RP_child_SSD_Id %>% mutate(LO_known=ifelse(is.na(LO_year) | LO_year<=0, 0, 1)) %>% 
                                                  group_by(Firstname1, Firstname2, Firstname3, Lastname, B_day, B_month, B_year) %>% mutate(LO_known=sum(LO_known, na.rm=T))
  #1 registratie bekend
  HSN_RP_child_SSD_Id1 <- HSN_RP_child_SSD_Id[HSN_RP_child_SSD_Id$n==1,]
  #>1 overlijden bekend
  HSN_RP_child_SSD_Id2 <- HSN_RP_child_SSD_Id[HSN_RP_child_SSD_Id$n>1 & HSN_RP_child_SSD_Id$Death_known>=1,] %>% 
                                               group_by(Firstname1, Firstname2, Firstname3, Lastname, B_day, B_month, B_year) %>% 
                                               filter(D_year==max(D_year, na.rm=T)) %>% 
                                               filter(IDNR_first_gen==min(IDNR_first_gen))
  #>1 geen overlijden, wel huwelijk bekend
  HSN_RP_child_SSD_Id3 <- HSN_RP_child_SSD_Id[HSN_RP_child_SSD_Id$n>1 & HSN_RP_child_SSD_Id$Death_known==0 & HSN_RP_child_SSD_Id$Marriage_known>=1,] %>% arrange(Firstname1, Lastname, B_year) %>% 
                                               group_by(Firstname1, Firstname2, Firstname3, Lastname, B_day, B_month, B_year) %>% 
                                               filter(M_year_first_partner==max(M_year_first_partner, na.rm=T)) %>% 
                                               filter(IDNR_first_gen==min(IDNR_first_gen))
  #>1 geen overlijden en huwelijk, wel laatste observatie
  HSN_RP_child_SSD_Id4 <- HSN_RP_child_SSD_Id[HSN_RP_child_SSD_Id$n>1 & HSN_RP_child_SSD_Id$Death_known==0 & HSN_RP_child_SSD_Id$Marriage_known==0 & HSN_RP_child_SSD_Id$LO_known>=1,] %>% arrange(Firstname1, Lastname, B_year) %>% 
                                               group_by(Firstname1, Firstname2, Firstname3, Lastname, B_day, B_month, B_year) %>% 
                                               filter(LO_year==max(LO_year, na.rm=T)) %>% 
                                               filter(IDNR_first_gen==min(IDNR_first_gen))
  HSN_RP_child_SSD_Id5 <- HSN_RP_child_SSD_Id[HSN_RP_child_SSD_Id$n>1 & HSN_RP_child_SSD_Id$Death_known==0 & HSN_RP_child_SSD_Id$Marriage_known==0 & HSN_RP_child_SSD_Id$LO_known==0,] %>% arrange(Firstname1, Lastname, B_year) %>% 
                                               group_by(Firstname1, Firstname2, Firstname3, Lastname, B_day, B_month, B_year) %>% 
                                               filter(IDNR_first_gen==min(IDNR_first_gen, na.rm=T)) %>% 
                                               filter(IDNR_first_gen==min(IDNR_first_gen))
  #>1 geen overlijden, huwelijk, of laatste observatie
  HSN_RP_child_SSD_Id <- rbind(HSN_RP_child_SSD_Id1, HSN_RP_child_SSD_Id2, HSN_RP_child_SSD_Id3, HSN_RP_child_SSD_Id4, HSN_RP_child_SSD_Id5)
  rm(HSN_RP_child_SSD_Id1, HSN_RP_child_SSD_Id2, HSN_RP_child_SSD_Id3, HSN_RP_child_SSD_Id4, HSN_RP_child_SSD_Id5)
  HSN_RP_child_SSD_Id <- subset(HSN_RP_child_SSD_Id, select=-(c(Death_known, Marriage_known, LO_known, n)))
  
  
 #Q_12f: Add sex
  as.data.frame(table(HSN_RP_child_SSD_Id$Relation))
  HSN_RP_child_SSD_Id$Sex <- ifelse(tolower(HSN_RP_child_SSD_Id$Relation)=="zoon" | 
                                      tolower(HSN_RP_child_SSD_Id$Relation)=="z" |
                                      tolower(HSN_RP_child_SSD_Id$Relation)=="pleegzoon" |
                                      tolower(HSN_RP_child_SSD_Id$Relation)=="stiefzoon" | 
                                      tolower(HSN_RP_child_SSD_Id$Relation)=="stz" |
                                      tolower(HSN_RP_child_SSD_Id$Relation)=="sz", "male", "unknown")
  HSN_RP_child_SSD_Id$Sex <- ifelse(tolower(HSN_RP_child_SSD_Id$Relation)=="dochter" |
                                      tolower(HSN_RP_child_SSD_Id$Relation)==" dochter" |
                                      tolower(HSN_RP_child_SSD_Id$Relation)=="d" |
                                      tolower(HSN_RP_child_SSD_Id$Relation)=="ad" |
                                      tolower(HSN_RP_child_SSD_Id$Relation)=="kleindochter" |
                                      tolower(HSN_RP_child_SSD_Id$Relation)=="pleegdochter" |
                                      tolower(HSN_RP_child_SSD_Id$Relation)=="stiefdochter" | 
                                      tolower(HSN_RP_child_SSD_Id$Relation)=="stiefdo" | 
                                      tolower(HSN_RP_child_SSD_Id$Relation)=="std" |
                                      tolower(HSN_RP_child_SSD_Id$Relation)=="sd", "female", HSN_RP_child_SSD_Id$Sex)
  
  
 #Q_12a-e: standardize relations
  as.data.frame(table(HSN_RP_child_SSD_Id$Relation))
  HSN_RP_child_SSD_Id$Relation <- ifelse(tolower(HSN_RP_child_SSD_Id$Relation)=="zoon" | 
                                           tolower(HSN_RP_child_SSD_Id$Relation)=="z" |
                                           tolower(HSN_RP_child_SSD_Id$Relation)=="kind" & HSN_RP_child_SSD_Id$Sex=="male", "Son", HSN_RP_child_SSD_Id$Relation)
  HSN_RP_child_SSD_Id$Relation <- ifelse(tolower(HSN_RP_child_SSD_Id$Relation)=="dochter" |
                                           tolower(HSN_RP_child_SSD_Id$Relation)==" dochter" |
                                           tolower(HSN_RP_child_SSD_Id$Relation)=="d" |
                                           tolower(HSN_RP_child_SSD_Id$Relation)=="kind" & HSN_RP_child_SSD_Id$Sex=="female", "Daughter", HSN_RP_child_SSD_Id$Relation)
  HSN_RP_child_SSD_Id$Relation <- ifelse(tolower(HSN_RP_child_SSD_Id$Relation)=="kind", "Child", HSN_RP_child_SSD_Id$Relation)
  HSN_RP_child_SSD_Id$Relation <- ifelse(tolower(HSN_RP_child_SSD_Id$Relation)=="stiefzoon" | 
                                           tolower(HSN_RP_child_SSD_Id$Relation)=="stz" |
                                           tolower(HSN_RP_child_SSD_Id$Relation)=="sz", "Stepson", HSN_RP_child_SSD_Id$Relation)
  HSN_RP_child_SSD_Id$Relation <- ifelse(tolower(HSN_RP_child_SSD_Id$Relation)=="stiefdochter" | 
                                           tolower(HSN_RP_child_SSD_Id$Relation)=="stiefdo" | 
                                           tolower(HSN_RP_child_SSD_Id$Relation)=="std" |
                                           tolower(HSN_RP_child_SSD_Id$Relation)=="sd", "Stepdaughter", HSN_RP_child_SSD_Id$Relation)
  HSN_RP_child_SSD_Id$Relation <- ifelse(HSN_RP_child_SSD_Id$Relation!="Son" & 
                                           HSN_RP_child_SSD_Id$Relation!="Daughter" & 
                                           HSN_RP_child_SSD_Id$Relation!="Stepdaughter" & 
                                           HSN_RP_child_SSD_Id$Relation!="Stepson" & 
                                           HSN_RP_child_SSD_Id$Relation!="Child", "Else", HSN_RP_child_SSD_Id$Relation)
  
  
 #Q_13a: add birth date mother
  ##from pkknd
  HSN_RP_child_SSD_Id <- merge(HSN_RP_child_SSD_Id, pkknd[pkknd$GSLPERP=="v", c("IDNR", "GDGPERP", "GMDPERP", "GJRPERP", "GPLPERP")], 
                               by.x="IDNR_first_gen", by.y="IDNR", all.x=T)
  HSN_RP_child_SSD_Id$B_day_mother <- ifelse(HSN_RP_child_SSD_Id$Relation %in% c("Daughter", "Son", "Child"), 
                                                HSN_RP_child_SSD_Id$GDGPERP, NA)
  HSN_RP_child_SSD_Id$B_month_mother <- ifelse(HSN_RP_child_SSD_Id$Relation %in% c("Daughter", "Son", "Child"), 
                                                  HSN_RP_child_SSD_Id$GMDPERP, NA)
  HSN_RP_child_SSD_Id$B_year_mother <- ifelse(HSN_RP_child_SSD_Id$Relation %in% c("Daughter", "Son", "Child"), 
                                                 HSN_RP_child_SSD_Id$GJRPERP, NA)
  HSN_RP_child_SSD_Id$B_place_mother <- ifelse(HSN_RP_child_SSD_Id$Relation %in% c("Daughter", "Son", "Child"), 
                                                  HSN_RP_child_SSD_Id$GPLPERP, NA)
  HSN_RP_child_SSD_Id <- subset(HSN_RP_child_SSD_Id, select=-(c(GDGPERP, GMDPERP, GJRPERP, GPLPERP)))
  ##via marriage certificate
  moeders <- merge(HSN_RP_child_SSD_Id[, c("IDNR_first_gen", "IDNR_child", "B_year")], 
                   pkhuw[pkhuw$IDNR %in% pkknd[pkknd$GSLPERP=="m", ]$IDNR, c("IDNR", "HJRHUWP", "GDGHUWP", "GMDHUWP", "GJRHUWP", "GPLHUWP")], 
                   by.x="IDNR_first_gen", by.y="IDNR", all=F)
  moeders <- moeders[moeders$HJRHUWP<=moeders$B_year,]
  moeders <- moeders %>% group_by(IDNR_child) %>% filter(HJRHUWP==max(HJRHUWP))
  moeders <- moeders[!duplicated(moeders$IDNR_child),]
  moeders <- moeders[moeders$HJRHUWP>0,]
  HSN_RP_child_SSD_Id <- merge(HSN_RP_child_SSD_Id, 
                               moeders[,c("IDNR_child", "GDGHUWP", "GMDHUWP", "GJRHUWP", "GPLHUWP")], 
                               by="IDNR_child", all.x=T)
  rm(moeders)
  HSN_RP_child_SSD_Id$B_day_mother <- ifelse(is.na(HSN_RP_child_SSD_Id$B_day_mother) | HSN_RP_child_SSD_Id$B_day_mother<0,
                                                ifelse(HSN_RP_child_SSD_Id$Relation %in% c("Daughter", "Son", "Child"), 
                                                       HSN_RP_child_SSD_Id$GDGHUWP, NA),
                                                HSN_RP_child_SSD_Id$B_day_mother)
  HSN_RP_child_SSD_Id$B_month_mother <- ifelse(is.na(HSN_RP_child_SSD_Id$B_month_mother) | HSN_RP_child_SSD_Id$B_month_mother<0,
                                                  ifelse(HSN_RP_child_SSD_Id$Relation %in% c("Daughter", "Son", "Child"), 
                                                         HSN_RP_child_SSD_Id$GMDHUWP, NA),
                                                  HSN_RP_child_SSD_Id$B_month_mother)
  HSN_RP_child_SSD_Id$B_year_mother <- ifelse(is.na(HSN_RP_child_SSD_Id$B_year_mother) | HSN_RP_child_SSD_Id$B_year_mother<0,
                                                 ifelse(HSN_RP_child_SSD_Id$Relation %in% c("Daughter", "Son", "Child"), 
                                                        HSN_RP_child_SSD_Id$GJRHUWP, NA),
                                                 HSN_RP_child_SSD_Id$B_year_mother)
  HSN_RP_child_SSD_Id$B_place_mother <- ifelse(is.na(HSN_RP_child_SSD_Id$B_place_mother) | HSN_RP_child_SSD_Id$B_place_mother<0,
                                                  ifelse(HSN_RP_child_SSD_Id$Relation %in% c("Daughter", "Son", "Child"), 
                                                         HSN_RP_child_SSD_Id$GPLHUWP, NA),
                                                  HSN_RP_child_SSD_Id$B_place_mother)
  HSN_RP_child_SSD_Id <- subset(HSN_RP_child_SSD_Id, select=-(c(GDGHUWP, GMDHUWP, GJRHUWP, GPLHUWP)))
  #via husband
  moeders <- merge(HSN_RP_child_SSD_Id, 
                   pkknd[, c("IDNR", "GDGPERP", "GMDPERP", "GJRPERP", "GPLPERP", "OJRPERP")], 
                   by.x="IDNR_RP", by.y="IDNR", all.x=T)
  moeders <- merge(moeders, 
                   HSN_RP_SSD_Id[,c("IDNR", "M_year_last_partner")], 
                   by.x="IDNR_RP", by.y="IDNR", all.x=T)
  moeders <- moeders[!is.na(moeders$B_year) & moeders$B_year>0 &
                       !is.na(moeders$OJRPERP) & moeders$OJRPERP>0 &
                       !is.na(moeders$M_year_last_partner) & moeders$M_year_last_partner>0 &
                       !is.na(moeders$GDGPERP) & !is.na(moeders$GMDPERP) & !is.na(moeders$GJRPERP),]
  moeders <- moeders[is.na(moeders$B_year_mother) | moeders$B_year_mother<0,]
  moeders <- moeders[moeders$Relation %in% c("Daughter", "Son", "Child") & moeders$B_year>=moeders$M_year_last_partner & moeders$B_year<=moeders$OJRPERP,]
  HSN_RP_child_SSD_Id <- merge(HSN_RP_child_SSD_Id,
                               moeders[,c("IDNR_child", "GDGPERP", "GMDPERP", "GJRPERP", "GPLPERP")],
                               by="IDNR_child", all.x=T)
  rm(moeders)
  HSN_RP_child_SSD_Id$B_day_mother <- ifelse(is.na(HSN_RP_child_SSD_Id$GDGPERP), 
                                             HSN_RP_child_SSD_Id$B_day_mother, HSN_RP_child_SSD_Id$GDGPERP)
  HSN_RP_child_SSD_Id$B_month_mother <- ifelse(is.na(HSN_RP_child_SSD_Id$GMDPERP), 
                                             HSN_RP_child_SSD_Id$B_month_mother, HSN_RP_child_SSD_Id$GMDPERP)
  HSN_RP_child_SSD_Id$B_year_mother <- ifelse(is.na(HSN_RP_child_SSD_Id$GJRPERP), 
                                             HSN_RP_child_SSD_Id$B_year_mother, HSN_RP_child_SSD_Id$GJRPERP)
  HSN_RP_child_SSD_Id$B_place_mother <- ifelse(is.na(HSN_RP_child_SSD_Id$GPLPERP), 
                                             HSN_RP_child_SSD_Id$B_place_mother, HSN_RP_child_SSD_Id$GPLPERP)
  HSN_RP_child_SSD_Id <- subset(HSN_RP_child_SSD_Id, select=-(c(GDGPERP, GMDPERP, GJRPERP, GPLPERP)))
  
  
 #Q_13b: add birth date father
  ##from pkknd
  HSN_RP_child_SSD_Id <- merge(HSN_RP_child_SSD_Id, pkknd[pkknd$GSLPERP=="m", c("IDNR", "GDGPERP", "GMDPERP", "GJRPERP", "GPLPERP")], 
                               by.x="IDNR_first_gen", by.y="IDNR", all.x=T)
  HSN_RP_child_SSD_Id$B_day_father <- ifelse(HSN_RP_child_SSD_Id$Relation %in% c("Daughter", "Son", "Child"), 
                                             HSN_RP_child_SSD_Id$GDGPERP, NA)
  HSN_RP_child_SSD_Id$B_month_father <- ifelse(HSN_RP_child_SSD_Id$Relation %in% c("Daughter", "Son", "Child"), 
                                               HSN_RP_child_SSD_Id$GMDPERP, NA)
  HSN_RP_child_SSD_Id$B_year_father <- ifelse(HSN_RP_child_SSD_Id$Relation %in% c("Daughter", "Son", "Child"), 
                                              HSN_RP_child_SSD_Id$GJRPERP, NA)  
  HSN_RP_child_SSD_Id$B_place_father <- ifelse(HSN_RP_child_SSD_Id$Relation %in% c("Daughter", "Son", "Child"), 
                                               HSN_RP_child_SSD_Id$GPLPERP, NA)
  HSN_RP_child_SSD_Id <- subset(HSN_RP_child_SSD_Id, select=-(c(GDGPERP, GMDPERP, GJRPERP, GPLPERP)))
  ##via marriage certificate
  vaders <- merge(HSN_RP_child_SSD_Id[, c("IDNR_first_gen", "IDNR_child", "B_year")], 
                   pkhuw[pkhuw$IDNR %in% pkknd[pkknd$GSLPERP=="v", ]$IDNR, c("IDNR", "HJRHUWP", "GDGHUWP", "GMDHUWP", "GJRHUWP", "GPLHUWP")], 
                   by.x="IDNR_first_gen", by.y="IDNR", all=F)
  vaders <- vaders[vaders$HJRHUWP<=vaders$B_year,]
  vaders <- vaders %>% group_by(IDNR_child) %>% filter(HJRHUWP==max(HJRHUWP))
  vaders <- vaders[!duplicated(vaders$IDNR_child),]
  vaders <- vaders[vaders$HJRHUWP>0,]
  HSN_RP_child_SSD_Id <- merge(HSN_RP_child_SSD_Id, 
                               vaders[,c("IDNR_child", "GDGHUWP", "GMDHUWP", "GJRHUWP", "GPLHUWP")], 
                               by="IDNR_child", all.x=T)
  rm(vaders)
  HSN_RP_child_SSD_Id$B_day_father <- ifelse(is.na(HSN_RP_child_SSD_Id$B_day_father)| HSN_RP_child_SSD_Id$B_day_father<0,
                                                ifelse(HSN_RP_child_SSD_Id$Relation %in% c("Daughter", "Son", "Child"), 
                                                       HSN_RP_child_SSD_Id$GDGHUWP, NA),
                                                HSN_RP_child_SSD_Id$B_day_father)
  HSN_RP_child_SSD_Id$B_month_father <- ifelse(is.na(HSN_RP_child_SSD_Id$B_month_father) | HSN_RP_child_SSD_Id$B_month_father<0,
                                                  ifelse(HSN_RP_child_SSD_Id$Relation %in% c("Daughter", "Son", "Child"), 
                                                         HSN_RP_child_SSD_Id$GMDHUWP, NA),
                                                  HSN_RP_child_SSD_Id$B_month_father)
  HSN_RP_child_SSD_Id$B_year_father <- ifelse(is.na(HSN_RP_child_SSD_Id$B_year_father) | HSN_RP_child_SSD_Id$B_year_father<0,
                                                 ifelse(HSN_RP_child_SSD_Id$Relation %in% c("Daughter", "Son", "Child"), 
                                                        HSN_RP_child_SSD_Id$GJRHUWP, NA),
                                                 HSN_RP_child_SSD_Id$B_year_father)
  HSN_RP_child_SSD_Id$B_place_father <- ifelse(is.na(HSN_RP_child_SSD_Id$B_place_father) | HSN_RP_child_SSD_Id$B_place_father<0,
                                                  ifelse(HSN_RP_child_SSD_Id$Relation %in% c("Daughter", "Son", "Child"), 
                                                         HSN_RP_child_SSD_Id$GPLHUWP, NA),
                                                  HSN_RP_child_SSD_Id$B_place_father)
  HSN_RP_child_SSD_Id <- subset(HSN_RP_child_SSD_Id, select=-(c(GDGHUWP, GMDHUWP, GJRHUWP, GPLHUWP)))
  
  
 #remove problematic pks with missing information on the mother
  #Q_14a: remove no pk for RP mother
  HSN_RP_child_SSD_Id <- HSN_RP_child_SSD_Id[!(HSN_RP_child_SSD_Id$IDNR_first_gen>600000 & !HSN_RP_child_SSD_Id$IDNR_RP %in% pkknd$IDNR), ]
  #Q_14b-d: remove mothers with different birth date on own pk 
  HSN_RP_child_SSD_Id <- merge(HSN_RP_child_SSD_Id, pkknd[, c("IDNR", "GDGPERP", "GMDPERP", "GJRPERP", "GPLPERP")], by.x="IDNR_RP", by.y="IDNR", all.x=T)
  HSN_RP_child_SSD_Id$flag <- ifelse(HSN_RP_child_SSD_Id$IDNR_RP==0, 0, 
                                     ifelse(HSN_RP_child_SSD_Id$IDNR_first_gen>600000 & HSN_RP_child_SSD_Id$B_year_mother!=HSN_RP_child_SSD_Id$GJRPERP |
                                              HSN_RP_child_SSD_Id$IDNR_first_gen>600000 & HSN_RP_child_SSD_Id$B_month_mother!=HSN_RP_child_SSD_Id$GMDPERP |
                                              HSN_RP_child_SSD_Id$IDNR_first_gen>600000 & HSN_RP_child_SSD_Id$B_day_mother!=HSN_RP_child_SSD_Id$GDGPERP,
                                            1, 0))
  HSN_RP_child_SSD_Id <- HSN_RP_child_SSD_Id[which(HSN_RP_child_SSD_Id$flag==0),]
  HSN_RP_child_SSD_Id <- subset(HSN_RP_child_SSD_Id, select=-(c(GDGPERP, GMDPERP, GJRPERP, GPLPERP, flag)))
  
  
 #Q_14f: update relation
  HSN_RP_child_SSD_Id$Relation <- ifelse(HSN_RP_child_SSD_Id$IDNR_first_gen>600000 & HSN_RP_child_SSD_Id$Sex=="female",
                                         "Daughter", HSN_RP_child_SSD_Id$Relation)
  HSN_RP_child_SSD_Id$Relation <- ifelse(HSN_RP_child_SSD_Id$IDNR_first_gen>600000 & HSN_RP_child_SSD_Id$Sex=="male",
                                         "Son", HSN_RP_child_SSD_Id$Relation)
  
  
 #Q_15: Identify source
  HSN_RP_child_SSD_Id <- merge(HSN_RP_child_SSD_Id, pkknd[, c("IDNR", "PKTYPE")], by.x="IDNR_first_gen", by.y="IDNR", all.x=T)
  HSN_RP_child_SSD_Id$Source <- ifelse(HSN_RP_child_SSD_Id$IDNR_first_gen<300000 & HSN_RP_child_SSD_Id$PKTYPE==1, "PK", NA)
  HSN_RP_child_SSD_Id$Source <- ifelse(HSN_RP_child_SSD_Id$IDNR_first_gen<600000 & HSN_RP_child_SSD_Id$PKTYPE==1, "PK-man", HSN_RP_child_SSD_Id$Source)
  HSN_RP_child_SSD_Id$Source <- ifelse(HSN_RP_child_SSD_Id$IDNR_first_gen<300000 & HSN_RP_child_SSD_Id$PKTYPE==8 |
                                         HSN_RP_child_SSD_Id$IDNR_first_gen<300000 & HSN_RP_child_SSD_Id$PKTYPE==9, "PL", HSN_RP_child_SSD_Id$Source)
  HSN_RP_child_SSD_Id$Source <- ifelse(HSN_RP_child_SSD_Id$IDNR_first_gen<600000 & HSN_RP_child_SSD_Id$PKTYPE==8 |
                                         HSN_RP_child_SSD_Id$IDNR_first_gen<300000 & HSN_RP_child_SSD_Id$PKTYPE==9, "PL-man", HSN_RP_child_SSD_Id$Source)
  HSN_RP_child_SSD_Id$Source <- ifelse(is.na(HSN_RP_child_SSD_Id$Source), "Else", HSN_RP_child_SSD_Id$Source)
  
  
 #reorder
  HSN_RP_child_SSD_Id <- HSN_RP_child_SSD_Id[,c("IDNR_child", "IDNR_first_gen", "IDNR_RP", 
                                                "Source", "Relation", "Sex",
                                                "Firstname1", "Firstname2", "Firstname3", "Lastname", 
                                                "B_day", "B_month", "B_year", "B_place",
                                                "D_day", "D_month", "D_year", "D_place",
                                                "B_day_mother", "B_month_mother", "B_year_mother", "B_place_mother",
                                                "B_day_father", "B_month_father", "B_year_father", "B_place_father",
                                                "Firstname_first_partner", "Lastname_first_partner",
                                                "M_day_first_partner", "M_month_first_partner", "M_year_first_partner", "M_place_first_partner",
                                                "LO_day", "LO_month", "LO_year", "LO_place"
                                                )]
  HSN_RP_child_SSD_Id_naamloos <- HSN_RP_child_SSD_Id[,c("IDNR_child", "IDNR_first_gen", "IDNR_RP", 
                                                         "Source", "Relation", "Sex",
                                                         "B_day", "B_month", "B_year", "B_place",
                                                         "D_day", "D_month", "D_year", "D_place",
                                                         "B_day_mother", "B_month_mother", "B_year_mother", "B_place_mother",
                                                         "B_day_father", "B_month_father", "B_year_father", "B_place_father",
                                                         "M_day_first_partner", "M_month_first_partner", "M_year_first_partner", "M_place_first_partner",
                                                         "LO_day", "LO_month", "LO_year", "LO_place"
                                                         )]
  
  
  #save outfiles
  write.table(HSN_RP_child_SSD_Id, file="Output/HSN_RP_child_SSD_Id_NAMED.csv", sep =",", col.names=T, row.names=F, fileEncoding="UTF-8")
  write.table(HSN_RP_child_SSD_Id_naamloos, file="Output/HSN_RP_child_SSD_Id.csv", quote=T, sep =",", col.names=T, row.names=F, fileEncoding="UTF-8")
  
  
  #to do:
  # 2. plaatsnamen standaardiseren
  # 4. addressen in logische structuur zetten
  
  
  Adressen <- pkadres[,c("IDNR", "VGNRADP", 
                         "DGADRP", "MDADRP", "JRADRP", 
                         "VERNUM", 
                         "STRADRP", "PLADRP", "LNDADRP")] %>% arrange(IDNR, VGNRADP)
  colnames(Adressen) <- c("IDNR", "nr", 
                          "Day", "Month", "Year", 
                          "RenumberedAdres", 
                          "Address", "Place", "Country")
  Adressen$RenumberedAdres[Adressen$RenumberedAdres=="V"] <- "v"
  
  write.table(Adressen, file="Output/Adresses.csv", sep =",", col.names=T, row.names=F, fileEncoding="UTF-8")
  
  
  Occupations <- pkbrp[,c("IDNR", "VGNRBRP",
                          "BEROEPP", "BRPPOSP")] %>% arrange(IDNR, VGNRBRP)
  colnames(Occupations) <- c("IDNR", "nr", 
                             "Occupation_title", "Status")
  Occupations$Status <- ifelse(Occupations$Status=="h", "Self-employed or director/head",
                               ifelse(Occupations$Status=="o", "Employee", "Not known (or not relevant)"))
  Occupations$Occupation_title <- tolower(Occupations$Occupation_title)
  Occupations <- merge(Occupations, ref_occupation, by.x="Occupation_title", by.y="Original", all.x=T)
  Occupations <- Occupations[,c("IDNR", "nr", 
                                "Occupation_title", "Status",
                                "HISCO", "STATUS", "RELATION", "PRODUCT",
                                "HISCLASS", "HISCAM_U1", "HISCAM_NL", "SOCPO", "OCC1950")]
  colnames(Occupations) <- c("IDNR", "nr", 
                             "Occupation_title", "Status",
                             "HISCO", "HISCO_status", "HISCO_relation", "HISCO_product",
                             "HISCLASS", "HISCAM_U1", "HISCAM_NL", "SOCPO", "OCC1950")
  write.table(Occupations, file="Output/Occupations.csv", sep =",", col.names=T, row.names=F, fileEncoding="UTF-8")
  
  
  
  
