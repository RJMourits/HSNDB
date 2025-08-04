library(openxlsx)

setwd("c:/ff")

cpn <- read.xlsx("IISG_CPN.xlsx")

#inspect columns
colnames(cpn)

#drop redundant column Voornaam.+.voorvoegsel
cpn$'Voornaam.+.voorvoegsel' <- NULL

#allign column names
colnames(cpn)[colnames(cpn)=="arrestatiedatum_norm"] <- "Arrestatiedatum_norm"
colnames(cpn)[colnames(cpn)=="Arrestatie-datum"] <- "Arrestatiedatum"
colnames(cpn)[colnames(cpn)=="Arrestatiegrond.-.Activiteit"] <- "Arrestatiegrond"
colnames(cpn)[colnames(cpn)=="datum.overlijden"] <- "Overlijdensdatum"
colnames(cpn)[colnames(cpn)=="plaats.overlijden"] <- "Overlijdensplaats"

#Geboortedatum
cpn$birthDate <- as.character(convertToDate(cpn$Geboortedatum))
#deal with missings
cpn$birthDate2 <- ifelse(is.na(cpn$birthDate), cpn$Geboortedatum, NA)
#fix dates that don't have dd-mm-yyyy format
cpn$birthDate2[cpn$birthDate2=="10-5-1893"] <- "10-05-1893"
cpn$birthDate2[cpn$birthDate2=="3-7-1898"] <- "03-07-1898"
cpn$birthDate2[cpn$birthDate2=="02--7-1912"] <- "02-07-1912"
cpn$birthDate2[cpn$birthDate2=="04--6-1913"] <- "04-06-1913"
#put into yyyy-mm-dd
cpn$birthDate2 <- ifelse(is.na(cpn$birthDate2), NA, paste(substr(cpn$birthDate2, 7, 12), substr(cpn$birthDate2, 4, 5), substr(cpn$birthDate2, 1, 2), sep="-"))
#combine
cpn$Geboortedatum <- ifelse(is.na(cpn$birthDate), cpn$birthDate2, cpn$birthDate)
#clean environment
cpn$birthDate <- cpn$birthDate2 <- NULL

#Overlijdensdatum
cpn$deathDate <- as.character(convertToDate(cpn$Overlijdensdatum))
#deal with missings
cpn$deathDate2 <- ifelse(is.na(cpn$deathDate), cpn$Overlijdensdatum, NA)
cpn$deathDate2[cpn$deathDate2=="27=04-1942"] <- "27-04-1942"
#put into yyyy-mm-dd
cpn$deathDate2 <- ifelse(is.na(cpn$deathDate2), NA, paste(substr(cpn$deathDate2, 7, 12), substr(cpn$deathDate2, 4, 5), substr(cpn$deathDate2, 1, 2), sep="-"))
#combine
cpn$Overlijdensdatum <- ifelse(is.na(cpn$deathDate), cpn$deathDate2, cpn$deathDate)
#clean environment
cpn$deathDate <- cpn$deathDate2 <- NULL


#Arrestatiedatum
cpn$arrestDate <- as.character(convertToDate(cpn$Arrestatiedatum))
#deal with missings
cpn$arrestDate2 <- ifelse(is.na(cpn$arrestDate), cpn$Arrestatiedatum, NA)
cpn$arrestDate[cpn$arrestDate2=="18-6-1842"] <- "1942-06-18"
cpn$arrestDate[cpn$arrestDate2=="22-11--1943"] <- "1943-11-22"
cpn$arrestDate[cpn$arrestDate2=="30-2-1945"] <- "1945-02-30"
cpn$arrestDate2[cpn$arrestDate2=="30--1942"] <- "1942"
cpn$arrestDate2[cpn$arrestDate2=="Begin 1941"] <- "begin 1941?"
cpn$arrestDate2[cpn$arrestDate2=="?1941"] <- "1941?"
cpn$arrestDate2[cpn$arrestDate2=="eind 4-42"] <- "eind april 1942"
cpn$arrestDate2[cpn$arrestDate2=="eind 42"] <- "eind 1942"
cpn$arrestDate2[cpn$arrestDate2=="okt 42 razzia"] <- "Razzia oktober 1942"
cpn$arrestDate2[cpn$arrestDate2=="kerst 44"] <- "Kerst 1944"
#combine
cpn$Arrestatiedatum <- ifelse(is.na(cpn$arrestDate), cpn$arrestDate2, cpn$arrestDate)
#clean environment
cpn$arrestDate <- cpn$arrestDate2 <- NULL


#Arrestatiedatum
cpn$Arrestatiedatum_norm <- NULL



cpn$PO <- paste0('IISG-CPN:', gsub(' ', '%20', paste0(cpn$Voornaam, ' ', cpn$Voorvoegsel, ' ', cpn$Naam)), ' 
                 prov:hadPrimarySource ', 'IISG-CPN:', cpn$'Mapnr..CPN-archief', '" ;
                 ', 'sdo:name "', cpn$Voornaam, ' ', cpn$Voorvoegsel, ' ', cpn$Naam, '" ;
                 ', 'sdo:familyName "', cpn$Voorvoegsel, ' ', cpn$Naam, '" ;
                 ', 'sdo:giveName "', cpn$Voornaam, '" ;
                 ', 'sdo:alternateName "', cpn$Roepnaam, '" ;
                 ', 'sdo:underName "', cpn$'Naam.op.formulier-kaart.(indien.afwijkend)', '" ;
                 ', 'sdo:additionalName [a "pnv:PersonName" ;
                                         pnv:literalName "', cpn$Voornaam, ' ', cpn$Voorvoegsel, ' ', cpn$Naam, '" ;
                                         pnv:givenName "', cpn$Voornaam, '" ;
                                         pnv:baseSurname "', cpn$Naam, '" ;
                                         pnv:surnamePrefix "', cpn$Voorvoegsel, '" ; ] ;
                 ', 'sdo:gender ', ifelse(cpn$Geslacht=='M', 'sdo:Male', 'sdo:Female'), ' ;
                 ', 'sdo:birthDate "', cpn$Geboortedatum, '" ;
                 ', 'sdo:birthPlace "', cpn$Geboorteplaats, '" ;
                 ', 'sdo:deathDate "', cpn$Overlijdensdatum, '" ;
                 ', 'sdo:deathPlace "', cpn$Overlijdensplaats, '" ;
                 ', 'picom:hasAge "', cpn$Leeftijd, '" ;
                 ', 'picom:hasReligion "', ifelse(cpn$Joods=='J', 'iisg-lord:Israelitisch', NA), '" ;
                 ', 'sdo:occupation "', cpn$Beroep, '" ;
                 ', ifelse(is.na(cpn$Afdeling.CPN), '', paste0('sdo:memberOf [a sdo:PoliticalParty ;
                                   sdo:name "Afdeling ', cpn$Afdeling.CPN, '" ;
                                   sdo:date ', cpn$Arrestatiedatum, ' ;
                                   sdo:subOrganization "District ', cpn$District.CPN, '" ;
                 '))
                 , 'sdo:memberOf [a sdo:PoliticalParty ;
                                   sdo:name "Afdeling ', cpn$District.CPN, '" ;
                                   sdo:date ', cpn$Arrestatiedatum, ' ;
                                   sdo:startDate "', cpn$Lid.CPN.sinds, '" ;
                                   roleBefore1940 "', cpn$Voor.1940.actief, '" ;
                                   sdo:subOrganization "<https://www.wikidata.org/entity/Q385543>" ; ] ;
                 ', 'sdo:startDate ', cpn$Lid.CPN.sinds, '" ;
                 ', 'shoah:arrestDate "', cpn$Arrestatiedatum, '" ;
                 ', 'shoah:reasonOfArrest "', cpn$Arrestatiegrond, '" ;
                 ', 'notities "', cpn$Notities, '" .
                 '
                 )
#filter NAs
cpn$PO <- gsub('%20NA', '', cpn$PO)
cpn$PO <- gsub(' NA ', '', cpn$PO)
cpn$PO <- gsub('sdo:familyName "NA ', 'sdo:familyName "', cpn$PO)
cpn$PO <- gsub('picom:hasReligion "NA" ;
               ', '', cpn$PO)
cpn$PO <- gsub('sdo:alternateName "NA" ;
               ', '', cpn$PO)
cpn$PO <- gsub('sdo:underName "NA" ;
               ', '', cpn$PO)
cpn$PO <- gsub('pnv:surnamePrefix "NA" ; ', '', cpn$PO)





cpn_source <- cpn[!is.na(cpn$'Mapnr..CPN-archief') & cpn$'Mapnr..CPN-archief'!="" & !duplicated(cpn$'Mapnr..CPN-archief'),]
cpn_source$Source <- paste0('IISG-CPN:', cpn_source$'Mapnr..CPN-archief', '
                            a sdo:ArchiveComponent ;
                            sdo:additionalType "XXX" ;
                            sdo:name "XXX" ;
                            sdo:holdingArchive <https://iisg.amsterdam> ;
                            sdo:url <> ;
                            sdo:dateCreated <> ;
                            sdo:locationCreated "Amsterdam" . 
                            ')



prefix <- '#prefix
@prefix iisg-lord: <https://iisg.amsterdam/resources/ListOfReligiousDenominations/>.
@prefix picom:     <https://personsincontext.org/model#/>.
@prefix sdo:       <https://schema.org/>.
@prefix shoah:     <http://dati.cdec.it/lod/shoah/>.
@prefix wdt:       <https://www.wikidata.org/entity/>.
'

ttl <- c(prefix, cpn$PO, cpn_source$Source)


write.table(cpn[,1:21], "cpn.csv", quote=T, sep=",", col.names=T, row.names=F, fileEncoding="UTF-8", na="")
write.table(ttl, "cpn.ttl", quote=F, sep="", col.names=F, row.names=F, fileEncoding="UTF-8")


