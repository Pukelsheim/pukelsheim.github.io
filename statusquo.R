## DIESES SKRIPT NACH PUKELSHEIM AUSFUEHREN (FUER INDEX)
## Definition des Szenario (hier 2021)
s<-data.frame(Oberland=c(54018,50844,20026,14127,7370),Unterland=c(18343,21475,5917,8329,1186),row.names = c('VU','FBP','FL','DPL','DU'))

## Definition der Variablen
partei1_ol<-s[1,1]
partei1_ol<-ifelse(is.na(partei1_ol)==TRUE,0,partei1_ol)
partei1_ul<-s[1,2]
partei1_ul<-ifelse(is.na(partei1_ul)==TRUE,0,partei1_ul)
partei2_ol<-s[2,1]
partei2_ol<-ifelse(is.na(partei2_ol)==TRUE,0,partei2_ol)
partei2_ul<-s[2,2]
partei2_ul<-ifelse(is.na(partei2_ul)==TRUE,0,partei2_ul)
partei3_ol<-s[3,1]
partei3_ol<-ifelse(is.na(partei3_ol)==TRUE,0,partei3_ol)
partei3_ul<-s[3,2]
partei3_ul<-ifelse(is.na(partei3_ul)==TRUE,0,partei3_ul)
partei4_ol<-s[4,1]
partei4_ol<-ifelse(is.na(partei4_ol)==TRUE,0,partei4_ol)
partei4_ul<-s[4,2]
partei4_ul<-ifelse(is.na(partei4_ul)==TRUE,0,partei4_ul)
partei5_ol<-s[5,1]
partei5_ol<-ifelse(is.na(partei5_ol)==TRUE,0,partei5_ol)
partei5_ul<-s[5,2]
partei5_ul<-ifelse(is.na(partei5_ul)==TRUE,0,partei5_ul)
partei6_ol<-s[6,1]
partei6_ol<-ifelse(is.na(partei6_ol)==TRUE,0,partei6_ol)
partei6_ul<-s[6,2]
partei6_ul<-ifelse(is.na(partei6_ul)==TRUE,0,partei6_ul)

## Berechung der Huerde
partei1_sq_gesamt<-partei1_ol+partei1_ul
partei2_sq_gesamt<-partei2_ol+partei2_ul
partei3_sq_gesamt<-partei3_ol+partei3_ul
partei4_sq_gesamt<-partei4_ol+partei4_ul
partei5_sq_gesamt<-partei5_ol+partei5_ul
partei6_sq_gesamt<-partei6_ol+partei6_ul

parteistimmen_gesamt<-sum(partei1_sq_gesamt,partei2_sq_gesamt,partei3_sq_gesamt,partei4_sq_gesamt,partei5_sq_gesamt,partei6_sq_gesamt)

partei1_sq_anteil<-partei1_sq_gesamt/parteistimmen_gesamt
ifelse(partei1_sq_anteil<0.08,partei1_sq_gesamt<-0,"")
ifelse(partei1_sq_anteil<0.08,partei1_ol<-0,"")
ifelse(partei1_sq_anteil<0.08,partei1_ul<-0,"")

partei2_sq_anteil<-partei2_sq_gesamt/parteistimmen_gesamt
ifelse(partei2_sq_anteil<0.08,partei2_sq_gesamt<-0,"")
ifelse(partei2_sq_anteil<0.08,partei2_ol<-0,"")
ifelse(partei2_sq_anteil<0.08,partei2_ul<-0,"")

partei3_sq_anteil<-partei3_sq_gesamt/parteistimmen_gesamt
ifelse(partei3_sq_anteil<0.08,partei3_sq_gesamt<-0,"")
ifelse(partei3_sq_anteil<0.08,partei3_ol<-0,"")
ifelse(partei3_sq_anteil<0.08,partei3_ul<-0,"")

partei4_sq_anteil<-partei4_sq_gesamt/parteistimmen_gesamt
ifelse(partei4_sq_anteil<0.08,partei4_sq_gesamt<-0,"")
ifelse(partei4_sq_anteil<0.08,partei4_ol<-0,"")
ifelse(partei4_sq_anteil<0.08,partei4_ul<-0,"")

partei5_sq_anteil<-partei5_sq_gesamt/parteistimmen_gesamt
ifelse(partei5_sq_anteil<0.08,partei5_sq_gesamt<-0,"")
ifelse(partei5_sq_anteil<0.08,partei5_ol<-0,"")
ifelse(partei5_sq_anteil<0.08,partei5_ul<-0,"")

partei6_sq_anteil<-partei6_sq_gesamt/parteistimmen_gesamt
ifelse(partei6_sq_anteil<0.08,partei6_sq_gesamt<-0,"")
ifelse(partei6_sq_anteil<0.08,partei6_ol<-0,"")
ifelse(partei6_sq_anteil<0.08,partei6_ul<-0,"")

## Oberland
## Grundmandatsverteilung
stimmen_ol<-sum(partei1_ol,partei2_ol,partei3_ol,partei4_ol,partei5_ol,partei6_ol)

wahlzahl_ol<-ceiling(stimmen_ol/16)

grundmandate_partei1_ol<-floor(partei1_ol/wahlzahl_ol)
reststimmen_partei1_ol<-partei1_ol-grundmandate_partei1_ol*wahlzahl_ol

grundmandate_partei2_ol<-floor(partei2_ol/wahlzahl_ol)
reststimmen_partei2_ol<-partei2_ol-grundmandate_partei2_ol*wahlzahl_ol

grundmandate_partei3_ol<-floor(partei3_ol/wahlzahl_ol)
reststimmen_partei3_ol<-partei3_ol-grundmandate_partei3_ol*wahlzahl_ol

grundmandate_partei4_ol<-floor(partei4_ol/wahlzahl_ol)
reststimmen_partei4_ol<-partei4_ol-grundmandate_partei4_ol*wahlzahl_ol

grundmandate_partei5_ol<-floor(partei5_ol/wahlzahl_ol)
reststimmen_partei5_ol<-partei5_ol-grundmandate_partei5_ol*wahlzahl_ol

grundmandate_partei6_ol<-floor(partei6_ol/wahlzahl_ol)
reststimmen_partei6_ol<-partei6_ol-grundmandate_partei6_ol*wahlzahl_ol

## Restmandatsverteilung
restmandate_ol=15-sum(grundmandate_partei1_ol,grundmandate_partei2_ol,grundmandate_partei3_ol,grundmandate_partei4_ol,grundmandate_partei5_ol,grundmandate_partei6_ol)

reststimmen_list_ol<-c(c(reststimmen_partei1_ol,reststimmen_partei2_ol,reststimmen_partei3_ol,reststimmen_partei4_ol,reststimmen_partei5_ol,reststimmen_partei6_ol),c(reststimmen_partei1_ol,reststimmen_partei2_ol,reststimmen_partei3_ol,reststimmen_partei4_ol,reststimmen_partei5_ol,reststimmen_partei6_ol)/2,c(reststimmen_partei1_ol,reststimmen_partei2_ol,reststimmen_partei3_ol,reststimmen_partei4_ol,reststimmen_partei5_ol,reststimmen_partei6_ol)/3,c(reststimmen_partei1_ol,reststimmen_partei2_ol,reststimmen_partei3_ol,reststimmen_partei4_ol,reststimmen_partei5_ol,reststimmen_partei6_ol)/4)

wahlzahl_ol_rest<-sort(reststimmen_list_ol)[length(reststimmen_list_ol) - restmandate_ol+1]
wahlzahl_ol_rest<-ifelse(is.na(wahlzahl_ol_rest)==TRUE,1000000000000,wahlzahl_ol_rest)

restmandate_partei1_ol<-floor(reststimmen_partei1_ol/wahlzahl_ol_rest)
mandate_partei1_ol<-restmandate_partei1_ol+grundmandate_partei1_ol

restmandate_partei2_ol<-floor(reststimmen_partei2_ol/wahlzahl_ol_rest)
mandate_partei2_ol<-restmandate_partei2_ol+grundmandate_partei2_ol

restmandate_partei3_ol<-floor(reststimmen_partei3_ol/wahlzahl_ol_rest)
mandate_partei3_ol<-restmandate_partei3_ol+grundmandate_partei3_ol

restmandate_partei4_ol<-floor(reststimmen_partei4_ol/wahlzahl_ol_rest)
mandate_partei4_ol<-restmandate_partei4_ol+grundmandate_partei4_ol

restmandate_partei5_ol<-floor(reststimmen_partei5_ol/wahlzahl_ol_rest)
mandate_partei5_ol<-restmandate_partei5_ol+grundmandate_partei5_ol

restmandate_partei6_ol<-floor(reststimmen_partei6_ol/wahlzahl_ol_rest)
mandate_partei6_ol<-restmandate_partei6_ol+grundmandate_partei6_ol


## Unterland
## Grundmandatsverteilung
stimmen_ul<-sum(partei1_ul,partei2_ul,partei3_ul,partei4_ul,partei5_ul,partei6_ul)

wahlzahl_ul<-ceiling(stimmen_ul/11)

grundmandate_partei1_ul<-floor(partei1_ul/wahlzahl_ul)
reststimmen_partei1_ul<-partei1_ul-grundmandate_partei1_ul*wahlzahl_ul

grundmandate_partei2_ul<-floor(partei2_ul/wahlzahl_ul)
reststimmen_partei2_ul<-partei2_ul-grundmandate_partei2_ul*wahlzahl_ul

grundmandate_partei3_ul<-floor(partei3_ul/wahlzahl_ul)
reststimmen_partei3_ul<-partei3_ul-grundmandate_partei3_ul*wahlzahl_ul

grundmandate_partei4_ul<-floor(partei4_ul/wahlzahl_ul)
reststimmen_partei4_ul<-partei4_ul-grundmandate_partei4_ul*wahlzahl_ul

grundmandate_partei5_ul<-floor(partei5_ul/wahlzahl_ul)
reststimmen_partei5_ul<-partei5_ul-grundmandate_partei5_ul*wahlzahl_ul

grundmandate_partei6_ul<-floor(partei6_ul/wahlzahl_ul)
reststimmen_partei6_ul<-partei6_ul-grundmandate_partei6_ul*wahlzahl_ul

## Restmandatsverteilung
restmandate_ul=10-sum(grundmandate_partei1_ul,grundmandate_partei2_ul,grundmandate_partei3_ul,grundmandate_partei4_ul,grundmandate_partei5_ul,grundmandate_partei6_ul)

reststimmen_list_ul<-c(c(reststimmen_partei1_ul,reststimmen_partei2_ul,reststimmen_partei3_ul,reststimmen_partei4_ul,reststimmen_partei5_ul,reststimmen_partei6_ul),c(reststimmen_partei1_ul,reststimmen_partei2_ul,reststimmen_partei3_ul,reststimmen_partei4_ul,reststimmen_partei5_ul,reststimmen_partei6_ul)/2,c(reststimmen_partei1_ul,reststimmen_partei2_ul,reststimmen_partei3_ul,reststimmen_partei4_ul,reststimmen_partei5_ul,reststimmen_partei6_ul)/3,c(reststimmen_partei1_ul,reststimmen_partei2_ul,reststimmen_partei3_ul,reststimmen_partei4_ul,reststimmen_partei5_ul,reststimmen_partei6_ul)/4)

wahlzahl_ul_rest<-sort(reststimmen_list_ul)[length(reststimmen_list_ul) - restmandate_ul+1]
wahlzahl_ul_rest<-ifelse(is.na(wahlzahl_ul_rest)==TRUE,1000000000000,wahlzahl_ul_rest)


restmandate_partei1_ul<-floor(reststimmen_partei1_ul/wahlzahl_ul_rest)
mandate_partei1_ul<-restmandate_partei1_ul+grundmandate_partei1_ul

restmandate_partei2_ul<-floor(reststimmen_partei2_ul/wahlzahl_ul_rest)
mandate_partei2_ul<-restmandate_partei2_ul+grundmandate_partei2_ul

restmandate_partei3_ul<-floor(reststimmen_partei3_ul/wahlzahl_ul_rest)
mandate_partei3_ul<-restmandate_partei3_ul+grundmandate_partei3_ul

restmandate_partei4_ul<-floor(reststimmen_partei4_ul/wahlzahl_ul_rest)
mandate_partei4_ul<-restmandate_partei4_ul+grundmandate_partei4_ul

restmandate_partei5_ul<-floor(reststimmen_partei5_ul/wahlzahl_ul_rest)
mandate_partei5_ul<-restmandate_partei5_ul+grundmandate_partei5_ul

restmandate_partei6_ul<-floor(reststimmen_partei6_ul/wahlzahl_ul_rest)
mandate_partei6_ul<-restmandate_partei6_ul+grundmandate_partei6_ul

partei1_sitze_sq<-mandate_partei1_ol+mandate_partei1_ul

partei2_sitze_sq<-mandate_partei2_ol+mandate_partei2_ul

partei3_sitze_sq<-mandate_partei3_ol+mandate_partei3_ul

partei4_sitze_sq<-mandate_partei4_ol+mandate_partei4_ul

partei5_sitze_sq<-mandate_partei5_ol+mandate_partei5_ul

partei6_sitze_sq<-mandate_partei6_ol+mandate_partei6_ul

index_sq=sqrt(0.5*sum((partei1_anteil-partei1_sitze_sq/25)^2,(partei2_anteil-partei2_sitze_sq/25)^2,(partei3_anteil-partei3_sitze_sq/25)^2,(partei4_anteil-partei4_sitze_sq/25)^2,(partei5_anteil-partei5_sitze_sq/25)^2,(partei6_anteil-partei6_sitze_sq/25)^2))

ergebnis_sq<-data.frame(Oberland=c(mandate_partei1_ol,mandate_partei2_ol,mandate_partei3_ol,mandate_partei4_ol,mandate_partei5_ol,mandate_partei6_ol),Unterland=c(mandate_partei1_ul,mandate_partei2_ul,mandate_partei3_ul,mandate_partei4_ul,mandate_partei5_ul,mandate_partei6_ul),Gesamt=c(partei1_sitze_sq,partei2_sitze_sq,partei3_sitze_sq,partei4_sitze_sq,partei5_sitze_sq,partei6_sitze_sq),row.names = c('Partei 1','Partei 2','Partei 3','Partei 4','Partei 5','Partei 6'))

index_sq

View(ergebnis_sq)
