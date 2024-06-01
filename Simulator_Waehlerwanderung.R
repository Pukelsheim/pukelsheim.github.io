## title: "Simulator fuer Gallagher-Index"
## author: "Ott Raphael M."
## date created: 2024-04-11
## email: raphael.ott@adon.li
## email: rapott@student.ethz.ch

## ------------------------------------------------------------------

## Dieses Skript berechnet den Gallagher-Index fuer den Doppelproporz sowie fuer den Statusquo im Falle von 5 Parteien auf Basis der Landtagswahlen 2021 mit simulierten Wahlergebnissen

## Anzahl Berechnungen
x<-250

## Auswahl von Wäherwanderung (1) oder von Zufallsgenerator (0)
methode<-1

## Seed (für Reproduzierbarkeit)
set.seed(1)

## Berechnungen
sum_index_pk<-0
sum_index_sq<-0
gleich<-0
pk_besser<-0
sq_besser<-0
i=0

columns = c("Pukelsheim","Statusquo","Unterschied") 
df = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(df) = columns

while (i<x) {

if (methode==1){
  ## Zufallsgenerator
  ## Definition 2021
s2021<-data.frame(Oberland=c(54018,50844,20026,14127,7370),Unterland=c(18343,21475,5917,8329,1186),row.names = c('Partei 1','Partei 2','Partei 3','Partei 4','Partei 5'))

std1<-0.1     # selsbt
std2<-0.025   # Gross zu Gross
std3<-0.01    # x zu klein
std4<-0.05    # Klein zu Gross

r_ws_p1=c(abs(rnorm(1,mean=0.8,sd=std1)),abs(rnorm(1,mean=0.125,sd=std2)),abs(rnorm(3,0.025,sd=std3)))
ws_p1<-r_ws_p1/sum(r_ws_p1)

r_ws_p2=c(abs(rnorm(1,mean=0.125,sd=std2)),abs(rnorm(1,mean=0.8,sd=std1)),abs(rnorm(3,0.025,sd=std3)))
ws_p2<-r_ws_p2/sum(r_ws_p2)

r_ws_p3=c(abs(rnorm(2,mean=0.1,sd=std4)),abs(rnorm(1,mean=0.75,sd=std1)),abs(rnorm(2,0.025,sd=std3)))
ws_p3<-r_ws_p3/sum(r_ws_p3)

r_ws_p4=c(abs(rnorm(2,mean=0.1,sd=std4)),abs(rnorm(1,mean=0.025,sd=std3)),abs(rnorm(1,mean=0.75,sd=std1)),abs(rnorm(1,0.0333,sd=std3)))
ws_p4<-r_ws_p4/sum(r_ws_p4)

r_ws_p5=c(abs(rnorm(2,mean=0.2,sd=std4)),abs(rnorm(2,mean=0.05,sd=std3)),abs(rnorm(1,mean=0.5,sd=std1)))
ws_p5<-r_ws_p5/sum(r_ws_p5)

waehlerstrom<-data.frame(partei1=ws_p1,partei2=ws_p2,partei3=ws_p3,partei4=ws_p4,partei5=ws_p5,row.names=c('partei1','partei2','partei3','partei4','partei5'))

partei1_ol<-round(s2021[1,1]*waehlerstrom[1,1]+s2021[2,1]*waehlerstrom[1,2]+s2021[3,1]*waehlerstrom[1,3]+s2021[4,1]*waehlerstrom[1,4]+s2021[5,1]*waehlerstrom[1,5])
partei1_ul<-round(s2021[1,2]*waehlerstrom[1,1]+s2021[2,2]*waehlerstrom[1,2]+s2021[3,2]*waehlerstrom[1,3]+s2021[4,2]*waehlerstrom[1,4]+s2021[5,2]*waehlerstrom[1,5])
partei2_ol<-round(s2021[1,1]*waehlerstrom[2,1]+s2021[2,1]*waehlerstrom[2,2]+s2021[3,1]*waehlerstrom[2,3]+s2021[4,1]*waehlerstrom[2,4]+s2021[5,1]*waehlerstrom[2,5])
partei2_ul<-round(s2021[1,2]*waehlerstrom[2,1]+s2021[2,2]*waehlerstrom[2,2]+s2021[3,2]*waehlerstrom[2,3]+s2021[4,2]*waehlerstrom[2,4]+s2021[5,2]*waehlerstrom[2,5])
partei3_ol<-round(s2021[1,1]*waehlerstrom[3,1]+s2021[2,1]*waehlerstrom[3,2]+s2021[3,1]*waehlerstrom[3,3]+s2021[4,1]*waehlerstrom[3,4]+s2021[5,1]*waehlerstrom[3,5])
partei3_ul<-round(s2021[1,2]*waehlerstrom[3,1]+s2021[2,2]*waehlerstrom[3,2]+s2021[3,2]*waehlerstrom[3,3]+s2021[4,2]*waehlerstrom[3,4]+s2021[5,2]*waehlerstrom[3,5])
partei4_ol<-round(s2021[1,1]*waehlerstrom[4,1]+s2021[2,1]*waehlerstrom[4,2]+s2021[3,1]*waehlerstrom[4,3]+s2021[4,1]*waehlerstrom[4,4]+s2021[5,1]*waehlerstrom[4,5])
partei4_ul<-round(s2021[1,2]*waehlerstrom[4,1]+s2021[2,2]*waehlerstrom[4,2]+s2021[3,2]*waehlerstrom[4,3]+s2021[4,2]*waehlerstrom[4,4]+s2021[5,2]*waehlerstrom[4,5])
partei5_ol<-round(s2021[1,1]*waehlerstrom[5,1]+s2021[2,1]*waehlerstrom[5,2]+s2021[3,1]*waehlerstrom[5,3]+s2021[4,1]*waehlerstrom[5,4]+s2021[5,1]*waehlerstrom[5,5])
partei5_ul<-round(s2021[1,2]*waehlerstrom[5,1]+s2021[2,2]*waehlerstrom[5,2]+s2021[3,2]*waehlerstrom[5,3]+s2021[4,2]*waehlerstrom[5,4]+s2021[5,2]*waehlerstrom[5,5])

waehler_ol<-sum(partei1_ol,partei2_ol,partei3_ol,partei4_ol,partei5_ol)/15
waehler_ul<-sum(partei1_ul,partei2_ul,partei3_ul,partei4_ul,partei5_ul)/10

waehler_ol
waehler_ul

  s<-data.frame(Oberland=c(partei1_ol,partei2_ol,partei3_ol,partei4_ol,partei5_ol,0,waehler_ol),Unterland=c(partei1_ul,partei2_ul,partei3_ul,partei4_ul,partei5_ul,0,waehler_ul),row.names = c('Partei 1','Partei 2','Partei 3','Partei 4','Partei 5','Partei6 (nicht in Simulation)','Waehler'))
  
}
  
if(methode==0){
  
  ## Zufallsgenerator
  ## Zahlen von 2021 sowie realistische Annahmen von Standartverteilung
  stimmberechtigte_ol<-13137
  stimmberechtigte_ul<-7247
  
  beteiligung_ol<-0.771
  beteiligung_ul<-0.796
  
  sd_beteiligung<-0.03
  
  sd_wk<-0.15
  
  ## Defninition des Szenratio
  wa_partei1<-runif(1,min=0.4,max=1)
  wa_partei2<-runif(1,min=0.4,max=1)
  wa_partei3<-runif(1,min=0.4,max=1)
  wa_partei4<-runif(1,min=0.4,max=1)
  wa_partei5<-runif(1,min=0.4,max=1)
  
  sum_wa<-sum(wa_partei1,wa_partei2,wa_partei3,wa_partei4,wa_partei5)
  
  w_partei1<-wa_partei1/sum_wa
  w_partei2<-wa_partei2/sum_wa
  w_partei3<-wa_partei3/sum_wa
  w_partei4<-wa_partei4/sum_wa
  w_partei5<-wa_partei5/sum_wa
  
  waehler_ol<-round(stimmberechtigte_ol*rnorm(1,mean=beteiligung_ol,sd=sd_beteiligung))
  waehler_ul<-round(stimmberechtigte_ul*rnorm(1,mean=beteiligung_ul,sd=sd_beteiligung))
  
  partei1_ol<-abs(round(rnorm(1,mean=1,sd=sd_wk)*w_partei1*waehler_ol)*15)
  partei2_ol<-abs(round(rnorm(1,mean=1,sd=sd_wk)*w_partei2*waehler_ol)*15)
  partei3_ol<-abs(round(rnorm(1,mean=1,sd=sd_wk)*w_partei3*waehler_ol)*15)
  partei4_ol<-abs(round(rnorm(1,mean=1,sd=sd_wk)*w_partei4*waehler_ol)*15)
  partei5_ol<-abs(round(rnorm(1,mean=1,sd=sd_wk)*w_partei5*waehler_ol)*15)
  
  partei1_ul<-abs(round(rnorm(1,mean=1,sd=sd_wk)*w_partei1*waehler_ul)*10)
  partei2_ul<-abs(round(rnorm(1,mean=1,sd=sd_wk)*w_partei2*waehler_ul)*10)
  partei3_ul<-abs(round(rnorm(1,mean=1,sd=sd_wk)*w_partei3*waehler_ul)*10)
  partei4_ul<-abs(round(rnorm(1,mean=1,sd=sd_wk)*w_partei4*waehler_ul)*10)
  partei5_ul<-abs(round(rnorm(1,mean=1,sd=sd_wk)*w_partei5*waehler_ul)*10)
  
  s<-data.frame(Oberland=c(partei1_ol,partei2_ol,partei3_ol,partei4_ol,partei5_ol,0,waehler_ol),Unterland=c(partei1_ul,partei2_ul,partei3_ul,partei4_ul,partei5_ul,0,waehler_ul),row.names = c('Partei 1','Partei 2','Partei 3','Partei 4','Partei 5','Partei6 (nicht in Simulation)','Waehler'))
  
}
  
  partei1_anteil<-0
  partei2_anteil<-0
  partei3_anteil<-0
  partei4_anteil<-0
  partei5_anteil<-0
  partei6_anteil<-0
  
  ## Pukelsheim
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
  
  ## Berechnung der WÃ¤hlerstimmen
  partei1_waehler<-partei1_ol/15+partei1_ul/10
  partei2_waehler<-partei2_ol/15+partei2_ul/10
  partei3_waehler<-partei3_ol/15+partei3_ul/10
  partei4_waehler<-partei4_ol/15+partei4_ul/10
  partei5_waehler<-partei5_ol/15+partei5_ul/10
  partei6_waehler<-partei6_ol/15+partei6_ul/10
  
  waehler<-sum(partei1_waehler,partei2_waehler,partei3_waehler,partei4_waehler,partei5_waehler,partei6_waehler)
  
  ## Berechung der 8%-Huerde
  partei1_anteil<-partei1_waehler/waehler
  ifelse(partei1_anteil<0.08,partei1_waehler<-0,"")
  ifelse(partei1_anteil<0.08,partei1_ol<-0,"")
  ifelse(partei1_anteil<0.08,partei1_ul<-0,"")
  
  partei2_anteil<-partei2_waehler/waehler
  ifelse(partei2_anteil<0.08,partei2_waehler<-0,"")
  ifelse(partei2_anteil<0.08,partei2_ol<-0,"")
  ifelse(partei2_anteil<0.08,partei2_ul<-0,"")
  
  partei3_anteil<-partei3_waehler/waehler
  ifelse(partei3_anteil<0.08,partei3_waehler<-0,"")
  ifelse(partei3_anteil<0.08,partei3_ol<-0,"")
  ifelse(partei3_anteil<0.08,partei3_ul<-0,"")
  
  partei4_anteil<-partei4_waehler/waehler
  ifelse(partei4_anteil<0.08,partei4_waehler<-0,"")
  ifelse(partei4_anteil<0.08,partei4_ol<-0,"")
  ifelse(partei4_anteil<0.08,partei4_ul<-0,"")
  
  partei5_anteil<-partei5_waehler/waehler
  ifelse(partei5_anteil<0.08,partei5_waehler<-0,"")
  ifelse(partei5_anteil<0.08,partei5_ol<-0,"")
  ifelse(partei5_anteil<0.08,partei5_ul<-0,"")
  
  partei6_anteil<-partei6_waehler/waehler
  ifelse(partei6_anteil<0.08,partei6_waehler<-0,"")
  ifelse(partei6_anteil<0.08,partei6_ol<-0,"")
  ifelse(partei6_anteil<0.08,partei6_ul<-0,"")
  
  waehler_nach_huerde<-sum(partei1_waehler,partei2_waehler,partei3_waehler,partei4_waehler,partei5_waehler,partei6_waehler)
  
  #Jetzt wird die Oberzuteilung vorgenommen
  Wahlschlüssel<-ceiling(waehler_nach_huerde/25)
  partei1_sitze<-round(partei1_waehler/Wahlschlüssel)
  partei2_sitze<-round(partei2_waehler/Wahlschlüssel)
  partei3_sitze<-round(partei3_waehler/Wahlschlüssel)
  partei4_sitze<-round(partei4_waehler/Wahlschlüssel)
  partei5_sitze<-round(partei5_waehler/Wahlschlüssel)
  partei6_sitze<-round(partei6_waehler/Wahlschlüssel)
  
  Landtag_Oberzuteilung<-sum(partei1_sitze,partei2_sitze,partei3_sitze,partei4_sitze,partei5_sitze,partei6_sitze)
  
  #Hier kommt ein Loop falls der SchlÃ¼ssel nicht stimmt
  while (Landtag_Oberzuteilung<25) {
    Wahlschlüssel<-Wahlschlüssel-0.01
    partei1_sitze<-round(partei1_waehler/Wahlschlüssel)
    partei2_sitze<-round(partei2_waehler/Wahlschlüssel)
    partei3_sitze<-round(partei3_waehler/Wahlschlüssel)
    partei4_sitze<-round(partei4_waehler/Wahlschlüssel)
    partei5_sitze<-round(partei5_waehler/Wahlschlüssel)
    partei6_sitze<-round(partei6_waehler/Wahlschlüssel)
    
    Landtag_Oberzuteilung<-sum(partei1_sitze,partei2_sitze,partei3_sitze,partei4_sitze,partei5_sitze,partei6_sitze)
  }
  while (Landtag_Oberzuteilung>25) {
    Wahlschlüssel<-Wahlschlüssel+0.01
    partei1_sitze<-round(partei1_waehler/Wahlschlüssel)
    partei2_sitze<-round(partei2_waehler/Wahlschlüssel)
    partei3_sitze<-round(partei3_waehler/Wahlschlüssel)
    partei4_sitze<-round(partei4_waehler/Wahlschlüssel)
    partei5_sitze<-round(partei5_waehler/Wahlschlüssel)
    partei6_sitze<-round(partei6_waehler/Wahlschlüssel)
    
    Landtag_Oberzuteilung<-sum(partei1_sitze,partei2_sitze,partei3_sitze,partei4_sitze,partei5_sitze,partei6_sitze)
  }
  
  ## Damit ist die Oberzuteilung erledigt
  ## Unterzuetilung:
  
  Wahlkreis_Divisor_Oberland<-sum(partei1_ol,partei2_ol,partei3_ol,partei3_ol,partei4_ol,partei5_ol,partei6_ol)/15
  Wahlkreis_Divisor_Unterland<-sum(partei1_ul,partei2_ul,partei3_ul,partei3_ul,partei4_ul,partei5_ul,partei6_ul)/10
  
  partei1_divisor<-1
  partei2_divisor<-1
  partei3_divisor<-1
  partei4_divisor<-1
  partei5_divisor<-1
  partei6_divisor<-1
  
  kontrolle<-0
  
  while (kontrolle==0) {
    
    #Oberland
    partei1_sitze_ol<-round(partei1_ol/(Wahlkreis_Divisor_Oberland*partei1_divisor))
    partei2_sitze_ol<-round(partei2_ol/(Wahlkreis_Divisor_Oberland*partei2_divisor))
    partei3_sitze_ol<-round(partei3_ol/(Wahlkreis_Divisor_Oberland*partei3_divisor))
    partei4_sitze_ol<-round(partei4_ol/(Wahlkreis_Divisor_Oberland*partei4_divisor))
    partei5_sitze_ol<-round(partei5_ol/(Wahlkreis_Divisor_Oberland*partei5_divisor))
    partei6_sitze_ol<-round(partei6_ol/(Wahlkreis_Divisor_Oberland*partei6_divisor))
    
    Oberland_Sitze<-sum(partei1_sitze_ol,partei2_sitze_ol,partei3_sitze_ol,partei4_sitze_ol,partei5_sitze_ol,partei6_sitze_ol)
    
    while (Oberland_Sitze<15) {
      Wahlkreis_Divisor_Oberland<-Wahlkreis_Divisor_Oberland-0.001
      partei1_sitze_ol<-round(partei1_ol/(Wahlkreis_Divisor_Oberland*partei1_divisor))
      partei2_sitze_ol<-round(partei2_ol/(Wahlkreis_Divisor_Oberland*partei2_divisor))
      partei3_sitze_ol<-round(partei3_ol/(Wahlkreis_Divisor_Oberland*partei3_divisor))
      partei4_sitze_ol<-round(partei4_ol/(Wahlkreis_Divisor_Oberland*partei4_divisor))
      partei5_sitze_ol<-round(partei5_ol/(Wahlkreis_Divisor_Oberland*partei5_divisor))
      partei6_sitze_ol<-round(partei6_ol/(Wahlkreis_Divisor_Oberland*partei6_divisor))
      
      Oberland_Sitze<-sum(partei1_sitze_ol,partei2_sitze_ol,partei3_sitze_ol,partei4_sitze_ol,partei5_sitze_ol,partei6_sitze_ol)
    }
    while (Oberland_Sitze>15) {
      Wahlkreis_Divisor_Oberland<-Wahlkreis_Divisor_Oberland+0.001
      partei1_sitze_ol<-round(partei1_ol/(Wahlkreis_Divisor_Oberland*partei1_divisor))
      partei2_sitze_ol<-round(partei2_ol/(Wahlkreis_Divisor_Oberland*partei2_divisor))
      partei3_sitze_ol<-round(partei3_ol/(Wahlkreis_Divisor_Oberland*partei3_divisor))
      partei4_sitze_ol<-round(partei4_ol/(Wahlkreis_Divisor_Oberland*partei4_divisor))
      partei5_sitze_ol<-round(partei5_ol/(Wahlkreis_Divisor_Oberland*partei5_divisor))
      partei6_sitze_ol<-round(partei6_ol/(Wahlkreis_Divisor_Oberland*partei6_divisor))
      
      Oberland_Sitze<-sum(partei1_sitze_ol,partei2_sitze_ol,partei3_sitze_ol,partei4_sitze_ol,partei5_sitze_ol,partei6_sitze_ol)
    }
    
    #Unterland
    partei1_sitze_ul<-round(partei1_ul/(Wahlkreis_Divisor_Unterland*partei1_divisor))
    partei2_sitze_ul<-round(partei2_ul/(Wahlkreis_Divisor_Unterland*partei2_divisor))
    partei3_sitze_ul<-round(partei3_ul/(Wahlkreis_Divisor_Unterland*partei3_divisor))
    partei4_sitze_ul<-round(partei4_ul/(Wahlkreis_Divisor_Unterland*partei4_divisor))
    partei5_sitze_ul<-round(partei5_ul/(Wahlkreis_Divisor_Unterland*partei5_divisor))
    partei6_sitze_ul<-round(partei6_ul/(Wahlkreis_Divisor_Unterland*partei6_divisor))
    
    Unterland_Sitze<-sum(partei1_sitze_ul,partei2_sitze_ul,partei3_sitze_ul,partei4_sitze_ul,partei5_sitze_ul,partei6_sitze_ul)
    
    while (Unterland_Sitze<10) {
      Wahlkreis_Divisor_Unterland<-Wahlkreis_Divisor_Unterland-0.001
      partei1_sitze_ul<-round(partei1_ul/(Wahlkreis_Divisor_Unterland*partei1_divisor))
      partei2_sitze_ul<-round(partei2_ul/(Wahlkreis_Divisor_Unterland*partei2_divisor))
      partei3_sitze_ul<-round(partei3_ul/(Wahlkreis_Divisor_Unterland*partei3_divisor))
      partei4_sitze_ul<-round(partei4_ul/(Wahlkreis_Divisor_Unterland*partei4_divisor))
      partei5_sitze_ul<-round(partei5_ul/(Wahlkreis_Divisor_Unterland*partei5_divisor))
      partei6_sitze_ul<-round(partei6_ul/(Wahlkreis_Divisor_Unterland*partei6_divisor))
      
      Unterland_Sitze<-sum(partei1_sitze_ul,partei2_sitze_ul,partei3_sitze_ul,partei4_sitze_ul,partei5_sitze_ul,partei6_sitze_ul)
    }
    
    while (Unterland_Sitze>10) {
      Wahlkreis_Divisor_Unterland<-Wahlkreis_Divisor_Unterland+0.001
      partei1_sitze_ul<-round(partei1_ul/(Wahlkreis_Divisor_Unterland*partei1_divisor))
      partei2_sitze_ul<-round(partei2_ul/(Wahlkreis_Divisor_Unterland*partei2_divisor))
      partei3_sitze_ul<-round(partei3_ul/(Wahlkreis_Divisor_Unterland*partei3_divisor))
      partei4_sitze_ul<-round(partei4_ul/(Wahlkreis_Divisor_Unterland*partei4_divisor))
      partei5_sitze_ul<-round(partei5_ul/(Wahlkreis_Divisor_Unterland*partei5_divisor))
      partei6_sitze_ul<-round(partei6_ul/(Wahlkreis_Divisor_Unterland*partei6_divisor))
      
      Unterland_Sitze<-sum(partei1_sitze_ul,partei2_sitze_ul,partei3_sitze_ul,partei4_sitze_ul,partei5_sitze_ul,partei6_sitze_ul)
    }
    
    #Jetzt mÃ¼ssen noch die WÃ¤hlergruppen_Divisoren gefunden werden
    partei1_Sitze_Unterzuetilung<-partei1_sitze_ol+partei1_sitze_ul
    while (partei1_Sitze_Unterzuetilung<partei1_sitze) {
      partei1_divisor<-partei1_divisor-0.001
      partei1_sitze_ul<-round(partei1_ul/(Wahlkreis_Divisor_Unterland*partei1_divisor))
      partei1_sitze_ol<-round(partei1_ol/(Wahlkreis_Divisor_Oberland*partei1_divisor))
      
      partei1_Sitze_Unterzuetilung<-partei1_sitze_ol+partei1_sitze_ul
    }
    while (partei1_Sitze_Unterzuetilung>partei1_sitze) {
      partei1_divisor<-partei1_divisor+0.001
      partei1_sitze_ul<-round(partei1_ul/(Wahlkreis_Divisor_Unterland*partei1_divisor))
      partei1_sitze_ol<-round(partei1_ol/(Wahlkreis_Divisor_Oberland*partei1_divisor))
      
      partei1_Sitze_Unterzuetilung<-partei1_sitze_ol+partei1_sitze_ul
    }
    
    partei2_Sitze_Unterzuetilung<-partei2_sitze_ol+partei2_sitze_ul
    while (partei2_Sitze_Unterzuetilung<partei2_sitze) {
      partei2_divisor<-partei2_divisor-0.001
      partei2_sitze_ul<-round(partei2_ul/(Wahlkreis_Divisor_Unterland*partei2_divisor))
      partei2_sitze_ol<-round(partei2_ol/(Wahlkreis_Divisor_Oberland*partei2_divisor))
      
      partei2_Sitze_Unterzuetilung<-partei2_sitze_ol+partei2_sitze_ul
    }
    while (partei2_Sitze_Unterzuetilung>partei2_sitze) {
      partei2_divisor<-partei2_divisor+0.001
      partei2_sitze_ul<-round(partei2_ul/(Wahlkreis_Divisor_Unterland*partei2_divisor))
      partei2_sitze_ol<-round(partei2_ol/(Wahlkreis_Divisor_Oberland*partei2_divisor))
      
      partei2_Sitze_Unterzuetilung<-partei2_sitze_ol+partei2_sitze_ul
    }
    
    partei3_Sitze_Unterzuetilung<-partei3_sitze_ol+partei3_sitze_ul
    while (partei3_Sitze_Unterzuetilung<partei3_sitze) {
      partei3_divisor<-partei3_divisor-0.001
      partei3_sitze_ul<-round(partei3_ul/(Wahlkreis_Divisor_Unterland*partei3_divisor))
      partei3_sitze_ol<-round(partei3_ol/(Wahlkreis_Divisor_Oberland*partei3_divisor))
      
      partei3_Sitze_Unterzuetilung<-partei3_sitze_ol+partei3_sitze_ul
    }
    while (partei3_Sitze_Unterzuetilung>partei3_sitze) {
      partei3_divisor<-partei3_divisor+0.001
      partei3_sitze_ul<-round(partei3_ul/(Wahlkreis_Divisor_Unterland*partei3_divisor))
      partei3_sitze_ol<-round(partei3_ol/(Wahlkreis_Divisor_Oberland*partei3_divisor))
      
      partei3_Sitze_Unterzuetilung<-partei3_sitze_ol+partei3_sitze_ul
    }
    
    partei4_Sitze_Unterzuetilung<-partei4_sitze_ol+partei4_sitze_ul
    while (partei4_Sitze_Unterzuetilung<partei4_sitze) {
      partei4_divisor<-partei4_divisor-0.001
      partei4_sitze_ul<-round(partei4_ul/(Wahlkreis_Divisor_Unterland*partei4_divisor))
      partei4_sitze_ol<-round(partei4_ol/(Wahlkreis_Divisor_Oberland*partei4_divisor))
      
      partei4_Sitze_Unterzuetilung<-partei4_sitze_ol+partei4_sitze_ul
    }
    while (partei4_Sitze_Unterzuetilung>partei4_sitze) {
      partei4_divisor<-partei4_divisor+0.001
      partei4_sitze_ul<-round(partei4_ul/(Wahlkreis_Divisor_Unterland*partei4_divisor))
      partei4_sitze_ol<-round(partei4_ol/(Wahlkreis_Divisor_Oberland*partei4_divisor))
      
      partei4_Sitze_Unterzuetilung<-partei4_sitze_ol+partei4_sitze_ul
    }
    
    partei5_Sitze_Unterzuetilung<-partei5_sitze_ol+partei5_sitze_ul
    while (partei5_Sitze_Unterzuetilung<partei5_sitze) {
      partei5_divisor<-partei5_divisor-0.001
      partei5_sitze_ul<-round(partei5_ul/(Wahlkreis_Divisor_Unterland*partei5_divisor))
      partei5_sitze_ol<-round(partei5_ol/(Wahlkreis_Divisor_Oberland*partei5_divisor))
      
      partei5_Sitze_Unterzuetilung<-partei5_sitze_ol+partei5_sitze_ul
    }
    while (partei5_Sitze_Unterzuetilung>partei5_sitze) {
      partei5_divisor<-partei5_divisor+0.001
      partei5_sitze_ul<-round(partei5_ul/(Wahlkreis_Divisor_Unterland*partei5_divisor))
      partei5_sitze_ol<-round(partei5_ol/(Wahlkreis_Divisor_Oberland*partei5_divisor))
      
      partei5_Sitze_Unterzuetilung<-partei5_sitze_ol+partei5_sitze_ul
    }
    
    partei6_Sitze_Unterzuetilung<-partei6_sitze_ol+partei6_sitze_ul
    while (partei6_Sitze_Unterzuetilung<partei6_sitze) {
      partei6_divisor<-partei6_divisor-0.001
      partei6_sitze_ul<-round(partei6_ul/(Wahlkreis_Divisor_Unterland*partei6_divisor))
      partei6_sitze_ol<-round(partei6_ol/(Wahlkreis_Divisor_Oberland*partei6_divisor))
      
      partei6_Sitze_Unterzuetilung<-partei6_sitze_ol+partei6_sitze_ul
    }
    while (partei6_Sitze_Unterzuetilung>partei6_sitze) {
      partei6_divisor<-partei6_divisor+0.001
      partei6_sitze_ul<-round(partei6_ul/(Wahlkreis_Divisor_Unterland*partei6_divisor))
      partei6_sitze_ol<-round(partei6_ol/(Wahlkreis_Divisor_Oberland*partei6_divisor))
      
      partei6_Sitze_Unterzuetilung<-partei6_sitze_ol+partei6_sitze_ul
    }
    
    Oberland_Sitze<-sum(partei1_sitze_ol,partei2_sitze_ol,partei3_sitze_ol,partei4_sitze_ol,partei5_sitze_ol,partei6_sitze_ol)
    
    Unterland_Sitze<-sum(partei1_sitze_ul,partei2_sitze_ul,partei3_sitze_ul,partei4_sitze_ul,partei5_sitze_ul,partei6_sitze_ul)
    
    ## Kontrolle ob alles stimmt, sonst weiterrechnen
    if (Unterland_Sitze==10){
      if (Oberland_Sitze==15){
        if (partei1_Sitze_Unterzuetilung==partei1_sitze) {
          if (partei2_Sitze_Unterzuetilung==partei2_sitze) {
            if (partei3_Sitze_Unterzuetilung==partei3_sitze) {
              if (partei4_Sitze_Unterzuetilung==partei4_sitze) {
                if (partei5_Sitze_Unterzuetilung==partei5_sitze) {
                  if (partei6_Sitze_Unterzuetilung==partei6_sitze) {
                    kontrolle<-1
                  }
                }
              }
            }
          }
        }
      }
    }
    
  }
  
  index_pk=sqrt(0.5*sum((partei1_anteil-partei1_sitze/25)^2,(partei2_anteil-partei2_sitze/25)^2,(partei3_anteil-partei3_sitze/25)^2,(partei4_anteil-partei4_sitze/25)^2,(partei5_anteil-partei5_sitze/25)^2,(partei6_anteil-partei6_sitze/25)^2))
  
  
  ## Status quo
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
  
  ## Aufsummierung der Indizes
  sum_index_pk<-sum_index_pk+index_pk
  sum_index_sq<-sum_index_sq+index_sq
  if(index_pk==index_sq){gleich<-gleich+1}
  if(index_pk<index_sq){pk_besser<-pk_besser+1}
  if(index_pk>index_sq){sq_besser<-sq_besser+1}
  
  i<-i+1
  mean_pk<-sum_index_pk/i
  mean_sq<-sum_index_sq/i
  
  df<-rbind(df,c(index_pk,index_sq,ifelse(index_pk==index_sq,0,1)))
  
  print(i)
  print(mean_pk)
  print(mean_sq)
  print(pk_besser)
  print(sq_besser)
  print(gleich)
}

colnames(df) = columns

## Berechnung der Mittelwerte
mean_pk<-sum_index_pk/x
mean_sq<-sum_index_sq/x

mean_pk
mean_sq
pk_besser
sq_besser
gleich

boxplot(subset(df, select = -c(Unterschied)), col = "white")

test<-t.test(df$Pukelsheim,df$Statusquo,alternative="less",paired=TRUE)
test
