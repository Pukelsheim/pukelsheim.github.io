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

## Berechnung der Wählerstimmen
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

#Hier kommt ein Loop falls der Schlüssel nicht stimmt
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
  
  #Jetzt müssen noch die Wählergruppen_Divisoren gefunden werden
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

index=sqrt(0.5*sum((partei1_anteil-partei1_sitze/25)^2,(partei2_anteil-partei2_sitze/25)^2,(partei3_anteil-partei3_sitze/25)^2,(partei4_anteil-partei4_sitze/25)^2,(partei5_anteil-partei5_sitze/25)^2,(partei6_anteil-partei6_sitze/25)^2))

ergebnis<-data.frame(Oberland=c(partei1_sitze_ol,partei2_sitze_ol,partei3_sitze_ol,partei4_sitze_ol,partei5_sitze_ol,partei6_sitze_ol,Wahlkreis_Divisor_Oberland),Unterland=c(partei1_sitze_ul,partei2_sitze_ul,partei3_sitze_ul,partei4_sitze_ul,partei5_sitze_ul,partei6_sitze_ul,Wahlkreis_Divisor_Unterland),Gesamt=c(partei1_sitze,partei2_sitze,partei3_sitze,partei4_sitze,partei5_sitze,partei6_sitze,Landtag_Oberzuteilung),Divisor=c(partei1_divisor,partei2_divisor,partei3_divisor,partei4_divisor,partei5_divisor,partei6_divisor,index),row.names = c('Partei 1','Partei 2','Partei 3','Partei 4','Partei 5','Partei 6','Divisor'))

View(ergebnis)
