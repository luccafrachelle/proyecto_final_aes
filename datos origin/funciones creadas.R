library(tidyverse)

zonas_30<-function(coordx,coordy,inv.coordy=TRUE){
  if(inv.coordy==TRUE){
    loc.y<-80-coordy
    zonas<-case_when(
      coordx<=18 & loc.y<18 ~ 1,
      coordx<=18 & loc.y<30 ~ 2,
      coordx<=18 & loc.y<50 ~ 3,
      coordx<=18 & loc.y<62 ~ 4,
      coordx<=18 & loc.y<=80 ~ 5,
      coordx<=39 & loc.y<18 ~ 6,
      coordx<=39 & loc.y<30 ~ 7,
      coordx<=39 & loc.y<50 ~ 8,
      coordx<=39 & loc.y<62 ~ 9,
      coordx<=39 & loc.y<=80 ~ 10,
      coordx<=60 & loc.y<18 ~ 11,
      coordx<=60 & loc.y<30 ~ 12,
      coordx<=60 & loc.y<50 ~ 13,
      coordx<=60 & loc.y<62 ~ 14,
      coordx<=60 & loc.y<=80 ~ 15,
      coordx<=81 & loc.y<18 ~ 16,
      coordx<=81 & loc.y<30 ~ 17,
      coordx<=81 & loc.y<50 ~ 18,
      coordx<=81 & loc.y<62 ~ 19,
      coordx<=81 & loc.y<=80 ~ 20,
      coordx<=102 & loc.y<18 ~ 21,
      coordx<=102 & loc.y<30 ~ 22,
      coordx<=102 & loc.y<50 ~ 23,
      coordx<=102 & loc.y<62 ~ 24,
      coordx<=102 & loc.y<=80 ~ 25,
      coordx<=120 & loc.y<18 ~ 26,
      coordx<=120 & loc.y<30 ~ 27,
      coordx<=120 & loc.y<50 ~ 28,
      coordx<=120 & loc.y<62 ~ 29,
      TRUE ~ 30)} else {zonas<-case_when(
        coordx<=18 & coordy<18 ~ 1,
        coordx<=18 & coordy<30 ~ 2,
        coordx<=18 & coordy<50 ~ 3,
        coordx<=18 & coordy<62 ~ 4,
        coordx<=18 & coordy<=80 ~ 5,
        coordx<=39 & coordy<18 ~ 6,
        coordx<=39 & coordy<30 ~ 7,
        coordx<=39 & coordy<50 ~ 8,
        coordx<=39 & coordy<62 ~ 9,
        coordx<=39 & coordy<=80 ~ 10,
        coordx<=60 & coordy<18 ~ 11,
        coordx<=60 & coordy<30 ~ 12,
        coordx<=60 & coordy<50 ~ 13,
        coordx<=60 & coordy<62 ~ 14,
        coordx<=60 & coordy<=80 ~ 15,
        coordx<=81 & coordy<18 ~ 16,
        coordx<=81 & coordy<30 ~ 17,
        coordx<=81 & coordy<50 ~ 18,
        coordx<=81 & coordy<62 ~ 19,
        coordx<=81 & coordy<=80 ~ 20,
        coordx<=102 & coordy<18 ~ 21,
        coordx<=102 & coordy<30 ~ 22,
        coordx<=102 & coordy<50 ~ 23,
        coordx<=102 & coordy<62 ~ 24,
        coordx<=102 & coordy<=80 ~ 25,
        coordx<=120 & coordy<18 ~ 26,
        coordx<=120 & coordy<30 ~ 27,
        coordx<=120 & coordy<50 ~ 28,
        coordx<=120 & coordy<62 ~ 29,
        TRUE ~ 30)}
}

corregir_tiempo<-function(datos){
  datos %>%  
    mutate(segundos=as.numeric(substr(timestamp,nchar(timestamp)-5,nchar(timestamp))),
           segs=segundos+minute*60) %>% 
    #select(match_id,possession,segs,type.name) %>% 
    group_by(match_id,possession) %>% 
    mutate(correccion_segs=case_when(
      is.na(lag(segs))~segs,
      lag(segs)>segs~lag(segs)+1,
      TRUE~segs)) %>% 
    pull(correccion_segs)
}

grafz<-function(zonas=30){
  if(zonas==30){
    ggplot()+geom_segment(aes(x=0,xend=120,y=0,yend=0))+
      geom_segment(aes(x=0,xend=120,y=80,yend=80))+
      geom_segment(aes(x=0,xend=0,y=0,yend=80))+
      geom_segment(aes(x=120,xend=120,y=0,yend=80))+
      geom_segment(aes(x=60,xend=60,y=0,yend=80))+
      geom_segment(aes(x=0,xend=120,y=62,yend=62))+
      geom_segment(aes(x=0,xend=120,y=18,yend=18))+
      geom_segment(aes(x=0,xend=120,y=30,yend=30))+
      geom_segment(aes(x=0,xend=120,y=50,yend=50))+
      geom_segment(aes(x=18,xend=18,y=0,yend=80))+
      geom_segment(aes(x=39,xend=39,y=0,yend=80))+
      geom_segment(aes(x=102,xend=102,y=0,yend=80))+
      geom_segment(aes(x=81,xend=81,y=0,yend=80)) +
      geom_text(aes(x=10,y=10,label=1))+
      geom_text(aes(x=10,y=25,label=2))+
      geom_text(aes(x=10,y=40,label=3))+
      geom_text(aes(x=10,y=55,label=4))+
      geom_text(aes(x=10,y=75,label=5))+
      geom_text(aes(x=30,y=10,label=6))+
      geom_text(aes(x=30,y=25,label=7))+
      geom_text(aes(x=30,y=40,label=8))+
      geom_text(aes(x=30,y=55,label=9))+
      geom_text(aes(x=30,y=75,label=10))+
      geom_text(aes(x=50,y=10,label=11))+
      geom_text(aes(x=50,y=25,label=12))+
      geom_text(aes(x=50,y=40,label=13))+
      geom_text(aes(x=50,y=55,label=14))+
      geom_text(aes(x=50,y=75,label=15))+
      geom_text(aes(x=70,y=10,label=16))+
      geom_text(aes(x=70,y=25,label=17))+
      geom_text(aes(x=70,y=40,label=18))+
      geom_text(aes(x=70,y=55,label=19))+
      geom_text(aes(x=70,y=75,label=20))+
      geom_text(aes(x=90,y=10,label=21))+
      geom_text(aes(x=90,y=25,label=22))+
      geom_text(aes(x=90,y=40,label=23))+
      geom_text(aes(x=90,y=55,label=24))+
      geom_text(aes(x=90,y=75,label=25))+
      geom_text(aes(x=110,y=10,label=26))+
      geom_text(aes(x=110,y=25,label=27))+
      geom_text(aes(x=110,y=40,label=28))+
      geom_text(aes(x=110,y=55,label=29))+
      geom_text(aes(x=110,y=75,label=30))} else if (zonas==8) {
        # grafico zonas paper Shen, Santo, Akande
        ggplot() +
          geom_segment(aes(x=0,xend=120,y=0,yend=0))+
          geom_segment(aes(x=0,xend=120,y=80,yend=80))+
          geom_segment(aes(x=0,xend=0,y=0,yend=80))+
          geom_segment(aes(x=120,xend=120,y=0,yend=80))+
          geom_segment(aes(x=60,xend=60,y=0,yend=80))+
          geom_segment(aes(x=18,xend=18,y=18,yend=62))+
          geom_segment(aes(x=18,xend=0,y=18,yend=0))+
          geom_segment(aes(x=18,xend=0,y=62,yend=80))+
          geom_segment(aes(x=102,xend=102,y=18,yend=62))+
          geom_segment(aes(x=102,xend=120,y=18,yend=0))+
          geom_segment(aes(x=102,xend=120,y=62,yend=80))+
          geom_segment(aes(x=18,xend=102,y=62,yend=62))+
          geom_segment(aes(x=18,xend=102,y=18,yend=18))+
          geom_text(aes(x=10,y=40,label=1))+
          geom_text(aes(x=30,y=70,label=2))+
          geom_text(aes(x=80,y=70,label=5))+
          geom_text(aes(x=30,y=10,label=4))+
          geom_text(aes(x=80,y=10,label=7))+
          geom_text(aes(x=35,y=40,label=3))+
          geom_text(aes(x=80,y=40,label=6))+
          geom_text(aes(x=110,y=40,label=8))
      }else if (zonas==3) {
        ggplot() +
          geom_segment(aes(x=0,xend=120,y=0,yend=0))+
          geom_segment(aes(x=0,xend=120,y=80,yend=80))+
          geom_segment(aes(x=0,xend=0,y=0,yend=80))+
          geom_segment(aes(x=120,xend=120,y=0,yend=80))+
          geom_segment(aes(x=40,xend=40,y=0,yend=80))+
          geom_segment(aes(x=80,xend=80,y=0,yend=80))+
          geom_text(aes(x=20,y=40,label=1))+
          geom_text(aes(x=60,y=40,label=2))+
          geom_text(aes(x=100,y=40,label=3))+
          geom_text(aes(x=20,y=30,label="Fase de iniciacion"))+
          geom_text(aes(x=60,y=30,label="Fase de creacion"))+
          geom_text(aes(x=100,y=30,label="Fase de finalizacion"))
      }}


prueba<-d %>% 
  bind_rows(d_mundialfem) %>% 
  filter(period<5) %>% 
  select(match_id,possession,period,minute,second,type.id,possession_team.id,possession_team.name,team.id,team.name,resultado) %>% 
  mutate(segs=second+minute*60)

duracion_pos2<-data.frame()
for (i in unique(prueba$match_id)) {
  partido<- prueba %>%  filter(match_id==i)
  eq1<-filter(partido,possession_team.id==unique(partido$possession_team.id)[1]) %>% 
    group_by(possession) %>% 
    mutate(inicio=first(segs),fin=last(segs),
           duracion=fin-inicio,
           fin2=ifelse(inicio>fin,nth(segs,-2)+1,segs),
           duracion2=ifelse(inicio>fin,fin2-inicio,duracion)) %>%  
    summarise(d=first(duracion2)) %>% ungroup() %>% 
    summarise(sum(d))
  eq2<-filter(partido,possession_team.id==unique(partido$possession_team.id)[2]) %>% 
    group_by(possession) %>% 
    mutate(inicio=first(segs),fin=last(segs),
           duracion=fin-inicio,
           fin2=ifelse(inicio>fin,nth(segs,-2)+1,segs),
           duracion2=ifelse(inicio>fin,fin2-inicio,duracion)) %>%
    summarise(d=first(duracion2)) %>% ungroup() %>% 
    summarise(sum(d))
  dur<-data.frame(rbind(cbind(match_id=i,possession_team.id=unique(partido$possession_team.id)[1],duracion_pos=as.numeric(eq1)),
                        cbind(match_id=i,possession_team.id=unique(partido$possession_team.id)[2],duracion_pos=as.numeric(eq2))))
  duracion_pos2<-rbind(duracion_pos2,dur) %>% arrange(duracion_pos)
  
}

duracion_pos2<-duracion_pos2 %>% 
  left_join(distinct(select(mutate(d,mundial='M'),c(match_id,mundial))),by="match_id") %>% 
  mutate(mundial=ifelse(is.na(mundial),'F','M')) %>% 
  arrange(match_id)