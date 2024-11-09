##minutos jugados por jugador

titulares<-data.frame()
for (i in unique(bind_rows(Matches,Matches_2023)$match_id)) {
 formaciones<-bind_rows(d,d_mundialfem) %>% filter(type.id==35 & !is.na(tactics.formation) & match_id==i) %>% select(match_id,tactics.lineup,team.id,player.id) 
 tit<-do.call(rbind,formaciones$tactics.lineup) %>% cbind(c(rep(formaciones$team.id[1],11),rep(formaciones$team.id[2],11))) %>% 
   rename(team.id=ncol(.)) %>% 
   mutate(match_id=i) %>% 
   select(-starts_with("position"),-jersey_number)
 titulares<-bind_rows(titulares,tit)
}
duracion <- bind_rows(d,d_mundialfem) %>% 
  filter(period<5) %>%
  mutate(aj= round(((minute*60)+second)/60) ) %>% 
  group_by(match_id) %>% 
  summarise(dur=last(aj),tiempos=last(period)) 

titulares<- titulares %>% 
  left_join(duracion,by="match_id")

suplentes<-bind_rows(d,d_mundialfem) %>% 
  mutate(aa= round(((minute*60)+second)/60) ,
         tiempo_reg=ifelse(period<=2,2,4)) %>% 
  left_join(summarise(group_by(.,match_id,tiempo_reg),duracion=last(aa)),by=c("match_id","tiempo_reg")) %>% 
  filter(period!=5 & type.name=="Substitution") %>% 
  select(match_id,period,minute,second,duracion, player.id, team.id,
         player.name,substitution.replacement.id,substitution.replacement.name) %>%
  rename(Sale= player.name,Entra=substitution.replacement.name,
         Sale_id=player.id,
         Entra_id=substitution.replacement.id,
         dur=duracion) %>%
  mutate(mins=round(((minute*60)+second)/60) ,
         tiempojuego= dur - mins) 

tiempojuego<-bind_rows(select(titulares,-tiempos), select(suplentes,-c())) %>% 
  select(Entra_id,Entra,team.id,match_id,tiempojuego) %>% 
    rename(player.id=1,player.name=2)) %>% 
  left_join(select(suplentes,c(match_id,player.id,tiempojuego)),by=c("match_id","player.id"),
            suffix = c(".a", ".b")) %>% 
  mutate(tiempojuego=ifelse(is.na(tiempojuego.b),tiempojuego.a,tiempojuego.a-tiempojuego.b)) %>% 
  select(-tiempojuego.a,-tiempojuego.b) 

#minutos equipos
tiempojuego_eq<-duracion %>% 
  left_join(distinct(select(bind_rows(d,d_mundialfem),
                            c(match_id,team.id,team.name))),
            by="match_id")

tiempojuego_eq.res<- bind_rows(d,d_mundialfem) %>% 
  filter(period<5) %>%
  mutate(aj= round(((minute*60)+second)/60)) %>% 
  group_by(match_id,team.id,team.name) %>% 
  mutate(ec=lag(resultado,default = first(resultado))!=resultado,
         ie=if_else(ec,aj,NA)) %>% 
  fill(ie, .direction = "down") %>% 
  mutate(duracion = lead(aj, default = max(aj)) - aj) %>% 
  group_by(team.id,team.name,resultado) %>%
  summarise(tiempo_total = sum(duracion, na.rm = TRUE)) %>% 
  ungroup()

rm(titulares,suplentes,tit)
