
titulares <- data.frame()
for (i in unique(bind_rows(Matches, Matches_2023)$match_id)) {
  formaciones <- bind_rows(d, d_mundialfem) %>%
    filter(type.id == 35 & !is.na(tactics.formation) & match_id == i) %>%
    select(match_id, tactics.lineup, team.id, player.id)
  
  tit <- do.call(rbind, formaciones$tactics.lineup) %>%
    cbind(c(rep(formaciones$team.id[1], 11), rep(formaciones$team.id[2], 11))) %>%
    rename(team.id = ncol(.)) %>%
    mutate(match_id = i) %>%
    select(-starts_with("position"), -jersey_number)
  
  titulares <- bind_rows(titulares, tit)
}

# 2. Calcular la duración total de cada partido
duracion <- bind_rows(d, d_mundialfem) %>%
  filter(period < 5) %>%
  mutate(aj = round(((minute * 60) + second) / 60)) %>%
  group_by(match_id) %>%
  summarise(dur = last(aj), tiempos = last(period))

titulares <- titulares %>%
  left_join(duracion, by = "match_id")

suplentes <- bind_rows(d, d_mundialfem) %>%
  mutate(aa = round(((minute * 60) + second) / 60),
         tiempo_reg = ifelse(period <= 2, 2, 4)) %>%
  left_join(
    summarise(group_by(., match_id, tiempo_reg), duracion = last(aa)), 
    by = c("match_id", "tiempo_reg")
  ) %>%
  filter(period != 5 & type.name == "Substitution") %>%
  select(match_id, period, minute, second, duracion, player.id, team.id) %>% # Asegurar `player.id`
  rename(dur = duracion) %>%
  mutate(
    mins = round(((minute * 60) + second) / 60),
    tiempojuego = dur - mins
  )

tiempojuego <- bind_rows(
  titulares %>% select(match_id, team.id, player.id, dur) %>% rename(tiempojuego = dur),
  suplentes %>% select(match_id, team.id, player.id, tiempojuego)
) %>%
  group_by(match_id, player.id) %>%
  summarise(tiempojuego = sum(tiempojuego, na.rm = TRUE), .groups = "drop")

# 6. Calcular minutos jugados por cada equipo
tiempojuego_eq <- duracion %>%
  left_join(distinct(select(bind_rows(d, d_mundialfem), match_id, team.id, team.name)), by = "match_id")

# 7. Resumir el tiempo total de juego para cada equipo
tiempojuego_eq.res <- bind_rows(d, d_mundialfem) %>%
  filter(period < 5) %>%
  mutate(aj = round(((minute * 60) + second) / 60)) %>%
  group_by(match_id, team.id, team.name) %>%
  mutate(ec = lag(resultado, default = first(resultado)) != resultado,
         ie = if_else(ec, aj, NA)) %>%
  fill(ie, .direction = "down") %>%
  mutate(duracion = lead(aj, default = max(aj)) - aj) %>%
  group_by(team.id, team.name, resultado) %>%
  summarise(tiempo_total = sum(duracion, na.rm = TRUE)) %>%
  ungroup()

rm(titulares, suplentes, tit)