#librerias
library(tidyverse)
library(jsonlite)
library(httr)
library(lubridate)
library(janitor)


# asegurar que la carpeta data existe
if (!dir.exists("data")) dir.create("data")

#cargar las jornadas
ronda_df <- read_csv(
  "https://raw.githubusercontent.com/IvoVillanueva/BOXSCORES-EUROLEAGE-2025_26/refs/heads/main/data/gamecodes_eurocup_2025-26.csv",
  show_col_types = FALSE,
  progress = FALSE
) 

#extraer los codigos de partidos hasta la fecha
gamecode <- ronda_df %>%
  arrange(jornada, gamecode, date) %>%
  filter(date < today(tzone = "Europe/Madrid")) %>%
  pull(gamecode)

#función que extrae los boxscores
boxscores_fn <- function(gamecode) {
  url <- paste0("https://live.euroleague.net/api/Boxscore?gamecode=", gamecode, "&seasoncode=U2025")
  
  
  round <- ronda_df %>%
    rename(codigo = gamecode) %>%
    arrange(jornada, codigo) %>%
    filter(codigo == gamecode) %>%
    pull(jornada)
  
  fecha <- ronda_df %>%
    rename(codigo = gamecode) %>%
    arrange(jornada, codigo) %>%
    filter(codigo == gamecode) %>%
    pull(date)
  
  raw_teams <- httr::GET(url, query = list()) %>%
    content()
  
  # Comprueba si el elemento principal "Stats" existe y tiene al menos 2 equipos (elementos)
  if (!is.list(raw_teams) || is.null(raw_teams$Stats) || length(raw_teams$Stats) < 2) {
    message(paste("Saltando gamecode:", gamecode, ". Juego no jugado o sin datos. Devolviendo NULL."))
    return(NULL) 
  }
  
  tm <- pluck(raw_teams, "Stats") %>%
    tibble(value = .) %>%
    unnest_wider(value) %>%
    select(Team)
  
  
  
  df <- pluck(raw_teams, "Stats", 1, "PlayersStats") %>%
    tibble(value = .) %>%
    unnest_wider(value) %>%
    mutate(
      team_name = tm$Team[1],
      opp_team_name = tm$Team[2],
      id_match = gamecode
    )
  
  df1 <- purrr::pluck(raw_teams, "Stats", 2, "PlayersStats") %>%
    tibble(value = .) %>%
    unnest_wider(value) %>%
    mutate(
      team_name = tm$Team[2],
      opp_team_name = tm$Team[1],
      id_match = gamecode
    )
  
  df2 <- rbind(df, df1) %>%
    select(id_match, Player_ID:opp_team_name) %>%
    janitor::clean_names() %>%
      mutate(
      competicion = "Eurocup",
      fecha = fecha,
      semana = lubridate::isoweek(fecha),
      player_id = str_squish(player_id),
      ronda = round,
      .before = id_match,
    )
}

boxscores_df <- map_df(gamecode, boxscores_fn)

#escribir el dataframe en la carpeta "data/"
write.csv(boxscores_df, "data/eurocup_boxscore_2025_26.csv", row.names = F)
