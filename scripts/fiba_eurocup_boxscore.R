
links <- "https://www.fiba.basketball/en/events/fiba-europe-cup-25-26" %>%
  read_html() %>%
  html_elements("a.wa01av9") %>%
  html_attr("href") %>%
  paste0("https://www.fiba.basketball", ., "#playByPlay")



page <- "https://www.fiba.basketball/en/events/fiba-europe-cup-25-26/games/128834-BCPD-SBB#playByPlay"
my_session <- session(url = page)


raw_json_embed <- my_session %>%
  html_elements("script") %>%
  html_text() %>%
  .[114] %>%
  str_remove_all("\\n|\\t") %>%
  str_remove_all("\\\\")


clean <- str_extract(raw_json_embed, "\\{.*\\}")

pbp_things <- jsonlite::fromJSON(clean)


pbp_list <- jsonlite::fromJSON(clean, simplifyVector = FALSE)


fecha <- pbp_things$game$gameDateTime



team <- my_session %>%
  html_elements("script") %>%
  html_text() %>%
  .[157] %>%
  str_remove_all("\\n|\\t") %>%
  str_remove_all("\\\\") %>%
  str_extract(., "\\{.*\\}") %>%
  fromJSON(simplifyVector = TRUE)

team_A_abb <- team$teamA$code
team_B_abb <- team$teamB$code

short_name_A <- pbp_things$game$teamA$shortName
short_name_B <- pbp_things$game$teamB$shortName

teamA_players <- map_dfr(pbp_list$playersTeamA, as_tibble) %>%
  mutate(
    team = "A",
    player_id = paste0("P_", personId)
  )

teamB_players <- map_dfr(pbp_list$playersTeamB, as_tibble) %>%
  mutate(
    team = "B",
    player_id = paste0("P_", personId)
  )

teamA_stats <- map_dfr(
  pbp_list$gameDetails$c[[1]]$Children,
  ~ as_tibble(.x$Stats) %>%
    mutate(player_id = .x$Id)
)

teamB_stats <- map_dfr(
  pbp_list$gameDetails$c[[2]]$Children,
  ~ as_tibble(.x$Stats) %>%
    mutate(player_id = .x$Id)
)


teamA_box <- teamA_players %>%
  left_join(teamA_stats, by = "player_id") %>%
  mutate(
    abb = team_A_abb,
    equipo = short_name_A,
    op_abb = team_B_abb,
    op_equipo = short_name_B
  ) %>%
  relocate(abb, equipo, op_abb, op_equipo, .after = last_col())

teamB_box <- teamB_players %>%
  left_join(teamB_stats, by = "player_id") %>%
  mutate(
    abb = team_B_abb,
    equipo = short_name_B,
    op_abb = team_A_abb,
    op_equipo = short_name_A
  ) %>%
  relocate(abb, equipo, op_abb, op_equipo, .after = last_col())


boxscore <- bind_rows(teamA_box, teamB_box) %>%
  mutate(
    date = fecha,
    match_id = pbp_list$gameDetails$id,
    .before = 1
  )

links tiene todos los links de los partidos de la competicion primer paso hacer un bucle o una funcion que cuando fecha sea mayor  que hoy stop (pare)
