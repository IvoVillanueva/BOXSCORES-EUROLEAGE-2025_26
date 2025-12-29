library(tidyverse)
library(rvest)
library(jsonlite)
library(lubridate)
library(httr)

# cargar los links de los partidos

links <- readr::read_csv("data/gamecodes_fiba_europecup_2025-26.csv",
                         show_col_types = FALSE)$links


pausa <- 1
pause_every <- 100
pause_seconds <- 5

# extraer los boxscores
boxscores_europe_cup <- list()
i <- 0

for (link in links) {
  i <- i + 1
  message("Procesando partido ", i, " / ", length(links_ok))

  res <- tryCatch(
    {
      my_session <- session(url = link)

      scripts <- my_session %>%
        html_elements("script") %>%
        html_text()

      idx <- which(stringr::str_detect(scripts, "technicalName"))[1]
      if (is.na(idx)) stop("No se encuentra technicalName")

      raw_json_embed <- scripts[idx] %>%
        stringr::str_remove_all("\\n|\\t") %>%
        stringr::str_remove_all("\\\\") %>%
        stringr::str_extract("\\{.*\\}")

      # ---- JSON seguro (things)

      pbp_things <- tryCatch(
        {
          jsonlite::fromJSON(raw_json_embed)
        },
        error = function(e) {
          fixed_json <- raw_json_embed %>%
            stringr::str_replace('^self\\.__next_f\\.push\\(\\[\\d+,"', "") %>%
            stringr::str_replace('"\\]\\)$', "") %>%
            stringr::str_replace_all('(?<![:\\,\\[\\{])"(?![\\:\\,\\}\\]])',
                                     '\\\\"')

          out <- jsonlite::fromJSON(fixed_json)

          if (is.character(out)) {
            out <- jsonlite::fromJSON(out)
          }
        }
      )


      pbp_list <- tryCatch(
        {
          jsonlite::fromJSON(raw_json_embed, simplifyVector = FALSE)
        },
        error = function(e) {
          fixed_json <- raw_json_embed %>%
            stringr::str_replace('^self\\.__next_f\\.push\\(\\[\\d+,"', "") %>%
            stringr::str_replace('"\\]\\)$', "") %>%
            stringr::str_replace_all('(?<![:\\,\\[\\{])"(?![\\:\\,\\}\\]])',
                                     '\\\\"')

          out <- jsonlite::fromJSON(fixed_json, simplifyVector = FALSE)

          if (is.character(out)) {
            out <- jsonlite::fromJSON(out, simplifyVector = FALSE)
          }
        }
      )


      fecha <- as.Date(pbp_things$game$gameDateTime)

      if (fecha > Sys.Date()) {
        message("⏹️ Parando: partido futuro detectado (", fecha, ")")
        break
      }

      short_name_A <- pbp_things$game$teamA$shortName
      short_name_B <- pbp_things$game$teamB$shortName

      teamA_players <- pbp_things$playersTeamA %>%
        tibble() %>%
        mutate(
          personId = as.character(personId),
          uniformNumber = as.character(uniformNumber),
          team = "A",
          player_id = paste0("P_", personId)
        )

      teamB_players <- pbp_things$playersTeamB %>%
        tibble() %>%
        mutate(
          personId = as.character(personId),
          uniformNumber = as.character(uniformNumber),
          team = "B",
          player_id = paste0("P_", personId)
        )

      teamA_stats <- map_dfr(
        pbp_list$gameDetails$c[[1]]$Children,
        ~ as_tibble(.x$Stats) %>% mutate(player_id = .x$Id)
      )

      teamB_stats <- map_dfr(
        pbp_list$gameDetails$c[[2]]$Children,
        ~ as_tibble(.x$Stats) %>% mutate(player_id = .x$Id)
      )

      teamA_box <- teamA_players %>%
        left_join(teamA_stats, by = "player_id") %>%
        mutate(equipo = short_name_A, op_equipo = short_name_B)

      teamB_box <- teamB_players %>%
        left_join(teamB_stats, by = "player_id") %>%
        mutate(equipo = short_name_B, op_equipo = short_name_A)

      bind_rows(teamA_box, teamB_box) %>%
        mutate(
          date = fecha,
          match_id = pbp_list$gameDetails$id,
          .before = 1
        )
    },
    error = function(e) {
      message("❌ Error en ", link, " → ", e$message)
      NULL
    }
  )

  if (!is.null(res)) {
    boxscores_europe_cup[[length(boxscores_europe_cup) + 1]] <- res
  }

  Sys.sleep(pausa)
  if (i %% pause_every == 0) Sys.sleep(pause_seconds)
}
boxscores_europe_cup_df <- bind_rows(boxscores_europe_cup)

write.csv(boxscores_europe_cup_df, "data/boxscores_fiba_europecup_2025-26.csv",
          row.names = FALSE)
