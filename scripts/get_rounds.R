
library(tidyverse)
library(jsonlite)
library(httr)
library(httr2)


ronda_fn <- function(ronda) {
  df <- paste0("https://feeds.incrowdsports.com/provider/euroleague-feeds/v2/competitions/E/seasons/E2025/games?teamCode=&phaseTypeCode=RS&roundNumber=", ronda) %>%
    httr::GET(query = list()) %>%
    httr::content() %>%
    purrr::pluck("data") %>%
    dplyr::tibble(value = .) %>%
    tidyr::unnest_wider(value) %>%
    tidyr::unnest_wider(round) %>%
    tidyr::unnest_wider(confirmedDate, names_sep = "_") %>%
    transmute(jornada = round,
              gamecode = code,
              date = lubridate::with_tz(lubridate::ymd_hms(date),
                                        "Europe/Madrid"),
              semana = lubridate::isoweek(date))
}

ronda_df <- map_df(1:38, ronda_fn)

write_csv(ronda_df, here::here("data/gamecodes_euroleague_2025-26.csv"))


ronda_eu <- function(ronda) {
  df <- paste0("https://feeds.incrowdsports.com/provider/euroleague-feeds/v2/competitions/U/seasons/U2025/games?teamCode=&phaseTypeCode=RS&roundNumber=", ronda) %>%
    httr::GET(query = list()) %>%
    httr::content() %>%
    purrr::pluck("data") %>%
    dplyr::tibble(value = .) %>%
    tidyr::unnest_wider(value) %>%
    tidyr::unnest_wider(round) %>%
    tidyr::unnest_wider(confirmedDate, names_sep = "_") %>%
    transmute(jornada = round,
              gamecode = code,
              date = lubridate::with_tz(lubridate::ymd_hms(date),
                                        "Europe/Madrid"),
              semana = lubridate::isoweek(date))
}

ronda_eu <- map_df(1:18, ronda_eu)

write_csv(ronda_eu, here::here("data/gamecodes_eurocup_2025-26.csv"))
