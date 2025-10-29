# 🏀 Euroleague Boxscore Extractor 2025/26

Este repositorio ejecuta automáticamente un script en **R** que **extrae y actualiza los boxscores de todos los partidos de la Euroliga 2025/26** desde la API de **Euroleague Live**, y guarda los datos en `data/euroleague_boxscore_2025_26.csv`.

> **Fuente de partidos:** lista de gamecodes por jornada mantenida en `gamecodes_2025-26.csv`.  
> **Salida:** CSV único con todos los jugadores y partidos jugados hasta la fecha.

---

## ✨ Características

- Obtiene **todos los partidos jugados** hasta la fecha actual (zona horaria de Madrid).
- Limpia y normaliza los datos (nombres de columnas en *snake_case*).
- Añade metadatos útiles: `ronda`, `date`, `id_match`, `team_name`, `opp_team_name`, `isLeague = "euroleague"`.
- **Automatizado con GitHub Actions**: actualización programada de martes a sábado.
- **Commit & push automáticos** del CSV actualizado.

---

## ⚙️ Cómo funciona (pipeline)

1. **Carga de jornadas**  
   Lee `gamecodes/gamecodes_2025-26.csv` (contiene `jornada`, `gamecode`, `date`).

2. **Filtrado temporal**  
   Toma solo los `gamecode` con `date < today("Europe/Madrid")`.

3. **Extracción por partido**  
   Para cada `gamecode`, consulta:
   https://live.euroleague.net/api/Boxscore?gamecode={GC}&seasoncode=E2025
y obtiene `PlayersStats` para ambos equipos, añadiendo:
- `team_name`, `opp_team_name`
- `id_match` (el código del partido)
- `ronda` (jornada), `date`
- `isLeague = "euroleague"`

4. **Salida final**  
Combina todo en un único dataframe y lo guarda en: data/euroleague_boxscore_2025_26.csv

---

## 🧪 Ejecutar localmente

### Requisitos en R
```r
install.packages(c("tidyverse", "jsonlite", "httr", "lubridate", "janitor"))
