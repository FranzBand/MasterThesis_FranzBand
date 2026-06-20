library(dplyr)

# ── Constraints ────────────────────────────────────────────────────────────
set.seed(42)
n_obs      <- 40000

sectors    <- 1:21          # sector codes 1–21
segments   <- 1:39          # segment codes 1–39
years      <- 1996:2022

lnl_range  <- c(1.0, 14.0)
lnk_range  <- c(2.0, 8.0)
lnva_range <- c(2.5, 9.0)
empl_range <- c(1L,  500L)

# ── Build the dataframe ────────────────────────────────────────────────────
df <- tibble(
  sector   = sample(sectors,  n_obs, replace = TRUE),
  segment  = sample(segments, n_obs, replace = TRUE),
  jaar     = sample(years,    n_obs, replace = TRUE),
  empl     = as.integer(runif(n_obs, empl_range[1], empl_range[2])),
  lnl      = runif(n_obs, lnl_range[1],  lnl_range[2]),
  lnk      = runif(n_obs, lnk_range[1],  lnk_range[2]),
  lnva     = runif(n_obs, lnva_range[1], lnva_range[2])
) |>
  mutate(
    # interaction code: e.g. sector 3, segment 12 → "3_12"
    code = paste(sector, segment, sep = "_")
  ) |>
  select(jaar, sector, segment, code, empl, lnl, lnk, lnva)

glimpse(df)
head(df)