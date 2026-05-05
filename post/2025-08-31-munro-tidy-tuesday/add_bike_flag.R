# ============================================================
# Add 'bike' flag to existing Munro and Corbett CSVs
# Visits only the already-known route URLs (no re-scraping)
# ============================================================

library(tidyverse)
library(rvest)
library(xml2)
library(progress)

# --- Utility: read with UTF-8 and retry ---
read_html_utf8 <- function(url, tries = 3, sleep_min = 0.2, sleep_max = 0.6) {
  for (i in seq_len(tries)) {
    out <- try(read_html(url, encoding = "UTF-8"), silent = TRUE)
    if (!inherits(out, "try-error")) return(out)
    Sys.sleep(runif(1, sleep_min, sleep_max))
  }
  stop("Failed to read HTML after retries: ", url)
}

# --- Check a single route URL for bike/cycle mentions ---
check_bike <- function(route_url) {
  if (is.na(route_url) || !nzchar(route_url)) return(NA)
  pg  <- read_html_utf8(route_url)
  txt <- html_text2(pg)
  stringr::str_detect(txt, stringr::regex("\\bbike\\w*\\b|\\bcycl\\w*\\b", ignore_case = TRUE))
}

# ============================================================
# 1) Munros
# ============================================================

munro_csv <- "post/2025-08-31-munro-tidy-tuesday/walkhighlands.csv"
munros <- read_csv(munro_csv)

# Get unique route URLs to avoid re-fetching duplicates
unique_routes <- munros |>
  distinct(first_route_url) |>
  filter(!is.na(first_route_url), nzchar(first_route_url))

cat("Checking", nrow(unique_routes), "unique Munro route URLs for bike/cycle mentions...\n")

pb <- progress_bar$new(
  total = nrow(unique_routes),
  format = "Munros [:bar] :current/:total (:percent) eta: :eta"
)

bike_lookup <- unique_routes |>
  mutate(bike = map_lgl(first_route_url, function(url) {
    pb$tick()
    Sys.sleep(runif(1, 1, 3))
    tryCatch(check_bike(url), error = \(e) NA)
  }))

munros <- munros |>
  left_join(bike_lookup, by = "first_route_url")

write_csv(munros, munro_csv)
cat("Munro CSV updated with bike column.\n\n")

# ============================================================
# 2) Corbetts
# ============================================================

corbett_csv <- "post/2025-09-25-corbetts/walkhighlands_corbetts.csv"
corbetts <- read_csv(corbett_csv)

unique_routes_c <- corbetts |>
  distinct(first_route_url) |>
  filter(!is.na(first_route_url), nzchar(first_route_url))

cat("Checking", nrow(unique_routes_c), "unique Corbett route URLs for bike/cycle mentions...\n")

pb <- progress_bar$new(
  total = nrow(unique_routes_c),
  format = "Corbetts [:bar] :current/:total (:percent) eta: :eta"
)

bike_lookup_c <- unique_routes_c |>
  mutate(bike = map_lgl(first_route_url, function(url) {
    pb$tick()
    Sys.sleep(runif(1, 1, 3))
    tryCatch(check_bike(url), error = \(e) NA)
  }))

corbetts <- corbetts |>
  left_join(bike_lookup_c, by = "first_route_url")

write_csv(corbetts, corbett_csv)
cat("Corbett CSV updated with bike column.\n")
