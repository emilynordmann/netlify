# ============================================================
# Walkhighlands Munro route scrape (first route per Munro)
# Robust UTF-8 reader, multi-pass link search, and .shtml+# fix
# ============================================================

library(tidyverse)
library(rvest)
library(xml2)
library(fuzzyjoin)
library(progress)
library(ggthemes)
library(waffle)
library(ggrain)
library(treemap)
library(ggridges)
library(flextable)

# ---------------- Utilities ----------------

# Read with UTF-8 and a light retry (helps with transient fetch hiccups)
read_html_utf8 <- function(url, tries = 3, sleep_min = 0.2, sleep_max = 0.6) {
  for (i in seq_len(tries)) {
    out <- try(read_html(url, encoding = "UTF-8"), silent = TRUE)
    if (!inherits(out, "try-error")) return(out)
    Sys.sleep(runif(1, sleep_min, sleep_max))
  }
  stop("Failed to read HTML after retries: ", url)
}

# Keep only genuine route pages; drop site chrome
# Accept .shtml at end, or followed by ?query or #fragment
is_route_href <- function(href) {
  str_detect(href, "(?i)\\.shtml($|[?#])") &&
    !str_detect(href, "(?i)(about|advertis|cookie|privacy|shortlist|forum)\\.shtml")
}

# ---------------- 1) Munro A–Z ----------------

az_url <- "https://www.walkhighlands.co.uk/munros/munros-a-z"
pg_az  <- read_html_utf8(az_url)

munros_az_links <-
  html_elements(pg_az, "table") |>
  map_dfr(function(tab) {
    rows <- html_elements(tab, "tr")
    if (length(rows) > 1) rows <- rows[-1]
    map_dfr(rows, function(tr) {
      a <- html_element(tr, "td:nth-child(1) a")
      tibble(
        munro      = a |> html_text2(),
        region     = html_element(tr, "td:nth-child(2)") |> html_text2(),
        altitude_m = html_element(tr, "td:nth-child(3)") |> html_text2() |> readr::parse_number(),
        munro_url  = a |> html_attr("href") |> rvest::url_absolute(az_url)
      )
    })
  }) |>
  distinct()

# ---------------- 2) Find the first route link (multi-pass) ----------------

get_first_route_link <- function(munro_page_url) {
  pg <- read_html_utf8(munro_page_url)
  
  # Helper: return first eligible link (title + absolute URL) from a node set
  pick_first <- function(nodes) {
    if (length(nodes) == 0) return(NULL)
    hrefs <- html_attr(nodes, "href") |> rvest::url_absolute(munro_page_url)
    keep  <- vapply(hrefs, is_route_href, logical(1))
    if (!any(keep)) return(NULL)
    i <- which(keep)[1]
    list(title = html_text2(nodes[[i]]), url = hrefs[i])
  }
  
  # Primary: within the "Detailed route" section (between this h2 and the next h2)
  h2 <- html_element(
    pg,
    xpath = "//h2[contains(translate(normalize-space(.),
            'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz'),
            'detailed route')]"
  )
  
  if (!inherits(h2, "xml_missing") && length(h2) > 0) {
    scope <- html_elements(
      h2,
      xpath = "following-sibling::*[preceding-sibling::h2[1]
              [contains(translate(normalize-space(.),
              'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz'),
              'detailed route')]]"
    )
    cand <- pick_first(html_elements(scope, xpath = ".//a[@href]"))
    if (!is.null(cand)) return(tibble(first_route_title = cand$title, first_route_url = cand$url))
    
    # Fallback A: anywhere after the h2 in document order
    cand <- pick_first(html_elements(h2, xpath = "following::a[@href]"))
    if (!is.null(cand)) return(tibble(first_route_title = cand$title, first_route_url = cand$url))
  }
  
  # Fallback B: anywhere on the page
  cand <- pick_first(html_elements(pg, xpath = "//a[@href]"))
  if (!is.null(cand)) return(tibble(first_route_title = cand$title, first_route_url = cand$url))
  
  tibble(first_route_title = NA_character_, first_route_url = NA_character_)
}

# ---------------- 3) Parse a route page (flags, time, distance, ascent) ----------------

extract_flags_time_distance <- function(route_url) {
  if (is.na(route_url) || !nzchar(route_url)) {
    return(tibble(
      scramble = NA, exposed = NA, spate = NA, river = NA, arete = NA, bog = NA,
      toilet = NA, bothy = NA, pub = NA, car_park = NA, deer_fence = NA,
      wood = NA,
      time_hours_min = NA_real_, time_hours_max = NA_real_, distance_km = NA_real_,
      ascent = NA_real_
    ))
  }
  
  pg  <- read_html_utf8(route_url)
  txt <- html_text2(pg)
  
  tm <- stringr::str_match(txt, "Time\\s*([0-9]+\\.?[0-9]*)\\s*(?:-|–|to)?\\s*([0-9]+\\.?[0-9]*)?")
  time_min <- suppressWarnings(as.numeric(tm[, 2]))
  time_max <- suppressWarnings(as.numeric(ifelse(!is.na(tm[, 3]), tm[, 3], tm[, 2])))
  
  km_str <- stringr::str_match(txt, "(?i)Distance\\s*([0-9]+\\.?[0-9]*)\\s*km")[, 2]
  mi_str <- stringr::str_match(txt, "(?i)Distance[^\\n]*?\\(\\s*([0-9]+\\.?[0-9]*)\\s*miles?\\s*\\)")[, 2]
  if (is.na(mi_str)) mi_str <- stringr::str_match(txt, "(?i)Distance\\s*([0-9]+\\.?[0-9]*)\\s*miles?")[, 2]
  km_val <- suppressWarnings(as.numeric(km_str))
  if (is.na(km_val) && !is.na(mi_str)) km_val <- suppressWarnings(as.numeric(mi_str)) * 1.60934
  
  # --- total ascent (m) ---
  asc_str <- stringr::str_match(txt, "(?i)Ascent\\s*([0-9,\\.]+)\\s*m")[, 2]
  ascent <- suppressWarnings(readr::parse_number(asc_str))
  
  # --- flags ---
  bog_hits_total <- stringr::str_count(txt, stringr::regex("\\bbog\\w*", ignore_case = TRUE))
  bog_flag <- bog_hits_total > 1  # TRUE only if 'bog*' occurs more than once
  
  wood_flag <- stringr::str_detect(
    txt,
    stringr::regex("\\bforest\\b|\\bwoodland\\b|\\bwoods?\\b", ignore_case = TRUE)
  )
  
  tibble(
    scramble = stringr::str_detect(txt, stringr::regex("\\bscrambl\\w*", ignore_case = TRUE)),
    exposed  = stringr::str_detect(txt, stringr::regex("\\bexposed\\b|\\bexposure\\b", ignore_case = TRUE)),
    spate    = stringr::str_detect(txt, stringr::regex("\\bspate\\b|\\bin\\s+spate\\b", ignore_case = TRUE)),
    river    = stringr::str_detect(txt, stringr::regex("\\brivers?\\b", ignore_case = TRUE)),
    arete    = stringr::str_detect(txt, stringr::regex("\\bar(?:e|ê)te\\b", ignore_case = TRUE)),
    bog      = bog_flag,
    wood     = wood_flag,
    toilet     = stringr::str_detect(txt, stringr::regex("\\btoilet\\b", ignore_case = TRUE)),
    bothy      = stringr::str_detect(txt, stringr::regex("\\bbothy\\b", ignore_case = TRUE)),
    pub        = stringr::str_detect(txt, stringr::regex("\\bpub\\b", ignore_case = TRUE)),
    car_park   = stringr::str_detect(txt, stringr::regex("\\bcar park\\b", ignore_case = TRUE)),
    deer_fence = stringr::str_detect(txt, stringr::regex("\\bdeer fence\\b", ignore_case = TRUE)),
    time_hours_min = time_min,
    time_hours_max = time_max,
    distance_km    = km_val,
    ascent         = ascent
  )
}

# ---------------- 4) Scrape all Munros ----------------

set.seed(1)
munros_sample <- munros_az_links

pb <- progress_bar$new(
  total = nrow(munros_sample),
  format = "Scrape [:bar] :current/:total (:percent) eta: :eta"
)

walkhighlands <-
  purrr::pmap_dfr(
    list(munros_sample$munro, munros_sample$region, munros_sample$altitude_m, munros_sample$munro_url),
    \(m_name, m_region, m_alt, m_url) {
      pb$tick()
      Sys.sleep(runif(1, 0.2, 0.6))  # polite delay
      
      fr <- tryCatch(
        get_first_route_link(m_url),
        error = \(e) tibble(first_route_title = NA_character_, first_route_url = NA_character_)
      )
      
      ft <- tryCatch(
        extract_flags_time_distance(fr$first_route_url),
        error = \(e) tibble(
          scramble = NA, exposed = NA, spate = NA, river = NA, arete = NA, bog = NA,
          toilet = NA, bothy = NA, pub = NA, car_park = NA, deer_fence = NA,
          wood = NA,
          time_hours_min = NA_real_, time_hours_max = NA_real_, distance_km = NA_real_,
          ascent = NA_real_
        )
      )
      
      tibble(
        munro              = m_name,
        region             = m_region,
        height             = m_alt,
        first_route_title  = fr$first_route_title,
        first_route_url    = fr$first_route_url,
        time_hours_min     = ft$time_hours_min,
        time_hours_max     = ft$time_hours_max,
        distance_km        = ft$distance_km,
        ascent             = ft$ascent,
        scramble           = ft$scramble,
        exposed            = ft$exposed,
        spate              = ft$spate,
        bog                = ft$bog,
        river              = ft$river,
        arete              = ft$arete,
        toilet             = ft$toilet,
        bothy              = ft$bothy,
        pub                = ft$pub,
        car_park           = ft$car_park,
        deer_fence         = ft$deer_fence,
        wood               = ft$wood
      )
    }
  )

# join with the missing routes collated manually

missing <- read_csv(
  "missing_routes.csv",
  locale = readr::locale(encoding = "latin1")   # or "UTF-8" if you prefer
)

walkhighlands <- dplyr::rows_patch(
  x  = walkhighlands,
  y  = missing,
  unmatched = "ignore"
)

# ---------------- 4b) Add 'most climbed' and 'by rating' ----------------
# Scrape: https://www.walkhighlands.co.uk/munros/most-climbed
mc_url <- "https://www.walkhighlands.co.uk/munros/most-climbed"
pg_mc  <- read_html_utf8(mc_url)

most_climbed <-
  html_elements(pg_mc, "table tr") |>
  (\(rows) if (length(rows) > 1) rows[-1] else rows)() |>
  map_dfr(function(tr) {
    name_node <- html_element(tr, "td:nth-child(2) a")   # col 2 = Mountain (link)
    count_td  <- html_element(tr, "td:nth-child(3)")     # col 3 = Ascents
    tibble(
      munro   = name_node |> html_text2(),
      ascents = count_td  |> html_text2() |> readr::parse_number()
    )
  }) |>
  dplyr::filter(!is.na(munro), nzchar(munro)) |>
  distinct(munro, .keep_all = TRUE)

# Scrape: https://www.walkhighlands.co.uk/munros/munros-by-rating
rt_url <- "https://www.walkhighlands.co.uk/munros/munros-by-rating"
pg_rt  <- read_html_utf8(rt_url)

munro_ratings <-
  html_elements(pg_rt, "table tr") |>
  (\(rows) if (length(rows) > 1) rows[-1] else rows)() |>
  map_dfr(function(tr) {
    name_node <- html_element(tr, "td:nth-child(2) a")   # col 2 = Mountain (link)
    rate_td   <- html_element(tr, "td:nth-child(3)")     # col 3 = Rating
    tibble(
      munro  = name_node |> html_text2(),
      rating = rate_td   |> html_text2() |> readr::parse_number()
    )
  }) |>
  dplyr::filter(!is.na(munro), nzchar(munro)) |>
  distinct(munro, .keep_all = TRUE)

# Join both onto the main table
walkhighlands <- walkhighlands |>
  left_join(most_climbed, by = "munro") |>
  left_join(munro_ratings, by = "munro")

# clean up
rm(pb, munros_az_links, az_url, pg_az, munros_sample, missing,
   mc_url, pg_mc, most_climbed, rt_url, pg_rt, munro_ratings)

write_csv(walkhighlands, file = "walkhighlands.csv")

