# ============================================================
# Walkhighlands Corbett scrape (first route per Corbett)
# With 'most climbed' and 'by rating' tables
# ============================================================

library(tidyverse)
library(rvest)
library(xml2)
library(progress)

# ---------------- Utilities ----------------

read_html_utf8 <- function(url, tries = 3, sleep_min = 0.2, sleep_max = 0.6) {
  for (i in seq_len(tries)) {
    out <- try(read_html(url, encoding = "UTF-8"), silent = TRUE)
    if (!inherits(out, "try-error")) return(out)
    Sys.sleep(runif(1, sleep_min, sleep_max))
  }
  stop("Failed to read HTML after retries: ", url)
}

is_route_href <- function(href) {
  str_detect(href, "(?i)\\.shtml($|[?#])") &&
    !str_detect(href, "(?i)(about|advertis|cookie|privacy|shortlist|forum)\\.shtml")
}

# ---------------- 1) Corbett A–Z ----------------

az_url <- "https://www.walkhighlands.co.uk/corbetts/corbetts-a-z"
pg_az  <- read_html_utf8(az_url)

corbetts_az_links <-
  html_elements(pg_az, "table") |>
  map_dfr(function(tab) {
    rows <- html_elements(tab, "tr")
    if (length(rows) > 1) rows <- rows[-1]
    map_dfr(rows, function(tr) {
      a <- html_element(tr, "td:nth-child(2) a")   # now column 2
      tibble(
        corbett     = a |> html_text2(),
        region      = html_element(tr, "td:nth-child(3)") |> html_text2(),
        altitude_m  = html_element(tr, "td:nth-child(4)") |> html_text2() |> readr::parse_number(),
        corbett_url = a |> html_attr("href") |> rvest::url_absolute(az_url)
      )
    })
  }) |>
  distinct()


# ---------------- 2) Find the first route link ----------------

get_first_route_link <- function(page_url) {
  pg <- read_html_utf8(page_url)
  
  pick_first <- function(nodes) {
    if (length(nodes) == 0) return(NULL)
    hrefs <- html_attr(nodes, "href") |> rvest::url_absolute(page_url)
    keep  <- vapply(hrefs, is_route_href, logical(1))
    
    # filter out region/area pages (heuristic: they live under /[a-z]+.shtml not in /walks/ )
    keep <- keep & !str_detect(hrefs, "(?i)/(isleofrum|lochlomond|cairngorms|torridon|perthshire|ullapool)\\.shtml$")
    
    if (!any(keep)) return(NULL)
    i <- which(keep)[1]
    list(title = html_text2(nodes[[i]]), url = hrefs[i])
  }
  
  # Primary: look under "Detailed route" section
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
    cand <- pick_first(html_elements(h2, xpath = "following::a[@href]"))
    if (!is.null(cand)) return(tibble(first_route_title = cand$title, first_route_url = cand$url))
  }
  
  # Fallback: any route link on page (filtering region links)
  cand <- pick_first(html_elements(pg, xpath = "//a[@href]"))
  if (!is.null(cand)) return(tibble(first_route_title = cand$title, first_route_url = cand$url))
  
  tibble(first_route_title = NA_character_, first_route_url = NA_character_)
}


# ---------------- 3) Parse a route page ----------------

extract_flags_time_distance <- function(route_url) {
  if (is.na(route_url) || !nzchar(route_url)) {
    return(tibble(
      scramble = NA, exposed = NA, spate = NA, river = NA, pathless = NA, bog = NA,
      toilet = NA, bothy = NA, pub = NA, car_park = NA, deer_fence = NA,
      time_hours_min = NA_real_, time_hours_max = NA_real_, distance_km = NA_real_,
      ascent = NA_real_
    ))
  }
  
  pg  <- read_html_utf8(route_url)
  txt <- html_text2(pg)
  
  # --- Time
  tm <- stringr::str_match(txt, "Time\\s*([0-9]+\\.?[0-9]*)\\s*(?:-|–|to)?\\s*([0-9]+\\.?[0-9]*)?")
  time_min <- suppressWarnings(as.numeric(tm[, 2]))
  time_max <- suppressWarnings(as.numeric(ifelse(!is.na(tm[, 3]), tm[, 3], tm[, 2])))
  
  # --- Distance (km or miles)
  km_str <- stringr::str_match(txt, "(?i)Distance\\s*([0-9]+\\.?[0-9]*)\\s*km")[, 2]
  mi_str <- stringr::str_match(txt, "(?i)Distance[^\\n]*?\\(\\s*([0-9]+\\.?[0-9]*)\\s*miles?\\s*\\)")[, 2]
  if (is.na(mi_str)) mi_str <- stringr::str_match(txt, "(?i)Distance\\s*([0-9]+\\.?[0-9]*)\\s*miles?")[, 2]
  km_val <- suppressWarnings(as.numeric(km_str))
  if (is.na(km_val) && !is.na(mi_str)) km_val <- suppressWarnings(as.numeric(mi_str)) * 1.60934
  
  # --- Ascent
  asc_str <- stringr::str_match(txt, "(?i)Ascent\\s*([0-9,\\.]+)\\s*m")[, 2]
  ascent <- suppressWarnings(readr::parse_number(asc_str))
  
  # --- Flags
  bog_hits_total <- stringr::str_count(txt, stringr::regex("\\bbog\\w*", ignore_case = TRUE))
  bog_flag <- bog_hits_total > 1
  
  tibble(
    scramble   = stringr::str_detect(txt, stringr::regex("\\bscrambl\\w*", ignore_case = TRUE)),
    exposed    = stringr::str_detect(txt, stringr::regex("\\bexposed\\b|\\bexposure\\b", ignore_case = TRUE)),
    spate      = stringr::str_detect(txt, stringr::regex("\\bspate\\b|\\bin\\s+spate\\b", ignore_case = TRUE)),
    river      = stringr::str_detect(txt, stringr::regex("\\brivers?\\b", ignore_case = TRUE)),
    bog        = bog_flag,
    pathless   = stringr::str_detect(txt, stringr::regex("\\bpathless\\b", ignore_case = TRUE)),
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

# ---------------- 4) Scrape all Corbetts ----------------

set.seed(1)
corbetts_sample <- corbetts_az_links

pb <- progress_bar$new(
  total = nrow(corbetts_sample),
  format = "Scrape [:bar] :current/:total (:percent) eta: :eta"
)

walkhighlands_corbetts <-
  purrr::pmap_dfr(
    list(corbetts_sample$corbett, corbetts_sample$region, corbetts_sample$altitude_m, corbetts_sample$corbett_url),
    \(c_name, c_region, c_alt, c_url) {
      pb$tick()
      Sys.sleep(runif(1, 0.2, 0.6))
      
      fr <- tryCatch(
        get_first_route_link(c_url),
        error = \(e) tibble(first_route_title = NA_character_, first_route_url = NA_character_)
      )
      
      ft <- tryCatch(
        extract_flags_time_distance(fr$first_route_url),
        error = \(e) tibble(
          scramble = NA, exposed = NA, spate = NA, river = NA, pathless = NA, bog = NA,
          toilet = NA, bothy = NA, pub = NA, car_park = NA, deer_fence = NA,
          time_hours_min = NA_real_, time_hours_max = NA_real_, distance_km = NA_real_,
          ascent = NA_real_
        )
      )
      
      tibble(
        corbett            = c_name,
        region             = c_region,
        height             = c_alt,
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
        pathless            = ft$pathless,
        toilet             = ft$toilet,
        bothy              = ft$bothy,
        pub                = ft$pub,
        car_park           = ft$car_park,
        deer_fence         = ft$deer_fence
      )
    }
  )

# ---------------- 5) Add 'most climbed' and 'by rating' ----------------

mc_url <- "https://www.walkhighlands.co.uk/corbetts/most-climbed"
pg_mc  <- read_html_utf8(mc_url)

most_climbed <-
  html_elements(pg_mc, "table tr") |>
  (\(rows) if (length(rows) > 1) rows[-1] else rows)() |>
  map_dfr(function(tr) {
    name_node <- html_element(tr, "td:nth-child(2) a")
    count_td  <- html_element(tr, "td:nth-child(3)")
    tibble(
      corbett = name_node |> html_text2(),
      ascents = count_td  |> html_text2() |> readr::parse_number()
    )
  }) |>
  filter(!is.na(corbett), nzchar(corbett)) |>
  distinct(corbett, .keep_all = TRUE)

rt_url <- "https://www.walkhighlands.co.uk/corbetts/corbetts-by-rating"
pg_rt  <- read_html_utf8(rt_url)

corbett_ratings <-
  html_elements(pg_rt, "table tr") |>
  (\(rows) if (length(rows) > 1) rows[-1] else rows)() |>
  map_dfr(function(tr) {
    name_node <- html_element(tr, "td:nth-child(2) a")
    rate_td   <- html_element(tr, "td:nth-child(3)")
    tibble(
      corbett = name_node |> html_text2(),
      rating  = rate_td   |> html_text2() |> readr::parse_number()
    )
  }) |>
  filter(!is.na(corbett), nzchar(corbett)) |>
  distinct(corbett, .keep_all = TRUE)

walkhighlands_corbetts <- walkhighlands_corbetts |>
  left_join(most_climbed, by = "corbett") |>
  left_join(corbett_ratings, by = "corbett")

write_csv(walkhighlands_corbetts, file = "walkhighlands_corbetts.csv")
