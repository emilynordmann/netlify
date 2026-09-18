# Shared toolkit for the Munro walk posts.
# Sourced by each walk post; not rendered by Quarto (underscore prefix).
#
# Input is a CSV decoded from the Garmin .fit file (one row per second-ish:
# time, lat, lon, ele, dist_m, speed_ms, hr, cadence, temp). The decoder is
# post/_fit2csv.py; Claude runs it on the zip that Garmin Connect exports.
#
# A post is then:
#   track <- munro_read_track(...); segs <- munro_segments(track); ...
# plus the plot calls, and a `stages` table that splits the walk into the
# sections the narrative follows.

library(tidyverse)
library(ggrepel)
library(ggspatial)
library(sf)
library(scales)

# ---- Fonts -------------------------------------------------------------------
# Display faces live in post/_fonts (Google Fonts, OFL).
munro_fonts_dir <- normalizePath(file.path("..", "_fonts"), mustWork = FALSE)
munro_register_fonts <- function() {
  reg <- function(name, file) {
    f <- file.path(munro_fonts_dir, file)
    if (file.exists(f) && !(name %in% systemfonts::registry_fonts()$family))
      systemfonts::register_font(name, f)
  }
  reg("Caveat", "Caveat-Bold.ttf")
  reg("Bebas Neue", "BebasNeue.ttf")
  reg("Fredoka", "Fredoka-Bold.ttf")
  invisible(TRUE)
}
munro_register_fonts()

munro_font_title <- "Fredoka"      # chart titles
munro_font_big   <- "Bebas Neue"   # poster title and stat numbers
munro_font_body  <- "Lato"
munro_font_hand  <- "Caveat"

# ---- Palette -----------------------------------------------------------------

munro_cols <- c(
  bg      = "#F5F1E8",  # paper
  ink     = "#22302A",
  muted   = "#6E756F",
  wood    = "#2F5D50",  # forest green (the track, poster band)
  contour = "#D9A441",  # gold (breaks)
  magenta = "#C0532B",  # rust (hard bits, summits)
  water   = "#3E7CB1",  # water blue (photos, river)
  hr      = "#B23A48",
  card    = "#FFFFFF",
  heat    = "#7A3410"   # dark brown, top of the heat-map scale
)

# Gradient classes used for the profile strip and the map path
munro_grad_breaks <- c(-Inf, -25, -10, 10, 25, Inf)
munro_grad_labels <- c("Steep down", "Down", "Flat-ish", "Up", "Steep up")
munro_grad_cols   <- c("Steep down" = "#4E3A66", "Down" = "#9E86B8", "Flat-ish" = "#B9C4B3",
                       "Up" = "#E0A94F", "Steep up" = "#C0532B")

munro_theme <- function(base_size = 14) {
  theme_minimal(base_size = base_size, base_family = munro_font_body) +
    theme(
      text                = element_text(colour = munro_cols[["ink"]]),
      plot.title          = element_text(family = munro_font_title, size = rel(1.4),
                                         margin = margin(b = 4)),
      plot.title.position = "plot",
      plot.subtitle       = element_text(colour = munro_cols[["muted"]], size = rel(0.9),
                                         lineheight = 1.1, margin = margin(b = 14)),
      plot.caption        = element_text(size = rel(0.7), colour = munro_cols[["muted"]],
                                         hjust = 0, margin = margin(t = 14)),
      plot.caption.position = "plot",
      axis.title          = element_text(size = rel(0.8), colour = munro_cols[["muted"]]),
      axis.text           = element_text(colour = munro_cols[["ink"]]),
      panel.grid.minor    = element_blank(),
      panel.grid.major    = element_line(colour = "#E4E0D6", linewidth = 0.4),
      legend.position     = "top",
      legend.justification = "left",
      legend.title        = element_blank(),
      plot.background     = element_rect(fill = munro_cols[["bg"]], colour = NA),
      panel.background    = element_rect(fill = NA, colour = NA)
    )
}

# ---- Reading and deriving ----------------------------------------------------

# Read the decoded track and derive everything the plots need.
#  calibrate_to: known height (m) of the highest summit. Barometric altimeters
#                drift, so the whole profile is shifted so the high point
#                matches the map. Ascent totals are unaffected by the shift.
#  smooth_s:     half-window (seconds) for the derived speed. The watch's own
#                speed field is unreliable on steep ground, so speed is
#                recomputed from distance travelled over +/- smooth_s.
munro_read_track <- function(file, tz = "Europe/London", calibrate_to = NULL,
                             smooth_s = 30, moving_ms = 0.15) {
  tr <- read_csv(file, show_col_types = FALSE) |>
    filter(!is.na(lat), !is.na(lon)) |>
    mutate(time = with_tz(as_datetime(time), tz),
           t    = as.numeric(difftime(time, first(time), units = "secs")),
           dist = dist_m / 1000)

  if (!is.null(calibrate_to)) tr <- tr |> mutate(ele = ele - (max(ele) - calibrate_to))

  t_before <- pmax(tr$t - smooth_s, 0)
  t_after  <- pmin(tr$t + smooth_s, max(tr$t))
  d_before <- approx(tr$t, tr$dist_m, xout = t_before, rule = 2, ties = mean)$y
  d_after  <- approx(tr$t, tr$dist_m, xout = t_after,  rule = 2, ties = mean)$y

  tr <- tr |>
    mutate(
      ele_s  = as.numeric(stats::runmed(ele, 9, endrule = "median")),
      speed  = (d_after - d_before) / pmax(t_after - t_before, 1),
      moving = speed >= moving_ms,
      dt     = c(0, diff(t)),
      d_ele  = c(0, diff(ele_s)),
      gain   = cumsum(pmax(d_ele, 0)),
      loss   = cumsum(pmax(-d_ele, 0)),
      km     = floor(dist) + 1
    )

  # Point gradient (%) over +/- 50 m of distance, for colouring the path
  e_before <- approx(tr$dist_m, tr$ele_s, xout = pmax(tr$dist_m - 50, 0), rule = 2, ties = mean)$y
  e_after  <- approx(tr$dist_m, tr$ele_s, xout = pmin(tr$dist_m + 50, max(tr$dist_m)), rule = 2, ties = mean)$y
  tr |>
    mutate(grad_pt    = e_after - e_before,
           grad_class = cut(grad_pt, munro_grad_breaks, labels = munro_grad_labels))
}

# Fixed-width segments along the route with the things that make a bit hard:
# steepness, moving pace and heart rate, each as a percentile of the walk,
# averaged into a blunt "difficulty" score.
munro_segments <- function(tr, width = 250) {
  tr |>
    mutate(seg = floor(dist_m / width)) |>
    group_by(seg) |>
    summarise(
      start       = min(dist), end = max(dist),
      time_start  = first(time), time_end = last(time),
      mins        = as.numeric(difftime(last(time), first(time), units = "mins")),
      moving_mins = sum(dt[moving], na.rm = TRUE) / 60,
      gain        = sum(pmax(d_ele, 0)), loss = sum(pmax(-d_ele, 0)),
      ele_start   = first(ele_s), ele_end = last(ele_s),
      ele_max     = max(ele_s),
      hr          = mean(hr, na.rm = TRUE),
      lat         = mean(lat), lon = mean(lon),
      .groups = "drop"
    ) |>
    mutate(mid      = (start + end) / 2,
           length   = end - start,
           gradient = (ele_end - ele_start) / (length * 1000) * 100,
           pace     = moving_mins / length,                 # min per km while moving
           climb_rate = gain / pmax(moving_mins, 0.5) * 60, # m of ascent per hour
           grad_class = cut(gradient, munro_grad_breaks, labels = munro_grad_labels)) |>
    filter(length >= width / 1000 * 0.5) |>
    mutate(steep_pct = percent_rank(abs(gradient)),
           pace_pct  = percent_rank(pace),
           hr_pct    = percent_rank(hr),
           # steepness and heart rate only: the watch's moving/stopped split is
           # not trusted, so pace stays out of the score
           difficulty = (steep_pct + hr_pct) / 2,
           rank = min_rank(-difficulty))
}

# The n hardest segments
munro_hard <- function(segs, n = 3) {
  segs |>
    slice_min(rank, n = n, with_ties = FALSE) |>
    arrange(start) |>
    mutate(hard = row_number())
}

# Breaks: runs of not-moving lasting at least min_minutes
munro_stops <- function(tr, min_minutes = 3) {
  tr |>
    mutate(run = consecutive_id(moving)) |>
    filter(!moving) |>
    group_by(run) |>
    summarise(start = first(time), end = last(time),
              mins = as.numeric(difftime(end, start, units = "mins")),
              dist = first(dist), ele = first(ele_s),
              lat = mean(lat), lon = mean(lon), .groups = "drop") |>
    filter(mins >= min_minutes) |>
    mutate(stop = row_number()) |>
    select(-run)
}

# Where and when each summit was reached: the first track point within
# `within` km of the summit, or the nearest point if never that close.
#  summits: tibble(name, height, lat, lon)
munro_summits <- function(tr, summits, within = 0.15) {
  summits |>
    mutate(idx = map2_int(lat, lon, function(la, lo) {
      d <- sqrt(((tr$lat - la) * 111.2)^2 + ((tr$lon - lo) * 111.2 * cos(la * pi / 180))^2)
      close <- which(d <= within)
      if (length(close) > 0) close[1] else which.min(d)
    })) |>
    mutate(time = tr$time[idx], dist = tr$dist[idx], ele_rec = tr$ele_s[idx],
           elapsed = tr$t[idx] / 3600,
           # keep the map position, but plot the summit where the track
           # actually got to (GPS and cloud being what they are)
           lat_map = lat, lon_map = lon,
           lat = tr$lat[idx], lon = tr$lon[idx]) |>
    select(-idx)
}

# photos.csv: file, lat, lon, time, caption. Phone photos usually have a time
# but no GPS, so each timed photo is placed at the track point nearest its
# time (local) and gets a distance and height. Photos with neither time nor
# position still appear in the gallery, without a marker.
munro_photos <- function(file = "photos.csv", tr = NULL) {
  if (!file.exists(file)) return(NULL)
  ph <- read_csv(file, show_col_types = FALSE,
                 col_types = cols(time = col_character(), lat = col_double(),
                                  lon = col_double(), .default = col_guess())) |>
    mutate(time = as_datetime(time, tz = if (is.null(tr)) "Europe/London" else tz(tr$time)))
  if (!is.null(tr)) {
    ph <- ph |>
      mutate(idx = map_int(time, function(t) {
        if (is.na(t)) return(NA_integer_)
        which.min(abs(as.numeric(difftime(tr$time, t, units = "secs"))))
      }),
      lat  = coalesce(lat, tr$lat[idx]),
      lon  = coalesce(lon, tr$lon[idx]),
      dist = tr$dist[idx],
      ele  = tr$ele_s[idx]) |>
      select(-idx)
  }
  ph |>
    mutate(order = coalesce(as.numeric(time), Inf)) |>
    arrange(order, file) |>
    mutate(n = row_number()) |>
    select(-order)
}

# Headline numbers as a named list, for text and cards
munro_stats <- function(tr, stops, summits = NULL) {
  total_s  <- max(tr$t)
  moving_s <- sum(tr$dt[tr$moving], na.rm = TRUE)
  list(
    distance_km = max(tr$dist),
    ascent_m    = max(tr$gain),
    descent_m   = max(tr$loss),
    high_m      = max(tr$ele_s),
    low_m       = min(tr$ele_s),
    total       = munro_hm(total_s),
    moving      = munro_hm(moving_s),
    stopped     = munro_hm(total_s - moving_s),
    n_stops     = nrow(stops),
    longest_stop = if (nrow(stops)) max(stops$mins) else 0,
    avg_hr      = mean(tr$hr, na.rm = TRUE),
    max_hr      = max(tr$hr, na.rm = TRUE),
    pace_moving = (moving_s / 60) / max(tr$dist),
    start       = min(tr$time), finish = max(tr$time),
    max_grade   = max(tr$grad_pt), min_grade = min(tr$grad_pt)
  )
}

munro_hm <- function(s) {
  h <- s %/% 3600; m <- round((s %% 3600) / 60)
  if (h == 0) sprintf("%d min", m) else sprintf("%d h %02d", h, m)
}

# ---- Stages ------------------------------------------------------------------
# A stages table splits the walk by distance:
#   tribble(~stage, ~name, ~from, ~to)   (from/to in km)
# munro_stages() adds the numbers for each; munro_stage_line() turns one row
# into the inline stats string used under each heading.
munro_stages <- function(tr, stages, segs = NULL) {
  out <- stages |>
    mutate(data = map2(from, to, ~ tr |> filter(dist >= .x, dist < .y))) |>
    mutate(
      km          = map_dbl(data, ~ max(.x$dist) - min(.x$dist)),
      gain        = map_dbl(data, ~ sum(pmax(.x$d_ele, 0))),
      loss        = map_dbl(data, ~ sum(pmax(-.x$d_ele, 0))),
      time_start  = map_vec(data, ~ min(.x$time)),
      time_end    = map_vec(data, ~ max(.x$time)),
      mins        = as.numeric(difftime(time_end, time_start, units = "mins")),
      moving_mins = map_dbl(data, ~ sum(.x$dt[.x$moving], na.rm = TRUE) / 60),
      avg_hr      = map_dbl(data, ~ mean(.x$hr, na.rm = TRUE)),
      max_hr      = map_dbl(data, ~ max(.x$hr, na.rm = TRUE)),
      max_grade   = map_dbl(data, ~ max(.x$grad_pt)),
      min_grade   = map_dbl(data, ~ min(.x$grad_pt)),
      ele_low     = map_dbl(data, ~ min(.x$ele_s)),
      ele_high    = map_dbl(data, ~ max(.x$ele_s))
    ) |>
    select(-data)
  if (!is.null(segs)) {
    out <- out |>
      mutate(difficulty = map2_dbl(from, to, ~ mean(segs$difficulty[segs$mid >= .x & segs$mid < .y])))
  }
  out
}

munro_stage_line <- function(st) {
  parts <- c(
    sprintf("%.1f km", st$km),
    if (st$gain >= 20) sprintf("%s m up", format(round(st$gain), big.mark = ",")),
    if (st$loss >= 20) sprintf("%s m down", format(round(st$loss), big.mark = ",")),
    munro_hm(st$mins * 60),
    sprintf("avg HR %d", round(st$avg_hr)),
    if (!is.null(st$difficulty) && !is.na(st$difficulty))
      sprintf("difficulty %d/100", round(st$difficulty * 100))
  )
  paste(parts, collapse = " · ")
}

# Zoomed map of one stage, for the top of each stage section: the whole
# track faint, the stage in colour with direction arrows, its breaks and its
# photo numbers. Tiles at zoom 15 unless the stage is long.
munro_stage_map <- function(tr, from, to, photos = NULL, stops = NULL, summits = NULL,
                            features = NULL, zoom = 15, pad = 0.15, margin = 0.55,
                            cachedir = "../_tiles", type = "opentopo",
                            base_size = 12, label_size = 4) {
  rosm::register_tile_source(
    opentopo = "https://a.tile.opentopomap.org/${z}/${x}/${y}.png")
  dir.create(cachedir, showWarnings = FALSE)
  p_tr  <- munro_project(tr)
  stage <- p_tr |> filter(dist >= from, dist <= to)
  xr <- range(stage$x); yr <- range(stage$y)
  # at least 1.2 km across so short stages are not absurdly zoomed
  if (diff(xr) < 1800) xr <- mean(xr) + c(-900, 900)
  if (diff(yr) < 1000) yr <- mean(yr) + c(-500, 500)
  xr <- xr + c(-1, 1) * diff(xr) * pad
  yr <- yr + c(-1, 1) * diff(yr) * pad
  mx <- diff(xr) * margin
  xlim <- c(xr[1] - mx, xr[2] + mx)
  arrows <- munro_arrows(tr |> filter(dist >= from, dist <= to), every = 0.5, from = from + 0.25)

  # only the features that fall inside this stage's window
  feats <- munro_features(tr, summits, NULL, features) |>
    filter(x >= xr[1], x <= xr[2], y >= yr[1], y <= yr[2])

  p <- ggplot() +
    annotation_map_tile(type = type, zoom = zoom, cachedir = cachedir, progress = "none") +
    annotate("rect", xmin = xr[1], xmax = xr[2], ymin = -Inf, ymax = Inf, fill = "white", alpha = 0.2) +
    annotate("rect", xmin = xlim[1], xmax = xr[1], ymin = -Inf, ymax = Inf, fill = munro_cols[["bg"]]) +
    annotate("rect", xmin = xr[2], xmax = xlim[2], ymin = -Inf, ymax = Inf, fill = munro_cols[["bg"]]) +
    annotate("rect", xmin = xr[1], xmax = xr[2], ymin = yr[1], ymax = yr[2],
             fill = NA, colour = munro_cols[["ink"]], linewidth = 0.5) +
    geom_path(data = p_tr, aes(x, y), colour = "white", linewidth = 3, lineend = "round", alpha = 0.7) +
    geom_path(data = p_tr, aes(x, y), colour = munro_cols[["muted"]], linewidth = 1,
              linetype = "22", lineend = "round") +
    geom_path(data = stage, aes(x, y), colour = "white", linewidth = 4.5, lineend = "round") +
    geom_path(data = stage, aes(x, y, colour = grad_class, group = 1), linewidth = 2.4,
              lineend = "round") +
    scale_colour_manual(values = munro_grad_cols, drop = FALSE, guide = "none")
  if (nrow(arrows)) p <- p + munro_arrow_layer(arrows)

  if (!is.null(stops)) {
    st <- munro_project(stops |> filter(dist >= from, dist <= to))
    if (nrow(st)) p <- p +
      geom_point(data = st, aes(x, y, size = mins), shape = 21, fill = munro_cols[["contour"]],
                 colour = munro_cols[["ink"]], stroke = 0.6) +
      scale_size_area(max_size = 8, guide = "none")
  }
  if (!is.null(photos) && "dist" %in% names(photos)) {
    ph <- photos |> filter(!is.na(dist), dist >= from, dist < to)
    if (nrow(ph)) {
      ph <- munro_project(ph)
      p <- p +
        geom_point(data = ph, aes(x, y), shape = 21, size = 2.2, fill = munro_cols[["water"]],
                   colour = "white", stroke = 0.8) +
        geom_label_repel(data = ph, aes(x, y, label = n), colour = "white",
                         fill = munro_cols[["water"]], size = 3.6, family = munro_font_body,
                         fontface = "bold", label.r = unit(0.5, "lines"),
                         label.padding = unit(0.22, "lines"), label.size = 0,
                         segment.colour = munro_cols[["water"]], seed = 7,
                         box.padding = 0.5, min.segment.length = 0, max.overlaps = Inf,
                         xlim = xr, ylim = yr)
    }
  }
  p <- p + munro_label_layers(feats, xr, yr, label_size = label_size, wrap = 14)
  p +
    coord_sf(crs = sf::st_crs(3857), xlim = xlim, ylim = yr, expand = FALSE, default_crs = NULL,
             clip = "off") +
    labs(caption = munro_map_key(stops, photos, "OpenTopoMap (CC BY-SA), © OpenStreetMap contributors")) +
    munro_theme(base_size = base_size) +
    theme(axis.title = element_blank(), axis.text = element_blank(), panel.grid = element_blank(),
          plot.margin = margin(4, 30, 4, 30))
}

# Caption line explaining the markers
munro_map_key <- function(stops = NULL, photos = NULL, credit = NULL) {
  parts <- c(
    if (!is.null(stops)) "Yellow dots: breaks of 3+ minutes, bigger = longer",
    if (!is.null(photos)) "Blue numbers: photos",
    credit)
  paste(parts, collapse = "  ·  ")
}

# Markdown for a photo gallery of the photos taken in a stage (results: asis)
munro_stage_gallery <- function(photos, from, to, ncol = 2, dir = "photos") {
  if (is.null(photos)) return(invisible(NULL))
  ph <- photos |> filter(!is.na(dist), dist >= from, dist < to)
  if (!nrow(ph)) return(invisible(NULL))
  cat(sprintf("\n::: {layout-ncol=%d}\n\n", min(ncol, nrow(ph))))
  for (i in seq_len(nrow(ph))) {
    cat(sprintf("![%d. %s (%s, %.1f km, %d m)](%s/%s){group=\"walk\"}\n\n",
                ph$n[i], ph$caption[i], format(ph$time[i], "%H:%M"), ph$dist[i],
                round(ph$ele[i]), dir, ph$file[i]))
  }
  cat(":::\n\n")
  invisible(ph)
}

# ---- Projection helpers ------------------------------------------------------
# Tiles are Web Mercator; project lon/lat once so ordinary geoms work on the map
munro_project <- function(df) {
  xy <- sf::sf_project("EPSG:4326", "EPSG:3857", as.matrix(df[, c("lon", "lat")]))
  df |> mutate(x = xy[, 1], y = xy[, 2])
}

# Direction arrows: one every `every` km. Each arrow is a short stretch of the
# track itself (about `look_m` metres) with an open head, so it bends with the
# route like a pen stroke rather than a straight ruler line.
munro_arrows <- function(tr, every = 1, look_m = 120, from = 0.5) {
  marks <- seq(from, max(tr$dist), by = every)
  map_dfr(seq_along(marks), function(i) {
    tr |>
      filter(dist_m >= marks[i] * 1000, dist_m <= marks[i] * 1000 + look_m) |>
      distinct(lat, lon, .keep_all = TRUE) |>
      transmute(arrow = i, lon, lat)
  }) |>
    munro_project()
}

munro_arrow_layer <- function(arrows, colour = munro_cols[["ink"]], linewidth = 1.2) {
  geom_path(data = arrows, aes(x, y, group = arrow), colour = colour,
            linewidth = linewidth, lineend = "round", linejoin = "round",
            arrow = arrow(length = unit(8, "pt"), type = "open", angle = 28))
}

# ---- Map ---------------------------------------------------------------------
# Route map on OpenTopoMap tiles. Labels sit in margins either side of the map
# with leader lines to the feature, so the map itself stays readable.
#  features: tibble(label, lat, lon, kind) - kind is one of
#            "summit", "start", "hard", "break", "river", "scramble", "note";
#            it picks the marker. Summits, start and hard bits are added
#            automatically from `summits`, `tr` and `hard`; pass extra rows
#            (river, scramble, lunch...) via `features`.
munro_feature_shapes <- c("summit" = 24, "start" = 23, "hard" = 22, "break" = 21,
                          "river" = 21, "scramble" = 21, "note" = 21)
munro_feature_fills  <- c("summit" = "#C0532B", "start" = "#FFFFFF", "hard" = "#C0532B",
                          "break" = "#D9A441", "river" = "#3E7CB1", "scramble" = "#22302A",
                          "note" = "#22302A")

# Build the feature table: summits, start, hard bits, plus any extra rows
munro_features <- function(tr, summits = NULL, hard = NULL, features = NULL) {
  bind_rows(
    if (!is.null(summits))
      summits |> transmute(label = paste0(name, ", ", height, " m"), lat, lon, kind = "summit"),
    tr |> slice(1) |> transmute(label = "Start and finish", lat, lon, kind = "start"),
    if (!is.null(hard))
      hard |> transmute(label = paste0("Hard bit ", hard), lat, lon, kind = "hard"),
    features
  ) |>
    munro_project()
}

# Margin labels for a map: each feature gets a slot in the left or right
# margin (whichever side of the map centre it sits), a hand-drawn curved
# leader with an arrowhead, and a marker. xr/yr is the map window (3857).
munro_label_layers <- function(feats, xr, yr, label_size = 4.2, wrap = 16,
                               font = munro_font_hand, hand_size = 2) {
  if (is.null(feats) || !nrow(feats)) return(NULL)
  w <- diff(xr); h <- diff(yr)
  feats <- feats |>
    mutate(label = str_wrap(label, wrap),
           side = if_else(x < mean(xr), "left", "right")) |>
    group_by(side) |>
    arrange(desc(y), .by_group = TRUE) |>
    mutate(slot = row_number(), n_side = n(),
           ly = yr[2] - h * 0.06 - (slot - 0.5) * (h * 0.88 / n_side),
           lx = if_else(side == "left", xr[1] - w * 0.03, xr[2] + w * 0.03),
           hjust = if_else(side == "left", 1, 0)) |>
    ungroup()
  left  <- feats |> filter(side == "left")
  right <- feats |> filter(side == "right")
  arr <- arrow(length = unit(7, "pt"), type = "open", angle = 25)
  list(
    if (nrow(left))
      geom_curve(data = left, aes(x = lx, y = ly, xend = x, yend = y),
                 colour = munro_cols[["ink"]], linewidth = 0.7, curvature = -0.25,
                 arrow = arr, lineend = "round"),
    if (nrow(right))
      geom_curve(data = right, aes(x = lx, y = ly, xend = x, yend = y),
                 colour = munro_cols[["ink"]], linewidth = 0.7, curvature = 0.25,
                 arrow = arr, lineend = "round"),
    geom_point(data = feats, aes(x, y, shape = kind, fill = kind), size = 4.2,
               colour = munro_cols[["ink"]], stroke = 0.8),
    scale_shape_manual(values = munro_feature_shapes, guide = "none"),
    scale_fill_manual(values = munro_feature_fills, guide = "none"),
    geom_text(data = feats, aes(lx, ly, label = label, hjust = hjust),
              family = font, size = label_size + hand_size, colour = munro_cols[["ink"]],
              lineheight = 0.8)
  )
}

munro_map <- function(tr, summits, stops = NULL, photos = NULL, hard = NULL,
                      features = NULL, zoom = 14, arrows_every = 1, pad = 0.06,
                      margin = 0.40, type = "opentopo", cachedir = "../_tiles",
                      title = NULL, subtitle = NULL, base_size = 14, label_size = 4.2,
                      tile_alpha = 0.22) {
  rosm::register_tile_source(
    opentopo = "https://a.tile.opentopomap.org/${z}/${x}/${y}.png")
  dir.create(cachedir, showWarnings = FALSE)

  p_tr <- munro_project(tr)
  xr <- range(p_tr$x); yr <- range(p_tr$y)
  xr <- xr + c(-1, 1) * diff(xr) * pad
  yr <- yr + c(-1, 1) * diff(yr) * pad * 2
  w  <- diff(xr); h <- diff(yr)
  mx <- w * margin                       # label margin each side
  xlim <- c(xr[1] - mx, xr[2] + mx)
  arrows <- munro_arrows(tr, every = arrows_every)

  # ---- the feature list, with positions
  feats <- munro_features(tr, summits, hard, features)

  p <- ggplot() +
    annotation_map_tile(type = type, zoom = zoom, cachedir = cachedir, progress = "none") +
    annotate("rect", xmin = xr[1], xmax = xr[2], ymin = yr[1], ymax = yr[2],
             fill = "white", alpha = tile_alpha) +
    # margins: paper over the tiles
    annotate("rect", xmin = xlim[1], xmax = xr[1], ymin = -Inf, ymax = Inf,
             fill = munro_cols[["bg"]]) +
    annotate("rect", xmin = xr[2], xmax = xlim[2], ymin = -Inf, ymax = Inf,
             fill = munro_cols[["bg"]]) +
    annotate("rect", xmin = xr[1], xmax = xr[2], ymin = yr[1], ymax = yr[2],
             fill = NA, colour = munro_cols[["ink"]], linewidth = 0.5) +
    geom_path(data = p_tr, aes(x, y), colour = "white", linewidth = 4, lineend = "round")

  if (!is.null(hard)) {
    for (i in seq_len(nrow(hard))) {
      seg <- p_tr |> filter(dist >= hard$start[i], dist <= hard$end[i])
      p <- p + geom_path(data = seg, aes(x, y), colour = munro_cols[["magenta"]],
                         linewidth = 9, alpha = 0.35, lineend = "round")
    }
  }

  p <- p +
    geom_path(data = p_tr, aes(x, y, colour = grad_class, group = 1),
              linewidth = 2, lineend = "round") +
    scale_colour_manual(values = munro_grad_cols, drop = FALSE) +
    munro_arrow_layer(arrows)

  if (!is.null(stops)) {
    p_st <- munro_project(stops)
    p <- p + geom_point(data = p_st, aes(x, y, size = mins), shape = 21,
                        fill = munro_cols[["contour"]], colour = munro_cols[["ink"]],
                        alpha = 0.9, stroke = 0.6) +
      scale_size_area(max_size = 8, guide = "none")
  }

  if (!is.null(photos) && any(!is.na(photos$lat))) {
    p_ph <- munro_project(photos |> filter(!is.na(lat), !is.na(lon)))
    p <- p +
      geom_point(data = p_ph, aes(x, y), shape = 21, size = 2, fill = munro_cols[["water"]],
                 colour = "white", stroke = 0.8) +
      geom_label_repel(data = p_ph, aes(x, y, label = n), colour = "white",
                       fill = munro_cols[["water"]], size = 3.2, family = munro_font_body,
                       fontface = "bold", label.r = unit(0.5, "lines"),
                       label.padding = unit(0.18, "lines"), label.size = 0,
                       segment.colour = munro_cols[["water"]], seed = 5,
                       min.segment.length = 0, box.padding = 0.35, max.overlaps = Inf,
                       xlim = xr, ylim = yr)
  }

  # feature markers, hand-drawn leaders and margin labels
  p <- p + munro_label_layers(feats, xr, yr, label_size = label_size)

  p +
    coord_sf(crs = sf::st_crs(3857), xlim = xlim, ylim = yr, expand = FALSE,
             default_crs = NULL) +
    labs(title = title, subtitle = subtitle,
         caption = munro_map_key(stops, photos,
                                 "Map tiles: OpenTopoMap (CC BY-SA), data © OpenStreetMap contributors")) +
    munro_theme(base_size = base_size) +
    theme(axis.title = element_blank(), axis.text = element_blank(),
          panel.grid = element_blank(),
          legend.position = "bottom", legend.justification = "left",
          legend.key.width = unit(18, "pt"))
}

# ---- Profile -----------------------------------------------------------------
# Elevation with gradient strip, heart rate, breaks, summits, hard bits, photos.
#  callouts: optional tibble(label, dist, dy) with dy in metres above the ground
munro_profile <- function(tr, segs, stops = NULL, summits = NULL, hard = NULL,
                          callouts = NULL, photos = NULL, show_hr = TRUE, title = NULL,
                          subtitle = NULL, base_size = 14, label_size = 4,
                          hard_labels = TRUE) {
  top <- max(tr$ele_s) * 1.12
  strip_h <- max(tr$ele_s) * 0.05
  hr_scale <- if (show_hr && any(!is.na(tr$hr))) top / max(tr$hr, na.rm = TRUE) else NA

  # one point per 10 m of distance: the raw track has hundreds of points
  # that share a distance (standing still), which draws as vertical lines
  prof <- tr |>
    group_by(bin = floor(dist_m / 10)) |>
    summarise(dist = mean(dist), ele_s = mean(ele_s), hr = mean(hr, na.rm = TRUE),
              .groups = "drop") |>
    mutate(hr = if_else(is.nan(hr), NA_real_, hr))

  p <- ggplot() +
    geom_area(data = prof, aes(dist, ele_s), fill = munro_cols[["wood"]], alpha = 0.5) +
    geom_line(data = prof, aes(dist, ele_s), colour = munro_cols[["wood"]], linewidth = 0.7) +
    geom_rect(data = segs, aes(xmin = start, xmax = end, ymin = -strip_h * 1.4,
                               ymax = -strip_h * 0.4, fill = grad_class)) +
    scale_fill_manual(values = munro_grad_cols, drop = FALSE)

  # hard bits: a thick rust stroke along the ground, labelled by hand below
  if (!is.null(hard)) {
    for (i in seq_len(nrow(hard))) {
      seg <- prof |> filter(dist >= hard$start[i], dist <= hard$end[i])
      p <- p + geom_line(data = seg, aes(dist, ele_s), colour = munro_cols[["magenta"]],
                         linewidth = 3, lineend = "round")
    }
  }

  # heart rate: smoothed over a minute and kept light so labels stay readable
  if (!is.na(hr_scale)) {
    hr_d <- prof |> filter(!is.na(hr)) |>
      mutate(hr_s = as.numeric(stats::runmed(hr, 15, endrule = "median")))
    p <- p + geom_line(data = hr_d, aes(dist, hr_s * hr_scale), colour = munro_cols[["hr"]],
                       linewidth = 0.6, alpha = 0.45)
  }

  if (!is.null(stops)) {
    p <- p + geom_point(data = stops, aes(dist, ele, size = mins), shape = 21,
                        fill = munro_cols[["contour"]], colour = munro_cols[["ink"]],
                        alpha = 0.95, stroke = 0.6) +
      scale_size_area(max_size = 8, guide = "none")
  }

  # summits become hand-drawn callouts like everything else
  if (!is.null(summits)) {
    p <- p +
      geom_point(data = summits, aes(dist, ele_rec), shape = 24, size = 4,
                 fill = munro_cols[["magenta"]], colour = munro_cols[["ink"]])
    su_call <- summits |>
      transmute(label = paste0(name, "\n", format(time, "%H:%M")), dist,
                dy = max(tr$ele_s) * 0.10,
                dx = if_else(row_number() %% 2 == 1, -0.55, 0.55))
    callouts <- bind_rows(callouts, su_call)
  }

  if (!is.null(photos) && "dist" %in% names(photos) && any(!is.na(photos$dist))) {
    p_ph <- photos |> filter(!is.na(dist))
    p <- p +
      geom_label_repel(data = p_ph, aes(dist, ele, label = n), colour = "white",
                       fill = munro_cols[["water"]], size = 3.2, family = munro_font_body,
                       fontface = "bold", label.r = unit(0.5, "lines"),
                       label.padding = unit(0.18, "lines"), label.size = 0,
                       segment.colour = munro_cols[["water"]], seed = 6,
                       direction = "x", nudge_y = -max(tr$ele_s) * 0.16,
                       min.segment.length = 0, max.overlaps = Inf)
  }

  if (!is.null(hard) && hard_labels) {
    hard_call <- tibble(label = "The hard bits", dist = mean((hard$start + hard$end) / 2),
                        dy = -max(tr$ele_s) * 0.35, dx = -0.8)
    callouts <- bind_rows(callouts, hard_call)
  }

  # Hand-drawn callouts: tibble(label, dist, dy, dx). The text sits dx km
  # along and dy m above the ground at `dist`; the curved arrow points back.
  # Text gets a paper halo so it reads over the heart-rate line.
  if (!is.null(callouts) && nrow(callouts)) {
    if (!"dx" %in% names(callouts)) callouts$dx <- 0.4
    callouts <- callouts |> mutate(dx = coalesce(dx, 0.4))
    ele_at <- approx(tr$dist, tr$ele_s, xout = callouts$dist, rule = 2, ties = mean)$y
    co <- callouts |> mutate(cx = dist + dx, cy = ele_at + dy, ey = ele_at + 8,
                             hjust = if_else(dx >= 0, 0, 1))
    arr <- arrow(length = unit(7, "pt"), type = "open", angle = 25)
    for (curv in c(-0.25, 0.25)) {
      d <- if (curv < 0) co |> filter(dx >= 0) else co |> filter(dx < 0)
      if (nrow(d)) p <- p +
        geom_curve(data = d, aes(x = cx, y = cy, xend = dist, yend = ey),
                   colour = munro_cols[["ink"]], linewidth = 0.7, curvature = curv,
                   arrow = arr, lineend = "round")
    }
    for (hj in c(0, 1)) {
      d <- co |> filter(hjust == hj)
      if (nrow(d)) p <- p +
        geom_label(data = d, aes(cx, cy, label = label), family = munro_font_hand,
                   size = label_size + 2, colour = munro_cols[["ink"]], hjust = hj, vjust = -0.1,
                   lineheight = 0.8, fill = alpha(munro_cols[["bg"]], 0.75), label.size = 0,
                   label.padding = unit(0.12, "lines"))
    }
  }

  y_scale <- if (!is.na(hr_scale)) {
    scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.02)),
                       sec.axis = sec_axis(~ . / hr_scale, name = "Heart rate (bpm)",
                                           breaks = seq(60, 200, 20)))
  } else {
    scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.02)))
  }

  p +
    y_scale +
    scale_x_continuous(breaks = seq(0, ceiling(max(tr$dist)), 1),
                       expand = expansion(mult = c(0.01, 0.01))) +
    coord_cartesian(ylim = c(-strip_h * 1.5, top), clip = "off") +
    labs(x = "Distance (km)", y = "Elevation (m)", title = title, subtitle = subtitle) +
    munro_theme(base_size = base_size) +
    theme(legend.position = "bottom", legend.justification = "left",
          axis.title.y.right = element_text(colour = munro_cols[["hr"]]),
          axis.text.y.right = element_text(colour = munro_cols[["hr"]]),
          panel.grid.major.x = element_blank())
}

# ---- Km-by-km strip ----------------------------------------------------------
# One column per kilometre, rows for the things that make it hard.
#  fixed = FALSE lets the tiles stretch to fill the space (for the poster)
munro_strip <- function(tr, title = NULL, subtitle = NULL, base_size = 14, fixed = TRUE,
                        caption = TRUE, text_size = 4) {
  kms <- munro_segments(tr, width = 1000) |>
    mutate(km = seg + 1,
           `Climb (m)`     = round(gain),
           `Descent (m)`   = round(loss),
           `Minutes`       = round(mins),
           `Heart rate`    = round(hr),
           `Difficulty`    = round(difficulty * 100)) |>
    select(km, `Climb (m)`, `Descent (m)`, `Minutes`, `Heart rate`, `Difficulty`) |>
    pivot_longer(-km, names_to = "metric", values_to = "value") |>
    group_by(metric) |>
    mutate(pct = if (n_distinct(value) > 1) rescale(value) else 0.5) |>
    ungroup() |>
    mutate(metric = factor(metric, levels = rev(c("Climb (m)", "Descent (m)", "Minutes",
                                                  "Heart rate", "Difficulty"))))

  p <- ggplot(kms, aes(km, metric, fill = pct)) +
    geom_tile(colour = munro_cols[["bg"]], linewidth = 1.5) +
    geom_text(aes(label = value, colour = pct > 0.6), family = munro_font_body,
              fontface = "bold", size = text_size) +
    scale_fill_gradient(low = "#F1E7D2", high = munro_cols[["heat"]], guide = "none") +
    scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = munro_cols[["ink"]]), guide = "none") +
    scale_x_continuous(breaks = unique(kms$km), expand = expansion(0.01), position = "top") +
    labs(x = "Kilometre", y = NULL, title = title, subtitle = subtitle,
         caption = if (caption) "Minutes include breaks. Difficulty averages the walk-percentiles of steepness and heart rate." else NULL) +
    munro_theme(base_size = base_size) +
    theme(panel.grid = element_blank(), axis.text.y = element_text(face = "bold", hjust = 1))
  if (fixed) p <- p + coord_fixed(ratio = 1)
  p
}

# ---- Stat cards --------------------------------------------------------------
munro_stat_cards <- function(stats, ncol = 4, base_size = 14) {
  cards <- tribble(
    ~label, ~value,
    "Distance",       sprintf("%.1f km", stats$distance_km),
    "Ascent",         sprintf("%s m", format(round(stats$ascent_m), big.mark = ",")),
    "Total time",     stats$total,
    "Moving time",    stats$moving,
    "High point",     sprintf("%s m", format(round(stats$high_m), big.mark = ",")),
    "Moving pace",    sprintf("%d min/km", round(stats$pace_moving)),
    "Avg / max HR",   sprintf("%d / %d", round(stats$avg_hr), round(stats$max_hr)),
    "Breaks (3+ min)", sprintf("%d", stats$n_stops)
  ) |>
    mutate(i = row_number() - 1, col = i %% ncol, row = -(i %/% ncol))

  ggplot(cards) +
    geom_tile(aes(col, row), width = 0.94, height = 0.9, fill = munro_cols[["card"]],
              colour = "#E4E0D6") +
    geom_text(aes(col, row + 0.06, label = value), family = munro_font_big,
              size = base_size * 0.9, colour = munro_cols[["wood"]]) +
    geom_text(aes(col, row - 0.26, label = label), family = munro_font_body,
              size = base_size * 0.3, colour = munro_cols[["muted"]]) +
    coord_fixed(ratio = 0.55, expand = FALSE, clip = "off") +
    scale_x_continuous(limits = c(-0.5, ncol - 0.5)) +
    scale_y_continuous(limits = c(min(cards$row) - 0.5, 0.5)) +
    theme_void() +
    theme(plot.background = element_rect(fill = munro_cols[["bg"]], colour = NA),
          plot.margin = margin(4, 4, 4, 4))
}

# ---- Poster ------------------------------------------------------------------
# A 1080 x 1350 canvas in pixel units, y upwards, saved at 2x.

poster_w <- 1080
poster_h <- 1350

poster_canvas <- function(bg) {
  ggplot() +
    coord_fixed(xlim = c(0, poster_w), ylim = c(0, poster_h), expand = FALSE, clip = "off") +
    theme_void() +
    theme(plot.background = element_rect(fill = bg, colour = NA),
          panel.background = element_rect(fill = bg, colour = NA),
          plot.margin = margin(0, 0, 0, 0))
}

poster_inset <- function(chart, x0, y0, x1, y1) {
  annotation_custom(ggplotGrob(chart), xmin = x0, xmax = x1, ymin = y0, ymax = y1)
}

poster_save <- function(plot, file, bg) {
  ggsave(file, plot, device = ragg::agg_png, width = 2 * poster_w, height = 2 * poster_h,
         units = "px", dpi = 200, bg = bg)
  invisible(file)
}

shape_rrect <- function(x0, y0, x1, y1, r = 16, n = 8) {
  a <- seq(0, pi / 2, length.out = n)
  corner <- function(cx, cy, start) {
    t <- start + a
    data.frame(x = cx + r * cos(t), y = cy + r * sin(t))
  }
  rbind(corner(x1 - r, y1 - r, 0), corner(x0 + r, y1 - r, pi / 2),
        corner(x0 + r, y0 + r, pi), corner(x1 - r, y0 + r, 3 * pi / 2))
}

draw_card <- function(x0, y0, x1, y1, fill, border = NA, r = 18, lwd = 1) {
  d <- shape_rrect(x0, y0, x1, y1, r)
  list(annotate("polygon", x = d$x, y = d$y, fill = fill, colour = border, linewidth = lwd))
}

# A stat card: big number over a small label
draw_stat <- function(x0, y0, x1, y1, value, label, fill = munro_cols[["card"]],
                      border = NA, num_col = munro_cols[["wood"]],
                      lab_col = munro_cols[["muted"]], num_size = 16, lab_size = 5) {
  cx <- (x0 + x1) / 2
  c(draw_card(x0, y0, x1, y1, fill, border),
    list(annotate("text", x = cx, y = y0 + (y1 - y0) * 0.58, label = value,
                  family = munro_font_big, size = num_size, colour = num_col),
         annotate("text", x = cx, y = y0 + (y1 - y0) * 0.22, label = label,
                  family = munro_font_body, size = lab_size, colour = lab_col)))
}

# The whole poster for a walk: title band, stat cards, map, profile, km strip.
# Charts are passed in already built so the post can reuse them.
munro_poster <- function(title, subtitle, stats, map, profile, strip, file,
                         footer = "emilynordmann.com", bg = munro_cols[["bg"]],
                         title_size = 19) {
  green <- munro_cols[["wood"]]
  card_y0 <- 1092; card_y1 <- 1200
  cards <- list(
    c(sprintf("%.1f KM", stats$distance_km), "distance"),
    c(sprintf("%s M", format(round(stats$ascent_m), big.mark = ",")), "ascent"),
    c(toupper(stats$total), "door to door"),
    c(sprintf("%s M", format(round(stats$high_m), big.mark = ",")), "high point"))
  xs <- seq(40, by = 256, length.out = 4)

  p <- poster_canvas(bg) +
    annotate("rect", xmin = 0, xmax = poster_w, ymin = 1222, ymax = poster_h, fill = green) +
    annotate("rect", xmin = 0, xmax = poster_w, ymin = 1214, ymax = 1222,
             fill = munro_cols[["contour"]]) +
    annotate("text", x = 40, y = 1308, label = title, family = munro_font_big,
             size = title_size, colour = "white", hjust = 0, vjust = 0.5) +
    annotate("text", x = 40, y = 1250, label = subtitle, family = munro_font_body, size = 6,
             colour = "#DCE7E0", hjust = 0, vjust = 0.5)

  for (i in seq_along(cards)) {
    p <- p + draw_stat(xs[i], card_y0, xs[i] + 236, card_y1, cards[[i]][1], cards[[i]][2])
  }

  p <- p +
    poster_inset(map,     20, 610, 1060, 1080) +
    poster_inset(profile, 20, 330, 1060, 600) +
    poster_inset(strip,   20,  40, 1060, 320) +
    annotate("text", x = poster_w - 40, y = 20, label = footer, family = munro_font_body,
             size = 4, colour = munro_cols[["muted"]], hjust = 1)

  poster_save(p, file, bg)
  p
}
