# Shared toolkit for the Aplotalypse social-media posters.
# Sourced by each game post; not rendered by Quarto (underscore prefix).
#
# Each poster is one ggplot canvas in pixel units (1080 wide x 1350 high,
# y upwards) saved at 2x. Charts are inset with poster_inset(); everything
# else (bands, cards, buttons, meeples, arrows) is drawn straight onto the
# canvas so each game can have its own look.

library(ggplot2)
library(ggtext)

info_players <- c(Emily = "#3B7A57", Kathleen = "#B8860B")
# lighter variants for dark backgrounds
info_players_light <- c(Emily = "#7CC48F", Kathleen = "#E5BC4E")

# ---- Fonts -------------------------------------------------------------------
# Display and handwriting faces live in post/_fonts (Google Fonts, OFL).
# Registered by family name so ggplot can use them through ragg.
info_fonts_dir <- normalizePath(file.path("..", "_fonts"), mustWork = FALSE)
info_register_fonts <- function() {
  reg <- function(name, file) {
    f <- file.path(info_fonts_dir, file)
    if (file.exists(f) && !(name %in% systemfonts::registry_fonts()$family))
      systemfonts::register_font(name, f)
  }
  reg("Caveat", "Caveat-Bold.ttf")
  reg("Fredoka", "Fredoka-Bold.ttf")
  reg("Cinzel", "Cinzel-Bold.ttf")
  reg("Cinzel Black", "Cinzel-Black.ttf")
  reg("MedievalSharp", "MedievalSharp.ttf")
  reg("Bebas Neue", "BebasNeue.ttf")
  invisible(TRUE)
}
info_register_fonts()

# ---- Canvas ------------------------------------------------------------------

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

# Drop a finished ggplot chart onto the canvas at the given pixel box
poster_inset <- function(chart, x0, y0, x1, y1) {
  annotation_custom(ggplotGrob(chart), xmin = x0, xmax = x1, ymin = y0, ymax = y1)
}

poster_save <- function(plot, file, bg) {
  ggsave(file, plot, device = ragg::agg_png, width = 2 * poster_w, height = 2 * poster_h,
         units = "px", dpi = 200, bg = bg)
  invisible(file)
}

# ---- Shapes ------------------------------------------------------------------

# Points around a rounded rectangle, for polygons and stitched outlines
shape_rrect <- function(x0, y0, x1, y1, r = 16, n = 8) {
  a <- seq(0, pi / 2, length.out = n)
  corner <- function(cx, cy, start) {
    t <- start + a
    data.frame(x = cx + r * cos(t), y = cy + r * sin(t))
  }
  rbind(corner(x1 - r, y1 - r, 0), corner(x0 + r, y1 - r, pi / 2),
        corner(x0 + r, y0 + r, pi), corner(x1 - r, y0 + r, 3 * pi / 2))
}

shape_circle <- function(cx, cy, r, n = 72) {
  t <- seq(0, 2 * pi, length.out = n)
  data.frame(x = cx + r * cos(t), y = cy + r * sin(t))
}

# A meeple, centred on (cx, cy) with the given height
shape_meeple <- function(cx, cy, h) {
  m <- data.frame(
    x = c(0.50, 0.59, 0.65, 0.66, 0.62, 0.59, 0.90, 0.97, 0.94, 0.70, 0.75, 0.59,
          0.50, 0.41, 0.25, 0.30, 0.06, 0.03, 0.10, 0.41, 0.38, 0.34, 0.35, 0.41),
    y = c(1.00, 0.98, 0.91, 0.82, 0.74, 0.71, 0.62, 0.52, 0.44, 0.49, 0.05, 0.05,
          0.30, 0.05, 0.05, 0.49, 0.44, 0.52, 0.62, 0.71, 0.74, 0.82, 0.91, 0.98))
  data.frame(x = cx + (m$x - 0.5) * h * 0.95, y = cy + (m$y - 0.5) * h)
}

# A button: disc, darker rim, four thread holes
draw_button <- function(cx, cy, r, fill, rim) {
  holes <- expand.grid(dx = c(-0.28, 0.28), dy = c(-0.28, 0.28))
  c(list(
    annotate("polygon", x = shape_circle(cx, cy, r)$x, y = shape_circle(cx, cy, r)$y,
             fill = rim, colour = NA),
    annotate("polygon", x = shape_circle(cx, cy, r * 0.86)$x, y = shape_circle(cx, cy, r * 0.86)$y,
             fill = fill, colour = NA)),
    lapply(seq_len(nrow(holes)), function(i)
      annotate("polygon", x = shape_circle(cx + holes$dx[i] * r, cy + holes$dy[i] * r, r * 0.09)$x,
               y = shape_circle(cx + holes$dx[i] * r, cy + holes$dy[i] * r, r * 0.09)$y,
               fill = rim, colour = NA)))
}

# A card panel with an optional stitched (dashed) outline
draw_card <- function(x0, y0, x1, y1, fill, border = NA, stitch = NA, r = 18, lwd = 1) {
  d <- shape_rrect(x0, y0, x1, y1, r)
  out <- list(annotate("polygon", x = d$x, y = d$y, fill = fill, colour = border, linewidth = lwd))
  if (!is.na(stitch)) {
    d2 <- shape_rrect(x0 + 10, y0 + 10, x1 - 10, y1 - 10, max(r - 8, 4))
    out <- c(out, list(annotate("path", x = c(d2$x, d2$x[1]), y = c(d2$y, d2$y[1]),
                                colour = stitch, linewidth = 1.1, linetype = "22")))
  }
  out
}

# Hand-lettered callout: a curved arrow from the label to the point of interest.
# Works in whatever coordinate system the plot uses (canvas or chart data).
draw_callout <- function(label, x, y, xend, yend, colour = "#2B2B2B", curvature = -0.3,
                         size = 6, hjust = 0.5, vjust = 0.5, family = "Caveat",
                         lineheight = 0.85, arrow_len = 6, linewidth = 0.9, label_gap = c(0, 0)) {
  list(
    annotate("curve", x = x, y = y, xend = xend, yend = yend, colour = colour,
             linewidth = linewidth, curvature = curvature,
             arrow = arrow(length = unit(arrow_len, "pt"), type = "closed")),
    annotate("text", x = x + label_gap[1], y = y + label_gap[2], label = label, colour = colour,
             family = family, size = size, hjust = hjust, vjust = vjust, lineheight = lineheight)
  )
}

# A vertical gradient background as a raster (top colour to bottom colour)
draw_gradient <- function(top, bottom, x0 = 0, y0 = 0, x1 = poster_w, y1 = poster_h, n = 200) {
  cols <- grDevices::colorRampPalette(c(top, bottom))(n)
  annotation_raster(matrix(cols, ncol = 1), xmin = x0, xmax = x1, ymin = y0, ymax = y1, interpolate = TRUE)
}

# Chart theme for insets: transparent, bigger type, minimal chrome
theme_poster_chart <- function(body = "Lato", title_font = "Montserrat", ink = "#2B2B2B",
                               muted = "#6B7370", grid = "#00000018", base_size = 17) {
  theme_minimal(base_size = base_size, base_family = body) +
    theme(
      text = element_text(colour = ink),
      plot.title = element_text(family = title_font, face = "bold", size = rel(1.35),
                                colour = ink, margin = margin(b = 10)),
      plot.title.position = "plot",
      plot.subtitle = element_text(colour = muted, size = rel(0.85), margin = margin(b = 12)),
      axis.title = element_text(colour = muted, size = rel(0.75)),
      axis.title.y = element_blank(),
      axis.text = element_text(colour = muted, size = rel(0.8)),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = grid, linewidth = 0.5),
      legend.position = "none",
      plot.background = element_rect(fill = NA, colour = NA),
      panel.background = element_rect(fill = NA, colour = NA),
      plot.margin = margin(8, 12, 4, 12)
    )
}
