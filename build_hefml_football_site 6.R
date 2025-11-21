# ---------------------------------------------------------------
# build_hefml_football_site.R
# Template-based HefML site + Power Grid with logo tooltips
# ---------------------------------------------------------------
# Regenerates:
#   * Power Grid (with team logos + hover tooltips)
#   * By Week tables
#   * By Team tables (with projected wins)
#
# Uses hefml_1107.html as a visual template so CSS, chips,
# and the header football graphic stay intact.
#
# Usage (in R):
#
#   predictions_2025 <- your_dataframe  # must have columns below
#
#   # optional: team logos from CFBD /teams endpoint
#   # team_logos should have columns: school, logo
#   # team_logos <- get_cfbd_team_logos(year = 2025)
#
#   source("build_hefml_football_site.R")
#   build_hefml_football_site(
#     predictions_2025,
#     template_file = "hefml_1107.html",
#     output_file   = "hefml_football.html",
#     team_logos    = team_logos  # or NULL
#   )
#
# Required columns in predictions_2025:
#   week             : numeric/integer week number
#   home_team        : home team name
#   away_team        : away team name
#   win_prob         : HOME win probability (0–1)
#   spread_pred      : model spread (home minus away, in points)
#   predicted_winner : team name that the model picks
#
# Optional column (for Date):
#   start_date       : a date/time string for the game.
#                      If present, it will be formatted as
#                      "Aug 23, 2025" style. If not present,
#                      the Date column will just show "Week X".
# ---------------------------------------------------------------

build_hefml_football_site <- function(predictions_2025,
                                      template_file = "hefml_1107.html",
                                      output_file   = "hefml_football.html",
                                      team_logos    = NULL) {

  # ---- required columns -----------------------------------------
  needed <- c("week", "home_team", "away_team",
              "win_prob", "spread_pred", "predicted_winner")
  missing <- setdiff(needed, names(predictions_2025))
  if (length(missing) > 0) {
    stop("predictions_2025 is missing columns: ",
         paste(missing, collapse = ", "))
  }

  # ---- read template & grab header / CSS / header SVG -----------
  if (!file.exists(template_file)) {
    stop("Template file not found: ", template_file,
         ". Put hefml_1107.html in the working directory or specify template_file explicitly.")
  }

  template_html <- paste(readLines(template_file, warn = FALSE, encoding = "UTF-8"),
                         collapse = "\n")

  marker <- '<div id="views"><h2>By Week</h2>'
  idx <- regexpr(marker, template_html, fixed = TRUE)[1]
  if (idx == -1) {
    stop("Could not find 'By Week' marker in template HTML: ", template_file)
  }

  # Everything before the first "By Week" views — this includes:
  # - doctype/html/head
  # - global CSS
  # - header SVG football graphic
  # - top card wrapper and inner <style> for the tables
  html_head <- substring(template_html, 1, idx - 1)

  # We'll generate our own <div id=\"views\"> ... </div> and then
  # close out the open tags from the top card / container / main.
  html_tail <- "</div></section></div></main></body></html>"

  # ---- local copy of data and minor cleanup ---------------------
  df <- predictions_2025

  junk <- intersect(c("Unnamed: 0", "X"), names(df))
  if (length(junk)) df[junk] <- NULL

  df$week <- as.integer(df$week)

  # ---- helpers --------------------------------------------------
  html_escape <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;",  x, fixed = TRUE)
    x <- gsub(">", "&gt;",  x, fixed = TRUE)
    x <- gsub("\"", "&quot;", x, fixed = TRUE)
    x
  }

  fmt_pct <- function(p) {
    ifelse(is.na(p), "–", sprintf("%.1f%%", 100 * p))
  }

  fmt_spread <- function(x) {
    ifelse(is.na(x), "–", sprintf("%+.1f", x))
  }

  # date label: use start_date if available, otherwise "Week X"
  if ("start_date" %in% names(df)) {
    suppressWarnings({
      dt <- as.POSIXct(df$start_date, tz = "UTC")
      if (all(is.na(dt))) {
        dt <- as.POSIXct(df$start_date)
      }
    })
    df$date_label <- ifelse(
      is.na(dt),
      paste0("Week ", df$week),
      format(dt, "%b %d, %Y")
    )
  } else {
    df$date_label <- paste0("Week ", df$week)
  }

  # Small chip style helper using HSL for a soft heatmap,
  # matching the inline style pattern in the original HTML.
  chip_style <- function(is_green = TRUE, intensity = 0.5) {
    hue <- if (is_green) 140 else 0  # green vs red
    intensity <- max(0, min(1, intensity))
    light <- 80 - 25 * intensity
    col_text <- if (is_green) "#000" else "#fff"
    paste0(
      "background-color:hsl(", hue, ",60%,", sprintf("%.1f", light), "%);",
      "color:", col_text, ";",
      "border-radius:10px;",
      "padding:3px 10px;",
      "display:inline-block;",
      "min-width:62px;",
      "text-align:center;",
      "font-weight:600"
    )
  }

  # Spread chip: green if spread >= 0 (home favored), red otherwise
  spread_chip <- function(spread) {
    if (is.na(spread)) {
      return('<span class="chip">–</span>')
    }
    is_green <- spread >= 0
    mag <- min(21, abs(spread))
    intensity <- mag / 21
    style <- chip_style(is_green = is_green, intensity = intensity)
    cls <- if (is_green) "oval green chip" else "oval red chip"
    paste0(
      '<span class="', cls, '" style="', style, '">',
      fmt_spread(spread),
      "</span>"
    )
  }

  # Win prob chip: green if prob >= 0.5, red otherwise
  prob_chip <- function(p) {
    if (is.na(p)) {
      return('<span class="chip">–</span>')
    }
    is_green <- p >= 0.5
    edge <- abs(p - 0.5) * 2  # 0 to 1
    style <- chip_style(is_green = is_green, intensity = edge)
    cls <- if (is_green) "oval green chip" else "oval red chip"
    paste0(
      '<span class="', cls, '" style="', style, '">',
      fmt_pct(p),
      "</span>"
    )
  }

  # ---- ordering & derived structures ----------------------------
  df <- df[order(df$week, df$home_team, df$away_team), ]

  weeks <- sort(unique(df$week))
  all_teams <- sort(unique(c(df$home_team, df$away_team)))

  # projected wins by team (all games, home or away)
  proj_wins <- vapply(all_teams, function(tm) {
    rows <- df[df$home_team == tm | df$away_team == tm, , drop = FALSE]
    if (!nrow(rows)) return(NA_real_)
    probs <- ifelse(rows$home_team == tm, rows$win_prob, 1 - rows$win_prob)
    sum(probs, na.rm = TRUE)
  }, numeric(1))
  names(proj_wins) <- all_teams

  # ---- POWER GRID data ------------------------------------------
  # For each team, look only at games where they are the model's
  # predicted_winner. X = sum of spread (home spread, away spread
  # as negative). Y = sum of that team's win probabilities in those
  # games.
  pg_df <- data.frame(
    team          = all_teams,
    total_spread  = NA_real_,
    total_winprob = NA_real_,
    stringsAsFactors = FALSE
  )

  for (i in seq_along(all_teams)) {
    tm <- all_teams[i]
    rows <- df[df$predicted_winner == tm, , drop = FALSE]
    if (!nrow(rows)) {
      pg_df$total_spread[i]  <- 0
      pg_df$total_winprob[i] <- 0
    } else {
      is_home <- rows$home_team == tm
      prob_tm <- ifelse(is_home, rows$win_prob, 1 - rows$win_prob)
      spread_tm <- ifelse(is_home, rows$spread_pred, -rows$spread_pred)
      pg_df$total_spread[i]  <- sum(spread_tm, na.rm = TRUE)
      pg_df$total_winprob[i] <- sum(prob_tm,   na.rm = TRUE)
    }
  }

  # join logos if provided (team_logos: school, logo)
  pg_df$logo <- NA_character_
  if (!is.null(team_logos) &&
      all(c("school", "logo") %in% names(team_logos))) {

    logo_map <- stats::setNames(team_logos$logo, team_logos$school)
    pg_df$logo <- unname(logo_map[pg_df$team])
  }

  # ---- builders for table rows ----------------------------------
  # WEEK table rows
  build_week_row <- function(row) {
    date  <- html_escape(as.character(row[["date_label"]]))
    home  <- html_escape(as.character(row[["home_team"]]))
    away  <- html_escape(as.character(row[["away_team"]]))
    wp    <- as.numeric(row[["win_prob"]])
    sp    <- as.numeric(row[["spread_pred"]])
    pick  <- html_escape(as.character(row[["predicted_winner"]]))

    spread_html <- spread_chip(sp)
    prob_html   <- prob_chip(wp)

    paste0(
      "<tr>",
      '<td class="td-text date-col">', date, "</td>",
      '<td class="td-text">', home, "</td>",
      '<td class="td-text">', away, "</td>",
      '<td class="td-num">', spread_html, "</td>",
      '<td class="td-num">', prob_html, "</td>",
      '<td class="td-text pick-col">', pick, "</td>",
      "</tr>\n"
    )
  }

  # TEAM table rows
  build_team_row <- function(row, team_name) {
    wk    <- as.integer(row[["week"]])
    date  <- html_escape(as.character(row[["date_label"]]))
    home  <- as.character(row[["home_team"]])
    away  <- as.character(row[["away_team"]])
    wp    <- as.numeric(row[["win_prob"]])
    sp    <- as.numeric(row[["spread_pred"]])
    pick  <- html_escape(as.character(row[["predicted_winner"]]))

    is_home <- identical(team_name, home)
    opp     <- if (is_home) away else home
    opp     <- html_escape(opp)
    venue   <- if (is_home) "Home" else "Away"

    # team-specific win probability
    wp_team <- if (is_home) wp else (1 - wp)

    spread_html <- spread_chip(sp)
    prob_html   <- prob_chip(wp_team)

    paste0(
      "<tr>",
      '<td class="td-num">', wk, "</td>",
      '<td class="td-text date-col">', date, "</td>",
      '<td class="td-text">', opp, "</td>",
      '<td class="td-text">', venue, "</td>",
      '<td class="td-num">', spread_html, "</td>",
      '<td class="td-num">', prob_html, "</td>",
      '<td class="td-text pick-col">', pick, "</td>",
      "</tr>\n"
    )
  }

  # ---- build "By Week" section ----------------------------------
  week_blocks <- character(length(weeks))

  for (i in seq_along(weeks)) {
    wk <- weeks[i]
    sub <- df[df$week == wk, , drop = FALSE]
    if (!nrow(sub)) next

    rows_html <- paste(apply(sub, 1, build_week_row), collapse = "")

    block <- paste0(
      "<details><summary>Week ", wk, "</summary>",
      '<table class="hefml hefml-week"><thead><tr>',
      "<th>Date</th>",
      "<th>Home</th>",
      "<th>Away</th>",
      "<th>Spread</th>",
      "<th>Win %</th>",
      "<th>Pick</th>",
      "</tr></thead><tbody>\n",
      rows_html,
      "</tbody></table></details>\n"
    )
    week_blocks[i] <- block
  }

  week_section <- paste(week_blocks, collapse = "")

  # ---- build "By Team" section ----------------------------------
  team_blocks <- character(length(all_teams))

  for (i in seq_along(all_teams)) {
    tm <- all_teams[i]
    sub <- df[df$home_team == tm | df$away_team == tm, , drop = FALSE]
    if (!nrow(sub)) next

    rows_html <- paste(
      apply(sub, 1, build_team_row, team_name = tm),
      collapse = ""
    )

    proj <- proj_wins[[tm]]
    proj_txt <- if (is.na(proj)) "Projected wins: –" else sprintf("Projected wins: %.1f", proj)

    block <- paste0(
      "<details><summary>", html_escape(tm), "</summary>",
      "<small>", proj_txt, "</small>",
      '<table class="hefml hefml-team"><thead><tr>',
      "<th>Week</th>",
      "<th>Date</th>",
      "<th>Opponent</th>",
      "<th>Venue</th>",
      "<th>Spread</th>",
      "<th>Win %</th>",
      "<th>Pick</th>",
      "</tr></thead><tbody>\n",
      rows_html,
      "</tbody></table></details>\n"
    )
    team_blocks[i] <- block
  }

  team_section <- paste(team_blocks, collapse = "")

  # ---- build POWER GRID SVG -------------------------------------
  # We'll create an SVG file via base R graphics, then inline it.
  tmp_svg <- tempfile(fileext = ".svg")
  grDevices::svg(tmp_svg, width = 6.8, height = 4.4, bg = "transparent")

  # preserve par settings
  op <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(op), add = TRUE)

  graphics::par(mar = c(4.2, 4.2, 2.8, 1.0))

  x <- pg_df$total_spread
  y <- pg_df$total_winprob

  # reasonable axis ranges with a bit of padding
  x_pad <- diff(range(x, na.rm = TRUE)) * 0.08
  y_pad <- diff(range(y, na.rm = TRUE)) * 0.08
  if (!is.finite(x_pad) || x_pad <= 0) x_pad <- 1
  if (!is.finite(y_pad) || y_pad <= 0) y_pad <- 1

  # mild jitter to reduce overlap (visual only)
  set.seed(2025)
  xj <- jitter(x, amount = x_pad * 0.10)
  yj <- jitter(y, amount = y_pad * 0.10)

  # base plot with white axes / labels / border, transparent bg
  graphics::plot(
    xj, yj,
    type = "n",
    xlab = "Sum of model spread when favored (points)",
    ylab = "Sum of win probability when favored",
    main = "HefML Power Grid (Favored Games Only)",
    xlim = range(xj, na.rm = TRUE) + c(-x_pad, x_pad),
    ylim = range(yj, na.rm = TRUE) + c(-y_pad, y_pad),
    col.axis = "white",
    col.lab  = "white",
    col.main = "white",
    col      = "white"
  )

  graphics::axis(1, col = "white", col.axis = "white")
  graphics::axis(2, col = "white", col.axis = "white")
  graphics::box(col = "white")

  graphics::abline(
    h = mean(yj, na.rm = TRUE),
    v = mean(xj, na.rm = TRUE),
    col = "white",
    lty = 3,
    lwd = 0.9
  )

  # decide whether to use logos
  use_logos <- !all(is.na(pg_df$logo)) && requireNamespace("png", quietly = TRUE)

  if (use_logos) {
    usr  <- graphics::par("usr")
    pin  <- graphics::par("pin")
    xrng <- usr[2] - usr[1]
    yrng <- usr[4] - usr[3]

    # data-units -> inches scaling
    x_in_per_data <- pin[1] / xrng
    y_in_per_data <- pin[2] / yrng

    # small logos: about 0.75% of x-range (data units)
    w_data <- xrng * 0.05

    for (i in seq_along(xj)) {
      logo_url <- pg_df$logo[i]
      if (is.na(logo_url) || !nzchar(logo_url)) next

      tmpf <- tempfile(fileext = ".png")
      ok <- try(utils::download.file(logo_url, tmpf, mode = "wb", quiet = TRUE), silent = TRUE)
      if (inherits(ok, "try-error")) next

      img <- try(png::readPNG(tmpf), silent = TRUE)
      if (inherits(img, "try-error")) next

      # aspect ratio in pixels: height / width
      hw <- nrow(img) / ncol(img)

      # we want (h_data * y_in_per_data) / (w_data * x_in_per_data) = hw
      h_data <- w_data * hw * (x_in_per_data / y_in_per_data)

      graphics::rasterImage(
        img,
        xj[i] - w_data/2, yj[i] - h_data/2,
        xj[i] + w_data/2, yj[i] + h_data/2,
        xpd = TRUE
      )
    }
  } else {
    # fallback: white points + white text labels
    graphics::points(xj, yj, pch = 21, bg = "white", col = "white", cex = 0.6)
    graphics::text(
      xj, yj,
      labels = pg_df$team,
      cex = 0.5,
      pos = 3,
      col = "white",
      xpd = TRUE
    )
  }

  grDevices::dev.off()

  svg_lines <- readLines(tmp_svg, warn = FALSE, encoding = "UTF-8")
  # strip XML/doctype if present for inline embedding
  svg_lines <- svg_lines[!grepl("^<\\?xml", svg_lines)]
  svg_lines <- svg_lines[!grepl("^<!DOCTYPE", svg_lines)]

  # Add tooltips: wrap each <image ... /> with a <title>TEAM</title>
  # so hovering a logo shows the team name.
  img_idx <- grep("<image", svg_lines, fixed = TRUE)
  if (length(img_idx)) {
    n <- min(length(img_idx), nrow(pg_df))
    new_svg <- character(0)
    img_counter <- 0L
    for (i in seq_along(svg_lines)) {
      line <- svg_lines[i]
      if (i %in% img_idx && img_counter < n) {
        img_counter <- img_counter + 1L
        team_name <- html_escape(pg_df$team[img_counter])
        if (grepl("/>$", line)) {
          line <- sub("/>$", ">", line)
          new_svg <- c(
            new_svg,
            line,
            paste0("<title>", team_name, "</title></image>")
          )
        } else {
          new_svg <- c(new_svg, line)
        }
      } else {
        new_svg <- c(new_svg, line)
      }
    }
    svg_lines <- new_svg
  }

  if (length(svg_lines)) {
    svg_lines[1] <- sub(
      "<svg ",
      '<svg style="max-width:100%;height:auto;display:block;margin:10px auto;" ',
      svg_lines[1],
      fixed = FALSE
    )
  }
  svg_markup <- paste(svg_lines, collapse = "\n")

  # ---- assemble views & full HTML -------------------------------
  power_block <- paste0(
    '<h2>Power Grid</h2>',
    '<div class="card"><div class="inner">',
    '<small>Each point is a team. X = sum of model spread in games where the team is the predicted winner (home spread, away spread as negative). ',
    'Y = sum of that team&#39;s win probabilities in those games.</small>',
    svg_markup,
    "</div></div>"
  )

  week_section_html <- paste0('<h2>By Week</h2>', week_section)
  team_section_html <- paste0('<h2>By Team</h2>', team_section)

  views_html <- paste0(
    '<div id="views">',
    power_block,
    week_section_html,
    team_section_html,
    "</div>"
  )

  full_html <- paste0(
    html_head,
    views_html,
    html_tail
  )

  con <- file(output_file, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(full_html), con)
  invisible(output_file)
}

# End of file
