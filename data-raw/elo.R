# Elo rating
#
# Function to
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

test <- function() {
  speedway:::getConnection()

  season <- 2016
  competition <- "DMP"
  teams <- DBI::dbGetQuery(
    {
      "
      SELECT
        e.id event_id,
        lc.type,
        e.round,
        lc.stage,
        lc.stage_level,
        et.team_idx,
        et.team_name,
        et.points_scored
      FROM league_competitions lc
      LEFT JOIN events e using(competition, season, stage)
      LEFT JOIN event_teams et on et.event_id = e.id
      WHERE
        e.competition = '%s' and
        e.season = %s and
        team_name is not null
      ORDER BY et.id
      "
    },
    competition,
    season
  )
  teams$points <- pointsCalc(teams$event_id, teams$team_name, teams$points_scored)

  x <- c(1, 1, 0, 0, NA, NA, 3, 3, 1, 1)
  speedway:::streakLength(x)


  #


  glicko(
    teams = c("A", "B", "C", "D"),
    rank = c(3, 4, 1, 2),
    days = rep(0, length(rank)),
    r = c(1500, 1400, 1550, 1700),
    rd = c(200, 30, 100, 300),
    init_r = 1500,
    init_rd = 100
  )
}


testScript1 <- function() {
  library(rvest)
  library(magrittr)
  library(dplyr)
  library(reshape2)
  options(scipen = 999, digits = 5, sqldf.driver = "SQLite", gsubfn.engine = "R")
  expandPairwise <- function(df, id, id2) {
    library(sqldf)
    grid <- sqldf("
                  SELECT
                  d1.id id_i,  d2.id id_j,
                  d1.id2 id2_i, d2.id2 id2_j
                  FROM df d1
                  LEFT JOIN df d2 on
                  d1.id = d2.id and
                  d1.id2 != d2.id2
                  ")

    colnames(df) <- paste0(colnames(df), "_i")
    grid <- left_join(grid, df)

    colnames(df) <- gsub(colnames(df), "_i", "_j", x = .)
    grid <- left_join(grid, df)
    return(grid)
  }

  speedway:::getConnectionLocal()
  raw_heats <- DBI::dbGetQuery({
    "
    SELECT
    event_id, heat, field, rider_name, points, position
    FROM event_heats eh
    WHERE
    rider_name is not null and
    rider_name != '' and
    points is not null"
  })
  raw_events <- DBI::dbGetQuery({
    "
    SELECT
    e.id event_id,e.date, e.competition, e.season, e.stage, e.place
    FROM events e
    "
  })
  DBI::dbDisconnect(con)

  # wrangle events -----
  events <- raw_events
  events$date <- as.POSIXct(strptime(events$date, "%Y-%m-%d %H:%M:%S"))

  # wrangle heats ----
  heats <- raw_heats
  heats <- heats %>%
    left_join(events[, c("event_id", "date")]) %>%
    arrange(date) %>%
    mutate(
      id =  paste0(event_id, heat),
      id = cumsum(!duplicated(id))
    )

  heats <- heats %>%
    filter(!is.na(points)) %>%
    group_by(id) %>%
    mutate(
      id2 = 1:n(),
      rank = as.integer(-points),
      rank = rank(rank, na.last = T, ties.method = "first")
    ) %>%
    ungroup() %>%
    select(-date, -position, -event_id, -points, -position)

  # initial parameters ----

  R <- setNames(
    rep(1500, length(unique(heats$rider_name))),
    unique(heats$rider_name)
  )

  RD <- setNames(
    rep(350, length(unique(heats$rider_name))),
    unique(heats$rider_name)
  )

  q <- log(10) / 400
  g <- function(x, q) {
    1 / sqrt(1 + 3 * q^2 * (x^2) / pi^2)
  }
  out_hat <- function(r_i, r_j, grd_j) {
    1 / (1 + 10^(-grd_j * (r_i - r_j) / 400))
  }
  d2 <- function(q, grd_j, out_hat) {
    (
      q^2 *
        sum(grd_j^2 * out_hat * (1 - out_hat))
    )^(-1)
  }
  likelihood <- function(grd_j, out, out_hat) {
    sum(grd_j * (out - out_hat))
  }
  r_prim <- function(r_i, rd_i, q, d2, likelihood) {
    r_i + q / (1 / rd_i^2 + 1 / d2) * likelihood
  }
  rd_prim <- function(rd_i, d2) {
    rd_prim <- sqrt(
      1 / (1 / rd_i^2 + 1 / d2)
    )

    ifelse(rd_prim > 350, 350, rd_prim)
  }

  # steps -----
  heats_list <- split(heats, heats$id)
  output_list <- list()

  for (inID in 1:length(heats_list)) {
    heat <- mutate(
      heats_list[[inID]],
      r = R[rider_name],
      rd = RD[rider_name]
    )

    if (nrow(heat) < 2) next

    heat_grid <- expandPairwise(heat)
    heat_grid <- mutate(
      heat_grid,
      grd_j = g(rd_j, q),
      grd_ij = g(sqrt(rd_i^2 + rd_j^2), q),
      out = as.integer(rank_i < rank_j),
      out_hat = out_hat(r_i, r_j, grd_j),
      e_out = out_hat(r_i, r_j, grd_ij)
    )

    output <- heat_grid %>%
      group_by(id2_i) %>%
      summarize(
        r_i = first(r_i),
        rd_i = first(rd_i),
        d2 = d2(q, grd_j, out_hat),
        likelihood = likelihood(grd_j, out, out_hat),
        r_prim = r_prim(r_i, rd_i, q, d2, likelihood),
        rd_prim = rd_prim(rd_i, d2),
        prob = prod(e_out)
      )

    if (sum(is.na(output)) > 0) stop()

    heat <- left_join(heat, output, by = c("id2" = "id2_i"))
    R[heat$rider_name] <- heat$r_prim
    RD[heat$rider_name] <- heat$rd_prim

    output_list[[inID]] <- output %>% mutate(id = inID)
  }

  outputs <- bind_rows(output_list)
  colnames(outputs) <- gsub(colnames(outputs), "_i$", "", x = .)
  outputs <- left_join(outputs, heats)
}

testScript2 <- function() {
  # Glicko -----
  expandPairwise <- function(df, id, id2) {
    library(sqldf)
    grid <- sqldf("
                  SELECT
                  d1.id id_i,  d2.id id_j,
                  d1.id2 id2_i, d2.id2 id2_j
                  FROM df d1
                  LEFT JOIN df d2 on
                  d1.id = d2.id and
                  d1.id2 != d2.id2
                  ")

    colnames(df) <- paste0(colnames(df), "_i")
    grid <- left_join(grid, df)

    colnames(df) <- gsub(colnames(df), "_i", "_j", x = .)
    grid <- left_join(grid, df)
    return(grid)
  }

  q <- log(10) / 400
  g <- function(x, q) {
    1 / sqrt(1 + 3 * q^2 * (x^2) / pi^2)
  }
  out_hat <- function(r_i, r_j, grd_j) {
    1 / (1 + 10^(-grd_j * (r_i - r_j) / 400))
  }
  d2 <- function(q, grd_j, out_hat) {
    (
      q^2 *
        sum(grd_j^2 * out_hat * (1 - out_hat))
    )^(-1)
  }
  likelihood <- function(grd_j, out, out_hat) {
    sum(grd_j * (out - out_hat))
  }
  r_prim <- function(r_i, rd_i, q, d2, likelihood) {
    r_i + q / (1 / rd_i^2 + 1 / d2) * likelihood
  }
  rd_prim <- function(rd_i, d2) {
    sqrt(
      1 / (1 / rd_i^2 + 1 / d2)
    )
  }

  heat <-
    data.frame(
      id = 1,
      id2 = 1:4,
      r = c(1500, 1400, 1550, 1700),
      rd = c(200, 30, 100, 300),
      rank = c(3, 4, 1, 2)
    )


  heat_grid <- expandPairwise(heat)
  heat_grid <- mutate(
    heat_grid,
    grd_j = g(rd_j, q),
    grd_ij = g(sqrt(rd_i^2 + rd_j^2), q),
    out = as.integer(rank_i < rank_j),
    out_hat = out_hat(r_i, r_j, grd_j),
    e_out = out_hat(r_i, r_j, grd_ij)
  )

  output <-
    heat_grid %>%
    group_by(id2_i) %>%
    summarize(
      r_i = first(r_i),
      rd_i = first(rd_i),
      d2 = d2(q, grd_j, out_hat),
      likelihood = likelihood(grd_j, out, out_hat),
      r_prim = r_prim(r_i, rd_i, q, d2, likelihood),
      rd_prim = rd_prim(rd_i, d2),
      prob = prod(e_out)
    )


  # EXPECTED OUTCOME OF THE GAME
  out_hat(
    r_i = 1400,
    r_j = 1500,
    g(sqrt(80^2 + 150^2), q)
  )

  # Glicko 2 ------
  # TrueSkills ----
  # MultiSkills -----
  # Elo -------
  # BT Model ------

  library(ggplot2)
  outputs %>%
    filter(rider_name %in% c("Greg Hancock", "Tomasz Gollob", "Tony Rickardsson")) %>%
    ggplot(aes(x = id, y = r_prim, group = rider_name, color = rider_name)) +
    geom_line() +
    geom_ribbon(aes(ymin = r_prim - 1.96 * rd_prim, ymax = r_prim + 1.96 * rd_prim, fill = rider_name, alpha = .1, color = NULL))
}
