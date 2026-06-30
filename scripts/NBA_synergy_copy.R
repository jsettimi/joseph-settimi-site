library(dplyr)
library(stringr)
library(ggplot2)
library(hoopR)
library(httr)
library(jsonlite)
library(patchwork)

filter_play_type <- function(playtype, player_or_team, off_or_def, download = FALSE, permode = 'PerGame', playoff_or_reg = 'Regular Season', season_year = '2025-26', position = "") {
  # Define the URL and parameters
  url <- "https://stats.nba.com/stats/synergyplaytypes"
  
  #PRRollman, Isolation, Transition, PRBallHandler, Postup, Spotup, Cut, OffScreen, Handoff, OffRebound
  
  params <- list(
    LeagueID = '00',
    PerMode = permode,
    PlayType = playtype,
    PlayerOrTeam = player_or_team,
    SeasonType = playoff_or_reg,
    SeasonYear = season_year,
    TypeGrouping = off_or_def,
    PlayerPosition = position 
  )
  
  # Define headers
  headers <- c(
    "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.4.1 Safari/605.1.15",
    "Accept" = "*/*",
    "Accept-Encoding" = "gzip, deflate, br",
    "Accept-Language" = "en-US,en;q=0.9",
    "Referer" = "https://www.nba.com/",
    "Origin" = "https://www.nba.com",
    "Connection" = "keep-alive",
    "Sec-Fetch-Site" = "same-site",
    "Sec-Fetch-Mode" = "cors",
    "Sec-Fetch-Dest" = "empty",
    "Host" = "stats.nba.com"
  )
  
  # Make the GET request
  response <- GET(url, query = params, add_headers(.headers = headers))
  
  # Check for a successful response
  if (status_code(response) == 200) {
    data <- content(response, as = "text") %>%
      fromJSON(flatten = TRUE)
    
    
    # Extract headers and rows
    headers <- data$resultSets$headers
    rows <- data$resultSets$rowSet
    
    # Convert to a data frame
    dataframe <- as_tibble(do.call(rbind, rows), stringsAsFactors = FALSE, .name_repair = "minimal")
    
  
    colnames(dataframe) <- headers
    
    parsed_headers <- unlist(strsplit(colnames(dataframe), ","))
    parsed_headers <- gsub('^c\\(|\\)$|\\\"', "", parsed_headers)
    parsed_headers <- trimws(parsed_headers)
    parsed_headers <- parsed_headers[!is.na(parsed_headers)]
    if (off_or_def == 'defensive'){
      parsed_headers[7:length(parsed_headers)] <- paste0('def_', parsed_headers[7:length(parsed_headers)])}
    
    colnames(dataframe) <- parsed_headers
    
    # Assuming `my_tibble` is your tibble
    dataframe <- dataframe %>%
      dplyr::mutate(across(7:ncol(.), as.numeric))
    
    dataframe <- dataframe %>% select(-PLAY_TYPE, -TYPE_GROUPING)
    
    # Save to CSV if required
    if (download) {
      filepath <- sprintf("stat_%s_%s_%s__%s_%s.csv", playtype, player_or_team, off_or_def, playoff_or_reg, season_year)
      write.csv(dataframe, file = filepath, row.names = FALSE)
      cat("Data saved to", filepath, "\n")
    }
    
    return(dataframe)
  } else {
    cat("Failed to retrieve data. Status code:", status_code(response), "\n")
    return(NULL)
  }
}

# Load necessary libraries


# Define the function to filter shooting data
filter_shooting_data <- function(stat = "Efficiency", position = "", team = '""', opponent = '""', download = FALSE, player = 'Player', szn = '2025-26', szn_type = 'Regular Season') {
  
  # Define the URL for the stats endpoint
  url <- "https://stats.nba.com/stats/leaguedashptstats"
  
  # Define the team abbreviation lookup (you should define this mapping)
  team_abbr <- c(
    MIL = 1610612749,
    DEN = 1610612743,
    HOU = 1610612745,
    IND = 1610612754,
    OKC = 1610612760,
    CHI = 1610612741,
    PHI = 1610612755,
    BOS = 1610612738,
    MIA = 1610612748,
    SAC = 1610612758,
    WAS = 1610612764,
    LAC = 1610612746,
    GSW = 1610612744,
    POR = 1610612757,
    ORL = 1610612753,
    LAL = 1610612747,
    MIN = 1610612750,
    NOP = 1610612740,
    NYK = 1610612752,
    BKN = 1610612751,
    SAS = 1610612759,
    ATL = 1610612737,
    PHX = 1610612756,
    MEM = 1610612763,
    CHA = 1610612766,
    DAL = 1610612742,
    UTA = 1610612762,
    TOR = 1610612761,
    DET = 1610612765,
    CLE = 1610612739,
    '""' = 0 # Empty string as key
  )
  
  
  # Define the query parameters
  params <- list(
    College = "",
    Conference = "",
    Country = "",
    DateFrom = "",
    DateTo = "",
    Division = "",
    DraftPick = "",
    DraftYear = "",
    GameScope = "",
    Height = "",
    ISTRound = "",
    LastNGames = "0",
    LeagueID = "00",
    Location = "",
    Month = "0",
    OpponentTeamID = team_abbr[opponent],
    Outcome = "",
    PORound = "0",
    PerMode = "PerGame",
    PlayerExperience = "",
    PlayerOrTeam = player,
    PlayerPosition = position,
    PtMeasureType = stat,
    Season = szn,
    SeasonSegment = "",
    SeasonType = szn_type,
    StarterBench = "",
    TeamID = team_abbr[team],
    VsConference = "",
    VsDivision = "",
    Weight = ""
  )
  
  # Set headers to mimic a browser request
  headers <- c(
    "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.4.1 Safari/605.1.15",
    "Accept" = "*/*",
    "Accept-Encoding" = "gzip, deflate, br",
    "Accept-Language" = "en-US,en;q=0.9",
    "Referer" = "https://www.nba.com/",
    "Origin" = "https://www.nba.com",
    "Connection" = "keep-alive",
    "Sec-Fetch-Site" = "same-site",
    "Sec-Fetch-Mode" = "cors",
    "Sec-Fetch-Dest" = "empty",
    "Host" = "stats.nba.com"
  )
  
  # Send a GET request with the parameters and headers
  response <- GET(url, query = params, add_headers(.headers = headers))
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
    headers <- data$resultSets$headers
    rows <- data$resultSets$rowSet
    
    # Convert to a data frame
    dataframe <- as_tibble(do.call(rbind, rows), stringsAsFactors = FALSE, .name_repair = "minimal")
    
    
    colnames(dataframe) <- headers
    
    parsed_headers <- unlist(strsplit(colnames(dataframe), ","))
    parsed_headers <- gsub('^c\\(|\\)$|\\\"', "", parsed_headers)
    parsed_headers <- trimws(parsed_headers)
    parsed_headers <- parsed_headers[!is.na(parsed_headers)]
    
    colnames(dataframe) <- parsed_headers
    
    # Assuming `my_tibble` is your tibble
    dataframe <- dataframe %>%
      dplyr::mutate(across(7:ncol(.), as.numeric), year = szn)
    
    # If download is TRUE, save the dataframe to a CSV file
    if (download) {
      file_path <- paste0("/Users/josephsettimi/Desktop/team_", team, "_against_", opponent, "_position_", position, "_shooting_efficiency.csv")
      write.csv(dataframe, file_path, row.names = FALSE)
    }
    
    return(dataframe)
  } else {
    print(paste("Failed to retrieve data:", status_code(response)))
    return(NULL)
  }
}

teams <- filter_shooting_data(player = 'Team')
tc <- teams %>% select(TEAM_NAME, TEAM_ABBREVIATION) %>% left_join(teamcolors::teamcolors %>% rename('TEAM_NAME' = name), by = 'TEAM_NAME')

# Example usage:
# result <- filter_shooting_data(stat = "Efficiency", team = "LAL", opponent = "BOS", position = "G", download = TRUE)
library(ggplot2)
library(grid)
library(png)
library(RCurl)

GeomNbaHeadshots <- ggproto("GeomNbaHeadshots", Geom,
                            required_aes = c("player_id"),
                            default_aes = aes(size = 0.1),
                            
                            draw_panel = function(data, panel_params, coord) {
                              # Transform data for coordinate system
                              coords <- coord$transform(data, panel_params)
                              
                              grobs <- lapply(seq_len(nrow(coords)), function(i) {
                                player_id <- coords$player_id[i]
                                headshot_url <- nba_playerheadshot(player_id)
                                
                                # Fetch and read PNG image from URL
                                img <- tryCatch({
                                  readPNG(RCurl::getURLContent(headshot_url, binary = TRUE))
                                }, error = function(e) {
                                  warning(paste("Could not fetch image for player_id:", player_id))
                                  matrix(1, nrow = 2, ncol = 2) # Placeholder (white square) in case of error
                                })
                                
                                # Create a rasterGrob with size mapping
                                rasterGrob(img, 
                                           x = coords$x[i], 
                                           y = coords$y[i], 
                                           width = unit(data$size[i], "npc"), 
                                           height = unit(data$size[i], "npc"),
                                           just = c("center", "center"))
                              })
                              
                              gTree(children = do.call(gList, grobs))
                            }
)

geom_nba_headshots <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ...) {
  layer(
    geom = GeomNbaHeadshots, mapping = mapping, data = data, 
    stat = stat, position = position, inherit.aes = TRUE, ...
  )
}

element_nba_headshots <- function(size = 1) {
  structure(list(size = size), class = c("element_nba_headshots", "element_text", "element"))
}

element_grob.element_nba_headshots <- function(element, label, ...) {
  if (length(label) == 0) return(nullGrob())
  
  # Generate headshot images as grobs for each label
  grobs <- lapply(label, function(player_id) {
    player_id <- as.character(player_id)
    
    headshot_url <- nba_playerheadshot(player_id)
    img <- tryCatch({
      readPNG(RCurl::getURLContent(headshot_url, binary = TRUE))
    }, error = function(e) {
      warning(paste("Could not fetch image for player_id:", player_id))
      matrix(1, nrow = 2, ncol = 2) # Placeholder
    })
    
    rasterGrob(img, width = unit(element$size, "lines"), height = unit(element$size, "lines"))
  })
  
  gTree(children = do.call(gList, grobs))
}

chart_ovd <- function(stat, offense, defense, pos = NA, year = '2025-26') {
  
  if (!(stat %in% c('POINTS', 'DRIVE', 'CATCH_SHOOT', 'PULL_UP', 'PAINT', 'POST', 'ELBOW'))){
    return("Stat must be one of POINTS, DRIVE, CATCH_SHOOT, PULL_UP, PAINT, POST, ELBOW")
  }
  
  players <- filter_shooting_data(szn = year)
  def <- filter_shooting_data(opponent = defense, szn = year)
  off <- filter_shooting_data(team = offense, szn = year)
  
  if (!is.na(pos)){
    def <- filter_shooting_data(opponent = defense, position = pos, szn = year)
    off <- filter_shooting_data(team = offense, position = pos, szn = year)
  }
  
  col_map <- c(
    POINTS       = "POINTS",
    DRIVE        = "DRIVE_PTS",
    CATCH_SHOOT  = "CATCH_SHOOT_PTS",
    PULL_UP      = "PULL_UP_PTS",
    PAINT        = "PAINT_TOUCH_PTS",
    POST         = "POST_TOUCH_PTS",
    ELBOW        = "ELBOW_TOUCH_PTS"
  )
  
  def_color1 <- tc %>% dplyr::filter(TEAM_ABBREVIATION == defense) %>% pull(primary)
  def_color2 <- tc %>% dplyr::filter(TEAM_ABBREVIATION == defense) %>% pull(secondary)
  
  off_color1 <- tc %>% dplyr::filter(TEAM_ABBREVIATION == offense) %>% pull(primary)
  off_color2 <- tc %>% dplyr::filter(TEAM_ABBREVIATION == offense) %>% pull(secondary)
  
  if (is.na(def_color2)){def_color2 = 'black'}
  if (is.na(off_color2)){off_color2 = 'black'}
  
  suff <- paste0('_vs_', defense)
  suff2 <- paste0('_', offense)
  
  stat_clean <- gsub("_", " ", stat)
  tit <- paste0("Players ", stat_clean, " PPG vs. ", defense, " Compared to average")
  xlab <- paste0(stat_clean, " Average PPG")
  ylab <- paste0(stat_clean, " PPG Average vs. ", defense)
  subtit <- paste0('Via NBA.com | ', offense, ' Top 5 highlighted')
  
  tit2 <- paste0("Share of ", stat_clean ," points")
  
  off5 <- off %>% slice_max(order_by = off %>% pull(col_map[stat]), n = 5) %>% pull(col_map[stat])
  off5_name <- off %>% slice_max(order_by = off %>% pull(col_map[stat]), n = 5)
  
  vs_def <- def %>% inner_join(players, by = join_by(PLAYER_ID, PLAYER_NAME), suffix = c(suff, 'avg')) %>% ggplot(aes_string(paste0(col_map[stat], 'avg'), paste0(col_map[stat], suff))) + geom_point(colour = def_color1) + geom_abline(slope = 1, intercept = 0, linewidth = 1.1, color = def_color2) + geom_smooth(method = 'loess', se = T, color = NA, alpha = 0.4) + geom_vline(xintercept = off5, linetype = 'dashed', color = off_color1) + geom_text(inherit.aes = F, data = off5_name, x = off5, aes(label = PLAYER_NAME, y= 13), fontface = 'bold.italic', color = off_color1, angle = 90, size = 4, vjust = -0.3, hjust = 0.5, position = "jitter") + coord_cartesian(clip = "off") + theme_minimal() + labs(title = tit, x = xlab, y = ylab, subtitle = subtit) + theme(plot.title = element_text(face = 'bold'), plot.subtitle = element_text(color = 'slategray'), axis.title = element_text(face = 'bold.italic'))
  
  vs_off <- off %>% ggplot(aes(x = PLAYER_NAME)) + geom_col(aes(y = POINTS), alpha = 0.6, fill = off_color2) + geom_col(aes_string(y = col_map[stat]), fill = off_color1) + labs(title = tit2, subtitle = 'via NBA.com') + theme_minimal() + theme(plot.title = element_text(face = 'bold'), plot.subtitle = element_text(color = 'slategray'), axis.title.x = element_blank(), axis.text.x = element_text(angle = 80, vjust = 0.5, face = 'bold'), axis.title.y = element_text(face = 'bold.italic'))
  
  return(list(vs_def, vs_off))
}


shot_similarity <- function(tgt, players,  tgt_year = '2025-26', x = 10, type = 'Regular Season') {
  
  target <- filter_shooting_data(szn = tgt_year, szn_type = type) %>% dplyr::filter(PLAYER_NAME == tgt) %>% select(PLAYER_ID, PLAYER_NAME, MIN, POINTS, DRIVE_PTS, CATCH_SHOOT_PTS, PULL_UP_PTS, PAINT_TOUCH_PTS, POST_TOUCH_PTS, ELBOW_TOUCH_PTS)
  
  if (nrow(players) <= x){x = nrow(players)}
  else {x = x}
  
  shot_cols <- c("DRIVE_PTS", "CATCH_SHOOT_PTS", "PULL_UP_PTS",
                 "PAINT_TOUCH_PTS", "POST_TOUCH_PTS", "ELBOW_TOUCH_PTS")
  
  to_props <- function(row) {
    vals <- as.numeric(row[shot_cols])
    vals / sum(vals)
  }
  
  cosine <- function(a, b) sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))
  
  target_props <- to_props(target)
  
  df <- data.frame(
    PLAYER = players$PLAYER_NAME, YEAR = players$year, 
    SCORE  = round(apply(players, 1, function(r) cosine(target_props, to_props(r))) * 100, 2)
  ) |> (\(d) d[order(-d$SCORE), ])()
  
  df <- df %>% slice_max(order_by = SCORE, n = x)
  
  return(df)}


plot_player_shots <- function(target_player,
                              target_team,
                              opposing_team,
                              game_year,
                              game_id = NULL) {
  
  # ---------------------------------------------------------
  # 1. NO game_id -> list matchups between the two teams that year
  # ---------------------------------------------------------
  if (is.null(game_id)) {
    
    sched <- hoopR::load_nba_schedule(seasons = game_year)
    
    matchups <- sched %>%
      dplyr::filter(
        (stringr::str_detect(home_display_name, target_team) | stringr::str_detect(away_display_name, target_team)),
        (stringr::str_detect(home_display_name, opposing_team) | stringr::str_detect(away_display_name, opposing_team))
      ) %>%
      dplyr::mutate(
        label = paste0(away_display_name, " @ ", home_display_name,
                       ", ", as.Date(game_date), ": ", game_id)
      ) %>%
      pull(label)
    
    if (length(matchups) == 0) {
      message("No games found between ", target_team, " and ", opposing_team, " in ", game_year, ".")
      return(invisible(NULL))
    }
    
    message("No game_id supplied. Here are the matchups for ", game_year, ":")
    return(matchups)
  }
  
  # ---------------------------------------------------------
  # 2. game_id supplied -> build the chart
  # ---------------------------------------------------------
  #pbp_all <- hoopR::load_nba_pbp(seasons = game_year) game_id == as.integer(.env$game_id)
  pbp <- hoopR::load_nba_pbp(seasons = game_year) %>% filter(game_id == as.integer(.env$game_id))
  
  if (!('points_attempted' %in% colnames(pbp))) {
    pbp <- pbp %>% dplyr::mutate(
      points_attempted = case_when(
        stringr::str_detect(text, regex("Free Throw", ignore_case = TRUE)) ~ 1L,
        stringr::str_detect(text, regex("Three Point|Three Pointer", ignore_case = TRUE)) ~ 3L,
        stringr::str_detect(text, regex("Jumper|Shot|Layup|Dunk|Hook Shot|Tip Shot|Running Jumper", ignore_case = TRUE)) ~ 2L,
        TRUE ~ NA_integer_
      )
    )
  }
  
  # roster for the target team, that season, so athlete IDs match correctly
  roster <- hoopR::load_nba_player_box(seasons = game_year) %>%
    dplyr::filter(team_name == target_team) %>%
    select(athlete_id, athlete_display_name) %>%
    distinct(athlete_id, .keep_all = TRUE)
  
  # figure out home/away orientation for this specific game so the
  # "lead" sign is always from target_team's perspective
  game_meta <- hoopR::load_nba_schedule(seasons = game_year) %>%
    dplyr::filter(game_id == .env$game_id) %>%
    slice(1)
  
  target_is_home <- stringr::str_detect(game_meta$home_display_name, target_team)
  
  game_o <- pbp %>%
    dplyr::mutate(
      diff = if (target_is_home) home_score - away_score else away_score - home_score
    ) %>%
    dplyr::filter(!type_id %in% c(99, 101, 102, 282) & shooting_play) %>%
    select(end_game_seconds_remaining, athlete_id_1, points_attempted, diff) %>%
    rename(athlete_id = athlete_id_1) %>%
    dplyr::filter(athlete_id %in% roster$athlete_id) %>%
    inner_join(roster, by = "athlete_id")
  
  # ---------------------------------------------------------
  # dynamic team colors
  # ---------------------------------------------------------
  colors <- teamcolors::teamcolors %>%
    dplyr::filter(league == 'nba', mascot == target_team)
  
  highlight_color <- colors$primary[1]
  base_color      <- colors$secondary[1]
  
  # fallback in case secondary is missing/identical to primary
  if (is.na(base_color) || base_color == highlight_color) {
    base_color <- '#008080'
  }
  
  color_values <- setNames(highlight_color, target_player)
  
  # ---------------------------------------------------------
  # dynamic labels
  # ---------------------------------------------------------
  game_date_lbl <- as.Date(game_meta$game_date)
  
  if (game_meta$season_type == 3) {
    
    playoff_games <- hoopR::load_nba_schedule(seasons = game_year) %>%
      dplyr::filter(
        season_type == 3,
        (
          (stringr::str_detect(home_display_name, target_team) &
             stringr::str_detect(away_display_name, opposing_team)) |
            (stringr::str_detect(home_display_name, opposing_team) &
               stringr::str_detect(away_display_name, target_team))
        )
      ) %>%
      dplyr::arrange(game_date)
    
    game_number <- match(game_id, playoff_games$game_id)
    
    plot_title <- paste0(
      target_team,
      " shots vs ",
      opposing_team,
      " (",
      game_year,
      ", Game ",
      game_number,
      ") with ",
      target_player,
      " highlighted"
    )
    
  } else {
    
    plot_title <- paste0(
      target_team,
      " shots vs ",
      opposing_team,
      " (",
      game_year,
      ") with ",
      target_player,
      " highlighted"
    )
    
  }
  
  plot_subtitle <- paste0(
    "Negative means losing | ",
    game_date_lbl
  )
  
  # ---------------------------------------------------------
  # plot
  # ---------------------------------------------------------
  p <- game_o %>%
    ggplot(aes(x = end_game_seconds_remaining, y = diff, colour = athlete_display_name)) +
    geom_point(size = 3) +
    scale_colour_manual(values = color_values, na.value = base_color) +
    scale_x_reverse() +
    theme_minimal() +
    theme(
      plot.title = element_text(face = 'bold', size = 11),
      plot.subtitle = element_text(face = 'italic', color = 'slategrey'),
      axis.title.x = element_text(vjust = -0.75, face = 'bold', hjust = 0.5),
      axis.title.y = element_text(vjust = 0.75, face = 'bold'),
      axis.text.y = element_text(size = 9.5),
      legend.position = 'none'
    ) +
    guides(size = 'none') +
    labs(
      x = 'Seconds remaining in game',
      y = paste0(target_team, ' lead'),
      title = plot_title,
      subtitle = plot_subtitle
    ) +
    geom_vline(xintercept = seq(0, 2880, 720), linetype = 'dashed', color = 'black') +
    geom_hline(yintercept = 0)
  
  q <- game_o %>%
    ggplot(aes(x = end_game_seconds_remaining, y = points_attempted, colour = athlete_display_name)) +
    geom_point(size = 3) +
    scale_colour_manual(values = color_values, na.value = base_color) +
    scale_x_reverse() + scale_y_continuous(breaks = c(1,2,3)) + 
    theme_minimal() +
    theme(
      plot.title = element_text(face = 'bold', size = 11),
      plot.subtitle = element_text(face = 'italic', color = 'slategrey'),
      axis.title.x = element_text(vjust = -0.75, face = 'bold', hjust = 0.5),
      axis.title.y = element_text(vjust = 0.75, face = 'bold'),
      axis.text.y = element_text(size = 9.5),
      legend.position = 'none'
    ) +
    guides(size = 'none') +
    labs(
      x = 'Seconds remaining in game',
      y = 'Shot Type',
      title = plot_title,
      subtitle = plot_subtitle
    ) +
    geom_vline(xintercept = seq(0, 2880, 720), linetype = 'dashed', color = 'black') 
  
  return(list(p, q))
}

make_master_playtype <- function(yr){
  play_types <- c('Isolation', 'PRRollman', 'Transition', 'PRBallHandler', 'Postup', 'Spotup', 'Cut', 'OffScreen', 'Handoff', 'OffRebound')
  master <- filter_play_type(playtype = 'Isolation', player_or_team = 'P', off_or_def = 'offensive', season_year = yr)
  
  master <- master %>%
    rename_with(
      ~ paste0(.x, "_Isolation"),
      -c(SEASON_ID, PLAYER_ID, PLAYER_NAME,
         TEAM_ID, TEAM_ABBREVIATION, TEAM_NAME)
    )
  
  for (x in play_types[-1]) {
    
    y <- filter_play_type(
      playtype = x,
      player_or_team = 'P',
      off_or_def = 'offensive'
    ) %>%
      rename_with(
        ~ paste0(.x, "_", x),
        -c(SEASON_ID, PLAYER_ID, PLAYER_NAME,
           TEAM_ID, TEAM_ABBREVIATION, TEAM_NAME)
      )
    
    master <- master %>%
      right_join(
        y,
        by = c(
          'SEASON_ID', 'PLAYER_ID', 'PLAYER_NAME',
          'TEAM_ID', 'TEAM_ABBREVIATION', 'TEAM_NAME'
        )
      )
    play_cols <- c(
      "POSS_PCT_Isolation",
      "POSS_PCT_PRRollman",
      "POSS_PCT_Transition",
      "POSS_PCT_PRBallHandler",
      "POSS_PCT_Postup",
      "POSS_PCT_Spotup",
      "POSS_PCT_Cut",
      "POSS_PCT_OffScreen",
      "POSS_PCT_Handoff",
      "POSS_PCT_OffRebound"
    )
    
    master[play_cols] <- lapply(master[play_cols], \(x) replace_na(x, 0))
  }
  return(master)}  
  
chart_synergy <- function(playtype,
                              offense,
                              pos = NA,
                              year = '2025-26') {
  
  play_types <- c(
    'Isolation', 'PRRollman', 'Transition', 'PRBallHandler',
    'Postup', 'Spotup', 'Cut', 'OffScreen',
    'Handoff', 'OffRebound'
  )
  
  if (!(playtype %in% play_types)) {
    return(
      paste(
        "playtype must be one of:",
        paste(play_types, collapse = ", ")
      )
    )
  }
  
  players <- filter_play_type(
    playtype = playtype,
    player_or_team = "P",
    off_or_def = "offensive",
    season_year = year
  )
  
  if (!is.na(pos)) {
    players <- players %>%
      filter(POSITION == pos)
  }
  
  off <- players %>%
    filter(TEAM_ABBREVIATION == offense)
  
  off_color1 <- tc %>%
    filter(TEAM_ABBREVIATION == offense) %>%
    pull(primary)
  
  off_color2 <- tc %>%
    filter(TEAM_ABBREVIATION == offense) %>%
    pull(secondary)
  
  if (offense == 'SAS') {
    off_color1 <- "black"
    off_color2 <- "grey70"}
  
  if (length(off_color1) == 0) off_color1 <- "black"
  if (length(off_color2) == 0) off_color2 <- "grey70"
  
  tit <- paste0(playtype, ": Usage vs Efficiency")
  
  subtit <- paste0(
    offense,
    " players highlighted | Via Synergy"
  )
  
  ggplot() +
    geom_point(
      data = players,
      aes(
        x = POSS_PCT,
        y = PPP
      ),
      color = off_color2,
      alpha = 0.45,
      size = 2
    ) +
    geom_point(
      data = off,
      aes(
        x = POSS_PCT,
        y = PPP
      ),
      color = off_color1,
      size = 3
    ) +
    ggrepel::geom_text_repel(
      data = off,
      aes(
        x = POSS_PCT,
        y = PPP,
        label = PLAYER_NAME
      ),
      color = off_color1,
      fontface = "bold",
      size = 4,
      max.overlaps = Inf
    ) +
    theme_minimal() +
    labs(
      title = tit,
      subtitle = subtit,
      x = "Frequency of Possessions (%)",
      y = "Points Per Possession"
    ) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "slategray"),
      axis.title = element_text(face = "bold.italic")
    ) + geom_vline(
      xintercept = mean(players$POSS_PCT, na.rm = TRUE),
      linetype = "dashed",
      color = "grey50"
    ) +
    geom_hline(
      yintercept = mean(players$PPP, na.rm = TRUE),
      linetype = "dashed",
      color = "grey50"
    )
}