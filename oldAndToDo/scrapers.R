#Cole Conte
#scrapers.R

#Reference: https://github.com/MrDAndersen/ff.datascrape/blob/master/R/scrape_yahoo.R

scrape_yahoo <- function(stat_type = c("Projected",  "Actual", "Remaining Season",
                                       "Next 4 weeks", "Last 4 Weeks", "Avg Last 4 Weeks" ),
                         position = c("O", "DP", "QB", "RB", "WR", "TE", "K", "DST",
                                      "D", "DB", "DL", "LB", "DT", "DE", "CB", "S"),
                         season = NULL, week = NULL){
  
  
  position <- match.arg(position)
  stat_type <- match.arg(stat_type)
  
  
  if(!is.null(week) && week != 0){
    if(!(week %in% 1:17))
      stop("When specifying a week please only use numbers between 1 and 17", call. = FALSE)
  }
  
  league_id <- "14458"
  if(is.null(league_id))
    stop("Yahoo League ID is not set. Please set yahoo league ID with options('ffdata.yahoo_league'='leagueid')", call. = FALSE )
  
  
  yahoo_base <- str_to_url("https://football.fantasysports.yahoo.com/f1/")
  
  yahoo_qry <- list(sort = "PTS", sdir = "1", status = "A", pos = ifelse(position == "DST", "DEF", position),
                    stat1 = "", jsenabled = 1, count = 0)
  
  yahoo_qry$stat1 <- switch(
    stat_type,
    "Projected" = ifelse(week > 0, paste0("S_PW_", week) ,
                         paste0("S_PS_", season)),
    "Actual" = ifelse(week > 0, paste0("S_W_", week) ,
                      paste0("S_S_", season)),
    "Remaining Season" = paste0("S_PSR_", season),
    "Next 4 weeks" = "S_PN4W",
    "Last 4 Weeks" = "S_L4W",
    "Avg Last 4 Weeks" = "S_AL4W"
  )
  
  yahoo_path <- paste(league_id, "players", sep = "/")
  yahoo_path <- paste0(parse_url(yahoo_base)$path, yahoo_path)
  
  yahoo_url <- modify_url(yahoo_base, path = yahoo_path, query = yahoo_qry)
  
  yahoo_session <- yahoo_url %>% html_session()
  
  player_cols <- c("Offense", "Kickers", "Defense/Special Teams", "Defensive Players")
  
  yahoo_data <- scrape_html_data(yahoo_url)
  
  player_col <- intersect(player_cols, names(yahoo_data))
  names(player_col) <- "Yahoo_Player"
  yahoo_data <- yahoo_data %>% rename(!!!player_col)
  
  yahoo_data <- yahoo_data %>%
    extract(., Yahoo_Player, c("Note", "Player", "Team", "Pos", "Status/Game/Opp"),
            "\\s*(.+Note[s]*)\\s+(.+)\\s([[:alpha:]]{2,3})\\s\\-\\s([[:alpha:]]{1,3},*[[:alpha:]]*)\\s{2,}(.+)") %>%
    select(., -one_of(c("Note", "Status/Game/Opp")))
  
  names(yahoo_data) <- names(yahoo_data) %>%
    gsub("Fantasy ", "", .) %>%
    gsub("Rankings ", "", .) %>% gsub("GP", "Games", .)
  
  if(position %in% c("O", "QB", "RB", "WR", "TE")){
    names(yahoo_data) <- names(yahoo_data) %>%
      gsub("YdTD", "Yd TD", .) %>% gsub("Yd TD", "TD", .) %>%
      gsub("(40 Yd)\\s(Cmp|Att|TD|Rec)","\\1", . ) %>%
      gsub("Misc 2PT", "two pts", .) %>%
      gsub(" Downs", "", .) %>%
      gsub("Passing Sack", "Sacks", .)  %>%
      offensive_columns()
  }
  
  if(position == "K"){
    names(yahoo_data) <- names(yahoo_data) %>%
      gsub("Field Goals Made", "fg", .) %>%
      gsub("Field Goals", "fg", .) %>%
      gsub("PAT Made", "xp", .) %>%
      gsub("PAT", "xp", .) %>%
      gsub("Missed", "miss", .) %>%
      gsub("([^0-9])([0-9])([^0-9])", "\\10\\2\\3", . ) %>%
      gsub("\\-", "",.)
  }
  
  if(position == "DST"){
    def_cols <- c(dst_pts_allow = "Pts vs.",  dst_sack = "Tackles Sack",
                  dst_safety = "Tackles Safe", dst_TFL = "Tackles TFL",
                  dst_Int = "Turnovers Int",    dst_Fum_Rec = "Turnovers Fum Rec",
                  dst_td = "TD TD",  dst_Blk = "Miscellaneous Blk Kick",
                  dst_4_down = "Miscellaneous 4 Dwn Stops",
                  dst_Yds_Allow = "Miscellaneous Yds Allow",
                  dst_3_Out = "Miscellaneous 3 And Outs",
                  dst_Return_Yds = "Return Yds",
                  dst_Return_Tds = "Return TD")
    
    rename_cols <- def_cols[which(def_cols %in% names(yahoo_data))]
    yahoo_data <- yahoo_data %>% rename(!!!rename_cols) %>%
      mutate(src_id = recode(src_id, !!!yahoo_def))
    
    yahoo_data <- yahoo_data %>% mutate(id = id_col(yahoo_data$src_id, "stats_id"))
  }
  
  if(position %in% c("D", "DB", "DL", "LB", "DT", "DE", "CB", "S")){
    idp_cols <- c(idp_Ret_Yds = "Return Yds", idp_Ret_Tds = "Return TD",
                  idp_Solo = "Tackles Tack Solo", idp_Asst = "Tackles Tack Ast",
                  idp_TFL = "Tackles TFL",  idp_Sack = "Tackles Sack",
                  idp_Safety = "Tackles Safe", idp_PD = "Misc Pass Def",
                  idp_Blk = "Misc Blk Kick", idp_Int = "Turnovers Int",
                  idp_Fum_Force = "Turnovers Fum Force",
                  idp_Fum_Rec = "Turnovers Fum Rec", idp_Ret_Yds = "Turnovers Ret Yds",
                  idp_TD = "TD TD")
    
    rename_cols <- idp_cols[which(idp_cols %in% names(yahoo_data))]
    yahoo_data <- yahoo_data %>% rename(!!!rename_cols)
  }
  
  yahoo_data <- ff_clean_names(yahoo_data)
  
  structure(yahoo_data, source = "Yahoo", type = stat_type, season = season, week = week, position = position)
}

scrape_yahoo(season=2019,week=12)
#' loadEspnProjections <- function(){
#'   #' Load projections from ESPN.
#'   #' @return A dataframe containing ESPN player data and projections
#'   espnprojections = data.frame("PLAYER, TEAM POS"=character(),
#'                                "First.Name"=character(),
#'                                "OPP"=character(), 
#'                                "STATUS ET"=character(),
#'                                "C/A" = character(),
#'                                "PASSYDS" = double(),
#'                                "PASSTD" = double(),
#'                                "INT" = double(),
#'                                "RUSH" = double(),
#'                                "RUSHYDS" = double(),
#'                                "RUSHTD" = double(),
#'                                "REC" = double(),
#'                                "RECYDS" = double(),
#'                                "RECTD" = double(),
#'                                "PTS" = double())
#'   for (row in seq(0,240,40)){
#'     rows = {readHTMLTable(paste0("https://fantasy.espn.com/football/players/projections",row), head=TRUE, as.data.frame=TRUE, stringsAsFactors=FALSE)$playertable_0}
#'     rows = rows[-1,]
#'     names(rows) <- c("PLAYER, TEAM POS", "OPP", "STATUS ET", "C/A", "PASSYDS", "PASSTD", "INT", "RUSH", "RUSHYDS", "RUSHTD", "REC", "RECYDS", "RECTD", "PTS")
#'     rows["PASSYDS"] = lapply(rows["PASSYDS"],as.double)
#'     rows["INT"] = lapply(rows["INT"],as.double)
#'     rows["RUSH"] = lapply(rows["RUSH"],as.double)
#'     rows["RUSHYDS"] = lapply(rows["RUSHYDS"],as.double)
#'     rows["RUSHTD"] = lapply(rows["RUSHTD"],as.double)
#'     rows["REC"] = lapply(rows["REC"],as.double)
#'     rows["RECYDS"] = lapply(rows["RECYDS"],as.double)
#'     rows["RECTD"] = lapply(rows["RECTD"],as.double)
#'     rows["PTS"] = lapply(rows["PTS"],as.double)
#'     rows["PTS"] = rows["PTS"] + 0.5*rows["REC"]         #Half PPR fanduel adjustment
#'     espnprojections = rbind(espnprojections,rows)
#'   }
#' }
#' 
#' loadFanDuelData <- function(){
#'   fanduelprices = read.csv(file="FanDuel-NFL-2016-12-24-17365-players-list.csv",head=TRUE,sep=",",stringsAsFactors=FALSE)
#'   fanduelprices$Team[fanduelprices$Team=="BAL"] = "Bal"
#'   fanduelprices$Team[fanduelprices$Team=="CIN"] = "Cin"
#'   fanduelprices$Team[fanduelprices$Team=="CLE"] = "Cle"
#'   fanduelprices$Team[fanduelprices$Team=="PIT"] = "Pit"
#'   fanduelprices$Team[fanduelprices$Team=="HOU"] = "Hou"
#'   fanduelprices$Team[fanduelprices$Team=="IND"] = "Ind"
#'   fanduelprices$Team[fanduelprices$Team=="JAC"] = "Jax"
#'   fanduelprices$Team[fanduelprices$Team=="TEN"] = "Ten"
#'   fanduelprices$Team[fanduelprices$Team=="BUF"] = "Buf"
#'   fanduelprices$Team[fanduelprices$Team=="MIA"] = "Mia"
#'   fanduelprices$Team[fanduelprices$Team=="DEN"] = "Den"
#'   fanduelprices$Team[fanduelprices$Team=="OAK"] = "Oak"
#'   fanduelprices$Team[fanduelprices$Team=="CHI"] = "Chi"
#'   fanduelprices$Team[fanduelprices$Team=="DET"] = "Det"
#'   fanduelprices$Team[fanduelprices$Team=="MIN"] = "Min"
#'   fanduelprices$Team[fanduelprices$Team=="ATL"] = "Atl"
#'   fanduelprices$Team[fanduelprices$Team=="CAR"] = "Car"
#'   fanduelprices$Team[fanduelprices$Team=="DAL"] = "Dal"
#'   fanduelprices$Team[fanduelprices$Team=="PHI"] = "Phi"
#'   fanduelprices$Team[fanduelprices$Team=="WAS"] = "Was"
#'   fanduelprices$Team[fanduelprices$Team=="ARI"] = "Ari"
#'   fanduelprices$Team[fanduelprices$Team=="SEA"] = "Sea"
#'   
#'   fanduelprices$Nickname = NULL
#'   fanduelprices$Id = NULL
#' }
