library(nflfastR)
library(data.table)
library(tidyverse)

# =================================================== DATA CLEANING FUNCTIONS ===================================================

# game_date    --> Probably too fine grained to focus on specific days. Can we do game month instead? 
# goal_to_go   --> is this needed if we also have yardline_100? 
# all "kickoff_" type variables --> unrelated to run or pass, seems like we could filter all these out?
# defensive_extra_point_attempt --> This column is 100% filled with zeros. Which makes sense reading the data dictionary entry for it, this would
#                                   be an extremely rare occurrence.
# defensive_extra_point_conv --> Same as above.
# order_sequence --> data dictionary says a column to fix "out of order plays". Unsure what this is really doing.
# home_opening_kickoff --> this will be 0 or 1 and stay the same value for the entire game. Seems like this same information is already
#                          accounted for in the column tracking which team has possession.
# roof --> does this add new information beyond what is already in the home team feature?
# drive_start_yard_line --> does this add new information beyond was is already tracked in yardline_100? 
# stadium --> does this add new information beyond what is already tracked in the home team feature?
# weather --> This is "not-tidy" data. If we want to use this, should split it up into separate features for temp, humidity and wind. 


# These wouldn't be known until after the play is run. So we can't use it to predict what would happen on the play. 
# -> quarter_end, yards_gained, qb_dropback, qb_spike, qb_scramble, incomplete_pass, touchback, interception,
#    solo_tackle, safety, penalty, tackled_for_loss, fumble_lost, sack, touchdown, pass_touchdown, rush_touchdown, 
#    return_touchdown, extra_point_attempt, two_point_attempt, field_goal_attempt, fumble, complete_pass, assist_tackle,
#    lateral_reception, lateral_rush, lateral_return, lateral_recovery, lateral_receiving_yards, forced_fumble_player_1_team,
#    forced_fumble_player_2_team, series_success, series_result, touchback, out_of_bounds, aborted_play, drive_end_yard_line,
#    drive_play_id_ended, drive_game_clock_end, drive_ended_with_score, drive_quarter_end, drive_yards_penalized
#    end_yard_line, fixed_drive_result


get_additional_columns_to_remove <- function(){
  
  #cols_remove <- c('id', 'week', 'qtr', 'side_of_field', 'drive', 'sp', 
  #                 'time', 'yrdln', 'ydsnet', 'desc', 'qb_kneel', 'pass_length', 
  #                 'pass_location', 'air_yards', 'yards_after_catch', 'run_location', 'run_gap', 
  #                 'field_goal_result', 'kick_distance', 'extra_point_result', 'two_point_conv_result', 
  #                 'home_timeouts_remaining', 'away_timeouts_remaining', 'timeout', 'timeout_team', 'td_team', 
  #                 'total_home_score', 'total_away_score', 'posteam_score','defteam_score', 'posteam_score_post', 
  #                 'defteam_score_post', 'score_differential_post', 'extra_point_prob', 'two_point_conversion_prob', 
  #                 'game_half', 'posteam_type', 'play_type_nfl')
  
  
  
  remove_misc <- c('home_team', 'away_team', 'game_date', 'season', 'season_type', 'stadium', 
                   'game_stadium', 'weather', 'id', 'week', 'qtr', 'side_of_field', 'drive', 
                   'sp', 'time', 'start_time', 'time_of_day', 'play_clock', 'yrdln', 'ydsnet', 
                   'desc', 'sack', 'interception', 'touchback', "two_point_attempt", "field_goal_attempt", 
                   'return_yards', 'pass_length', 'pass_location', 'air_yards', 'yards_after_catch', 
                   'run_location', 'run_gap', 'field_goal_result', 'kick_distance', 'two_point_conv_result', 
                   'safety', 'home_timeouts_remaining', 'away_timeouts_remaining', 'timeout', 'timeout_team', 
                   'td_team', 'total_home_score', 'total_away_score', 'posteam_score','defteam_score', 
                   'posteam_score_post', 'defteam_score_post', 'score_differential_post', 'two_point_conversion_prob', 
                   "defensive_two_point_attempt", "defensive_two_point_conv", 'game_half', 'posteam_type', 
                   'return_team', 'penalty_team', 'penalty', 'penalty_type', 'play_deleted', 'aborted_play', 
                   'series_result', "replay_or_challenge", "replay_or_challenge_result", 'play_type_nfl', 
                   'special_teams_play', 'st_play_type', 'special', 'end_clock_time', 'end_yard_line', 'order_sequence', 
                   'away_score', 'home_score', 'location', 'result', 'total', 'spread_line', 'total_line', 'success', 
                   'home_coach', 'away_coach', 'rusher', 'receiver', 'passer', 'name', 'pass', 'rush', 'fantasy', 
                   'out_of_bounds', 'play', 'pass_oe')
  
  return(remove_misc)
}


# Function to apply all row-based filters used during data cleaning
apply_row_filters <- function(df){
  
  # Filter rows to only 1st, 2nd, and 3rd down.
  df <- df[df[,"down"] %in% c(1,2,3),]
  
  # Filter rows to only "run" and "pass" play_types
  df <- df[df[,"play_type"] %in% c("run", "pass"),]
  
  # Filter to only regular season games (no playoffs)
  df <- df[df[,"season_type"] == "REG",]
  
  return(df)
}


apply_column_filters <- function(df){
  
  # Returns the name of all columns that end in "_id"
  # 53 column names returned. 
  id_columns <- grep(pattern="_id$", x=colnames(df), value=TRUE)
  
  # Returns the name of all columns that end in "_name"
  # 43 column names returned. 
  name_columns <- grep(pattern="_name$", x=colnames(df), value=TRUE)
  
  # Returns the name of all columns that end in "_epa"
  # 20 column names returned. 
  epa_columns <- grep(pattern="_epa$", x=colnames(df), value=TRUE)
  
  # Returns the name of all columns that end in "jersey_number"
  # 20 column names returned. 
  jersey_number_columns <-  grep(pattern="jersey_number$", x=colnames(df), value=TRUE)
  
  # Returns the name of all columns that start with "punt"
  # 9 column names returned
  punt_columns <- grep(pattern="^punt_", x=colnames(df), value=TRUE) 
  
  
  # Returns an assortment of 14 column names that include some digit folowed by the phrase "_team"
  # Examples: 1) forced_fumble_player_1_team, 2) solo_tackle_1_team, 3) assist_tackle_1_team. 4) fumble_recovery_1_team
  # All of these columns contain information that wouldn't be known before the play is run.
  # They are also mostly all missing values anyways.
  numeric_team_columns <- grep(pattern="[[:digit:]]_team", x=colnames(df), value=TRUE)
  
  # Columns pertaining to kickoffs, which are not related to run and pass plays.
  kickoff_columns <- grep(pattern="kickoff", x=colnames(df), value=TRUE)
  
  # Columns, pertaining to fumbles, which are not known prior to the play.
  fumble_columns <- grep(pattern="^fumble", x=colnames(df), value=TRUE)
  
  # Returns a wide variety of column names, however none of these are important for run and pass play modeling.
  # Examples: "extra_point_result", "extra_point_prob", "extra_point_attempt", "defensive_extra_point_attempt",
  #           "defensive_extra_point_conv", "fixed_drive", "fixed_drive_result", "xyac_epa", "xyac_mean_yardage", 
  #           "xyac_median_yardage", "xyac_success", "xyac_fd", "xpass"    
  x_columns <- grep(pattern="x", x=colnames(df), value=TRUE) 
  
  # Returns a collection of columns about "lateral_" statistics.
  # Examples: "lateral_reception", "lateral_rush", "lateral_kickoff_returner_player_name". 
  lateral_columns <- grep(pattern="^lateral_", x=colnames(df), value=TRUE) 
  
  # Returns four columns relating to scoring a touchdown on the play
  # touchdown, pass_touchdown, rush_touchdown, return_touchdown
  touchdown_columns <- grep(pattern="touchdown", x=colnames(df), value=TRUE)
  
  # Returns a collection of columns pertaining to tackles that occurred on the play
  tackle_columns <- grep(pattern="tackle", x=colnames(df), value=TRUE)
  
  # Returns columns related to converting 3rd and 4th down:
  # Examples: third_down_converted, third_down_failed, fourth_down_converted, fourth_down_failed
  conversion_columns <- grep(pattern="down_['c-f']", x=colnames(df), value=TRUE)
  
  # Returns columns related to quarterback actions on the play.
  # Examples: qb_dropbackm qb_kneel, qb_spike, qb_scramble, qb_epa
  quarterback_columns <- grep(pattern="^qb_", x=colnames(df), value=TRUE) 
  
  # Returns columns relating to drive statistics.
  # Examples: drive_real_start_time, drive_play_count, drive_time_of_possession, drive_first_downs
  drive_columns <- grep(pattern="^drive_", x=colnames(df), value=TRUE) 
  
  # Grabbing a list of other assorted column names that need to be removed.
  additional_columns <- get_additional_columns_to_remove()
  
  # Combine all the above columns into a single vector
  columns_to_remove <- c(additional_columns, punt_columns, jersey_number_columns, 
                         epa_columns, name_columns, id_columns, numeric_team_columns,
                         kickoff_columns, fumble_columns, x_columns, lateral_columns, 
                         touchdown_columns, tackle_columns, conversion_columns, quarterback_columns,
                         drive_columns)
  
  # Remove all columns found in the columns_to_remove vector
  df <- df[,!(names(df) %in% columns_to_remove)]
  
  return(df)
}


save_dataset_to_file <- function(df, save_path, start_year, end_year, base_save_name="clean_play_by_play_"){
  
  # If save_path is not null, meaning a save_name was passed by the user, then
  # use the path the user passed
  if(!is.null(save_path)){
    full_save_path <- save_path
  }else{
    full_save_path <- paste0("./", base_save_name, start_year, "_to_", end_year, ".csv.gz")
  }
  
  
  fwrite(x=df, file=full_save_path, compress="gzip")
  
  return(full_save_path)
}


# =================================================== END DATA CLEANING FUNCTIONS ===================================================



# =================================================== PLOTTING FUNCTIONS ===================================================


add_cat_proportions_plot_columns <- function(df, categorical_predictor, target_column, target_level){
  
  # Temporary column for the categorical predictor we want to plot the levels of.  
  df[, "categorical_predictor"] <- as.factor(df[, categorical_predictor])
  
  
  # Map the target_column to 1's and 0's. This makes it easier to count occurrences when calculating proportions.
  df[,"binarized_target_column"] <- ifelse(df[, target_column] == target_level, 1, 0)
  
  # Not needed, because the ifelse above ensures target_column is of numeric data type
  # Temporary column for a binary version of the target
  # =1 if target=target_level, else 0
  # if(class(df[, target_column]) == "factor"){
  #   df <- factor_to_numeric(df=df, col_name="target_column")
  # }
  
  return(df)
  
}

get_proportion_cis <- function(df){
  
  # For each level of categorical_predictor, count the total number of times that the target
  # was equal to target_level (see plotting function), as well as the total number of occurrences.
  #
  # level_counts       --> number of times that categorical_predictor had this level AND the target had target_level
  # total_level_trials --> total number of times categorical_predictor had this level
  #
  level_counts <- df %>%
    group_by(categorical_predictor) %>% 
    summarise(level_counts=sum(binarized_target_column), 
              total_level_trials=length(binarized_target_column))
  
  # Initialize new dataframe columns we want to create, these are populated in the loop below.
  level_counts[,c("proportion_estimates", "lower_ci", "upper_ci", 
                  "p_values", "test_statistic")] <- -1
  
  # For each level in categorical_predictor
  for(row_num in 1:nrow(level_counts)){
    
    # Calculate the proportion of values in this level where the target=target_level
    test_result <- stats::prop.test(x=as.numeric(level_counts[[row_num,"level_counts"]]),
                                    n=as.numeric(level_counts[[row_num,"total_level_trials"]]))
    
    
    level_counts[row_num,"proportion_estimates"] <- test_result$estimate
    level_counts[row_num,"lower_ci"] <-  test_result$conf.int[1]
    level_counts[row_num,"upper_ci"] <- test_result$conf.int[2]
    level_counts[row_num,"p_values"] <- test_result$p.value
    level_counts[row_num,"statistic"] <- test_result$statistic
    
  }  
  
  level_counts[,"overall_proportion"] <- sum(df[,"binarized_target_column"]) / nrow(df)
  
  return(as.data.frame(level_counts))
  
}

factor_to_numeric <- function(df, col_name){
  
  df[,col_name] <- as.character(df[,col_name])
  df[,col_name] <- as.numeric(df[,col_name])
  return(df)
}


# =================================================== END PLOTTING FUNCTIONS ===================================================






