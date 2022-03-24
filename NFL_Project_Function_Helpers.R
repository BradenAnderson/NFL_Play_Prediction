library(nflfastR)
library(data.table)
library(tidyverse)
library(stringr)
library(ggpmisc)
library(splitTools)

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
  

  remove_misc <- c('game_date', 'season', 'season_type', 'stadium', 'game_stadium', 'weather', 
                   'id', 'week', 'qtr', 'side_of_field', 'drive', 'sp', 'time', 'start_time', 
                   'time_of_day', 'play_clock', 'quarter_end', 'yrdln', 'ydsnet', 'desc', 'sack', 
                   'touchback', 'no_huddle', 'shotgun', 'complete_pass', 'incomplete_pass', 
                   "two_point_attempt", "field_goal_attempt", 'pass_length', 'pass_location', 
                   'run_location', 'run_gap', 'field_goal_result', 'kick_distance', 'two_point_conv_result', 
                   'safety', 'home_timeouts_remaining', 'away_timeouts_remaining', 'timeout', 
                   'score_differential_post', "no_score_prob", "opp_fg_prob", 'opp_safety_prob', 
                   "opp_td_prob", "safety_prob", 'two_point_conversion_prob', "defensive_two_point_attempt", 
                   "defensive_two_point_conv", 'game_half', 'penalty', 'penalty_type', 'play_deleted', 
                   'aborted_play', 'series_result', 'play_type_nfl', 'st_play_type', 'special', 'rush_attempt', 
                   'pass_attempt', 'series', 'series_success', 'end_clock_time', 'end_yard_line', 
                   'order_sequence', 'away_score', 'home_score', 'location', 'surface', 'result', 
                   'total', 'spread_line', 'success', 'home_coach', 'away_coach', 'rusher', 
                   'receiver', 'passer', 'name', 'pass', 'rush', 'fantasy', 'out_of_bounds', 
                   'play', 'pass_oe', 'cpoe', 'wpa', 'quarter_seconds_remaining', 'cp', 'wpa', 
                   'quarter_seconds_remaining', 'vegaswp')
  
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
  # epa_columns <- grep(pattern="_epa$", x=colnames(df), value=TRUE)
  
  # Returns all columns containing ep
  ep_columns <- grep(pattern="ep", x=colnames(df), value=TRUE)
  
  # Returns all columns containing "wp_"
  # 13 cols; keeps "wp"
  wp_columns <- grep(pattern="_wp", x=colnames(df), value=TRUE) 
  
  # Returns all column names containing the word "team"
  team_columns <- grep(pattern="team", x=colnames(df), value=TRUE) 
  
  # Returns all columns starting with the word "total".
  total_columns <- grep(pattern="^total_", x=colnames(df), value=TRUE)
  
  # Returns the name of all columns that end in "jersey_number"
  # 20 column names returned. 
  jersey_number_columns <-  grep(pattern="jersey_number$", x=colnames(df), value=TRUE)
  
  # Returns the name of all columns that start with "punt"
  # 9 column names returned
  punt_columns <- grep(pattern="^punt_", x=colnames(df), value=TRUE) 
  
  # Returns columns containing "yards"
  yards_columns <- grep(pattern="yards", x=colnames(df), value=TRUE) # 8 cols
  
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
  
  # Returns columns starting with "first"
  first_columns <- grep(pattern="^first_", x=colnames(df), value=TRUE) 
  
  # Grabbing a list of other assorted column names that need to be removed.
  additional_columns <- get_additional_columns_to_remove()
  
  # Combine all the above columns into a single vector
  columns_to_remove <- c(additional_columns, punt_columns, jersey_number_columns, 
                         ep_columns, name_columns, id_columns, numeric_team_columns,
                         kickoff_columns, fumble_columns, x_columns, lateral_columns, 
                         touchdown_columns, tackle_columns, conversion_columns, quarterback_columns,
                         drive_columns, team_columns, total_columns, wp_columns, team_columns,
                         yards_columns, first_columns)
  
  # Remove all columns found in the columns_to_remove vector
  df <- df[,!(names(df) %in% columns_to_remove)]
  
  return(df)
}

generate_new_features <- function(df){
  
  # Generate the home field advantage feature
  # home_field_adv = 1 if the home team has possession
  # home_field_adv = 0 if the home team doesn't have possession
  # df <- df %>% mutate(home_field_adv = if_else(home_team == posteam, 1, 0))
  
  df[,"vegaswp"] <- df[,"vegas_wp"]
  
  df <- create_play_proportions_per_game(df)
  
  return(df)
}

fillin_missing_values <- function(df){
  
  # Fillin missing values in the "wind" column with zero
  df[is.na(df[,"wind"]),"wind"] <- 0
  
  # Fillin missing values in the "temp" column with 75
  df[is.na(df[,"temp"]), "temp"] <- 75
  
  # Fillin missing values in "roof" with "retractable"
  df[is.na(df[,"roof"]), "roof"] <- 'retractable'
  
  #Fillin missing value in "cp" column with the mean
  mean_cp <- mean(df[!is.na(df[,"cp"]),"cp"])
  df[is.na(df[,"cp"]),"cp"] <- mean_cp
  
  df[,"pos_to"] <- df[,"posteam_timeouts_remaining"]
  
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


get_numeric_variables <- function(){
  
  numeric_variables <- c("yardline_100", "ydstogo", "fg_prob", "td_prob", "wp", "vegaswp", "temp", "wind", "pos_to",
                         "half_seconds_remaining", "game_seconds_remaining", "score_differential", "pt_run_props", "pt_pass_props")
  
  return(numeric_variables)
}

get_categorical_variables <- function(){
  
  categorical_variables <- c("goal_to_go", "div_game", "roof", "down", "play_type")
  
  return(categorical_variables)
}

set_datatypes <- function(df){
  
  numeric_vars <- get_numeric_variables()
  
  for(numeric_index in 1:length(numeric_vars)){
    
    numeric_variable_name <- numeric_vars[[numeric_index]]
    
    if(numeric_variable_name %in% names(df)){
      df[,numeric_variable_name] <- as.numeric(df[,numeric_variable_name])  
    }
    
  }
  
  categorical_vars <- get_categorical_variables()
  
  for(categorical_index in 1:length(categorical_vars)){
    
    categorical_variable_name <- categorical_vars[[categorical_index]]
    
    if(categorical_variable_name %in% names(df)){
      df[,categorical_variable_name] <- factor(df[,categorical_variable_name])  
    }
    
  }
  
  return(df)
}


create_play_proportions_per_game <- function(df){
  
  # Sort rows decreasing by game_seconds_remaining, within each game_id group
  df <- df[order(df$game_id, df$game_seconds_remaining, decreasing=TRUE),]
  
  # List of unique game_ids
  unique_games <- unique(df[,"game_id"])
  
  # Initialize the possession team run and pass proportions to -1
  # these get filled in, inside the loops
  df[,"pt_run_props"] <- -1
  df[,"pt_pass_props"] <- -1
  
  # For each game
  for(game_index in 1:length(unique_games)){
    
    # Grab the game_id
    current_game_id <- unique_games[game_index]
    
    # Create a dataframe containing on the plays for this game
    game_df <- df[df[,"game_id"] == current_game_id, c("posteam", "play_type", "home_team", "away_team")]
    
    # Variables to track home team plays
    home_team <- game_df[1,"home_team"]
    home_team_play_count <- 0
    home_team_pass_count <- 0
    home_team_run_count <- 0
    
    # Variables to track away team plays
    away_team <- game_df[1,"away_team"]
    away_team_play_count <- 0
    away_team_pass_count <- 0
    away_team_run_count <- 0
    
    # Vectors to track running play proportions
    # for the current possessing team
    pos_team_pass_proportions <- c()
    pos_team_run_proportions <- c()
    
    # For each play in the game
    for(play_index in 1:nrow(game_df)){
      
      # Grab the name of the team that has the ball, and the play they ran
      possession_team <- game_df[play_index, "posteam"]
      play_type <- game_df[play_index, "play_type"]
      
      # If the home team has the ball, update the home team
      # variables to account for this play
      if(possession_team == home_team){
        
        home_team_play_count <- home_team_play_count + 1
        
        if(play_type == "pass"){
          home_team_pass_count <- home_team_pass_count + 1
        }else if(play_type == "run"){
          home_team_run_count <- home_team_run_count + 1
        }
        
        # Update the running proportions with the current home team proportions
        pos_team_pass_proportions <- append(x=pos_team_pass_proportions, values=home_team_pass_count/home_team_play_count)
        pos_team_run_proportions <- append(x=pos_team_run_proportions, values=home_team_run_count/home_team_play_count)
        
        # Else if the away team has the ball
      }else if(possession_team == away_team){
        
        away_team_play_count <- away_team_play_count + 1
        
        if(play_type == "pass"){
          away_team_pass_count <- away_team_pass_count + 1
        }else if(play_type == "run"){
          away_team_run_count <- away_team_run_count + 1
        }
        
        pos_team_pass_proportions <- append(x=pos_team_pass_proportions, values=away_team_pass_count/away_team_play_count)
        pos_team_run_proportions <- append(x=pos_team_run_proportions, values=away_team_run_count/away_team_play_count)
      }
    }
    
    df[df[,"game_id"] == current_game_id, "pt_run_props"] <- pos_team_run_proportions
    df[df[,"game_id"] == current_game_id, "pt_pass_props"] <- pos_team_pass_proportions
    
  }
  
  return(df)
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




get_table_location <- function(table_loc){
  
  if(table_loc == "upper_right"){
    x_coord <- Inf
    y_coord <- Inf
  }
  else if(table_loc == "lower_right"){
    x_coord <- Inf
    y_coord <- -Inf
    
  }
  else if(table_loc == "upper_left"){
    x_coord <- -Inf
    y_coord <- Inf
    
  }
  else if(table_loc == "lower_left"){
    x_coord <- -Inf
    y_coord <- -Inf
  }
  
  coords <- list(x_coordinate=x_coord, 
                 y_coordinate=y_coord)
  
  return(coords)
  
}


add_two_sample_test_and_combine <- function(df, left_plot, right_plot, binary_variable, continuous_variable, round_digits, test_type, exact, conf_level, 
                                            add_means_test_table, means_test_table_loc, means_table_plot, fig_height, fig_width){
  
  
  if(!add_means_test_table){
    return(gridExtra::grid.arrange(left_plot, right_plot, nrow=1, ncol=2, heights=c(fig_height), widths=c(fig_width, fig_width)) )
  }
  
  annot_df <- get_two_sample_test_annot_df(df=df, 
                                           binary_variable=binary_variable, 
                                           continuous_variable=continuous_variable, 
                                           round_digits=round_digits, 
                                           test_type=test_type, 
                                           exact=exact, 
                                           conf_level=conf_level)
  
  
  table_coordinates <- get_table_location(table_loc=means_test_table_loc)
  
  if(means_table_plot == "Left"){
    left_plot <- left_plot + 
      annotate(geom="table", 
               x=table_coordinates[["x_coordinate"]], 
               y=table_coordinates[["y_coordinate"]], 
               label=list(annot_df), 
               table.rownames=TRUE)  
  }else{
    right_plot <- right_plot + 
      annotate(geom="table", 
               x=table_coordinates[["x_coordinate"]], 
               y=table_coordinates[["y_coordinate"]], 
               label=list(annot_df), 
               table.rownames=TRUE)  
    
  }
  
  
  combined_p <- gridExtra::grid.arrange(gridExtra::arrangeGrob(left_plot, widths=c(fig_width)), gridExtra::arrangeGrob(right_plot, widths=c(fig_width)), 
                                        nrow=1, ncol=2, heights=c(fig_height), widths=c(fig_width, fig_width))
  
  return(combined_p)
}


get_two_sample_test_annot_df <- function(df, binary_variable, continuous_variable, round_digits, test_type, exact, conf_level){
  
  two_sample_test <- get_two_sample_test_results(df=df, 
                                                 binary_variable=binary_variable, 
                                                 continuous_variable=continuous_variable, 
                                                 round_digits=round_digits, 
                                                 test_type=test_type, 
                                                 exact=exact, 
                                                 conf_level=conf_level)
  
  if(test_type == "students_t" | test_type == "welches_t"){
    
    
    row_names <- c("diff_means", "t_stat", "p_value", "upr_ci", "lwr_ci")
    
    annot_df <- data.frame(matrix(ncol = 1, nrow = length(row_names)))
    
    rownames(annot_df) <- row_names
    
    colnames(annot_df) <- two_sample_test$test_type
    
    annot_df[,two_sample_test$test_type] <- c(two_sample_test$est_diff_means, 
                                              two_sample_test$test_stat, 
                                              two_sample_test$p_value, 
                                              two_sample_test$upper_ci, 
                                              two_sample_test$lower_ci)
    
  }else if(test_type == "rank_sum"){
    
    row_names <- c("t_stat", "p_value", "upr_ci", "lwr_ci")
    
    annot_df <- data.frame(matrix(ncol = 1, nrow = length(row_names)))
    
    rownames(annot_df) <- row_names
    
    colnames(annot_df) <- two_sample_test$test_type
    
    annot_df[,two_sample_test$test_type] <- c(two_sample_test$test_stat, 
                                              two_sample_test$p_value, 
                                              two_sample_test$upper_ci, 
                                              two_sample_test$lower_ci)
    
    
  }
  
  return(annot_df)
  
}

get_two_sample_test_results <- function(df, binary_variable, continuous_variable, round_digits, test_type="students_t", 
                                        exact=FALSE, conf_level=0.95) {
  
  unique_values <- levels(df[,binary_variable])
  value1 <- unique_values[1]
  value2 <- unique_values[2]
  
  group1 <- df[df[binary_variable] == value1, continuous_variable]
  group2 <- df[df[binary_variable] == value2, continuous_variable]
  
  if(test_type == "students_t" | test_type == "welches_t"){
    
    # T-test
    return_list <- get_ttest_results(group1=group1, group2=group2, conf_level=conf_level, test_type=test_type, round_digits=round_digits)
    
    
  }else if(test_type=="rank_sum"){
    
    return_list <- get_rank_sum_results(group1=group1, group2=group2, conf_level=conf_level, test_type=test_type, round_digits=round_digits, exact=exact)
    
  }
  
  return(return_list)
  
}


get_rank_sum_results <- function(group1, group2, conf_level, exact, round_digits, test_type){
  
  # Ranksum test
  test <- wilcox.test(x=group1, y=group2, conf.level=conf_level, exact=exact, conf.int=TRUE)
  
  p_value <- round(test$p.value, round_digits)
  test_stat <- round(test$statistic, round_digits)
  lower_ci <- round(test$conf.int[1], round_digits)
  upper_ci <- round(test$conf.int[2], round_digits)
  est <- round(test$estimate, round_digits)
  est_diff_means <- NULL
  
  txt <- paste0("T-stat = ", test_stat, "\n", 
                "P-value = ", p_value, "\n",
                "CI: (", lower_ci, ", ", 
                upper_ci, ")")
  
  return_list <- list(test_type=test_type,
                      estimate=est, 
                      p_value=p_value, 
                      test_stat=test_stat, 
                      est_diff_means=est_diff_means, 
                      lower_ci=lower_ci, 
                      upper_ci=upper_ci, 
                      text=txt)
  
  return(return_list)
}

get_ttest_results <- function(group1, group2, conf_level, test_type, round_digits){
  
  if(test_type == "students_t"){
    test <- t.test(x=group1, y=group2, var.equal=TRUE, conf.level=conf_level)  
    
  }else if(test_type == "welches_t"){
    test <- t.test(x=group1, y=group2, var.equal=FALSE, conf.level=conf_level)
  }
  
  p_value <- round(test$p.value, round_digits)
  test_stat <- round(test$statistic, round_digits)
  lower_ci <- round(test$conf.int[1], round_digits)
  upper_ci <- round(test$conf.int[2], round_digits)
  est <- round(test$estimate, round_digits)
  est_diff_means <- round(est[["mean of x"]] - est[["mean of y"]], round_digits)
  
  
  txt <- paste0("Est. Diff Means = ", est_diff_means, "\n", 
                "T-stat = ", test_stat, "\n", 
                "P-value = ", p_value, "\n",
                "CI: (", lower_ci, ", ", 
                upper_ci, ")")
  
  return_list <- list(test_type=test_type,
                      estimate=est, 
                      p_value=p_value, 
                      test_stat=test_stat, 
                      est_diff_means=est_diff_means, 
                      lower_ci=lower_ci, 
                      upper_ci=upper_ci, 
                      text=txt)
  
  return(return_list)
}

calculate_summary_stats <- function(df, column_name){
  
  summary_vector <- c(min(df[,column_name]), 
                      quantile(df[,column_name], probs=c(0.25)), 
                      mean(df[,column_name]), 
                      median(df[,column_name]), 
                      quantile(df[,column_name], probs=c(0.75)), 
                      max=max(df[,column_name]))
  
  
  summary_stat_names <- c("min", "mean", "median", "max")
  
  summary_df <- data.frame(summary_stats=summary_vector)
  rownames(summary_df) <- summary_stat_names
  
  return(summary_df)
  
}



add_summary_stats_table <- function(p, df1, lvl1, df2, lvl2, continuous_variable, table_loc, round_digits){
  
  #annot_table <- calculate_summary_stats(df=df, column_name=x_var)
  
  
  summary_stat_names <- c(paste0(lvl1, "_min"), 
                          paste0(lvl2, "_min"),
                          paste0(lvl1, "_mean"), 
                          paste0(lvl2, "_mean"),
                          paste0(lvl1, "_median"), 
                          paste0(lvl2, "_median"),
                          paste0(lvl1, "_max"), 
                          paste0(lvl2, "_max"))
  
  summary_vector <- c(min(df1[,continuous_variable]), 
                      min(df2[,continuous_variable]),
                      mean(df1[,continuous_variable]), 
                      mean(df2[,continuous_variable]),
                      median(df1[,continuous_variable]), 
                      median(df2[,continuous_variable]),
                      max=max(df1[,continuous_variable]), 
                      max=max(df2[,continuous_variable]))
  
  summary_vector <- round(summary_vector, round_digits)
  
  summary_df <- data.frame(summary_stats=summary_vector)
  rownames(summary_df) <- summary_stat_names
  
  table_coordinates <- get_table_location(table_loc=table_loc)
  
  
  p <- p + 
    annotate(geom="table", 
             x=table_coordinates[["x_coordinate"]], 
             y=table_coordinates[["y_coordinate"]], 
             label=list(summary_df), 
             table.rownames=TRUE)
  
  return(p)
  
}




# =================================================== END PLOTTING FUNCTIONS ===================================================







check_first_down_features <- function(df, play_type_to_check="run"){
  
  if(play_type_to_check == "run"){
    first_down_column = "first_down_rush"
  }else if(play_type_to_check == "pass"){
    first_down_column = "first_down_pass"
  }
  
  filepath <- paste0("./", play_type_to_check, "_first_down_col_check.txt")
  
  # Previous iterations value of first_down_column
  prev_down_col_value <- -1 
  
  # Number of times first_down_column = 1
  total_down_col_counts <- 0
  
  play_type_disagreements <- 0
  play_type_agreements <- 0
  play_type_disagreement_rows <- c()
  
  down_number_agreements <- 0
  down_number_disagreements <- 0
  down_number_disagreement_rows <- c()
  
  total_iterations <- 0
  
  for(row_index in 1:nrow(df)){
    
    total_iterations <- total_iterations + 1
    
    # Current value of first_down_column
    curr_down_col_value <- df[row_index, first_down_column]
    
    # Current value of play_type
    current_play_type <- df[row_index, "play_type"]
    
    # current down
    current_down <- df[row_index, "down"]
    
    # LOOKING FOR PLAY TYPE DISAGREEMENTS
    # If we ever have a mismatch on the play_type run and first_down_column result
    # Example: if we ever had first_down_pass = 1 but play_type wasn't pass, track that here. 
    if(curr_down_col_value == 1 & current_play_type != play_type_to_check){
      
      # Increment number of times where first_down_column=1 but the play wasn't of the same play_type
      play_type_disagreements <- play_type_disagreements + 1
      play_type_disagreement_rows <- append(x=play_type_disagreement_rows, values=row_index)
      
      # Increment the total number of times where first_down_column=1
      total_down_col_counts <- total_down_col_counts + 1
      
    }else if(curr_down_col_value == 1 & current_play_type == play_type_to_check){
      
      # No disagreement case
      # Increment the total number of times where first_down_column=1 and number of agreements
      total_down_col_counts <- total_down_col_counts + 1
      play_type_agreements <- play_type_agreements + 1
      
    }
    
    # DOWN NUMBER DISAGREEMENT CHECK
    # If in the previous iteration of the loop, we had first_down_column = 1, check if this next down is first down
    # 
    # Occurs when previous iteration had first_down_col=1 but this iteration is not a first down
    if(prev_down_col_value == 1 & current_down != 1){
      down_number_disagreements <- down_number_disagreements +1
      down_number_disagreement_rows <- append(x=down_number_disagreement_rows, values=row_index)
      
    }else if(prev_down_col_value == 1 & current_down == 1){
      down_number_agreements <- down_number_agreements + 1
    }
    
    prev_down_col_value <- curr_down_col_value
  }
  
  cat("=========================================\n", file=filepath)
  cat("         FIRST DOWN COLUMN --> ", play_type_to_check, "          \n", file=filepath, append=TRUE)
  cat("\n=========================================\n\n", file=filepath, append=TRUE)
  cat("Total instances where first_down_", play_type_to_check, "=1: ", total_down_col_counts, "\n\n", file=filepath, append=TRUE) 
  
  cat("Total instances where the associated play_type WAS ", play_type_to_check, ": ", play_type_agreements, "\n", file=filepath, append=TRUE)
  cat("Total instances where the associated play_type WAS NOT ", play_type_to_check, ": ", play_type_disagreements, "\n", file=filepath, append=TRUE)
  cat("Associated Row Numbers", play_type_disagreement_rows, "\n\n", file=filepath, append=TRUE)
  
  cat("Total instances where the NEXT play WAS a first down: ", down_number_agreements, "\n", file=filepath,  append=TRUE)
  cat("Total instances where the NEXT play WAS NOT a first down: ", down_number_disagreements, "\n", file=filepath,  append=TRUE)
  cat("Associated Row Numbers", down_number_disagreement_rows, "\n\n", file=filepath, append=TRUE)
  
  return(list(play_type_disagreements=play_type_disagreements, 
              down_number_disagreements=down_number_disagreements, 
              total_down_col_counts=total_down_col_counts,
              total_iterations=total_iterations))
}

