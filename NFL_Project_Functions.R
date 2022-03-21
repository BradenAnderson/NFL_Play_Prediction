source("./NFL_Project_Function_Helpers.R")


load_clean_data <- function(start_year=2018, end_year=2021, save_path=NULL){
  
  
  # load play by play data
  # 2018- 2021 has 195134 rows, 372 columns
  df <- as.data.frame(nflfastR::load_pbp(start_year:end_year))
  
  # Filter on row-based conditions. e.g., downs=1,2,3, play_type="run","pass"
  df <- apply_row_filters(df)
  
  # Fillin missing values
  df <- fillin_missing_values(df)
  
  # Generate new features (e.g. home_field_adv)
  df <- generate_new_features(df)
  
  # Filter based on column-based conditions
  df <- apply_column_filters(df)
  
  
  # Leave this operation at the end.
  # Set all variables to the appropriate datatype
  df <- set_datatypes(df)
  
  # Save the clean dataset to a compressed .csv file
  full_save_path <- save_dataset_to_file(df=df, 
                                         save_path=save_path, 
                                         start_year=start_year, 
                                         end_year=end_year)
  
  
  return(df)
}


# =================================================== PLOTTING FUNCTIONS ===================================================

plot_categorical_proportions <- function(df, categorical_predictor, target_column="play_type", target_level="pass", 
                                         hline_color="red", hline_linetype="dashed", round_digits=3, 
                                         txt_hjust=0, txt_vjust=-0.2, x_rotation=0, sorted=TRUE){
  
  
  
  # Adds temporary columns to the dataframe for:
  # 1. the categorical predictor, that we will plot proportions for each level of.
  # 2. the target column, converted to binary. 1 --> taget=target_level, else 0
  df <- add_cat_proportions_plot_columns(df=df, 
                                         categorical_predictor=categorical_predictor, 
                                         target_column=target_column, 
                                         target_level=target_level)
  
  # Calculate the proportion of instances where target_column=target_level, for each level of categorical_predictor.
  proportion_cis <- get_proportion_cis(df=df)
  
  if(sorted){
    
    # Order the dataframe according to the proportion estimates.
    proportion_cis <- proportion_cis[order(proportion_cis[,"proportion_estimates"]),]
    
    # Reset the levels of the x-axis factor to the new plotting order.
    proportion_cis[,"categorical_predictor"] <- factor(x=proportion_cis[,"categorical_predictor"],
                                                       levels=unique(proportion_cis[,"categorical_predictor"]))
  }
  
  
  # Calculated as --> # of rows where target_column=target_level / total number of rows.
  overall_prop <- as.numeric(proportion_cis[1,"overall_proportion"])
  
  
  # Generate a title and axis labels for the plot
  plot_title <- paste0("Proportion of plays where ", target_column, "=",  target_level, 
                       "\nfor each level of ", categorical_predictor )
  plot_ylabel <- paste0("Proportion of plays where ", target_column, "=", target_level)
  plot_xlabel <- categorical_predictor
  
  
  
  p <- ggplot(data=proportion_cis) +
    geom_point(mapping=aes(x=categorical_predictor, y=proportion_estimates)) + 
    geom_hline(yintercept=overall_prop, color=hline_color, linetype=hline_linetype) +
    geom_errorbar(aes(x=categorical_predictor, ymin=lower_ci, ymax=upper_ci), width=.1) +
    ggtitle(plot_title) + 
    xlab(plot_xlabel) + 
    ylab(plot_ylabel) +
    theme(axis.text.x = element_text(angle=x_rotation, face="bold"))
  
  p <- p + geom_text(aes(x=-Inf, y=overall_prop, label=round(overall_prop, round_digits)), 
                     hjust=txt_hjust, vjust=txt_vjust, color=hline_color)
  
  
  return(p)
  
}


plot_histogram <- function(df, continuous_variable, fill_color="#bc5090",  
                           outline_color="#003f5c", num_bins=NULL, binwidth=NULL,
                           title=NULL, table_loc="upper_right", add_table=FALSE){
  
  
  df[,"continuous_var"] <- df[,continuous_variable]
  
  
  if(is.null(title)){
    plot_title <-  paste0("Distribution of ", stringr::str_to_title(continuous_variable))   
  }
  else{
    plot_title <- title
  }
  
  
  p <- ggplot(data=df, mapping=aes(x=continuous_var)) +
    geom_histogram(binwidth=binwidth, bins=num_bins, fill=fill_color, color=outline_color) + 
    ggtitle(plot_title) +
    ylab(paste0("Distribution of ", stringr::str_to_title(continuous_variable))) +
    xlab(stringr::str_to_title(continuous_variable))
  
  
  if(add_table){
    p <- add_table_to_plot(p=p, df=df, x_var=continuous_variable, table_loc=table_loc)   
  }
  
  
  return(p)
}


# Function for plotting side by side histograms of some continuous feature, for each level of the binary target.
# Function includes parameters to run a two-sample test (t-test or rank sum) comparing the mean value of the
# continuous feature for each level of the binary target.
plot_histogram_by_target_level <- function(df, continuous_variable, binary_target="play_type", fill_color_left="#bc5090", outline_color_left="#003f5c", 
                                           fill_color_right="#bc5090", outline_color_right="#003f5c", num_bins_left=NULL, binwidth_left=NULL, 
                                           num_bins_right=NULL, binwidth_right=NULL, title_left=NULL, title_right=NULL, summary_table_loc="upper_right",
                                           summary_table_plot="Right", round_digits=5, exact=FALSE, conf_level=0.95,test_type="students_t", add_means_test_table=TRUE, 
                                           means_table_plot="Left", means_test_table_loc="upper_left", fig_width=10, fig_height=5){
  
  
  if(length(levels(df[,binary_target])) != 2){
    return(print(paste0("This function can only be used when ", binary_target, " is a factor with two levels.")))
  }
  
  lvls <- levels(df[, binary_target])
  
  if(is.null(title_left)){
    title_left <- paste0("Distribution of ", stringr::str_to_title(continuous_variable), " for ", binary_target, "=", lvls[1])  
  }
  
  if(is.null(title_right)){
    title_right <- paste0("Distribution of ", stringr::str_to_title(continuous_variable), " for ", binary_target, "=", lvls[2])  
  }
  
  lvl_left <- lvls[1]
  df_left <- df[df[,binary_target]==lvl_left,]
  
  lvl_right <- lvls[2]
  df_right <- df[df[,binary_target]==lvl_right,]
  
  # Plot for the first level
  left_plot <- plot_histogram(df=df_left, 
                              continuous_variable=continuous_variable, 
                              fill_color=fill_color_left, 
                              outline_color=outline_color_left, 
                              num_bins=num_bins_left,
                              binwidth=binwidth_left,
                              table_loc=NULL,
                              add_table=FALSE, 
                              title=title_left)
  
  
  
  # Plot for the second level
  right_plot <- plot_histogram(df=df_right, 
                               continuous_variable=continuous_variable, 
                               fill_color=fill_color_right, 
                               outline_color=outline_color_right, 
                               num_bins=num_bins_right,
                               binwidth=binwidth_right,
                               table_loc=NULL,
                               add_table=FALSE, 
                               title=title_right)
  
  if(summary_table_plot != FALSE){
    
    if(summary_table_plot == "Right"){
      
      right_plot <- add_summary_stats_table(p=right_plot, 
                                            df1=df_left, 
                                            lvl1=lvl_left, 
                                            df2=df_right, 
                                            lvl2=lvl_right, 
                                            continuous_variable=continuous_variable, 
                                            table_loc=summary_table_loc, 
                                            round_digits=round_digits)  
    }else{
      
      left_plot <- add_summary_stats_table(p=left_plot, 
                                            df1=df_left, 
                                            lvl1=lvl_left, 
                                            df2=df_right, 
                                            lvl2=lvl_right, 
                                            continuous_variable=continuous_variable, 
                                            table_loc=summary_table_loc, 
                                           round_digits=round_digits)  
    }
  }
  
  
  
  p <- add_two_sample_test_and_combine(df=df, 
                                       left_plot=left_plot, 
                                       right_plot=right_plot, 
                                       binary_variable=binary_target, 
                                       continuous_variable=continuous_variable, 
                                       round_digits=round_digits, 
                                       test_type=test_type, 
                                       exact=exact, 
                                       conf_level=conf_level,
                                       add_means_test_table=add_means_test_table,
                                       means_test_table_loc=means_test_table_loc, 
                                       means_table_plot=means_table_plot,
                                       fig_height=fig_height,
                                       fig_width=fig_width)
  
  return(p)
  
}


# =================================================== END PLOTTING FUNCTIONS ===================================================
