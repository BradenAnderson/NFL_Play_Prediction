source("./NFL_Project_Function_Helpers.R")


load_clean_data <- function(start_year=2018, end_year=2021, save_path=NULL){
  
  
  # load play by play data
  # 2018- 2021 has 195134 rows, 372 columns
  df <- as.data.frame(nflfastR::load_pbp(start_year:end_year))
  
  # Filter on row-based conditions. e.g., downs=1,2,3, play_type="run","pass"
  df <- apply_row_filters(df)
  
  # Generate new features (e.g. home_field_adv)
  df <- generate_new_features(df)
  
  # Filter based on column-based conditions
  df <- apply_column_filters(df)
  
  
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
  
  if(sorted ){
    
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



# =================================================== END PLOTTING FUNCTIONS ===================================================
