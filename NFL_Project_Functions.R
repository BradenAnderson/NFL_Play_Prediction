source("./NFL_Project_Function_Helpers.R")


load_clean_data <- function(start_year=2018, end_year=2021, save_path=NULL, remove_run_props=FALSE, 
                            remove_pass_props=TRUE, save_dataset=FALSE){
  
  
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
  df <- apply_column_filters(df, 
                             remove_run_props=remove_run_props, 
                             remove_pass_props=remove_pass_props)
  
  
  # Leave this operation at the end.
  # Set all variables to the appropriate datatype
  df <- set_datatypes(df)
  
  if(save_dataset){
  
    # Save the clean dataset to a compressed .csv file
    full_save_path <- save_dataset_to_file(df=df, 
                                           save_path=save_path, 
                                           start_year=start_year, 
                                           end_year=end_year)
  }
  
  
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



plot_scree <- function(df, features, scale=TRUE, prop_var_color="#00BFC4", prop_var_linetype="dashed", cum_var_color="#F8766D", 
                       cum_var_linetype="dashed", round_digits=3, prop_txt_vjust=-0.2, prop_txt_hjust=-0.1, cum_txt_vjust=-0.75, 
                       cum_txt_hjust=0.8, annotate_text=TRUE, add_table=TRUE, extend_x_by=1, table_x=Inf, table_y=Inf,
                       title="Proportion of Variance Explained by Principal Component", xlabel="Principal Component",
                       ylabel="Proportion of Variance Explained"){
  
  X <- df[,features]
  X <- as.matrix(X)
  pca <- stats::prcomp(X, scale=scale)
  
  sqrt_eigen_values <- pca$sdev
  
  prop_variance_explained <- sqrt_eigen_values^2/sum(sqrt_eigen_values^2)
  cum_prop_variance_explained <- cumsum(sqrt_eigen_values^2/sum(sqrt_eigen_values^2))
  
  num_components <- length(prop_variance_explained)
  
  plot_df <- data.frame(prop_variance_explained=prop_variance_explained, 
                        cum_variance_explained=cum_prop_variance_explained,
                        principal_component=seq(from=1, to=num_components, by=1),
                        prop_var_exp_round=round(prop_variance_explained, round_digits),
                        cum_var_exp_round=round(cum_prop_variance_explained, round_digits))
  
  
  p <- ggplot(data=plot_df) + 
    geom_line(mapping=aes(x=principal_component, y=prop_variance_explained), color=prop_var_color, linetype=prop_var_linetype) +
    geom_point(mapping=aes(x=principal_component, y=prop_variance_explained), color=prop_var_color) + 
    geom_line(mapping=aes(x=principal_component, y=cum_variance_explained), color=cum_var_color, linetype=cum_var_linetype) + 
    geom_point(mapping=aes(x=principal_component, y=cum_variance_explained), color=cum_var_color) + 
    scale_x_continuous(labels=paste0("PC",seq(from=1, to=num_components, by=1)),
                       breaks=seq(from=1, to=num_components, by=1)) +
    coord_cartesian(xlim=c(1, num_components+extend_x_by), clip='off') + 
    theme_minimal() +
    ggtitle(title) +
    xlab(xlabel) +
    ylab(ylabel)
  
  
  if(annotate_text){
    p <- p + 
      geom_text(mapping=aes(x=principal_component, y=prop_variance_explained, label=prop_var_exp_round), 
                color=prop_var_color, vjust=prop_txt_vjust, hjust=prop_txt_hjust) + 
      geom_text(mapping=aes(x=principal_component, y=cum_variance_explained, label=cum_var_exp_round), 
                color=cum_var_color, vjust=cum_txt_vjust, hjust=cum_txt_hjust)
    
  }
  
  if(add_table){
    table_df <- data.frame(prop=plot_df[,"prop_var_exp_round"],
                           cumm=plot_df[,"cum_var_exp_round"])
    
    p <- p + annotate(geom="table", 
                      x=table_x, 
                      y=table_y, 
                      label=list(table_df))
  }
  
  return(p)
  
}




# Measure --> can be "Gain", "Cover" or "Frequency"
plot_lgbm_feature_importance <- function(model, relative_percentages=FALSE, measure="Gain", num_features=15L){
  
  feature_importances <- lightgbm::lgb.importance(model=model, percentage = relative_percentages)
  
  feature_importances_plot <- lightgbm::lgb.plot.importance(tree_imp=feature_importances,
                                                            measure=measure,
                                                            top_n=num_features)
  
  return(feature_importances_plot)
}


# =================================================== END PLOTTING FUNCTIONS ===================================================


cv_added_complexity_models <- function(df, base_model_features, candidate_interaction_features, target="play_type", seed_number=42, 
                                       caret_cv_method="cv", caret_cv_folds=5, save_every=5, base_save_name="./added_compelxity_search",
                                       num_jobs=4){
  
  
  all_feature_sets <- generate_all_model_feature_sets(base_model_features=base_model_features,
                                                      candidate_interaction_features=candidate_interaction_features)
  
  iterations_counter <- 1
  
  for(model_index in 1:length(all_feature_sets)){
    
    set.seed(seed_number)  
    
    model_formula <- as.formula(paste0(target, "~",  stringr::str_c(all_feature_sets[[model_index]], collapse=" + ")))
    
    
    #cluster <- makePSOCKcluster(num_jobs)
    #registerDoParallel(cluster)
    
    logreg_cv <- caret::train(form=model_formula,
                              data=df,
                              trControl=caret::trainControl(method=caret_cv_method, number=caret_cv_folds),
                              method="glm",
                              family="binomial")
    
    
    #stopCluster(cluster)
    
    cv_result_df <- logreg_cv$results
    
    
    cv_result_df[1,"parameter"] <- stringr::str_c(all_feature_sets[[model_index]], collapse=" ")
    
    
    if(model_index == 1){
      final_result_df <- cv_result_df
    }else{
      final_result_df <- rbind(final_result_df, cv_result_df)
    }
    
    # Save the current grid search results
    if(iterations_counter %% save_every == 0){
      save_path <- paste0(base_save_name, "_iter_", iterations_counter, ".csv")
      write.csv(x=final_result_df, file=save_path, row.names=FALSE)
    }
    
    # Increment total iterations performed, keep this at the end
    iterations_counter <-  iterations_counter + 1 
    
  }
  
  final_result_df <- final_result_df[order(final_result_df[,"Accuracy"], decreasing=TRUE),]
  
  save_path <- paste0("added_complexity_search_FINAL", "_iter_", iterations_counter, ".csv")
  write.csv(x=final_result_df, file=save_path, row.names=FALSE)
  
  return(final_result_df)
}



# =================================================== MODELING FUNCTIONS ===================================================

get_dataset_splits <- function(df, seed=42, train_pct=0.8, test_pct=0.1, valid_pct=0.1){
  
  index <- splitTools::partition(df$play_type, p=c(train=train_pct, test=test_pct, valid=valid_pct), seed=seed)
  
  train <- df[index$train,]
  test <- df[index$test,]
  valid <- df[index$valid,]
  
  cross_validation <- rbind(train, valid)
  
  return(list(cross_validation=cross_validation,
              train=train,
              test=test,
              valid=valid))
  
}


get_categorical_feature_names <- function(df, target_column="play_type"){
  
  feature_classes <- sapply(df[,names(df) != target_column], class)
  
  categorical_feature_names <- names(feature_classes[feature_classes == "factor"])
  
  return(categorical_feature_names)
  
}


build_features_matrix_and_label <- function(df, target_column="play_type", conversion_rules=NULL){
  
  # Dataframe with features converted to a format that lightGBM accepts.
  lgb_prepared <- lightgbm::lgb.convert_with_rules(data=df)
  
  # The features to train the LightGBM model, in dataframe format
  features_df <- lgb_prepared$data[,!(names(lgb_prepared$data) %in% target_column)]
  
  # The label column for the classification task. 
  # Since the label has two classes, the lgb.convert_with_rules will have converted this column
  # to contain the values 1 and 2. However, for binary classification, lightGBM needs the target
  # to be zeros and ones. Therefore, we subtract 1 from this column before creating the lightGBM data set.
  #
  label = lgb_prepared$data[,target_column] - 1L # Subtract 1 to get the labels to be 0 and 1 instead of 1 and 2.
  
  # Convert the features dataframe to a matrix.
  features_matrix <- as.matrix(features_df)
  
  return(list(features_matrix=features_matrix,
              label=label,
              rules=lgb_prepared$rules))
  
}


create_lgb_dataset <- function(df, target_column="play_type", conversion_rules=NULL){
  
  
  prepared_data <- build_features_matrix_and_label(df=df, target_column=target_column, conversion_rules=conversion_rules)
  
  # Create the LightGBM data set.
  lgb_dataset <- lightgbm::lgb.Dataset(data=prepared_data$features_matrix, 
                                       label=prepared_data$label,
                                       categorical_feature=get_categorical_feature_names(df=df))
  
  return(list(ds=lgb_dataset,
              rules=prepared_data$rules))
}


### LightGBM Cross validation and gridsearch functions 

create_lgb_cv_datasets <- function(df, num_folds, target_column="play_type", cv_type="stratified"){
  
  folds <- splitTools::create_folds(y=df[,target_column], 
                                    k=num_folds, 
                                    type=cv_type, 
                                    use_names=FALSE)
  
  # List where the keys will be of the format "fold<foldnum>"
  # and the values will be all the required data to conducting training
  # and testing activities for that fold
  dataset_folds <- list()
  
  dataset_conversion_rules <- NULL
  
  for(fold_index in 1:length(folds)){
    
    # Rows to train on for this fold
    train_rows <- folds[[fold_index]]
    
    # Training and test dataframes
    train_df <- df[train_rows,]
    test_df <- df[-train_rows,]
    
    
    lgb_training_data <- create_lgb_dataset(df=train_df, 
                                            target_column=target_column,
                                            conversion_rules=dataset_conversion_rules)
    
    # On the first iteration of the loop, save the data set conversion rules. This allows us to ensure that
    # the data sets for all other folds will be created using the same rules. 
    if(fold_index == 1){
      dataset_conversion_rules <- lgb_training_data$rules
    }
    
    # A lightGBM dataset containing the training data and training labels
    train_lgb_dataset <- lgb_training_data$ds
    
    # Get the training data and training labels in matrix format.
    # This is useful for getting prediction metrics for the training set.
    train_data_matrix <- build_features_matrix_and_label(df=train_df, 
                                                         target_column=target_column,
                                                         conversion_rules=dataset_conversion_rules)
    
    # The features and label for the training set, in matrix format 
    train_matrix <- train_data_matrix$features_matrix
    train_labels <- train_data_matrix$label
    
    # Test data in matrix format, used for calculating test metrics
    test_data_matrix <- build_features_matrix_and_label(df=test_df, 
                                                        target_column=target_column,
                                                        conversion_rules=dataset_conversion_rules)
    
    # The features and label for the TEST set, in matrix format 
    test_matrix <- test_data_matrix$features_matrix
    test_labels <- test_data_matrix$label
    
    # Save all of the information required to perform training and testing on this fold.
    fold_data <- list(lgb_train_ds=train_lgb_dataset,
                      train_features_matrix=train_matrix,
                      train_labels=train_labels,
                      test_features_matrix=test_matrix,
                      test_labels=test_labels)
    
    # Key for indexing into the list containing all required dataset information for each fold.
    fold_key <- paste0("fold", fold_index)
    
    # Update the list that contains data for each fold.
    dataset_folds[[fold_key]] <- fold_data
  }
  
  return(dataset_folds)
  
}


get_cv_metric_names <- function(){
  return(c("accuracy", "auc", "precision", "recall", "logloss", "specificity"))
}


calculate_cv_fold_metrics <- function(model, fold_data, fold_number){
  
  
  # Predicted class probabilities on the training set
  predicted_probabilities_train <- predict(object=model, data=fold_data$train_features_matrix)
  
  # Actual predicted classes on the training set
  predicted_classes_train <- as.numeric(predicted_probabilities_train > 0.5)
  
  # Calculate the training set metrics for this fold
  fold_metrics_train <- calculate_fold_metrics(predicted_classes=predicted_classes_train,
                                               predicted_probabilities=predicted_probabilities_train,
                                               true_classes=fold_data$train_labels,
                                               metric_type="train",
                                               fold_number=fold_number)
  
  
  # Predicted class probabilities on the test set
  predicted_probabilities_test <- predict(object=model, data=fold_data$test_features_matrix)
  
  # Actual predicted classes on the test set
  predicted_classes_test <- as.numeric(predicted_probabilities_test > 0.5)
  
  
  # Calculate the test set metrics for this fold
  fold_metrics_test <- calculate_fold_metrics(predicted_classes=predicted_classes_test,
                                              predicted_probabilities=predicted_probabilities_test,
                                              true_classes=fold_data$test_labels,
                                              metric_type="test",
                                              fold_number=fold_number)
  
  
  all_fold_metrics <- cbind(fold_metrics_test, fold_metrics_train)
  
  return(all_fold_metrics)
  
}

calculate_fold_metrics <- function(predicted_classes, predicted_probabilities, true_classes, metric_type, fold_number, from_gridsearch=TRUE){
  
  
  # Metric names that indicate what metric is being calculate, the fold number it corresponds to, and 
  # the metric type (whether it is a training or test metric).
  metric_names <- get_cv_metric_names()
  
  if(from_gridsearch){ # If this function is being called from the gridsearch function
    full_metric_names <- paste0(metric_names, "_fold", fold_number, "_", metric_type)  
  }else{
    full_metric_names <- paste0(metric_names, "_", metric_type)
  }
  
  accuracy <- Metrics::accuracy(actual=true_classes,
                                predicted=predicted_classes)
  
  auc <- Metrics::auc(actual=true_classes,
                      predicted=predicted_probabilities)
  
  precision <- Metrics::precision(actual=true_classes,
                                  predicted=predicted_classes)
  
  
  recall <- Metrics::recall(actual=true_classes,
                            predicted=predicted_classes)  
  
  
  average_logloss <- Metrics::logLoss(actual=true_classes,
                                      predicted=predicted_probabilities)
  
  specificity <- InformationValue::specificity(actuals=true_classes,
                                               predictedScores=predicted_probabilities,
                                               threshold=0.5)
  
  # It is important that the columns are inserted into this dataframe in the same
  # order that the metric names appear in full_metric_names
  metrics_row <- data.frame(acc=accuracy,
                            auc=auc,
                            prec=precision,
                            recall=recall,
                            ll=average_logloss,
                            specificity=specificity)
  
  # Set the column names
  colnames(metrics_row) <- full_metric_names
  
  return(metrics_row)
}

cross_validate_lgbm <- function(cv_datasets, train_params){
  
  for(fold_number in 1:length(cv_datasets)){
    
    # Key for this fold, used to index into the cv_datasets list
    fold_key <- paste0("fold", fold_number)
    
    # Grab all the information required to train and test the model using 
    # the data for this cross validation fold. 
    fold_data <- cv_datasets[[fold_key]]
    
    # Train the model using the lightGBM training dataset for this fold.
    model <- lightgbm::lightgbm(data=fold_data$lgb_train_ds, 
                                params=train_params)
    
    
    # Calculate the training and test metrics for this fold  
    fold_metrics <- calculate_cv_fold_metrics(model=model, 
                                              fold_data=fold_data, 
                                              fold_number=fold_number)
    
    
    if(fold_number == 1){
      full_cv_metrics_row <- fold_metrics
    }else{
      full_cv_metrics_row <- cbind(full_cv_metrics_row, fold_metrics)
    }
    
  }
  
  complete_metrics_row <- add_avg_cv_value_per_metric(metrics_row=full_cv_metrics_row)
  
  return(complete_metrics_row)
}

add_avg_cv_value_per_metric <- function(metrics_row){
  
  # Vector of all metric names calculated for each fold and each set (train, test)
  metric_names <- get_cv_metric_names()
  
  # Name of all columns
  all_column_names <- names(metrics_row)
  
  for(metric_index in 1:length(metric_names)){
    
    # Get the name of this particular metric
    metric_name <- metric_names[metric_index]
    
    # Get the name of all columns that pertain to this metric
    metric_column_names <- grep(pattern=paste0("^", metric_name), x=all_column_names, value=TRUE)
    
    # All columns pertaining to this metric and the training set
    train_column_names <- grep(pattern="train", x=metric_column_names, value=TRUE)
    
    # Average value for this metric on the training set
    avg_train_metric <- rowMeans(metrics_row[,train_column_names])
    
    # New column name for the average value of this metric on the training set
    avg_metric_new_col_name_train <- paste0("avg_", metric_name, "_train")
    
    # All columns pertaining to this metric and the training set
    test_column_names <- grep(pattern="test", x=metric_column_names, value=TRUE)
    
    # Average value for this metric on the test set (across all the cv folds).
    avg_test_metric <- rowMeans(metrics_row[,test_column_names])
    
    # New column name for the average value of this metric on the test set
    avg_metric_new_col_name_test <- paste0("avg_", metric_name, "_test")
    
    # Add columns for the average value of this metric on the train and test set
    metrics_row[,avg_metric_new_col_name_train] <- avg_train_metric
    metrics_row[,avg_metric_new_col_name_test] <- avg_test_metric
    
  }
  
  return(metrics_row)
}

get_lgbm_training_parameters <- function(objective, metric, boosting_type, learning_rate, num_boosting_rounds, 
                                         num_leaves,num_threads, seed, feature_fraction, min_data_in_leaf, 
                                         extra_trees, force_row_wise, max_depth){
  
  train_params <- list(objective=objective,
                       metric=metric,
                       boosting=boosting_type,
                       force_row_wise=force_row_wise,
                       learning_rate=learning_rate,
                       num_rounds=num_boosting_rounds, 
                       num_leaves=num_leaves,
                       feature_fraction=feature_fraction,
                       max_depth=max_depth,
                       num_threads=num_threads,
                       min_data_in_leaf=min_data_in_leaf,
                       extra_trees=extra_trees,
                       seed=seed)
  
  return(train_params)
}

get_default_param_grid <- function(){
  
  default_param_grid <- list(boosting=c("gbdt", "dart"),
                             learning_rate=c(0.1),
                             num_rounds=c(100L), 
                             num_leaves=c(31),
                             min_data_in_leaf=c(20),
                             feature_fraction=c(1.0),
                             max_depth=c(-1),
                             extra_trees=c(FALSE, TRUE))
  return(default_param_grid)
}

add_lgbm_params_to_row <- function(metrics_row, boosting, learning_rate, num_rounds, num_leaves, min_data_in_leaf, feature_fraction, max_depth, extra_trees){
  
  param_row <- data.frame(boosting=boosting, 
                          learning_rate=learning_rate, 
                          num_rounds=num_rounds, 
                          num_leaves=num_leaves, 
                          min_data_in_leaf=min_data_in_leaf,
                          feature_fraction=feature_fraction,
                          max_depth=max_depth,
                          extra_trees=extra_trees)
  
  combined_row <- cbind(param_row, metrics_row)
  
  return(combined_row)
}

clean_gs_results_df <- function(df, sort_by="avg_accuracy_test"){
  
  cols_to_remove <- grep(pattern="_fold", x=names(df), value=TRUE)
  
  clean_gs_df <- df[,!(names(df) %in% cols_to_remove)]
  
  clean_gs_df <- clean_gs_df[order(clean_gs_df[,sort_by], decreasing = TRUE),]
  
  return(clean_gs_df)
}


gridsearch_lightgbm <- function(df, param_grid, num_folds=5, training_objective="binary", training_metric="binary_error", num_threads=5, 
                                seed=42, force_row_wise=TRUE, save_every=2, base_save_name="./lgbm_gridsearch"){
  
  # All datasets for k-fold cross validation on each set of parameters
  all_ds <- create_lgb_cv_datasets(df=df, num_folds=num_folds)
  
  iterations_counter <- 1

  for(extra_trees_index in 1:length(param_grid[["extra_trees"]])){
    for(boosting_index in 1:length(param_grid[["boosting"]])){
      for(num_rounds_index in 1:length(param_grid[["num_rounds"]])){
        for(num_leaves_index in 1:length(param_grid[["num_leaves"]])){
          for(min_data_in_leaf_index in 1:length(param_grid[["min_data_in_leaf"]])){
            for(max_depth_index in 1:length(param_grid[["max_depth"]])){
              for(feature_fraction_index in 1:length(param_grid[["feature_fraction"]])){
                for(learning_rate_index in 1:length(param_grid[["learning_rate"]])){
                  
                  # Unpack the hyper-parameters to use on this iteration
                  boosting_type <- param_grid[["boosting"]][boosting_index]
                  learning_rate <- param_grid[["learning_rate"]][learning_rate_index]
                  num_rounds <- param_grid[["num_rounds"]][num_rounds_index]
                  num_leaves <- param_grid[["num_leaves"]][num_leaves_index]
                  feature_fraction <- param_grid[["feature_fraction"]][feature_fraction_index]
                  min_data_in_leaf <- param_grid[["min_data_in_leaf"]][min_data_in_leaf_index]
                  extra_trees <- param_grid[["extra_trees"]][extra_trees_index]
                  max_depth <- param_grid[["max_depth"]][max_depth_index]
                  
                  # Grab the current set of parameters
                  training_parameters <- get_lgbm_training_parameters(objective=training_objective,
                                                                      metric=training_metric,
                                                                      num_threads=num_threads,
                                                                      seed=seed,
                                                                      force_row_wise=force_row_wise,
                                                                      boosting_type=boosting_type,
                                                                      learning_rate=learning_rate,
                                                                      num_boosting_rounds=num_rounds,
                                                                      num_leaves=num_leaves,
                                                                      feature_fraction=feature_fraction,
                                                                      min_data_in_leaf=min_data_in_leaf,
                                                                      extra_trees=extra_trees,
                                                                      max_depth=max_depth)
                  
                  # Perform cross validation with this set of parameters
                  cv_metrics_row <- cross_validate_lgbm(cv_datasets=all_ds,
                                                        train_params=training_parameters)
                  
                  # Add the hyper-params used to the information about model performance
                  # on each cv fold
                  model_row <- add_lgbm_params_to_row(metrics_row=cv_metrics_row, 
                                                      boosting=boosting_type, 
                                                      learning_rate=learning_rate, 
                                                      num_rounds=num_rounds, 
                                                      num_leaves=num_leaves, 
                                                      min_data_in_leaf=min_data_in_leaf, 
                                                      feature_fraction=feature_fraction, 
                                                      max_depth=max_depth, 
                                                      extra_trees=extra_trees)
                  
                  # Update the current grid search results dataframe
                  if(iterations_counter == 1){
                    all_model_rows <- model_row
                  }else{
                    all_model_rows <- rbind(all_model_rows, model_row)
                  }
                  
                  # Save the current grid search results
                  if(iterations_counter %% save_every == 0){
                    save_path <- paste0(base_save_name, "_iter_", iterations_counter, ".csv")
                    write.csv(x=all_model_rows, file=save_path, row.names=FALSE)
                  }
                  
                  # Increment total iterations performed, keep this at the end
                  iterations_counter <-  iterations_counter + 1
                  
                }
              }
            }
          }
        }
      } 
    }
  }
  
  
  final_model_df_clean <- clean_gs_results_df(df=all_model_rows,
                                              sort_by="avg_accuracy_test")
  
  save_path <- paste0("./FINAL_sorted_gridsearch", "_iter_", iterations_counter, ".csv")
  write.csv(x=final_model_df_clean, file=save_path, row.names=FALSE)

  save_path <- paste0("./FINAL_sorted_gridsearch_Expanded", "_iter_", iterations_counter, ".csv")
  write.csv(x=all_model_rows, file=save_path, row.names=FALSE)

  # Return here
  return(final_model_df_clean)
}


calculate_holdout_set_performance <- function(model, test_df, metric_type, label="play_type", classification_threshold=0.5){
  
  test_data_prepared <- build_features_matrix_and_label(df=test_df)
  
  # Predicted class probabilities on the test set
  predicted_probabilities <- predict(object=model, 
                                     data=test_data_prepared$features_matrix)
  
  # Convert the probability predictions into class predictions
  predicted_classes <- as.numeric(predicted_probabilities > classification_threshold)
  
  test_metrics <- calculate_fold_metrics(predicted_classes=predicted_classes,
                                         predicted_probabilities=predicted_probabilities,
                                         true_classes=test_data_prepared$label,
                                         metric_type=metric_type,
                                         fold_number="N/A",
                                         from_gridsearch=FALSE)
  
  return(test_metrics)
  
}

# =================================================== END MODELING FUNCTIONS ===================================================







