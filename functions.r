## Load necessary library
library(dplyr)
library(INLA)
# library(ggplot2)
# library(reshape2)
# library(lubridate)
# library(Metrics)


############################## Load dataset ##############################
df_features <- read.csv("features_2013-2020_ML.csv",header=TRUE)
## Calculate the implied probability
df_features$implied_pro <- df_features %>%
  group_by(race_id) %>%
  mutate(implied_pro = (1/win_odds)/sum((1/win_odds))) %>%
  pull(implied_pro)

############################## Functions ##############################
#### Construct RPS function
calculate_rps <- function(data,win_col) {
  r <- nrow(data)
  rps <- 0
  
  for (i in 1:(r-1)) {
    sum_inner <- 0
    for (j in 1:i) {
      # sum_inner <- sum_inner + (data[j,win_col] - data$won[j])
      sum_inner <- sum_inner + (data$win_mean_pred[j] - data$won[j])
    }
    rps <- rps + sum_inner^2
  }
  
  rps <- rps / (r - 1)
  return(rps)
  }
  
  
#### Construct RPS function
calculate_rps_IP <- function(data) {
  r <- nrow(data)
  rps <- 0
  
  for (i in 1:(r-1)) {
    sum_inner <- 0
    for (j in 1:i) {
      sum_inner <- sum_inner + (data$implied_pro[j] - data$won[j])
    }
    rps <- rps + sum_inner^2
  }
  
  rps <- rps / (r - 1)
  return(rps)
  }
  


#### Calculate the win rate in multinomial model
calculate_win_rate <- function(df, column_name) {
  df[,c(column_name,"race_id")] %>%
  group_by(race_id) %>%
    mutate(
      # max_value = max(cur_data()),
      normalized_value = abs(max(cur_data()) - cur_data()),
      exp_value = exp(normalized_value),
      win_rate = exp_value / sum(exp_value)
    ) %>%
    pull(win_rate)
}
  
#### Write a function to calculate the posterior win probability
post.win.probabiliy <- function(nsamp, result_model, type, df_pred){
  nsamp=nsamp;
  race.samples=inla.posterior.sample(n=nsamp, result=result_model)
    
  predictor_mean <- matrix(unlist(lapply(race.samples,function(x){
    x$latent[(nrow(train_data)+1):nrow(df_pred)]})),nrow=nrow(test_data),ncol=nsamp)
    
  if (type == "linear"){
    theta.samples=matrix(unlist(lapply(race.samples, function(x)(x$hyperpar[1]))),nrow=1,ncol=nsamp)
    sigma.samples=1/sqrt(theta.samples)
    post.pred.samples=predictor_mean+matrix(rnorm(n=nsamp*nrow(test_data),mean=0,sd=sigma.samples),nrow=nrow(test_data),ncol=nsamp)
    
    mean_predictions<- result_model$summary.fitted.values$mean[(nrow(train_data)+1):nrow(df_pred)]
      
    # Create a matrix to store the outcome
    win_matrix <- matrix(0,nrow=nrow(post.pred.samples), ncol=ncol(post.pred.samples))
    # train_won <- df_features$won[1:nrow(train_data)]
      
    for (i in 1:ncol(post.pred.samples)) {
      # Reshape the predicted velocities to match the races and horses
      df_pred_vel <- data.frame(race_id = df_pred$race_id[(nrow(train_data)+1):nrow(df_pred)], velocity = post.pred.samples[, i])
      max_velocity_per_race <- df_pred_vel %>%
        group_by(race_id) %>%
        summarize(max_velocity = max(velocity))
        
      # Join back to get the win indicator
      df_pred_vel <- df_pred_vel %>%
        left_join(max_velocity_per_race, by = "race_id") %>%
        mutate(is_winner = ifelse(velocity == max_velocity, 1, 0))
        
      # Fill the win_matrix
      win_matrix[, i] <- df_pred_vel$is_winner      
    }

    return(rowMeans(win_matrix))

  }else{
    predictor_samples_race = cbind(predictor_mean, df_pred$race_id[(nrow(train_data)+1):nrow(df_pred)])

    df_predictor_samples_race <- as.data.frame(predictor_samples_race)
    num_cols <- ncol(df_predictor_samples_race)

    # add column names
    colnames(df_predictor_samples_race) <- paste0("V", 1:num_cols)
    colnames(df_predictor_samples_race)[ncol(df_predictor_samples_race)] <- "race_id"

    # calculate the win probability
    for (col_name in paste0("V", 1:nsamp)) {
      df_predictor_samples_race[[paste0("win_rate_", col_name)]] <- calculate_win_rate(df_predictor_samples_race, col_name)
    }
    
    win_prob <- df_predictor_samples_race %>% 
      select(starts_with("win_rate_"))
    win_prob_mean <- rowMeans(win_prob)

    return(win_prob_mean)
  }   
}
  
  
#### Write a function to generate the rps for each model
generate_rps <- function(runtime,nsamp,type,result_model,data,times) {
  # Try more sampling numbers with for loop.
  runtime <- runtime
  nsamp <- nsamp
    
  results_list <- vector("list", runtime)
    
  for (i in 1:runtime) {
    results_list[[i]] <- post.win.probabiliy(nsamp,result_model,type,data)
  }
    
  post_win_pred <- do.call(cbind, results_list)
  
  # save the intermediate file
  filename_inter <- paste0("inter/",type,"_", times, ".csv")
  write.csv(post_win_pred, filename_inter, row.names = FALSE)
  # inter_linear[[times]] <- df_row_mean
    
  # Add the mean win probabilities to the original data
  data$win_mean_pred[(nrow(train_data)+1):nrow(df_pred)] <- rowMeans(post_win_pred)
    
  df_rps <- data[(nrow(train_data)+1):nrow(df_pred),c("race_id","won","win_mean_pred")]
  rps_result <- df_rps %>%
    group_by(race_id) %>%
    summarise(rps = calculate_rps(cur_data()))
  
  return(rps_result)
}
  

#### Function to generate the ACE results
calculate_ACE <- function(df, confidence_col, prediction_col, actual_col, race_col, num_bins = 10) {
  # Group the data by race.\
  # Find the max confidence in each race, and check if the horse with max confidence is the one who won.
  df <- df %>%
    group_by(!!sym(race_col)) %>%
    mutate(max_prob = max(!!sym(confidence_col)),
           pred_winner = ifelse(!!sym(confidence_col) == max_prob, !!sym(prediction_col), NA)) %>%
    ungroup() %>%
    mutate(is_max_prob_winner = ifelse(!is.na(pred_winner) & (!!sym(prediction_col) == pred_winner), 1, 0))
  
  # Find the record for the horse that win the race.
  df_summary <- df %>%
    filter(!is.na(pred_winner)) %>%
    select(pred_winner, !!sym(actual_col), max_prob) %>%
    rename(w = !!sym(actual_col), h = pred_winner, p = max_prob)
  
  # Order the record by P
  df_summary <- df_summary %>%
    arrange(p)
  
  # make sure the number of horses in each bin is similar.
  bin_size <- ceiling(nrow(df_summary) / num_bins)
  
  # Initialization 
  ece <- 0
  bin_details <- data.frame(bin = integer(), avg_confidence = numeric(), avg_accuracy = numeric(), bin_proportion = numeric(), abs_error = numeric())
  
  # Travel every bins
  for (i in 1:num_bins) {
    # Obtain the data in this bin
    start_index <- (i - 1) * bin_size + 1
    end_index <- min(i * bin_size, nrow(df_summary))
    bin_data <- df_summary[start_index:end_index, ]
    
    # If there is data in the bin
    if (nrow(bin_data) > 0) {
      # Compute the mean confidence
      avg_confidence <- mean(bin_data$p)
     
      # Compute the average accuracy
      avg_accuracy <- mean(bin_data$w)
      
      # Compute the proportion of samples in this bin
      bin_proportion <- nrow(bin_data) / nrow(df_summary)
      
      # Compute the absolute value of the error
      abs_error <- abs(avg_confidence - avg_accuracy)
      
      # accumulate the weighted abs error
      ece <- ece + bin_proportion * abs_error
      
      # save the details of this bin
      bin_details <- rbind(bin_details, data.frame(bin = i, avg_confidence = avg_confidence, avg_accuracy = avg_accuracy, bin_proportion = bin_proportion, abs_error = abs_error))
    }
  }
  
  print(bin_details) # print the details for every bins
  
  return(ece)
}

#### Function to choose the train set and test set
select_data <- function(data, test_race_size = 65, train_race_size = 3500) {
  # obtain all race IDs
  unique_races <- unique(data$race_id)
  
  # obtain all number of race
  total_races <- length(unique_races)
  
  # initial the result list
  results <- list()
  
  # compute the number of tiem for traveling all data
  num_iterations <- floor((total_races - test_race_size) / test_race_size)
  
  for (i in 0:num_iterations) {
    # Obtain the start day and end day for the test set
    test_end <- total_races - i * test_race_size
    test_start <- test_end - test_race_size + 1
    
    # Obtain the race IDs in test set
    test_races <- unique_races[test_start:test_end]
    
    # Generate the test set
    test_data <- data %>% filter(race_id %in% test_races)
    
    # Obtain the start day and end day for the train set
    train_end <- test_start - 1
    train_start <- max(1, train_end - train_race_size + 1)
    
    # Obtain the race IDs in test set
    train_races <- unique_races[train_start:train_end]
    
    # Generate the train set
    train_data <- data %>% filter(race_id %in% train_races)
    
    # Save the sub-set in result list
    results[[i + 1]] <- list(test_data = test_data, train_data = train_data)
  }
  
  return(results)
}


#### Data pre-processing function
data_pre_process <- function(train_data, test_data,type){
  # deal with the historical winning rate
  last_win_rate_horse <- train_data %>%
    group_by(horse_id) %>%
    slice_tail(n = 1) %>%
    ungroup()
  last_win_rate_horse_vector <- setNames(last_win_rate_horse$horse_win_rate, last_win_rate_horse$horse_id)
  last_win_rate_jockey <- train_data %>%
    group_by(jockey_id) %>%
    slice_tail(n = 1) %>%
    ungroup()
  last_win_rate_jockey_vector <- setNames(last_win_rate_jockey$jockey_win_rate, last_win_rate_jockey$jockey_id)
  
  test_data$jockey_win_rate <- NA
  test_data$horse_win_rate <- NA
  
  # update winning rate
  for (i in 1:nrow(test_data)) {
    horse <- test_data$horse_id[i]
    if (horse %in% names(last_win_rate_horse_vector)) {
      test_data$horse_win_rate[i] <- last_win_rate_horse_vector[horse]
    } else {
      test_data$horse_win_rate[i] <- NA
    }
    jockey <- test_data$jockey_id[i]
    if (jockey %in% names(last_win_rate_jockey_vector)) {
      test_data$jockey_win_rate[i] <- last_win_rate_jockey_vector[jockey]
    } else {
      test_data$jockey_win_rate[i] <- NA
    }
  }
  
  # use mean imputation
  mean_win_rate_horse <- mean(train_data$horse_win_rate, na.rm = TRUE)
  test_data$horse_win_rate[is.na(test_data$horse_win_rate)] <- mean_win_rate_horse
  mean_win_rate_jockey <- mean(train_data$jockey_win_rate, na.rm = TRUE)
  test_data$jockey_win_rate[is.na(test_data$jockey_win_rate)] <- mean_win_rate_jockey
  
  if(type=="linear"){
    test_data$velocity = NA
  }else{
    test_data$won = NA
  }
  
  df_pred = rbind(train_data,test_data)
  
  df_pred$track_condition = as.factor(df_pred$track_condition)
  df_pred$horse_class <- as.factor(df_pred$horse_class)
  df_pred$horse_id <- as.factor(df_pred$horse_id)
  df_pred$jockey_id <- as.factor(df_pred$jockey_id)
  df_pred$trainer_id <- as.factor(df_pred$trainer_id)
  
  return(df_pred)
}


#### 
generate_mean_win_probabilities <- function(runtime,nsamp,type,result_model,data,times) {
  # Try more sampling numbers with for loop.
  runtime <- runtime
  nsamp <- nsamp
  
  results_list <- vector("list", runtime)
  
  for (i in 1:runtime) {
    results_list[[i]] <- post.win.probabiliy(nsamp,result_model,type,data)
  }
  
  # Combine the posterior winning probabilities that is generated from all run time 
  post_win_pred <- do.call(cbind, results_list)
  
  # Return the mean of winning probabilities
  return(rowMeans(post_win_pred))
}

# ############################## Test the ACE function ##############################
# # calculate the ECE for each models
# folder_path <- "./inter/four years new winning probability"
# file_list <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)
# 
# 
# row_mean <- function(file) {
#   df <- read.csv(file)
#   rowMeans(df, na.rm = TRUE)
# }
# 
# mean_list <- lapply(file_list, row_mean)
# 
# combined_means <- do.call(cbind, mean_list)
# 
# combined_means_df <- as.data.frame(combined_means)
# combined_means_df$won <- tail(df_features$won, nrow(combined_means_df))
# combined_means_df$race_id <- tail(df_features$race_id, nrow(combined_means_df))
# 
# 
# # obtain the column to deal with
# value_columns <- paste0("V", 1:7)
# # handle each column
# for (col in value_columns) {
#   combined_means_df <- combined_means_df %>%
#     group_by(race_id) %>%
#     mutate(!!paste0(col, "_won") := ifelse(!!sym(col) == max(!!sym(col)), 1, 0))
#   ace_value <- calculate_ACE(combined_means_df, col, paste0(col, "_won"), "won","race_id")
#   cat("The ACE value is",ace_value,"\n")
# }
# # ece_value <- calculate_ACE(combined_means_df, "V1", "V1_won", "won","race_id")
# 
# ######################## RPS ########################
# for (col in value_columns) {
#   df_rps <- combined_means_df[,c("race_id","won",col)]
#   rps_result <- df_rps %>%
#     group_by(race_id) %>%
#     summarise(rps = calculate_rps(cur_data(),col))
#   cat("The the mean of RPS is",mean(rps_result$rps),"\n")
# }
# 
# ######################## check ECE by implied probability ########################
# implied_test <- data.frame(
#   implied_prob = df_features$implied_pro,
#   won = df_features$won,
#   race_id = df_features$race_id
# )
# 
# 
# implied_test <- implied_test %>%
#   group_by(race_id) %>%
#   mutate(implied_prob_won = ifelse(implied_prob == max(implied_prob), 1, 0))
# 
# ece_test <-calculate_ACE(implied_test, "implied_prob", "implied_prob_won", "won","race_id")
# print(ece_test)
