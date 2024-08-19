library(dplyr)
library(INLA)
library(lubridate)
setwd("C:\\Users\\Daniel Paulin\\OneDrive - University of Edinburgh\\horse_racing")
df_features <- read.csv("features_2013-2020_ML.csv",header=TRUE)
source("functions.r")
## Calculate the implied probability
df_features <- df_features %>%
  group_by(race_id) %>%
  mutate(implied_pro = (1 / win_odds) / sum(1 / win_odds)) %>%
  ungroup()
df_features$date <- as.Date(df_features$date)
df_features <- df_features %>% arrange(date)
## Reorder the race id
df_features$race_id <- df_features$phi_id

##################### Find the number of race in each month #####################
library(tidyverse)
#df_races <- read.csv("F:\\ED-Dissertation\\Dataset\\2013-2020\\races.csv",header = TRUE)
df_races <- read.csv("./HK2013-2020/races.csv",header = TRUE)
df_races$date <- as.Date(df_races$date)
matches <- df_races %>%
  mutate(month = format(date, "%Y-%m"))
monthly_matches <- matches %>%
  group_by(month) %>%
  summarise(count = n())
ggplot(monthly_matches, aes(x = month, y = count)) +
  geom_bar(stat = "identity") +
  labs(title = "The Number of Race in Each Month", x = "Month", y = "Number of Race") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(summary(monthly_matches$count)) ## Min:22 Mean:72 Max: 90


##################### Formula for Each Models #####################
linear_formula_list <- list(
  #### Fixed Effect
  velocity~1+implied_pro+scale(declared_weight)+scale(actual_weight)+scale(draw)+horse_class+track_condition,
  velocity~1+implied_pro+scale(declared_weight)+scale(actual_weight)+scale(draw)+horse_class+track_condition+scale(horse_win_rate)+scale(jockey_win_rate),
  velocity~1+implied_pro+scale(declared_weight)+scale(actual_weight)+scale(draw)+horse_class+track_condition++scale(horse_win_rate)+scale(jockey_win_rate)+ICA_1+ICA_2+ICA_3,
  #### Random Effect
  velocity~1+implied_pro+scale(declared_weight)+scale(actual_weight)+scale(draw)+horse_class+track_condition+f(horse_id,model="iid")+f(jockey_id,model="iid")+f(trainer_id,model="iid"),
  velocity~1+implied_pro+scale(declared_weight)+scale(actual_weight)+scale(draw)+horse_class+track_condition+f(horse_id,model="iid")+f(jockey_id,model="iid")+f(trainer_id,model="iid") + ICA_1+ICA_2+ICA_3,
  velocity~1+implied_pro+scale(declared_weight)+scale(actual_weight)+scale(draw)+horse_class+track_condition+scale(horse_win_rate)+scale(jockey_win_rate)+f(horse_id,model="iid")+f(jockey_id,model="iid")+f(trainer_id,model="iid"),
  velocity~1+implied_pro+scale(declared_weight)+scale(actual_weight)+scale(draw)+horse_class+track_condition+scale(horse_win_rate)+scale(jockey_win_rate)+f(horse_id,model="iid")+f(jockey_id,model="iid")+f(trainer_id,model="iid") + ICA_1+ICA_2+ICA_3
)


##################### Select the Data #####################
# Select all subset 
subsets <- select_data(df_features, test_race_size = 70, train_race_size = 3500)
# about 
cat("There are", length(seq_along(subsets)), "subsets for training. \n")


##################### Train Models and Obtain the Mean value of Winning Probabilities #####################
set <-  1
for (i in seq_along(subsets)){
  #### Data pre-processing for training and testing
  train_data <- subsets[[i]]$train_data
  test_data <- subsets[[i]]$test_data
  
  # Generate the df_pred with data pre-processing function
  df_pred <- data_pre_process(train_data, test_data, type="linear") 
  df_pred$horse_id <- as.factor(df_pred$horse_id)
  df_pred$jockey_id <- as.factor(df_pred$jockey_id)
  df_pred$trainer_id <- as.factor(df_pred$trainer_id)
  
  # Initialize a list to save the winning probabilities mean for each model
  mean_win_prob <- list()
  models <- 0
  
  #### Generate the mean of winning probabilities for all models
  for (formula in linear_formula_list){
    # train the model
    # obtain the posterior mean of winning probabilities -- need to write a new function
    m.random.post <- inla(formula, data = df_pred,
                          family="gaussian",
                          control.compute=list(config = TRUE,dic=T,cpo=T,waic=T),
                          control.predictor = list(compute = TRUE))
    filename <- paste0("models/linear_model_", set,"_",models, ".rds")
    saveRDS(m.random.post, file = filename)
    
    type<-"linear"
    models <- models+1
    mean_win_prob[[models]] <- generate_mean_win_probabilities(100,100,type,m.random.post,df_pred,times)
    
    
  }
  mean_win_prob_df <- data.frame(mean_win_prob)
  write.csv(mean_win_prob_df, paste0("winning probabilities mean/linear_mean_win_prob_df",set,".csv"), row.names = FALSE)
  cat("Finish sub-set", set,"\n")
  set <- set+1
}

















