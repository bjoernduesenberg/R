# Tidytuesday Data  - February 2020
# Version 1.1
# Date: 16.02.2021
# Björn Düsenberg

# Load libraries
library(tidyverse) 
library(tidymodels)
library(skimr)
library(GGally)

# Data is from following github repo:
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-02-11


# Script according to Julia Silges screencast
# https://www.youtube.com/watch?v=dbXDkEEuvCU&t=2762s

# Prediction of hotel bookings with/without children

# Load data
hotels <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv')

# write.csv(hotels, "hotels.csv")

# Take a first look
View(hotels)

hotel_stays <- hotels %>%
  filter(is_canceled == 0) %>% # Every row in which nobody has cancelled the reservation
  mutate(children = case_when(children + babies > 0 ~ "children", # if children + babies > 0 --> children
                              T ~ "none"), # no further distinction
         required_car_parking_spaces = case_when(required_car_parking_spaces > 0 ~ "parking",
                                                 T ~ "none")) %>% # parking or none
  select(-is_canceled, -reservation_status, -babies) # delete selected coloumns



skim(hotel_stays)
# --> means are extremely different in the features

### EDA Plots

# Overall
hotel_stays %>%
  mutate(arrival_date_month = factor(arrival_date_month, # Make arrival_date_month to factor
                                     levels = month.name)) %>%
  count(arrival_date_month, children) %>% # count how much children came and not came per month
  group_by(children) %>% 
  mutate(proportion = n / sum(n)) %>% # n is the number from the groupby function -> calculate proportion
  ggplot(aes(arrival_date_month, proportion, fill = children)) + 
  geom_col(position = "dodge") + 
  scale_y_continuous(labels = scales::percent_format()) + 
  theme_bw()

# By hotel
hotel_stays %>%
  mutate(arrival_date_month = factor(arrival_date_month, # Make arrival_date_month to factor
                                     levels = month.name)) %>%
  count(hotel, arrival_date_month, children) %>% # count how much children came and not came per month
  group_by(hotel, children) %>% 
  mutate(proportion = n / sum(n)) %>% # n is the number from the groupby function -> calculate proportion
  ggplot(aes(arrival_date_month, proportion, fill = children)) + 
  geom_col(position = "dodge") + 
  scale_y_continuous(labels = scales::percent_format()) + 
  facet_wrap(~hotel) +
  theme_bw()

# Parking
hotel_stays %>%
  count(hotel, required_car_parking_spaces, children) %>% # count how much children came and not came per month
  group_by(hotel, children) %>% 
  mutate(proportion = n / sum(n)) %>% # n is the number from the groupby function -> calculate proportion
  ggplot(aes(required_car_parking_spaces, proportion, fill = children)) + 
  geom_col(position = "dodge") + 
  scale_y_continuous(labels = scales::percent_format()) + 
  facet_wrap(~hotel) +
  theme_bw()

# Try GGally - Finding Pairs

hotel_stays %>%
  select(children, adr,
         required_car_parking_spaces,
         total_of_special_requests) %>%
  ggpairs(mapping = aes(color = children))

# Model building

hotels_df <- hotel_stays %>% # Modelling dataframe
  select(children, hotel, arrival_date_month, meal, adr, adults,
         required_car_parking_spaces, total_of_special_requests,
         stays_in_week_nights, stays_in_weekend_nights) %>%
  mutate_if(is.character, factor)

set.seed(1234)
split <- initial_split(hotels_df)

train <- training(split)
test <- testing(split)

### Modelling with recipes - preprocessing and feature engineering

# Recipe

hotel_rec <- recipe(children ~ ., data = train) %>%   # predict children with everything else
  themis::step_downsample(children) %>% # themis package has additional steps for recipes -> downsampling
  step_dummy(all_nominal(), -all_outcomes()) %>% # One-Hot-Encoding for every factor except children
  step_zv(all_numeric()) %>% # take everything out with zero veriance
  step_normalize(all_numeric()) %>% # normalize all numerics 0-1
  prep()

# Bake

test_proc <- bake(hotel_rec, new_data = test)


### Model building

# K-Nearest Neighbors
knn_spec <- nearest_neighbor() %>%
  set_engine("kknn") %>%
  set_mode("classification")
  
knn_fit <- knn_spec %>%
  fit(children ~ ., data = juice(hotel_rec)) # juice is practically getting the complete information back from the downsampling

# Decision tree
tree_spec <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification")

tree_fit <- tree_spec %>%
  fit(children ~ ., data = juice(hotel_rec)) # juice is practically getting the complete information back from the downsampling

### Evaluate model

set.seed(1234)
valid_split <- mc_cv(juice(hotel_rec), prop = 0.9, strata = children) # monte-carlo crossvalidation

knn_res <- fit_resamples(
  knn_spec, # first input has to be the model
  children ~ ., 
  valid_split,
  control = control_resamples(save_pred = T)
)

knn_res %>%
  collect_metrics()

tree_res <- fit_resamples(
  tree_spec, # first input has to be the model
  children ~ ., 
  valid_split,
  control = control_resamples(save_pred = T)
)

tree_res %>%
  collect_metrics()

### Visualizations

knn_res %>%
  unnest(.predictions) %>% # unnest the tibble with the predictions
  mutate(model = "kknn") %>% 
  bind_rows(tree_res %>%
              unnest(.predictions) %>% # unnest the tibble with the predictions
              mutate(model = "rpart")) %>%
  group_by(model) %>%
  roc_curve(children, .pred_children) %>%
  autoplot()

# gray line in the middle is guessing. Upper left is better than guessing, down right is worse

# Confusion matrix

knn_res %>%
  unnest(.predictions) %>%
  conf_mat(children, .pred_class) %>%
  autoplot() # type = "heatmap" is as input possible

tree_res %>%
  unnest(.predictions) %>%
  conf_mat(children, .pred_class) %>%
  autoplot()

# Application on test data

knn_fit %>%
  predict(new_data = test_proc, type = "prob") %>%
  mutate(truth = test$children) %>%
  roc_auc(truth, .pred_children)

tree_fit %>%
  predict(new_data = test_proc, type = "prob") %>%
  mutate(truth = test$children) %>%
  roc_auc(truth, .pred_children)

  
  