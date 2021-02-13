# Tidytuesday Data  - August 2019
# Data from Stockholm International Peace Research Institute
# Version 1.0
# Date: 12.02.2021
# Björn Düsenberg

# Load libraries
library(tidyverse) 
library(tidymodels)
library(vip) # Library for the determination of the importance of variables

# Import Data
nuclear_explosions <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")

# write.csv(nuclear_explosions, "nuclear_explosions.csv")

### Get an overview over the Data

head(nuclear_explosions)

print(nuclear_explosions %>% count(type, sort = T))
print(nuclear_explosions %>% count(purpose, sort = T))
print(nuclear_explosions %>% count(country, sort = T))

### Manipulate Data ###
nucl_exp_df <- nuclear_explosions %>% 
  transmute(type = case_when(str_detect(type, "SHAFT")~"SHAFT",
                             str_detect(type, "TUNNEL")~"TUNNEL",
                             str_detect(type, "ATMOSPH")~"ATMOSPH",
                             str_detect(type, "AIRDROP")~"AIRDROP",
                             str_detect(type, "TOWER")~"TOWER",
                             str_detect(type, "BALLOON")~"BALLOON",
                             str_detect(type, "UG")~"UNDERGROUND",
                             TRUE ~ "Other"),
            purpose = case_when(str_detect(purpose, "SE")~"SE", # Safety explosion
                                str_detect(purpose, "PNE")~"PNE", # Peaceful Nuclear Explosion
                                str_detect(purpose, "WR")~"WR", # Weapon research
                                str_detect(purpose, "WE")~"WE", # Weapon evaluation
                                TRUE ~ "Other"),
            country, latitude, longitude, depth, region, yield_lower, yield_upper) %>%
  mutate_if(is.character, factor) %>%
  na.omit(nucl_exp_df)

# Overview - Showing where the different types of explosions took place
world <- map_data("world")

nu_ex_map_type <- ggplot() +
  geom_map(data = world, map = world,
           aes(long, lat, map_id = region),
           alpha = 0.5) + 
  geom_point(data = nucl_exp_df, aes(longitude, latitude, color = type),
             alpha = 0.8, size = 3) +
  xlab("Longitude") + 
  ylab("Latitude") +
  theme_bw()

plot(nu_ex_map_type)

### Build a model ###

# Bootstrap resampling
nu_ex_boot <- bootstraps(nucl_exp_df)

# Create recipe

type_recipe <- recipe(type ~ ., data = nucl_exp_df) %>%
  step_other(region) %>%
  step_dummy(region, purpose, country) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())

# Prepare recipe

type_prep <- prep(type_recipe)

# Model

rf_type <- rand_forest(trees = 1000) %>%
  set_mode("classification") %>%
  set_engine("ranger")

# Workflow 

type_wf <- workflow() %>%
  add_recipe(type_recipe) %>%
  add_model(rf_type)

# Fit resampling

type_res <- fit_resamples(
  type_wf,
  resamples = nu_ex_boot,
  control = control_resamples(save_pred = T,
                              verbose = T)
)

### Explore results ###

# Show the accuracy and ROC-AUC of the model
type_met <- type_res %>% 
  collect_metrics() 

print(type_met)

# Create a confusion matrix 
type_pred <- type_res %>%
  collect_predictions() %>%
  conf_mat(type, .pred_class)

print(type_pred)

### Model understanding ###

# Importance of variables
rf_importance <- rf_type %>%
  set_engine("ranger", importance = "permutation") %>%
               fit(
                 type ~ .,
                 data = juice(type_prep)
               ) %>%
  vip(geom = "point")

plot(rf_importance)

# --> Latitude and Longitude are the most important variables

### Showing where the predictions where correct ###
correct_pred <- type_res %>%
  collect_predictions() %>%
  mutate(correct = type == .pred_class) %>%
  left_join(nucl_exp_df %>% mutate(.row = row_number()))
  
nucl_expl_map_predicted <- ggplot() +
  geom_map(data = world, map = world,
           aes(long, lat, map_id = region),
           alpha = 0.5) + 
  stat_summary_hex(data = correct_pred,
                   aes(x = longitude, y = latitude, 
                       z = as.integer(correct)),
                   fun = "mean",
                   alpha = 0.7, bins = 60) +
  scale_fill_gradient(high = "cyan3") +
  xlab("Longitude") + 
  ylab("Latitude") +
  theme_bw()

plot(nucl_expl_map_predicted)
  










  