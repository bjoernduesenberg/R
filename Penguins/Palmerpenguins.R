# Tidytuesday Data  - July 2020
# Version 1.2
# Date: 18.02.2021
# Björn Düsenberg


# Link to the github-repo
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-07-28

# Instead of loading the data as .csv you can install the package
# install.packages("palmerpenguins")

# Load libraries
library(tidyverse)
library(tidymodels)
library(palmerpenguins)

# Create dataframe from penguins library
# Is not necessary - We could use "penguins"
data <- penguins

View(data)

# First look
data %>% count(species) # 3 species in this dataset
data %>% count(island) # 3 islands in this dataset
data %>% count(island, species) # the species are on different islands
data %>% count(sex) # We have NA values

### EDA ###

# Looking on bill_length / flipper_length for the 3 species
# Showing the differences between the sex 
# Showing the influence of the weight

data %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(flipper_length_mm, bill_length_mm, 
             color = sex, size = body_mass_g)) +
  geom_point(alpha = 0.8) +
  facet_wrap(~species)

# --> Females have smaller bills and flippers in general
# --> There is some overlap

### Model building ###

# Creating a new dataframe for prediction
# Delete rows wich have NA in sex
# Select all columns but year and island

penguinds_df <- data %>%
  filter(!is.na(sex)) %>%
  select(-year, -island)

# Set Seed
set.seed(555)

# Split the data into trainingset and testset
# strata = sex --> Make sure, that the proportion between M and F is the same
penguin_split <- initial_split(penguinds_df, strata = sex)

train <- training(penguin_split)
test <- testing(penguin_split)

# Because it is a pretty small dataset
# --> use Bootstrap resampling 

set.seed(456)
penguin_boot <- bootstraps(train)

# Model specification - FIRST STEP
# These specifications are dynamic - you can build up a lot of different ones

# --> logistic regression
glm_spec <- logistic_reg() %>%
  set_engine("glm")

# --> random forest for classification
rf_spec <- rand_forest() %>%
  set_mode("classification") %>%
  set_engine("ranger")

# Create workflow - needs a preprocessor and a model

penguin_workflow <- workflow() %>%
  add_formula(sex ~ .) # Preprocessor - in this case a formula predict sex on everything else
  
# We have specified 2 models (logistic regression and random forest)
# So we dont add the model directly into the workflow
# We fit the workflow and the model together with the bootstrap resamples

glm_rs <- penguin_workflow %>%
  add_model(glm_spec) %>%
  fit_resamples(resamples = penguin_boot,
                control = control_resamples(save_pred = T, verbose = T))

rf_rs <- penguin_workflow %>%
  add_model(rf_spec) %>%
  fit_resamples(resamples = penguin_boot,
                control = control_resamples(save_pred = T, verbose = T))

# We put the resamples and a control argument in each fit_resamples function

# save_pred - A logical for whether the out-of-sample predictions should be saved 
# for each model evaluated.

# verbose - A logical for logging results as they are generated. Despite this argument,
# warnings and errors are always shown. If using a dark IDE theme, some logging messages 
# might be hard to see. If this is the case, try setting the tidymodels.dark option
# with options(tidymodels.dark = TRUE) to print lighter colors.

### Evaluate modeling ###

# Looking at the accuracy of the models
collect_metrics(glm_rs)
collect_metrics(rf_rs)

# --> The models are practically the same but the linear model is faster and less complicated

# Building a confusion matrix

glm_rs %>%
  conf_mat_resampled()

rf_rs %>%
  conf_mat_resampled()

# Building the ROC-Curves

glm_rs %>%
  collect_predictions() %>%
  group_by(id) %>%
  roc_curve(sex, .pred_female) %>%
  ggplot(aes(1 - specificity, sensitivity, color = id)) +
  geom_path(show.legend = F, alpha = 0.8, size = 1.2) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  coord_equal() +
  theme_bw()

rf_rs %>%
  collect_predictions() %>%
  group_by(id) %>%
  roc_curve(sex, .pred_female) %>%
  ggplot(aes(1 - specificity, sensitivity, color = id)) +
  geom_path(show.legend = F, alpha = 0.8, size = 1.2) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  coord_equal() +
  theme_bw()

# --> In general: as further the colored lines are away (left side) from the gray in the 
#     middle, the better is the model. Are the lines right from the gray line, the mode
#     is worse. 
# --> Every line stands for a resample

### Test the models on testdata ### 

peng_final_test_glm <- penguin_workflow %>%
  add_model(glm_spec) %>%
  last_fit(penguin_split)

peng_final_test_rf <- penguin_workflow %>%
  add_model(rf_spec) %>%
  last_fit(penguin_split)

### Evaluate the models from the testdata ###

# --> GLM
peng_final_test_glm %>%
  collect_metrics()

peng_final_test_glm %>%
  collect_predictions() %>%
  conf_mat(sex, .pred_class)

peng_final_test_glm$.workflow[[1]] %>%
  tidy(exponentiate = T) %>%
  arrange(estimate)

# --> RF
peng_final_test_rf %>%
  collect_metrics()

peng_final_test_rf %>%
  collect_predictions() %>%
  conf_mat(sex, .pred_class)

#peng_final_test_rf$.workflow[[1]] %>% # Error: No tidy method for objects of class ranger
  #tidy(exponentiate = T) %>%
  #arrange(estimate)

# --> You can see, that the bill_depth is much more important to
#     seperate and to classify the penguinsex from each other

data %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(bill_depth_mm, bill_length_mm, 
             color = sex, size = body_mass_g)) +
  geom_point(alpha = 0.8) +
  facet_wrap(~species)























