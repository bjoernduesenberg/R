# Tidytuesday Data  - March 2020
# Version 1.0
# Date: 19.02.2021
# Björn Düsenberg

# Link to github repo
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-03-31

# According to the screencast of Julia Silge:
# https://www.youtube.com/watch?v=7LGR1sEUXoI

# Load libraries
library(tidyverse)
library(tidymodels)

# Import data

brewing_materials_raw <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv')

View(brewing_materials)

brewing_materials_raw %>% count(type) # Count type

brewing_materials_raw %>% count(type, wt = month_current, sort = T) # type weighed by month
# --> Count of how much barrels of x the beer producer need

brewing_materials_raw %>% count(year)
# --> from 2008 to 2017 equal n for every year

brewing_materials_raw %>%
  filter(type %in% c("Malt and malt products", # filter for the types in list c(...)
                     "Sugar and syrups",
                     "Hops (dry)")) %>%
  mutate(date = paste0(year, "-",month, "-01"), # combine year - month - 01 to a date format
         date = lubridate::ymd(date)) %>% # explain to R, that this is a date
  ggplot(aes(date, month_current, color = type)) +
  geom_point()

# --> after 2016 the curve looks strange
# --> in 2014 and 2015 the december (month = 12 looks weird too)

brewing_materials_raw %>%
  filter(type %in% c("Malt and malt products", # filter for the types in list c(...)
                     "Sugar and syrups",
                     "Hops (dry)"),
         year < 2016, # data only until 2016
         month != 12) %>% # delete the decembers
  mutate(date = paste0(year, "-",month, "-01"), # combine year - month - 01 to a date format
         date = lubridate::ymd(date)) %>% # explain to R, that this is a date
  ggplot(aes(date, month_current, color = type)) +
  geom_point()

brewing_filterd <- brewing_materials_raw %>% # take the filter from above
  filter(type %in% c("Malt and malt products", # filter for the types in list c(...)
                     "Sugar and syrups",
                     "Hops (dry)"),
         year < 2016, # data only until 2016
         month != 12) %>%
  mutate(date = paste0(year, "-",month, "-01"), # combine year - month - 01 to a date format
         date = lubridate::ymd(date))

brewing_materials <- brewing_filterd %>% 
  select(date, type, month_current) %>%
  pivot_wider(names_from = type, # extend the dataframe: types to columns and month_current to values
              values_from = month_current) %>%
  janitor::clean_names()

brewing_materials %>%
  ggplot(aes(malt_and_malt_products, sugar_and_syrups)) + 
  geom_point() + 
  geom_smooth(method = "lm") # 'loess' and formula 'y ~ x' is interesting

# Simple linear fit
beer_fit <- lm(sugar_and_syrups ~ 0 + malt_and_malt_products,
   data = brewing_materials)

summary(beer_fit)
# Coefficients: Estimate is how much of a barrel sugar I need on each barrel of malt

# Bootstrap resampling - Creating datasets to train the model on based on the original data

beer_boot <- bootstraps(brewing_materials, # Dataset
                        times = 1e3, # How many bootstraps (1e3 = 1000)
                        apparent = T)

# Creating new column with linear models using the map function

beer_models <- beer_boot %>%
  mutate(model = map(splits, ~ lm(sugar_and_syrups ~ 0 + malt_and_malt_products,
                                  data = .)),
         coef_info = map(model, tidy))

beer_coef <- beer_models %>%
  unnest(coef_info)


# Evaluation of the model

beer_coef %>%
  ggplot(aes(estimate)) + 
  geom_histogram(bins = 70)

int_pctl(beer_models, coef_info)
# estimate and the upper/lower values with an alpha value -> Confidence interval

# Augment on the models 

beer_aug <- beer_models %>%
  sample_n(200) %>% # Take a sample
  mutate(augmented = map(model, augment)) %>% # use augment on the model and create new column
  unnest(augmented) # Unnest the column

# Blot the sample from above as line
# practically the area of the confidence interval
beer_aug %>%
  ggplot(aes(malt_and_malt_products, sugar_and_syrups)) + 
  geom_line(aes(y = .fitted, group = id), alpha = 0.1, color = "cyan3") +
  geom_point() 
  









