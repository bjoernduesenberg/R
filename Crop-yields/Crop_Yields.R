# Tidytuesday Data  - September 2020
# Version 1.0
# Date: 14.02.2021
# Björn Düsenberg

# Link to github repo
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-09-01


# Load libraries
library(tidyverse)
library(tidymodels)
library(ggrepel)

# load data from github
key_crop_yields <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv')
# fertilizer <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/cereal_crop_yield_vs_fertilizer_application.csv')
# tractors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/cereal_yields_vs_tractor_inputs_in_agriculture.csv')
land_use <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/land_use_vs_yield_change_in_cereal_production.csv')
# arable_land <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/arable_land_pin.csv')

# Just in case you want to save the data as .csv on your computer
# write.csv(DataFrame, "PATH/DataFrame.csv")
# Save every DataFrame like this

# --> I used 2 of the Datasets - according to Julia Silges screencast
# https://www.youtube.com/watch?v=rhhuNGjj3cU

View(key_crop_yields)
View(land_use)

# Get list of Top 30 countries
top_countries <- land_use %>% 
  janitor::clean_names() %>% # Clean names from spaces etc. from the dataset
  filter(!is.na(code), # Code is not NA
         entity != "World") %>% # entity is not "World"  so we have just entities (countries)
  group_by(entity) %>% # group the entities 
  filter(year == max(year)) %>% # get the latest entry in "year"
  ungroup() %>% # ungroup the countries again
  slice_max(total_population_gapminder, n = 30) %>% # Take the countries with the highest population
  pull(entity)

# Tidy the dataset
tidy_yields <- key_crop_yields %>%
  janitor::clean_names() %>% # Clean names from spaces etc. from the dataset
  pivot_longer(wheat_tonnes_per_hectare:bananas_tonnes_per_hectare, # bring to tidy format
               names_to = "crop", # pivot_longer extends the dataset, columns x:y to names and value
               values_to = "yield") %>% # rename names and value
  mutate(crop = str_remove(crop, "_tonnes_per_hectare")) %>% # remove part of the name
  filter(crop %in% c("wheat", "rice", "maize", "potatoes","barley"), # filter just some crops
         entity %in% top_countries,
         !is.na(yield)) 

tidy_yields %>%
  ggplot(aes(year, yield, color = crop)) +
  geom_line(alpha = 0.8, size = 1.2) +
  geom_point() +
  facet_wrap(~entity) +
  labs(x = NULL, y = "yield [t/ha]") +
  theme_bw()
  
# Not all countries produdes all kinds of crops
# Potatoes are by far the most produced crop!

# Train models

# Gets a coloumn with lm models
tidy_lm <- tidy_yields %>%
  nest(yields = c(year, yield)) %>% # Nest little tibbles in the overall dataset - "new column - yields"
  mutate(model = map(yields, # iterate over the data argument from above
                     ~lm(yield ~ year, data = .x))) # how do I iterate over the argument

slopes <- tidy_lm %>%
  mutate(coefs = map(model, tidy)) %>% # take model column and tidy it to get all coefficients
  unnest(coefs) %>% # take the coefficients out of the dataset
  filter(term == "year") %>% # just leave the term == "year"
  mutate(p.value = p.adjust(p.value)) # adjust the p values for a better comparison

# Explore the results

slopes %>%
  ggplot(aes(estimate, p.value, label = entity)) +
  geom_point(aes(color = crop), alpha = 0.8, size = 2,
             show.legend = F) +
  geom_vline(xintercept = 0, lty = 2, size = 1.5) +
  geom_text_repel(size = 3) + # from ggrepel
  facet_wrap(~crop) +
  scale_y_log10() + 
  theme_bw()

# --> Everything on the positive side of the vertical line is increasing -> crop yield is increasing
# --> p value is used in hypothesis testing
#     the smaller the p value, the stronger the evidence to reject the null hypothesis
# --> In this context "how likely was this event" for example is the p value of potatoes in the USA very
#     low. The chance is very unlikely

























