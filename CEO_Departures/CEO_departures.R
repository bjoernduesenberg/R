# Tidytuesday Data  - April 2021
# https://www.youtube.com/watch?v=UwSBDLcUQP0&t=203s - Screencast from Julia Silge
# Version 1.0
# Date: 05.07.2021
# Björn Düsenberg

# Load libraries
library(tidyverse) 
library(tidymodels)
library(broom)
library(rsample)

# Import Data
departures_raw <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-27/departures.csv")
write.csv(departures_raw, "departures_raw .csv")

# Get an overview over the Data
View(departures_raw)

# EDA
departures_raw %>%
  filter(departure_code < 9, fyear > 1995, fyear < 2019) %>% # Code 9 is missing value/missing information we don't use this code
  mutate(involuntary = if_else(departure_code %in% 3:4, "involuntary", "other")) %>% # 3 and 4 are involuntary and the others had reasons
  count(fyear, involuntary) %>% # count over the fiscal years
  ggplot(aes(fyear, n, color = involuntary)) + # Start of visualisation
  geom_line() +
  geom_point() + 
  geom_smooth(method = "lm", lty = 2) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = NULL, y = "Number of departures", color = NULL) +
  theme_bw()
  
# Filtered Dataset
departures <- departures_raw %>%
  filter(departure_code < 9, fyear > 1995, fyear < 2019) %>% 
  mutate(involuntary = if_else(departure_code %in% 3:4, "involuntary", "other"))

# One-time modelling with GLM
df <- departures %>%
  count(fyear, involuntary) %>%
  pivot_wider(names_from = involuntary, values_from = n)

# We define the involuntary as "successes" for our glm

mod <- glm(cbind(involuntary, other) ~fyear,
           data = df, family = "binomial")

summary(mod)
tidy(mod, exponentiate = T)

# --> estimate of fyear ~ 1.02 on a logarithmic scale 
# --> which means that every year it is 2 percent more likely to go involuntary

# Now with bootstrap resampling
set.seed(123)
ceo_folds <- bootstraps(departures, times = 1e3)

# Function to apply the model to a split
fit_binom <- function(split){
  df <- analysis(split) %>%
    count(fyear, involuntary) %>%
    pivot_wider(names_from = involuntary, values_from = n)
  
  mod <- glm(cbind(involuntary, other) ~fyear,
             data = df, family = "binomial")
  
  tidy(mod, exponentiate = T)
}

boot_models <- ceo_folds %>% mutate(coef_info = map(splits, fit_binom))

# Explore results
# --> Bootstrap CI's
intervals <- int_pctl(boot_models, coef_info)

boot_models %>%
  unnest(coef_info) %>%
  filter(term == "fyear") %>%
  ggplot(aes(estimate)) + 
  geom_vline(xintercept = 1, lty = 2, color = "gray50", size = 1.5) +
  geom_histogram(fill = "midnightblue") +
  theme_bw()

# --> Every year the involuntary departure of CEO's is about 1.5% more likely



