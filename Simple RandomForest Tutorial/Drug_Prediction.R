##########################################
# Skript - Drug Prediction
# https://www.kaggle.com/jalenguzman/using-decision-trees-to-predict-treatment
# Date: 01.07.2021
# Version: 1.0
# Author: Björn Düsenberg
##########################################

# Libraries
library(tidyverse)
library(tidymodels)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)

# Load Data
data <- read.csv("drug200.csv")

####################################################################################
# Further Information
#
# Our variables:
#  
# Age: In Years
# Sex: Biological Sex (Either Male or Female; Intersex patients may need more research)
# BP: Blood Pressure (Low, Normal, or High)
# Cholesterol: level of cholesterol in the blood (either Normal or High)
# Na_to_K: Sodium to Potassium ratio
# Drug: the Drug the patient responded to or was treated with (Either Drug A, B, C, X, or Y)
####################################################################################

View(data)

# Looking for null/NA --> 0 = No missing values
which(is.na(data)) 

# Declare columns as factor
data <- data %>%
  mutate(Drug = as.factor(Drug),
         Sex = as.factor(Sex),
         BP = as.factor(BP),
         Cholesterol = as.factor(Cholesterol))

summary(data)

# EDA
# --> Overview
data %>%
  ggplot(mapping = aes(x = Age, y = Na_to_K)) +
  geom_point(aes(colour = BP)) +
  theme_bw()

# --> Overview divided by BP and colored by cholesterol
data %>%
  ggplot(mapping = aes(x = Age, y = Na_to_K)) +
  geom_point(aes(colour = Cholesterol)) +
  facet_wrap(~BP) +
  theme_bw()

# --> Overview divided by cholesterol and colored by BP
data %>%
  ggplot(mapping = aes(x = Age, y = Na_to_K)) +
  geom_point(aes(colour = BP)) +
  facet_wrap(~Cholesterol) +
  theme_bw()

# --> Overview divided by the used drug, colored by BP and cholesterol marked by size of points
data %>%
  ggplot(mapping = aes(x = Age, y = Na_to_K)) +
  geom_point(aes(colour = BP, size = Cholesterol)) +
  facet_wrap(~Drug) +
  theme_bw()

# --> Overview divided by BP, colored by drug and cholesterol marked by size of points
data %>%
  ggplot(mapping = aes(x = Age, y = Na_to_K)) +
  geom_point(aes(colour = Drug, size = Cholesterol)) +
  facet_wrap(~BP) +
  theme_bw()

# Na to K ratio and BP in a boxplot
ggplot(data, aes(x = Drug, y = Na_to_K)) +
  geom_boxplot(color = 'steelblue') +
  theme_bw()

# --> Pie chart of drug types
drug_counter <- tidy(summary(data$Drug))

ggplot(drug_counter, aes(x = '', y = x, fill = names)) +
  geom_bar(stat = 'identity', width = 1) +
  geom_text(aes(label = paste(round(x / sum(x) * 100, 2), '%')), color = 'white',
            position = position_stack(vjust = 0.5)) +
  coord_polar('y', start = 0) +
  labs(fill = 'Drug') +
  theme_void()

# Differences between M and F
ggplot(data, aes(x = Sex, fill = Drug)) +
  geom_bar(position = 'stack') + 
  theme_bw()

# Modelling
set.seed(1234)

drug_split <- initial_split(data)
drug_train <- training(drug_split)
drug_test <- testing(drug_split)

# --> Standard RF with 500 trees
drug_rf <- randomForest(Drug ~ ., data = drug_train, importance = T)

# Prediction
drug_pred <- predict(drug_rf, drug_test)
confusionMatrix(drug_pred, drug_test$Drug)

# Plotting the decision tree


#enlargen plot graphic
fig <- function(width, heigth){
  options(repr.plot.width = width, repr.plot.height = heigth)
}
fig(20,10)

#plot using full dataset
full.rf <- rpart(Drug ~ ., data = data, cp = 0.02)
rpart.plot(full.rf, box.palette = 'RdBu', shadow.col = 'grey', nn = TRUE)


