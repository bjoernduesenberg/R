###############################################################################
# Tutorial KNN
# https://www.kaggle.com/xvivancos/tutorial-knn-in-the-iris-data-set
# Version 1.0
# Date: 28.06.2021
# Björn Düsenberg

# Load Libraries
library(class)
library(tidyverse)
library(tidymodels)
library(GGally)

# Load Data
iris <- iris

# First Overview
head(iris) # First rows
tail(iris) # Last rows
summary(iris) # Summary
str(iris) # Structure

# Perform some small changes
iris_changed <- iris %>%
  rename('SepalLength' = Sepal.Length, # Rename the columns
         'SepalWidth' = Sepal.Width,
         'PetalLength' = Petal.Length,
         'PetalWidth' = Petal.Width) %>%
  mutate(Species = fct_recode(Species, # Change factor levels of "Species"
                              'Setosa' = 'setosa',
                              'Versicolor' = 'versicolor',
                              'Virginica' = 'virginica'))

# Histogram
iris_changed %>%
  gather(Attributes, Value, 1:4) %>%
  ggplot(aes(x = Value, fill = Attributes)) +
  geom_histogram(colour = "black") +
  facet_wrap(~Species) +
  theme_bw() +
  labs(x = "Value",
       y = "Frequency",
       title = "Iris DataSet",
       subtitle = "Histogram for each species") +
  theme(legend.title = element_blank(),
        legend.position = 'bottom')
  
# Density plot
iris_changed %>%
  gather(Attributes, Value, 1:4) %>%
  ggplot(aes(x = Value, fill = Species)) +
  geom_density(colour = "black", alpha = 0.5) +
  facet_wrap(~Attributes, scales = "free_x") +
  theme_bw() +
  labs(x = "Value",
       y = "Density",
       title = "Iris DataSet",
       subtitle = "Density plot for each attribute") +
  theme(legend.title = element_blank(),
        legend.position = 'bottom')

# Violin plot
iris_changed %>%
  gather(Attributes, Value, 1:4) %>%
  ggplot(aes(x = reorder(Attributes, Value, FUN = median),
             y = Value,
             fill = Attributes)) +
  geom_violin(show.legend = F) +
  geom_boxplot(width = 0.05, fill = "white") +
  theme_bw() +
  labs(title="Iris data set",
       subtitle="Violin plot for each attribute") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())

# Boxplot for each attribute
iris_changed %>%
  gather(Attributes, Value, 1:4) %>%
  ggplot(aes(x = reorder(Attributes, Value, FUN = median), 
             y = Value, 
             fill = Attributes)) +
  geom_boxplot(show.legend = F) +
  labs(title = "Iris data set",
       subtitle = "Boxplot for each attribute") +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())

# Scatter plot and correlations
ggpairs(cbind(iris_changed, Cluster = as.factor(iris_changed$Species)),
        columns = 1:4, aes(colour = Cluster, alpha = 0.5),
        lower = list(continous = "points"),
        axisLabels = "none", switch = "both") +
  theme_bw()

# Data preparation
# --> Normalization
iris_norm <- iris
iris_norm[, -5] <- scale(iris[, -5])

set.seed(1234)

# Train-Test Split
iris_split <- initial_split(iris_norm)
iris_test <- testing(iris_split)
iris_train <- training(iris_split)

# KNN
# Performing KNN with different k
KNN_iris_1 <- knn(iris_train[, -5], iris_test[, -5], iris_train$Species, k = 1, prob = T)
KNN_iris_2 <- knn(iris_train[, -5], iris_test[, -5], iris_train$Species, k = 2, prob = T)
KNN_iris_3 <- knn(iris_train[, -5], iris_test[, -5], iris_train$Species, k = 3, prob = T)
KNN_iris_4 <- knn(iris_train[, -5], iris_test[, -5], iris_train$Species, k = 4, prob = T)
KNN_iris_5 <- knn(iris_train[, -5], iris_test[, -5], iris_train$Species, k = 5, prob = T)
KNN_iris_10 <- knn(iris_train[, -5], iris_test[, -5], iris_train$Species, k = 10, prob = T)

# Evaluation
Ev_KNN_1 <- table(iris_test$Species, KNN_iris_1)
Ev_KNN_1_value <- sum(KNN_iris_1==iris_test$Species)/length(iris_test$Species)*100

Ev_KNN_2 <- table(iris_test$Species, KNN_iris_2)
Ev_KNN_2_value <- sum(KNN_iris_2==iris_test$Species)/length(iris_test$Species)*100

Ev_KNN_3 <- table(iris_test$Species, KNN_iris_3)
Ev_KNN_3_value <- sum(KNN_iris_3==iris_test$Species)/length(iris_test$Species)*100

Ev_KNN_4 <- table(iris_test$Species, KNN_iris_4)
Ev_KNN_4_value <- sum(KNN_iris_4==iris_test$Species)/length(iris_test$Species)*100

Ev_KNN_5 <- table(iris_test$Species, KNN_iris_5)
Ev_KNN_5_value <- sum(KNN_iris_5==iris_test$Species)/length(iris_test$Species)*100

Ev_KNN_10 <- table(iris_test$Species, KNN_iris_10)
Ev_KNN_10_value <- sum(KNN_iris_10==iris_test$Species)/length(iris_test$Species)*100

# Visualization of "best" k
# First empty variables
KnnTestPrediction <- list()
accuracy <- numeric()

# for-Loop
for(k in 1:100){ # from k = 1 to k = 100
  
  # KNN Classification for all k's
  KnnTestPrediction[[k]] <- knn(iris_train[, -5], iris_test[, -5], iris_train$Species, k, prob = T)
  # Accuracy for all Classifications´
  accuracy[[k]] <- sum(KnnTestPrediction[[k]]==iris_test$Species)/length(iris_test$Species)*100
}

# Accuracy vs Choice of k
plot(accuracy, type="b", col="dodgerblue", cex=1, pch=20,
     xlab="k, number of neighbors", ylab="Classification accuracy", 
     main="Accuracy vs Neighbors")

# Add lines indicating k with best accuracy
abline(v=which(accuracy==max(accuracy)), col="darkorange", lwd=1.5)

# Add line for max accuracy seen
abline(h=max(accuracy), col="grey", lty=2)

# Add line for min accuracy seen 
abline(h=min(accuracy), col="grey", lty=2)

