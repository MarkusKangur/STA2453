# ------------------------------------------------------------------------------
# Filename: 02_modelling.R
# Performs modelling to predict the plankton labels.
# ------------------------------------------------------------------------------

library(raster)
library(tidyverse)
library(cowplot)
library(ggplot2)
library(ggfortify)
library(xgboost)

# Set this to where you downloaded the data
setwd("/Users/markuskangur/Desktop/Plankton")
data <- readRDS("ready_data.RDS")

# Create test and train split sets.
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(data), replace = TRUE, prob = c(0.8, 0.2))
train <- data[sample, ]
test <- data[!sample, ]

# Function for checking accuracy
accuracy <- function(vec1, vec2) {
  correct <- 0
  for (i in seq(length(vec1))) {
    if (vec1[i] == vec2[i]) {
      correct <- correct + 1
    }
  }
  return(correct / length(vec1))
}

# ------------------------------------------------------------------------------
# Preliminary modeling
mylogit <- glm(
  as.factor(important) ~ Image.Height + Image.Width + Intensity +
    Transparency + Symmetry + Circularity + Convex.Perimeter,
  data = train, family = "binomial"
)
summary(mylogit)
pred <- predict(mylogit, test, type = "response")
pred[pred > 0.5] <- 1
pred[pred <= 0.5] <- 0
accuracy(pred, test$important)

# IN PROGRESS!
