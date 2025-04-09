# ------------------------------------------------------------------------------
# Filename: 02_modelling.R
# Performs modelling to predict the plankton labels.
# ------------------------------------------------------------------------------

library(tidyverse)
library(ggfortify)
library(xgboost)
library(corrplot)
library(pROC)
library(car)
library(caret)

# Set to where the processed data was saved
setwd("/Users/markuskangur/Desktop/Plankton")
data <- readRDS("ready_data.RDS")

# Summary
summary(data)

# Recode the labels (0-6 for important, 7 for others)
important <- data[data$important == 1, ]
important$label <- as.numeric(as.factor(important$Class)) - 1
others <- data[data$important == 0, ]
others$label <- 7
data <- rbind(important, others)

# Remove missing data
data <- na.omit(data)
labels <- subset(data, select = c(Class, important, label))
data <- subset(data, select = -c(Class, important, label))

# Create test and train split sets.
set.seed(1)
indices <- sample(c(TRUE, FALSE), nrow(data), 
                  replace = TRUE, prob = c(0.8, 0.2))
train_data <- data[indices, ]
train_labels <- labels[indices, ]
test_data <- data[!indices, ]
test_labels <- labels[!indices, ]

# Accuracy function
# Arguments: vec1 (vector), vec2 (vector)
# Returns: Percent of entries that are equal between vec1 and vec2
accuracy <- function(vec1, vec2) {
  correct <- sum(vec1 == vec2)
  pct <- round((correct / length(vec1)) * 100, 2)
  return(paste(pct, "% accuracy"))
}

# Naive Logit Model ------------------------------------------------------------

logit_1 <- glm(as.factor(train_labels$important) ~ .,
  data = train_data, family = "binomial",
  control = list(trace = TRUE)
)
summary(logit_1)

# Examine Correlations
res <- cor(train_data)
corrplot(res, type = "upper", tl.col = "black", tl.srt = 45)

# Try removing some of the highly correlated variables via VIF
VIF <- vif(logit_1)
reduced <- subset(
  train_data, select = -c(Geodesic.Length, Diameter..ESD., Elongation, 
                          Convex.Perimeter, Diameter..FD., Length, Circularity))
logit_2 <- glm(as.factor(train_labels$important) ~ .,
  data = reduced, family = "binomial", control = list(trace = TRUE)
)
summary(logit_2)

# PCA --------------------------------------------------------------------------

pca <- prcomp(train_data, center = TRUE, scale = TRUE)
summary(pca)
n_comp <- 32
pca_data <- as.data.frame(pca$x[, 1:n_comp])
pca_logit <- glm(as.factor(train_labels$important) ~ ., data = pca_data, 
                 family = "binomial", control = list(trace = TRUE))
summary(pca_logit)

# Transform training dataset for ROC Curve
projected <- as.data.frame(cbind(predict(pca, train_data)[, 1:n_comp], 
                                 train_labels$important))
pred <- predict(pca_logit, projected, type = "response")

# Calculate ROC curve
roc_curve <- roc(train_labels$important, pred)
plot(roc_curve, col = "blue", main = "ROC Curve", print.auc = TRUE)
threshold <- coords(roc_curve, "best")$threshold

# XGBoost for "important" classes ----------------------------------------------

indices <- train_labels$important == 1
imp_train_data <- pca_data[indices, ]
imp_train_labels <- train_labels[indices, ]

indices <- test_labels$important == 1
imp_test_data <- test_data[indices, ]
imp_test_labels <- test_labels[indices, ]

train_matrix <- xgb.DMatrix(data = as.matrix(imp_train_data), 
                            label = as.matrix(imp_train_labels$label))
test_matrix <- xgb.DMatrix(data = as.matrix(imp_test_data), 
                           label = as.matrix(imp_test_labels$label))

# Set parameters
n_class <- length(unique(imp_train_labels$label))
xgb_params <- list(
  "objective" = "multi:softmax", "eval_metric" = "mlogloss",
  "num_class" = n_class
)

# Train model
pca_model <- xgb.train(params = xgb_params, data = train_matrix, nrounds = 60, 
                       verbosity = 2, eta = 0.5)

# Save model for later use
saveRDS(pca_model, file = "pca_model.RDS")
pca_model <- readRDS("pca_model.RDS")

# Implement PCA -> Logit -> XGBoost --------------------------------------------

# Project test data into PCA space
projected_test <- as.data.frame(cbind(predict(pca, test_data)[, 1:n_comp], 
                                      test_labels$important))
# Make logistic predictions
pred <- as.vector(predict(pca_logit, projected_test, type = "response"))
pred[pred > threshold] <- 1
pred[pred <= threshold] <- 0

# Take important predicted indices out
indices <- as.logical(pred)
important <- projected_test[indices, 1:n_comp]
important_labels <- test_labels[indices, ]
imp_matrix <- xgb.DMatrix(data = as.matrix(important), 
                          label = as.matrix(important_labels$label))

# Make XGBoost predictions
test_pred <- predict(pca_model, newdata = imp_matrix)

# Compute final accuracy
unimportant <- projected_test[!indices, ]$V33
correct_unimportant <- length(unimportant) - sum(unimportant)
compare <- test_labels[indices, ]$label
correct_important <- sum(compare == test_pred)
tot <- correct_important + correct_unimportant
pct <- tot / nrow(test_data)
pct

# Project train data into PCA space
projected_train <- as.data.frame(cbind(predict(pca, train_data)[, 1:n_comp], 
                                       train_labels$important))

# Make logistic predictions
pred <- as.vector(predict(pca_logit, projected_train, type = "response"))
pred[pred > threshold] <- 1
pred[pred <= threshold] <- 0

# Take predicted important indices out
indices <- as.logical(pred)
important <- projected_train[indices, 1:32]
important_labels <- train_labels[indices, ]
imp_matrix <- xgb.DMatrix(data = as.matrix(important), 
                          label = as.matrix(important_labels$label))

# Make XGBoost predictions
train_pred <- predict(pca_model, newdata = imp_matrix)

# Compute final accuracy
unimportant <- projected_train[!indices, ]$V33
correct_unimportant <- length(unimportant) - sum(unimportant)
compare <- train_labels[indices, ]$label
correct_important <- sum(compare == train_pred)
tot <- correct_important + correct_unimportant
pct <- tot / nrow(train_data)
pct

# Construct importance weights (per instance, not per class) -------------------

counts <- table(train_labels$label)
pct <- counts / nrow(train_labels)

# class 3 is the smallest one
weights <- pct[3 + 1] / pct

# Put weights into vector
weight_vec <- train_labels %>%
  mutate(weight = case_when(
    label == 0 ~ weights[1],
    label == 1 ~ weights[2],
    label == 2 ~ weights[3],
    label == 3 ~ weights[4],
    label == 4 ~ weights[5],
    label == 5 ~ weights[6],
    label == 6 ~ weights[7],
    label == 7 ~ weights[8]
  ))
weight_vec <- weight_vec[, 4]

# Model with XGBOOST directly --------------------------------------------------

train_matrix <- xgb.DMatrix(data = as.matrix(train_data), 
                            label = as.matrix(train_labels$label))
test_matrix <- xgb.DMatrix(data = as.matrix(test_data), 
                           label = as.matrix(test_labels$label))
n_class <- length(unique(train_labels$label))
xgb_params <- list(
  "objective" = "multi:softmax",
  "eval_metric" = "mlogloss",
  "num_class" = n_class
)

# Build model 1
model1 <- xgb.train(
  params = xgb_params, data = train_matrix, nrounds = 60,
  verbosity = 2, eta = 0.5
)

# Save model for later use
saveRDS(model1, file = "model1.RDS")
model1 <- readRDS("model1.RDS")

# In sample prediction
test_pred <- predict(model1, newdata = train_matrix)
result <- cbind(train_labels, test_pred)
accuracy(result$label, result$test_pred)

# Predict hold-out test set
test_pred <- predict(model1, newdata = test_matrix)
result <- cbind(test_labels, test_pred)
accuracy(result$label, result$test_pred)

# View the importance plot for which variables are important
importance <- xgb.importance(colnames(train_matrix), model1)
xgb.ggplot.importance(importance_matrix = importance)

# Reduced XGBoost (all but lowest cluster) -------------------------------------

# All but lowest cluster
red_train_data <- subset(train_data,
  select = c(
    Area..ABD., Intensity, Length, Aspect.Ratio,
    Circularity..Hu., Volume..ABD., Transparency,
    Fiber.Curl, Sigma.Intensity, trawltime,
    XWAVEHT, Edge.Gradient, Circle.Fit,
    Sum.Intensity, Geodesic.Thickness, Symmetry,
    DOY, Area..Filled.
  )
)
red_test_data <- subset(test_data,
  select = c(
    Area..ABD., Intensity, Length, Aspect.Ratio,
    Circularity..Hu., Volume..ABD., Transparency,
    Fiber.Curl, Sigma.Intensity, trawltime,
    XWAVEHT, Edge.Gradient, Circle.Fit,
    Sum.Intensity, Geodesic.Thickness, Symmetry,
    DOY, Area..Filled.
  )
)

train_matrix <- xgb.DMatrix(data = as.matrix(red_train_data), 
                            label = as.matrix(train_labels$label))
test_matrix <- xgb.DMatrix(data = as.matrix(red_test_data), 
                           label = as.matrix(test_labels$label))
n_class <- length(unique(train_labels$label))
xgb_params <- list(
  "objective" = "multi:softmax",
  "eval_metric" = "mlogloss",
  "num_class" = n_class
)

# Build model 2
model2 <- xgb.train(
  params = xgb_params, data = train_matrix, nrounds = 60,
  verbosity = 2, eta = 0.5
)

# Save model for later
saveRDS(model2, file = "model2.RDS")
model2 <- readRDS("model2.RDS")

# In sample prediction
test_pred <- predict(model2, newdata = train_matrix)
result <- cbind(train_labels, test_pred)
accuracy(result$label, result$test_pred)

# Predict hold-out test set
test_pred <- predict(model2, newdata = test_matrix)
result <- cbind(test_labels, test_pred)
accuracy(result$label, result$test_pred)

# View the importance plot for which variables are important
importance <- xgb.importance(colnames(train_matrix), model2)
xgb.ggplot.importance(importance_matrix = importance)

# Reduced XGBoost (retain 97.6% of importance) ---------------------------------

red_train_data <- subset(train_data,
  select = c(
    Area..ABD., Intensity, Length, Aspect.Ratio,
    Circularity..Hu., Volume..ABD., Transparency,
    Fiber.Curl, Sigma.Intensity, trawltime,
    XWAVEHT, Edge.Gradient, Circle.Fit,
    Sum.Intensity, Geodesic.Thickness, Symmetry,
    DOY, Area..Filled., Convexity, avgdepth,
    WaterT, WhitefishDen, volbest, Roughness,
    gdd2, Exposure
  )
)
red_test_data <- subset(test_data,
  select = c(
    Area..ABD., Intensity, Length, Aspect.Ratio,
    Circularity..Hu., Volume..ABD., Transparency,
    Fiber.Curl, Sigma.Intensity, trawltime,
    XWAVEHT, Edge.Gradient, Circle.Fit,
    Sum.Intensity, Geodesic.Thickness, Symmetry,
    DOY, Area..Filled., Convexity, avgdepth,
    WaterT, WhitefishDen, volbest, Roughness,
    gdd2, Exposure
  )
)

train_matrix <- xgb.DMatrix(data = as.matrix(red_train_data), 
                            label = as.matrix(train_labels$label))
test_matrix <- xgb.DMatrix(data = as.matrix(red_test_data), 
                           label = as.matrix(test_labels$label))
n_class <- length(unique(train_labels$label))
xgb_params <- list(
  "objective" = "multi:softmax",
  "eval_metric" = "mlogloss",
  "num_class" = n_class
)

# Train model 3
model3 <- xgb.train(
  params = xgb_params, data = train_matrix, nrounds = 60,
  verbosity = 2, eta = 0.5
)

# Save model for later
saveRDS(model3, file = "model3.RDS")
model3 <- readRDS("model3.RDS")

# In sample prediction
test_pred <- predict(model3, newdata = train_matrix)
result <- cbind(train_labels, test_pred)
accuracy(result$label, result$test_pred)

# Predict hold-out test set
test_pred <- predict(model3, newdata = test_matrix)
result <- cbind(test_labels, test_pred)
accuracy(result$label, result$test_pred)

# View the importance plot for which variables are important
importance <- xgb.importance(colnames(train_matrix), model3)
xgb.ggplot.importance(importance_matrix = importance)

# Final Model ------------------------------------------------------------------

red_train_data <- subset(train_data,
  select = c(
    Area..ABD., Intensity, Length, Aspect.Ratio,
    Circularity..Hu., Volume..ABD., Transparency,
    Fiber.Curl, Sigma.Intensity, trawltime,
    XWAVEHT, Edge.Gradient, Circle.Fit,
    Sum.Intensity, Geodesic.Thickness, Symmetry,
    DOY, Area..Filled., Convexity, avgdepth,
    WaterT, WhitefishDen, volbest, Roughness,
    gdd2, Exposure
  )
)
red_test_data <- subset(test_data,
  select = c(
    Area..ABD., Intensity, Length, Aspect.Ratio,
    Circularity..Hu., Volume..ABD., Transparency,
    Fiber.Curl, Sigma.Intensity, trawltime,
    XWAVEHT, Edge.Gradient, Circle.Fit,
    Sum.Intensity, Geodesic.Thickness, Symmetry,
    DOY, Area..Filled., Convexity, avgdepth,
    WaterT, WhitefishDen, volbest, Roughness,
    gdd2, Exposure
  )
)

train_matrix <- xgb.DMatrix(data = as.matrix(red_train_data), 
                            label = as.matrix(train_labels$label))
test_matrix <- xgb.DMatrix(data = as.matrix(red_test_data), 
                           label = as.matrix(test_labels$label))
n_class <- length(unique(train_labels$label))
xgb_params <- list(
  "objective" = "multi:softmax",
  "eval_metric" = "mlogloss",
  "num_class" = n_class
)

# Train final model
final_model <- xgb.train(
  params = xgb_params, data = train_matrix, nrounds = 100,
  verbosity = 2, eta = 0.5
)

# Save final model for later
saveRDS(final_model, file = "final_model.RDS")
final_model <- readRDS("final_model.RDS")

# In sample prediction
test_pred <- predict(final_model, newdata = train_matrix)
result <- cbind(train_labels, test_pred)
accuracy(result$label, result$test_pred)

# Predict hold-out test set
test_pred <- predict(final_model, newdata = test_matrix)
result <- cbind(test_labels, test_pred)
accuracy(result$label, result$test_pred)

# View the importance plot for which variables are important
importance <- xgb.importance(colnames(train_matrix), final_model)
xgb.ggplot.importance(importance_matrix = importance)

# Confusion matrix
confuse <- confusionMatrix(
  factor(result$test_pred),
  factor(result$label)
)
tab <- as.data.frame(confuse$table, stringsAsFactors = TRUE)
tab$Reference <- factor(tab$Reference, rev(levels(tab$Reference)))

ggplot(tab, aes(Prediction, Reference, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq)) +
  scale_fill_gradient(low = "white", high = "#3575b5") +
  labs(
    x = "Prediction", y = "Truth", title = "Confusion Matrix",
    fill = "Counts"
  )
