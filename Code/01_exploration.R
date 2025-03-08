# ------------------------------------------------------------------------------
# Filename: 01_exploration.R
# This script performs generatess some exploratory plots while also preparing
# the data for modeling.
# ------------------------------------------------------------------------------

library(tidyverse)
library(raster)
library(cowplot)
library(ggfortify)

# Set this to where you downloaded the data
setwd("/Users/markuskangur/Desktop/Plankton")
images <- readRDS("images.RDS")
data <- readRDS("data.RDS")

# Helper function for displaying an image stored as a matrix
plot_image <- function(M) {
  dimnames <- list(x = seq(1, nrow(M)), y = seq(1, ncol(M)))
  mat <- matrix(M, ncol = ncol(M), nrow = nrow(M), dimnames = dimnames)
  out <- as.data.frame(as.table(mat))
  plt <- ggplot(data = out) +
    geom_raster(aes(x = x, y = y, fill = Freq)) +
    scale_fill_gradient(low = "black", high = "white") +
    theme_void() +
    theme(legend.position = "none")
  return(plt)
}

# ------------------------------------------------------------------------------
# Work with the image data

# Visualize within-class variation for each "important" class
imp <- c(
  "Calanoid_1", "Cyclopoid_1", "Bosmina_1", "Herpacticoida", "Chironomid",
  "Chydoridae", "Daphnia"
)

for (class in imp) {
  one_class <- images[images$Class == class, ]
  plts <- list()
  for (i in seq(1, 9)) {
    test <- one_class[i, ]
    img <- as.data.frame(test$image)
    img1 <- as.matrix(img, nrow = dim(img)[1], ncol = dim(img)[2])
    plts[[i]] <- plot_image(img1)
  }
  print(cowplot::plot_grid(plotlist = plts, scale = 0.95))
}

# ------------------------------------------------------------------------------
# Prepare the non-image data

# Distribution of class labels
table(data$Class)

# Fix typo in class labels (Holopedidae = Holopediidae = Holopididae)
indices <- data$Class == "Holopedidae" | data$Class == "Holopididae"
data[indices, ]$Class <- "Holopediidae"

# Label classes with integers starting from 0
data$label <- as.numeric(factor(data$Class)) - 1

# Create indicator variable for "important" classes: Calanoid_1, Cyclopoid_1,
# Bosmina_1, Herpacticoida, Chironomid, Chydoridae, Daphnia
data$important <- 0
imp_labels <- c(2, 10, 0, 15, 4, 5, 11)
data[data$label %in% imp_labels, ]$important <- 1

# Remove columns that don't provide meaningful information
summary(data)
data <- subset(data, select = -c(
  Class.Particle.ID, Calibration.Factor, Calibration.Image, Camera, Capture.X,
  Capture.Y, Date, Elapsed.Time, Original.Reference.ID, Particles.Per.Chain,
  Source.Image, Sphere.Complement, Sphere.Count, Sphere.Unknown, Sphere.Volume,
  Time, Timestamp, Particle.ID, file, SAM, Year, Month, Day, Rep, repnum, Key,
  SITE, WIND, FR
))

# ------------------------------------------------------------------------------
# Create some visualizations

# Number of observations per class
a <- as.data.frame(table(data$Class))
ggplot(data = a, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = 0) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))

# Where were observations taken?
barplot(table(data$Loc))

# Distribution of image sizes by class
ggplot(data = data, aes(x = Image.Width, y = Image.Height, colour = Class)) +
  geom_point(size = 0.5, alpha = 0.5) +
  xlab("Image Width") +
  ylab("Image Height") # +xlim(c(0,400))+ylim(c(0,400))

# Distribution of image sizes by importance
ggplot(data = data,
       aes(x = Image.Width, y = Image.Height, colour = as.factor(important))) +
  geom_point(size = 0.5, alpha = 0.5) +
  xlim(c(0, 300)) +
  ylim(c(0, 300)) +
  xlab("Image Width") +
  ylab("Image Height")

# Check other pairs of variables (change variables below as needed)
ggplot(data = data, aes(x = Circularity, y = Convexity, colour = Class)) +
  geom_point(size = 0.5, alpha = 0.5) +
  xlab("Cicrcularity") +
  ylab("Convextity")
ggplot(data = data, aes(x = Perimeter, y = Symmetry, colour = Class)) +
  geom_point(size = 0.5, alpha = 0.5) +
  xlab("Perimeter") +
  ylab("Symmetry")

# Principal Component Analysis
num_data <- subset(data, select = c(
  Area..ABD., Area..Filled., Aspect.Ratio, Circle.Fit, Circularity,
  Circularity..Hu., Compactness, Convex.Perimeter, Convexity, Diameter..ABD.,
  Diameter..ESD., Diameter..FD., Edge.Gradient, Elongation, Feret.Angle.Max,
  Feret.Angle.Min, Fiber.Curl, Fiber.Straightness, Geodesic.Aspect.Ratio,
  Geodesic.Length, Geodesic.Thickness, Image.Height, Image.Width, Intensity,
  Length, Perimeter, Roughness, Sigma.Intensity, Sum.Intensity, Symmetry,
  Transparency, Volume..ABD., Volume..ESD., Width
))
PCA <- prcomp(num_data, center = TRUE, scale = TRUE)
autoplot(PCA, data = data, shape = 1, color = "Class", scale = 0)
autoplot(PCA, data = data, shape = 1, color = "Class", scale = 0) +
  xlim(c(0, 100)) +
  ylim(c(0, 100))
summary(PCA)

# Save the data for modeling
saveRDS(data, file = "ready_data.RDS")
rm(list = ls())
