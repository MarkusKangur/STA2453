# ------------------------------------------------------------------------------
# Filename: 00_preparation.R
# Prepares the zooplankton data for analysis. 
# ------------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(raster)

# Set this to where you downloaded the data
setwd("/Users/markuskangur/Desktop/Plankton")

# Helper function for storing a directory of csv files in a single dataframe
# Arguments: dir (string)
# Returns: Returns the data stored in directory dir in a dataframe
read_csvs <- function(dir) {
  csvs <- list.files(dir)
  data <- data.frame()
  for (file in csvs) {
    csv <- read.csv(paste(dir, file, sep = ""))
    csv$file <- file
    if (ncol(csv) == 58) {
      data <- rbind(data, csv)
    }
  }
  return(data)
}

simcoe <- read_csvs("SIMC_OverlapTiffsWithPP/SIMC.Overlap.csv/")
huron <- read_csvs("HURON_OverlapTiffsWithPP/HURONOvlerap_csv/")
data <- rbind(simcoe, huron)

# Merge with the master table
master <- read_excel("MasterTable_AI_FlowCAM.xlsx")
colnames(master)[1] <- "Image.File"
merged <- merge(data, master, by = "Image.File")

# Save processed data for analysis
saveRDS(merged, file = "data.RDS")

# Create a test dataset of extracted images ------------------------------------

dir <- "SIMC_OverlapTiffsWithPP/"
files <- list.files(dir)
images <- data.frame()

for (file in files[1:1500]) {
  img <- stack(paste(dir, file, sep = ""))
  img_mat <- as.matrix(raster(img, 1))

  info <- data[data$Image.File == file, ]
  info$image <- NA

  for (i in seq(1, length(info$Class))) {
    single <- info[i, ]
    left <- single$Image.X
    bottom <- single$Image.Y
    right <- left + single$Image.Width
    top <- bottom + single$Image.Height
    single_img <- img_mat[bottom:top, left:right]
    info[i, ]$image <- list(single_img)
  }
  images <- rbind(images, info)
}

# Save processed file for analysis
saveRDS(images, file = "images.RDS")
rm(list = ls())
