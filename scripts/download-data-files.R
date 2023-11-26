## Download data files from kaggle ##
  # Note: kaggle login required

# Load packages:
if(!require(tidyverse)) {install.packages("tidyverse",repos = "http://cran.us.r-project.org"); library(tidyverse)}
library(readr)
library(stringr)

# Download solar flare data set:
sf_dl <- "data/hessi.solar.flare.UP_To_2018.csv"
if(!file.exists(sf_dl))
  download.file("https://www.kaggle.com/datasets/khsamaha/solar-flares-rhessi/download?datasetVersionNumber=3", sf_dl)

# Download earthquake data set:
eq_dl <- "data/Global_Earthquake_Data.csv"
if(!file.exists(eq_dl))
  download.file("https://www.kaggle.com/datasets/garrickhague/world-earthquake-data-from-1906-2022/download?datasetVersionNumber=2", eq_dl)


# Create solar flare data tibble
flares <- read_csv(sf_dl)
save(flares, file = "data/flares.RData")

# Create earthquake data tibble
quakes <- read_csv(eq_dl)
save(quakes, file = "data/quakes.RData")