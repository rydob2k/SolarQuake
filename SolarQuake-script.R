########## SOLARQUAKE PROJECT SCRIPT - RYDOB2K ##########

### PURPOSE: The purpose of this project was to explore any connection between solar flare activity and earthquake activity.

####### LOAD PACKAGES #######
if(!require(tidyverse)) {install.packages("tidyverse", repos = "http://cran.us.r-project.org"); library(tidyverse)}
if(!require(caret)) {install.packages("caret", repos = "http://cran.us.r-project.org"); library(caret)}


####### IMPORT DATA #######
# Download from Kaggle, Kaggle login required

# Download solar flare data set:
sf_dl <- "data/hessi.solar.flare.UP_To_2018.csv"
if(!file.exists(sf_dl))
  download.file("https://www.kaggle.com/datasets/khsamaha/solar-flares-rhessi/download?datasetVersionNumber=3", sf_dl)

# Create solar flare data tibble
flares <- read_csv(sf_dl)
save(flares, file = "data/flares.RData")


# Download earthquake data set:
eq_dl <- "data/Global_Earthquake_Data.csv"
if(!file.exists(eq_dl))
  download.file("https://www.kaggle.com/datasets/garrickhague/world-earthquake-data-from-1906-2022/download?datasetVersionNumber=2", eq_dl)

# Create earthquake data tibble
quakes <- read_csv(eq_dl)
save(quakes, file = "data/quakes.RData")


####### DATA EXPLORATION #######
### SOLAR FLARES ###

# Definitions: https://www.kaggle.com/datasets/khsamaha/solar-flares-rhessi/data

# Remove non-solar events (flagged "NS")
index2 <- which(flares$flag.2 == "NS")
index3 <- which(flares$flag.3 == "NS")
index4 <- which(flares$flag.4 == "NS")
index5 <- which(flares$flag.5 == "NS")

flares_1 <- flares[-c(index2, index3, index4, index5),]

# Remove columns not relevant to this project
drops <- c('x.pos.asec', 'y.pos.asec', 'radial', 'active.region.ar', 'flag.1', 'flag.2', 'flag.3', 'flag.4', 'flag.5')
flares_1 <- flares_1 %>% select(!all_of(drops))

# Average difference between start.time and peak
mean(flares_1$peak - flares_1$start.time) # Time difference of -26.3991 secs

# Convert start.date to peak.date
flares_1$time.to.peak.s <- flares_1$peak-flares_1$start.time
flares_1$peak.date <- lubridate::as_date(ifelse(flares_1$time.to.peak.s < 0, flares_1$start.date + 1, flares_1$start.date))

# Convert energy.kev column to factor
levs <- c("3-6", "6-12", "12-25", "25-50", "50-100", "100-300", "300-800", "800-7000", "7000-20000")
flares_1 <- flares_1 %>% mutate(energy.kev = factor(energy.kev, levels = levs))

# Plot frequency of flares per day
flares_1 %>% group_by(peak.date) %>% 
  ggplot(aes(peak.date)) +
  geom_histogram(binwidth = 90, color = "black", fill = "darkorange") +
  ggtitle("Solar Flares Over Time")
ggsave("figs/flare-frequency.png")

# Align flare frequency data by date for downstream modeling
flares_per_day <- flares_1 %>% group_by(peak.date) %>%
  summarize(flare.count = n()) %>%
  mutate(date = peak.date) %>%
  select(date, flare.count, -peak.date)
ggplot(data = flares_per_day, aes(x = date)) +
  geom_histogram(binwidth = 90, color = "black", fill = "darkorange")

# Clean up workspace environment
rm(index2, index3, index4, index5, drops)


### EARTHQUAKES ###
# Add date column for alignment with solar flares
quakes_1 <- quakes %>% mutate(date = ymd(as_date(time)))

# Plot earthquake frequency
quakes_1 %>% group_by(date) %>%
  ggplot(aes(date)) +
  geom_histogram(binwidth = 365, color = "black", fill = "steelblue") +
  ggtitle("Earthquakes per Year on Record")
ggsave("figs/quakes-counts-recorded.png")

# Filter earthquakes for above-average magnitude
overallMag <- mean(quakes_1$mag)
quakes_1 %>% filter(mag > overallMag) %>%
  group_by(date) %>%
  ggplot(aes(date)) +
  geom_histogram(binwidth = 365)

# Filter for strong earthquakes >= 6.0
quakes_1 %>% filter(mag >= 6) %>%
  group_by(date) %>%
  ggplot(aes(date)) +
  geom_histogram(binwidth = 365, color = "black", fill = "steelblue") +
  ggtitle("Strong Earthquakes per Year (Magnitude > 6.0)") # Slight pattern emerges resembling solar cycle?
ggsave("figs/strong-quakes-counts.png")


## Compare solar flare frequency to earthquake frequency
# Align quakes timescale to flares and remove non-relevant columns
quakes_2 <- quakes_1 %>% filter(date >= min(flares_per_day$date) & date <= max(flares_per_day$date)) %>% 
  group_by(date) %>% mutate(daily.max.mag = max(mag), daily.avg.mag = mean(mag)) %>% 
  select(date, mag, daily.avg.mag, daily.max.mag)

# Plot earthquake frequency within flares timescale
quakes_2 %>% group_by(date) %>%
  ggplot(aes(date)) +
  geom_histogram(binwidth = 90, color = "black", fill = "steelblue") +
  ggtitle("Earthquake Frequency from 2002") # No apparent connection
ggsave("figs/quake-freq-2002.png")

# Plot strong earthquakes
quakes_2 %>% filter(mag >= 6) %>% group_by(date) %>%
  ggplot(aes(date)) +
  geom_histogram(binwidth = 90, color = "black", fill = "steelblue") +
  ggtitle("Strong Earthquake Frequency from 2002") # No apparent connection 
ggsave("figs/strong-quake-freq-2002.png")

# Align quake frequency data by date for downstream modeling
quakes_per_day <- quakes_2 %>% group_by(date) %>% 
  summarize(daily.avg.mag = mean(mag), daily.max.mag = max(mag), quake.count = n()) %>%
  mutate(quake.count = as.numeric(quake.count))

# Combine flares_per_day and quakes_per_day by date
solarquakes <- quakes_per_day %>% left_join(flares_per_day, by = "date") %>%
  mutate(flare.count = ifelse(is.na(flare.count), 0, flare.count))
save(solarquakes, file = "data/solarquakes.RData")

# Plot flare frequency vs quake data
solarquakes %>% ggplot(aes(flare.count, quake.count)) +
  geom_point() +
  ggtitle("Earthquake Frequency vs Solar Flare Frequency")
ggsave("figs/quakes-vs-flares-scatter.png")

solarquakes %>% ggplot(aes(date, y = quake.count/flare.count)) +
  geom_point() +
  geom_smooth()

solarquakes %>% select(date, flare.count, quake.count) %>%
  ggplot(aes(x = date)) + 
  geom_line(aes(y = flare.count), color = "darkorange") + 
  geom_line(aes(y = quake.count), color="steelblue", linetype="twodash") +
  labs(x = "Date", y = "Counts") +
  ggtitle("Solar Flare and Earthquake Frequency")
ggsave("figs/solarquakes-freq.png")

# Examine correlation between flare and quake frequency
cor(solarquakes$flare.count, solarquakes$quake.count)
# No correlation

# Correlation matrix
solarquakes %>% select(-date) %>% cor()
# No other correlations

# Filter for strong quakes, correlation
solarquakes_strong <- solarquakes %>% filter(daily.max.mag >= 6)
solarquakes_strong %>% select(-date) %>% cor()

solarquakes_strong %>% ggplot(aes(date, y = quake.count/flare.count)) +
  geom_point() +
  geom_smooth()


####### GENERATE PREDICTION MODELS #######
# Partition data for training and testing
set.seed(2002, sample.kind = "Rounding")
test_index <- createDataPartition(y = solarquakes$quake.count, times = 1, p = 0.2, list = FALSE)
trainSet <- solarquakes[-test_index,]
testSet <- solarquakes[test_index,]

# Create 'RMSE' function for comparing model success
RMSE <- function(actual_ratings, predicted_ratings){
  sqrt(mean((actual_ratings - predicted_ratings)^2))
}

### INITIAL MODEL ###
# Calculate "Just the Average" as prediction
just_avg_rmse <- RMSE(testSet$quake.count, mean(trainSet$quake.count))


### LINEAR MODEL ###
# Train and test linear model for predicting quake.count from flare.count
train_lm <- train(quake.count ~ flare.count, data = trainSet, method = "lm")
preds_lm <- testSet %>% mutate(pred.lm = predict(train_lm, newdata = testSet)) %>%
  pull(pred.lm)
linear_rmse <- RMSE(testSet$quake.count, preds_lm)


### KNN MODEL ###
# Train and test kNN model
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(quake.count ~ flare.count, data = trainSet, method = "knn", 
                   tuneGrid = data.frame(k = seq(9, 101, 2)),
                   trControl = control)
ggplot(train_knn, highlight = TRUE)
train_knn$finalModel
train_knn$bestTune

preds_knn <- testSet %>% mutate(pred.knn = predict(train_knn, newdata = testSet)) %>%
  pull(pred.knn)
knn_rmse <- RMSE(testSet$quake.count, preds_knn)


### RANDOM FOREST MODEL ###
# Train and test random forest model
grid <- data.frame(mtry = c(1, 5, 10, 25, 50, 100))
train_rf <- train(quake.count ~ flare.count, data = trainSet, method = "rf", 
                  tuneGrid = grid,
                  trControl = control)
train_rf$finalModel
train_rf$bestTune

preds_rf <- testSet %>% mutate(pred.rf = predict(train_rf, newdata = testSet)) %>%
  pull(pred.rf)
rf_rmse <- RMSE(testSet$quake.count, preds_rf)


### MODEL PERFORMANCE ###
models <- tibble(Model = c("Just the Average", 
                           "Linear Model", 
                           "kNN Model",
                           "Random Forest Model"), 
                 RMSE = c(just_avg_rmse, linear_rmse, knn_rmse, rf_rmse)
)