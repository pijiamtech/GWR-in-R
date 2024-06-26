#Install and import library
library(dplyr)
library(tidyverse)
library(GWmodel)
library(readxl)
library(spgwr)
library(sp)
library(sf)
library(spatialreg)
library(gwrr)
library(corrplot)

#Import training data
data2020 <- st_read("C:/Users/picha/Desktop/summer/griddd/2020selected/032020r.shp")
#Import test data
data2021 <- st_read("C:/Users/picha/Desktop/summer/griddd/2021selected/032021r.shp")

#Select Data out
data2020 <- data2020[, !names(data2020) %in% c("id","left","top","right","bottom","FID_1","id_1","emission","tempK")]
data2021 <- data2021[, !names(data2021) %in% c("id","left","top","right","bottom","FID_1","id_1","emission","tempK")]

#Append Data Frames
data <- data2020
data

#check missing
colSums(is.na(data))

#Create linear model
model_linear = lm(formula = pm25~emission_g+pblh+pressure+humidity+wind, data = data)
sum = summary(model_linear)
print(sum)
cat("AIC = ",AIC(model_linear))
cat("\nR2 = ",sum$r.squared)

#correlation
data_no_geometry <- st_drop_geometry(data)
numeric_columns <- data_no_geometry[, c("pm25", "emission_g", "pblh", "pressure", "humidity", "wind")]
numeric_columns <- data.frame(lapply(numeric_columns, as.numeric))
cordata <- cor(numeric_columns)
corrplot(cordata, method = "number")

#assign data to spatial
data_sp <- as(data, "Spatial")
data_sp

#Create GWRmodel
#Find optimize bandwidth
gwr_band <-gwr.sel(pm25~emission_g+pblh+pressure+humidity+wind, data_sp, gweight = gwr.Gauss)
gwr_band

#fit model
gwr.fit <- gwr(pm25~emission_g+pblh+pressure+humidity+wind, data_sp, bandwidth = gwr_band, se.fit=T, hatmatrix=T, gweight = gwr.Gauss)
gwr.fit

#evaluate between OLS and GWR
model = c("OLS","GWR")
R2 = c(sum$r.squared,0.9983858 ) # Quasi-global R2
AIC = c(AIC(model_linear),gwr.fit$results$AICh)
evaluate = data.frame(model,R2,AIC)
evaluate

#Model
df_gwr = as.data.frame(gwr.fit$SDF)
view(df_gwr)

#-------------------PREDICTION-------------------#
# Load necessary library
library(sp)
# Convert data2021 to Spatial object
data2021_sp <- as(data2021, "Spatial")
data2021_pred <- data2021[, !names(data2021) %in% c("pm25")]
# Initialize vectors to store predicted and actual PM2.5 values
predicted_values <- numeric(length(c(104, 28, 101, 100, 74, 102, 73, 152))) #list station we want
actual_values <- numeric(length(c(104, 28, 101, 100, 74, 102, 73, 152)))
# Specific indices to loop through
indices <- c(104, 28, 101, 100, 74, 102, 73, 152)
# Loop through each specified point
for (idx in seq_along(indices)) {
  i <- indices[idx]
  # Input i value to select one geometry
  select_geometry <- data2021_sp[i, ]
  # Extract the coordinates from the selected geometry
  select_coords <- coordinates(select_geometry)
  # Convert to matrix format
  select_coords_mat <- matrix(select_coords, ncol = 2, byrow = TRUE)
  # Input i value to select raw data
  data_row_ <- data2021_pred[i, ]
  # Replace new data
  new_data <- data.frame(
    emission_g = data_row_$emission_g,
    pblh = data_row_$pblh,
    pressure = data_row_$pressure,
    humidity = data_row_$humidity,
    wind = data_row_$wind
  )
  # Create a SpatialPointsDataFrame using the coordinates
  new_data_sp <- SpatialPointsDataFrame(select_coords_mat, new_data)
  # Predict using the GWR model
  nearest_location <- which.min(spDistsN1(coordinates(data_sp), select_coords))
  local_coefficients <- gwr.fit$SDF@data[nearest_location, ]
  # Extract the coefficients for the local model
  intercept <- local_coefficients[["X.Intercept."]]
  emission_g_coef <- local_coefficients[["emission_g"]]
  pblh_coef <- local_coefficients[["pblh"]]
  pressure_coef <- local_coefficients[["pressure"]]
  humidity_coef <- local_coefficients[["humidity"]]
  wind_coef <- local_coefficients[["wind"]]
  # Predict using the local model
  predicted_pm25 <- intercept +
    emission_g_coef * new_data$emission_g +
    pblh_coef * new_data$pblh +
    pressure_coef * new_data$pressure +
    humidity_coef * new_data$humidity +
    wind_coef * new_data$wind
  # Store the predicted and actual PM2.5 values
  predicted_values[idx] <- predicted_pm25
  actual_values[idx] <- data2021$pm25[i]
  error <- abs(predicted_values[idx] - actual_values[idx])
  # Print the predicted and actual PM2.5 values
  cat("Index:", i, "\nPredicted PM2.5 value:", predicted_pm25, "\nActual PM2.5 value:", data2021$pm25[i], "\nError:", error, "\n\n")
}
# Optionally, you can combine and view the results in a data frame
results <- data.frame(
  index = indices,
  predicted_pm25 = predicted_values,
  actual_pm25 = actual_values,
  error = abs(predicted_values - actual_values)
)
sum_error <- sum(results$error)
cat("Sum of Errors:", sum_error, "\n")
rmse <- sqrt(mean((results$error)^2))
cat("RMSE:", rmse, "\n")

