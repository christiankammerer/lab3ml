set.seed(1234567890)
library(geosphere)

# Data preprocessing
stations <- read.csv("stations.csv", fileEncoding = "latin1")
temps <- read.csv("temps50k.csv")
st <- merge(stations, temps, by = "station_number")
# st$coords <- apply(st, 1, function(row) list(c(row['longitude'], row['latitude'])))
st <- subset(st, select = c(latitude, longitude, date, time, air_temperature))
st$date <- as.Date(st$date)
st$time <- strptime(st$time, format = "%H:%M:%S")


# Decay values
h_distance <- 50
h_date <- 1/12
h_time <- 3

calc_spatial_distance <- function(coords_matrix, coords_target) {
  # Calculate absolute distance in km
  distances = apply(coords_matrix, 1, function(coord)
    distHaversine(coord, coords_target))
  abs_km_distances = abs(distances) / 1000
  return (abs_km_distances)
}

calc_date_distances <- function(date_list, date_target) {
  # Calculate absolute distance in years
  distances = lapply(date_list, function(date)
    difftime(date, date_target, units = "days"))
  num_abs_distances = abs(as.numeric(distances) / 365)
  return(num_abs_distances)
}

calc_time_distances <- function(time_list, time_target) {
  # Calculate absolute distance accounting for 24-hour day cycle (23 o'clock and 1 o'clock are close not far)
  distances = lapply(time_list, function(time)
    min(abs(
      difftime(time, time_target, units = "hours")
    ), 24 - abs(
      difftime(time, time_target, units = "hours")
    )))
  num_distances = as.numeric(distances)
  return(num_distances)
}

kernel_function <- function(distances, h) {
  weights <- lapply(distances, function(distance)
    exp(-(distance ^ 2 / (2 * h ^ 2))))
  return(unlist(weights))
}


predict_temp <- function(data,
                         coords,
                         date,
                         time,
                         weight_aggregation) {
 print(coords)
  data <- data[data$date < date, ] # filter for only prior data points
  coords_matrix <- cbind(data$latitude, data$longitude) # combine coordinates
  y <- data$air_temperature # extract target variable
  
  # Calculate distances & weights
  spatial_distances <- calc_spatial_distance(coords_matrix, coords)
  date_distances <- calc_date_distances(data$date, date)
  time_distances <- calc_time_distances(data$time, time)
  spatial_weights <- kernel_function(spatial_distances, h_distance)
  date_weights <- kernel_function(date_distances, h_date)
  time_weights <- kernel_function(time_distances, h_time)
  
  aggregated_weights <- NULL
  if (weight_aggregation == "sum") {
    aggregated_weights <- spatial_weights + date_weights + time_weights
  } else if (weight_aggregation == "multiply") {
    aggregated_weights <- spatial_weights * date_weights * time_weights
  }
  return(sum(aggregated_weights * y) / sum(aggregated_weights))
}
# Function to plot kernel decay
plot_kernel_decay <- function(min_value,
                              max_value,
                              h_values,
                              units,
                              title = "Spatial Distance") {
  # Define kernel function
  kernel_function <- function(distance, h) {
    exp(-(distance ^ 2) / (2 * h ^ 2))
  }
  
  # Generate distances based on the provided range
  distances <- seq(min_value, max_value, length.out = 500)
  
  # Plot setup
  plot(
    distances,
    kernel_function(distances, h_values[1]),
    type = "l",
    lwd = 2,
    col = "blue",
    ylim = c(0, 1),
    xlab = paste("Distance in", units),
    ylab = "Kernel Value",
    main = title
  )
  
  # Add additional curves for other h values
  if (length(h_values) > 1) {
    colors <- rainbow(length(h_values))
    for (i in seq_along(h_values)) {
      lines(
        distances,
        kernel_function(distances, h_values[i]),
        col = colors[i],
        lwd = 2
      )
    }
    legend(
      "topright",
      legend = paste0("h = ", h_values),
      col = colors,
      lwd = 2
    )
  } else {
    legend(
      "topright",
      legend = paste0("h = ", h_values[1]),
      col = "blue",
      lwd = 2
    )
  }
}

# Example Usage
plot_kernel_decay(
  min_value = 0,
  max_value = 1500,
  h_values = c(50, 100, 175, 300, 500, 1000),
  # Different h values to compare
  units = "kilometers",
  title = "Kernel Decay for Spatial Distance"
)

plot_kernel_decay(
  min_value = 0,
  max_value = 12,
  h_values = c(0.5, 1, 3, 6, 12),
  # Different h values to compare
  units = "hours",
  title = "Kernel Decay for Time Distance"
)

plot_kernel_decay(
  min_value = 0,
  max_value = 1,
  h_values = c(0.1, 0.25, 0.5, 1, 5),
  # Different h values to compare
  units = "years",
  title = "Kernel Decay for Date Distance"
)




dates = c('1999-01-08', # Bromma
         '2012-03-13', # Linköping
         '2001-07-13', # Arlanda
         '2004-09-04', # Gothenburg
         '2014-03-19', # Luleå 
         '2011-05-29', # Malmö
         '2010-01-25', # Västerås
         '2015-02-03', # Dalarna
         '2012-12-31', # Kiruna 
         '2011-09-09') # Jönköping

coordinates <- list(
  c(59.3544, 17.9415),  # Bromma Airport
  c(58.4062, 15.6805),  # Linköping Airport
  c(59.6519, 17.9186),  # Arlanda Airport
  c(57.6666, 12.2878),  # Gothenburg Airport
  c(65.5490, 22.1232),   # Luleå
  c(55.5366, 13.376),   # Malmö Airport
  c(59.5894, 16.6336),  # Västerås Airport
  c(60.4336, 15.5018),  # Dalarna Airport
  c(67.8210, 20.3368),  # Kiruna Airport
  c(57.7576, 14.0687)   # Jönköping Airport
)

locations <- c(
  "Bromma Airport", 
  "Linköping Airport", 
  "Arlanda Airport", 
  "Gothenburg Airport",
  "Luleå Airport",
  "Malmö Airport", 
  "Västerås Airport", 
  "Dalarna Airport", 
  "Kiruna Airport", 
  "Jönköping Airport"
)

times <- c("05:00:00", "06:00:00", "04:00:00", "20:00:00", "02:00:00", 
                   "08:00:00", "00:00:00", "02:00:00", "08:00:00", "10:00:00")

actual_temperatures <- c(-5, 5, 13.9, 15, -11, 12.2, -8.9, -7.8, -3.9, 15.5)

# Initialize a data frame to store results
results <- data.frame(
  Location = character(),
  Date = character(),
  Time = character(),
  Predicted_Temperature = numeric(),
  Actual_Temperature = numeric(),
  Deviation = numeric(),
  stringsAsFactors = FALSE
)

# Loop to calculate predictions and store results
for (i in seq_along(times)) {
  # Convert date and time
  date <- as.Date(dates[i])
  time <- strptime(times[i], format = "%H:%M:%S", tz = "UTC")
  
  # Validate coordinate length
  if (length(coordinates[[i]]) != 2) {
    stop(sprintf("Coordinates for %s are invalid: %s", locations[i], coordinates[[i]]))
  }
  
  # Predict temperature
  y_pred <- predict_temp(
    data = st,
    coords = coordinates[[i]],  # Use [[i]] to extract the vector
    time = time,
    date = date,
    weight_aggregation = "sum"
  )
  
  # Calculate deviation
  deviation <- actual_temperatures[i] - y_pred
  
  # Append to results data frame
  results <- rbind(results, data.frame(
    Location = locations[i],
    Date = dates[i],
    Time = times[i],
    Predicted_Temperature = round(y_pred, 2),
    Actual_Temperature = actual_temperatures[i],
    Deviation = round(deviation, 2),
    stringsAsFactors = FALSE
  ))
}

# Save the results to an RDS file for easier loading in R
write.csv(results, "predictions_kernel_sum.csv", row.names = FALSE)

