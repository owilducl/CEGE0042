# Install packages ----
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

if (!require(tidyr)) {
  install.packages("tidyr")
  library(tidyr)
}

if (!require(leaflet)) {
  install.packages("leaflet")
  library(leaflet)
}

library(scatterplot3d)
library(plot3D)
library(ggplot2)
library(rgl)
library(lubridate)


# SetWD and Get Data from CSVs ----
# Change your working directory using setwd()
setwd("/Users/olliewild/Desktop/AirPollutionSeoul")
getwd()

# Prepare full AQ data
air_quality_data <- read.csv("input/Measurement_summary.csv")
air_quality_data

# Prepare station info
station_data <- read.csv("input/Measurement_station_info.csv")
station_data

# Clean Data, Exclude rows with negative averaged values and select specific columns
cleaned_data <- air_quality_data %>%
  filter(PM2.5 >= 0) %>%
  select("Measurement.date", "Station.code", "Longitude", "Latitude", "PM2.5")
cleaned_data

# Averaging ----

# • Nowcast AQI
# Weighted averaging inspired by the nowcast system introduced by US EPA
nowcast_data <- cleaned_data %>%
  group_by(Station.code, Measurement.date) %>%
  arrange(Station.code, Measurement.date) %>%
  mutate(
    NowCastAQI = 0.5 * PM2.5 +
      0.5 * lag(PM2.5, default = 0) +
      0.25 * lag(PM2.5, n = 2, default = 0) +
      0.25 * lag(PM2.5, n = 3, default = 0) +
      0.25 * lag(PM2.5, n = 4, default = 0)  
    # Add more lag terms as needed
    # You can adjust the number of lag terms and weights based on your requirements
  )
nowcast_data

nowcast_data$Measurement.date <- as.Date(nowcast_data$Measurement.date)  # Convert to Date type

# • Daily
nowcast_daily <- nowcast_data %>%
  mutate(Measurement.date = format(Measurement.date, "%Y-%m-%d")) %>%
  group_by(Measurement.date, Station.code) %>%
  summarize(DailyAvgAQI = mean(NowCastAQI, na.rm = TRUE))

# Pivot the table to have a column for each day
daily_average_aqi_wide <- nowcast_daily %>%
  pivot_wider(names_from = Measurement.date, values_from = DailyAvgAQI)

# Merge Latitude and Longitude from station_data to daily_average_aqi_wide, then Re-order
daily_averages <- merge(daily_average_aqi_wide, station_data[c("Station.code", "Latitude", "Longitude")], by = "Station.code")
daily_averages <- daily_averages[, c("Station.code", "Latitude", "Longitude", setdiff(colnames(daily_averages), c("Station.code", "Latitude", "Longitude")))]

# Check first 10 cols
daily_averages[, 1:10]

# • Monthly
# Assuming that your `Measurement.date` column is in Date format
nowcast_monthly <- nowcast_data %>%
  mutate(Measurement.month = format(Measurement.date, "%Y-%m")) %>%
  group_by(Station.code, Measurement.month) %>%
  summarize(MonthlyAvgAQI = mean(NowCastAQI, na.rm = TRUE))

# Pivot the table to have a column for each month
monthly_average_aqi_wide <- nowcast_monthly %>%
  pivot_wider(names_from = Measurement.month, values_from = MonthlyAvgAQI)

# Merge Latitude and Longitude from station_data to monthly_average_aqi_wide, then Re-order
monthly_averages <- merge(monthly_average_aqi_wide, station_data[c("Station.code", "Latitude", "Longitude")], by = "Station.code")
monthly_averages <- monthly_averages[, c("Station.code", "Latitude", "Longitude", setdiff(colnames(monthly_averages), c("Station.code", "Latitude", "Longitude")))]

# Check first 10 cols
monthly_averages[, 1:10]


# Map Plotting ----

# Load the GeoJSON file
seoul_municipalities_geo <- geojsonio::geojson_read("input/seoul_municipalities_geo.json", what = "sp")

# Create a Leaflet map with OpenStreetMap basemap
map <- leaflet() %>%
  addTiles()  # This adds the OpenStreetMap basemap
map

# Add circle markers with custom style for air quality data and popups
for (i in 1:nrow(station_data)) {
  map <- map %>%
    addCircleMarkers(
      lat = station_data[i, "Latitude"],
      lng = station_data[i, "Longitude"],
      radius = 5,  # Adjust the radius as needed
      fill = TRUE,  # Make the circles hollow
      color = "blue",  # Color of the circles
      weight = 5,  # Stroke width
      popup = paste(
        "Station Code: ", station_data[i, "Station.code"], "<br>",
        "Station Name: ", station_data[i, "Station.name.district."], "<br>",
        "Address: ", station_data[i, "Address"]
      )
    )
}
map

# Add the municipal boundaries as polygons
map <- map %>%
  addPolygons(
    data = seoul_municipalities_geo,
    color = "black",  # Color of the boundaries
    weight = 1,  # Boundary line weight
    opacity = 0.8,  # Opacity of the boundaries
    fill = FALSE  # Do not fill the boundaries
  )
map

# Exploratory Analysis Plots ----

# Default histogram of AQI
hist(nowcast_data$NowCastAQI, main = "Histogram of Nowcast AQI", xlab = "AQI Value", ylab = "Frequency")
abline(v = mean(nowcast_data$NowCastAQI, na.rm = TRUE), col = "red")

# Histogram incorporating the AQI ranges and corresponding colours
min_aqi <- min(nowcast_data$NowCastAQI)
max_aqi <- max(nowcast_data$NowCastAQI)

# Define AQI ranges and corresponding colors based on the min and max values
aqi_ranges <- c(seq(0, max_aqi, by = 50), max_aqi)
colors <- c("green", "yellow", "orange", "red", "purple", "maroon")

# Create a histogram with custom breaks and colors
hist(
  nowcast_data$NowCastAQI,
  breaks = aqi_ranges,
  main = "Histogram of Nowcast AQI",
  xlab = "AQI Value",
  ylab = "Frequency",
  col = colors,
  xlim = c(0, 500)  # Set the x-axis limits to cover the entire AQI range
)

# Add a legend
legend("topright", legend = c("Good (0-50)", "Moderate (51-100)", "Unhealthy for Sensitive Groups (101-150)", "Unhealthy (151-200)", "Very Unhealthy (201-300)", "Hazardous (301-500)"), fill = colors)


# Q-Q Norm plots
qqnorm(cleaned_data$Avg_PM2.5)
qqline(cleaned_data$Avg_PM2.5, col="red")

qqnorm(nowcast_data$NowCastAQI)
qqline(nowcast_data$NowCastAQI, col="red")

# Scatterplot Matrix (Perhaps not useful?)
pairs(cleaned_data[, c("Longitude", "Latitude", "Avg_PM2.5")],
      main="Scatterplot Matrix")

# Create a 3D scatter plot using plot3d
plot3d(cleaned_data$Latitude, cleaned_data$Longitude, cleaned_data$Avg_PM2.5, col = "green",
       xlab = "Latitude (LAT)", ylab = "Longitude", zlab = "Average PM2.5")

# Convert Measurement.date to Date format
cleaned_data$Measurement.date <- as.Date(cleaned_data$Measurement.date)
cleaned_data

# Monthly Average Plot
monthly_avg_pm25 <- cleaned_data %>%
  group_by(Month = format(Measurement.date, "%Y-%m")) %>%
  summarize(Avg_PM2.5 = mean(PM2.5, na.rm = TRUE))

ggplot(data = monthly_avg_pm25, aes(x = as.Date(paste(Month, "01", sep = "-")), y = Avg_PM2.5)) +
  geom_line() +
  labs(x = "Measurement Date", y = "Average PM2.5") +
  ggtitle("Monthly Average PM2.5")

# Weekly Average Plot
weekly_avg_pm25 <- cleaned_data %>%
  group_by(Week = lubridate::week(Measurement.date), Year = lubridate::year(Measurement.date)) %>%
  summarize(Avg_PM2.5 = mean(PM2.5, na.rm = TRUE))

gg <- ggplot(data = weekly_avg_pm25, aes(x = as.Date(paste(Year, Week, "1", sep = "-"), format = "%Y-%U-%u"), ymin = Avg_PM2.5, ymax = Avg_PM2.5, y = Avg_PM2.5)) +
  geom_ribbon(fill = "blue", alpha = 0.3) +  # Shaded region (You can remove this line)
  geom_line(color = "blue") +  # Average line
  geom_point(color = "blue") +  # Average points
  labs(x = "Measurement Date", y = "Average PM2.5", title = "Basic Ribbon Plot of Average PM2.5") +
  theme_minimal()

monthly_xticks <- scale_x_date(date_minor_breaks = "1 month", date_labels = "%Y")
trend_lines <- geom_smooth(method = "loess", se = FALSE, aes(group = Year), color = "orange")

gg + monthly_xticks + trend_lines
