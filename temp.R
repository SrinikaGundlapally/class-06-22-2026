#!/usr/bin/env Rscript

# temp.R - Retrieve historical daily high temperatures using openmeteo
#
# Usage:
#   Rscript temp.R [location] [start_date] [end_date] [output_csv]
#
# Example:
#   Rscript temp.R "Portland" "1990-01-01"

# ----------------------------------------------------------------------
# 1. Dependency Checks & Setup
# ----------------------------------------------------------------------
required_packages <- c("openmeteo", "httr")
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop(sprintf(
    "Missing required package(s): %s. Please install them first (e.g., install.packages('%s')).",
    paste(missing_packages, collapse = ", "),
    missing_packages[1]
  ))
}

library(openmeteo)
library(httr)

# Configure httr:
# - ipresolve = 1: force IPv4 only to avoid SSL connection timeouts in Docker/Cloud environments.
# - timeout = 30: allow up to 30 seconds for the request to complete.
httr::set_config(httr::config(ipresolve = 1, timeout = 30))

# Helper function to retry a block of code on failure
retry_call <- function(expr, name = "API Call", max_attempts = 3, delay_secs = 3) {
  for (attempt in 1:max_attempts) {
    result <- tryCatch({
      expr
    }, error = function(e) {
      if (attempt == max_attempts) {
        stop(sprintf("Failed after %d attempts. Error: %s", max_attempts, e$message))
      }
      message(sprintf("%s failed (attempt %d/%d): %s. Retrying in %d seconds...", 
                      name, attempt, max_attempts, trimws(e$message), delay_secs))
      Sys.sleep(delay_secs)
      NULL
    })
    if (!is.null(result)) {
      return(result)
    }
  }
}

# ----------------------------------------------------------------------
# 2. Parse Arguments or Set Defaults
# ----------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

location_query <- if (length(args) >= 1) args[1] else "Portland"
start_date     <- if (length(args) >= 2) args[2] else "1990-01-01"
# Historical weather archive has a latency of 2-3 days, so default to 3 days ago
end_date       <- if (length(args) >= 3) args[3] else as.character(Sys.Date() - 3)
output_file    <- if (length(args) >= 4) args[4] else "portland_daily_highs.csv"

# ----------------------------------------------------------------------
# 3. Geocode Location to coordinates
# ----------------------------------------------------------------------
message(sprintf("Geocoding location: '%s'...", location_query))
location_info <- retry_call({
  geocode(location_query)
}, name = "Geocoding API")

if (is.null(location_info) || nrow(location_info) == 0) {
  stop(sprintf("No matches found for location: '%s'. Please try a more specific name.", location_query))
}

# Extract details from the first matching result
lat <- location_info[["latitude"]][1]
lon <- location_info[["longitude"]][1]
resolved_name <- location_info[["name"]][1]
admin_region <- if (!is.na(location_info[["admin1"]][1])) location_info[["admin1"]][1] else ""
country <- if (!is.na(location_info[["country"]][1])) location_info[["country"]][1] else location_info[["country_code"]][1]

message(sprintf("Resolved location: %s, %s, %s (Lat: %.4f, Lon: %.4f)", 
                resolved_name, admin_region, country, lat, lon))

# ----------------------------------------------------------------------
# 4. Fetch Weather Data from Open-Meteo
# ----------------------------------------------------------------------
message(sprintf("Fetching daily maximum temperatures from %s to %s...", start_date, end_date))

weather_data <- retry_call({
  weather_history(
    location = c(lat, lon),
    start = start_date,
    end = end_date,
    daily = "temperature_2m_max",
    response_units = list(temperature_unit = "fahrenheit")
  )
}, name = "Historical Weather API")

# ----------------------------------------------------------------------
# 5. Format and Clean Data
# ----------------------------------------------------------------------
# Rename columns
colnames(weather_data) <- c("Date", "Max_Temp_F")

# Compute Celsius equivalent
weather_data[["Max_Temp_C"]] <- round((weather_data[["Max_Temp_F"]] - 32) * 5/9, 1)

# Sort chronologically
weather_data <- weather_data[order(weather_data[["Date"]]), ]

# ----------------------------------------------------------------------
# 6. Save Data to CSV
# ----------------------------------------------------------------------
write.csv(weather_data, file = output_file, row.names = FALSE)
message(sprintf("Saved %d daily temperature records to '%s'.", nrow(weather_data), output_file))

# ----------------------------------------------------------------------
# 7. Calculate and Print Climate Insights
# ----------------------------------------------------------------------
message("\n==================================================")
message(sprintf("   CLIMATE STATISTICS FOR %s   ", toupper(resolved_name)))
message("==================================================")
cat(sprintf("Period:             %s to %s\n", min(weather_data[["Date"]]), max(weather_data[["Date"]])))
cat(sprintf("Total Observations: %d days\n", nrow(weather_data)))

# Summary statistics
avg_f <- mean(weather_data[["Max_Temp_F"]], na.rm = TRUE)
avg_c <- mean(weather_data[["Max_Temp_C"]], na.rm = TRUE)
cat(sprintf("Mean Daily High:    %.1f°F (%.1f°C)\n", avg_f, avg_c))

# Extremes
hottest_idx <- which.max(weather_data[["Max_Temp_F"]])
coldest_idx <- which.min(weather_data[["Max_Temp_F"]])

if (length(hottest_idx) > 0) {
  cat(sprintf("Hottest Daily High: %.1f°F (%.1f°C) on %s\n", 
              weather_data[["Max_Temp_F"]][hottest_idx], 
              weather_data[["Max_Temp_C"]][hottest_idx], 
              weather_data[["Date"]][hottest_idx]))
}
if (length(coldest_idx) > 0) {
  cat(sprintf("Coldest Daily High: %.1f°F (%.1f°C) on %s\n", 
              weather_data[["Max_Temp_F"]][coldest_idx], 
              weather_data[["Max_Temp_C"]][coldest_idx], 
              weather_data[["Date"]][coldest_idx]))
}

# Top 5 hottest days
message("\n--- Top 5 Hottest Days on Record ---")
top_hottest <- weather_data[order(-weather_data[["Max_Temp_F"]]), ][1:5, ]
for (i in 1:nrow(top_hottest)) {
  cat(sprintf("%d. %s: %.1f°F (%.1f°C)\n", 
              i, top_hottest[["Date"]][i], top_hottest[["Max_Temp_F"]][i], top_hottest[["Max_Temp_C"]][i]))
}

# Decade-over-decade trend
weather_data[["Decade"]] <- paste0(as.integer(format(weather_data[["Date"]], "%Y")) %/% 10 * 10, "s")
dec_mean <- aggregate(Max_Temp_F ~ Decade, data = weather_data, FUN = mean, na.rm = TRUE)
dec_max  <- aggregate(Max_Temp_F ~ Decade, data = weather_data, FUN = max, na.rm = TRUE)
decade_df <- merge(dec_mean, dec_max, by = "Decade")
colnames(decade_df) <- c("Decade", "Mean_High_F", "Record_High_F")

# Add Celsius conversions for readability
decade_df[["Mean_High_C"]] <- round((decade_df[["Mean_High_F"]] - 32) * 5/9, 1)
decade_df[["Record_High_C"]] <- round((decade_df[["Record_High_F"]] - 32) * 5/9, 1)

# Round values for display
decade_df[["Mean_High_F"]] <- round(decade_df[["Mean_High_F"]], 1)
decade_df[["Record_High_F"]] <- round(decade_df[["Record_High_F"]], 1)

message("\n--- Decade-over-Decade Trends ---")
print(decade_df, row.names = FALSE)
