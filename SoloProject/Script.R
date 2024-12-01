# Function to check if a year is a leap year
is_leap_year <- function(year) {
  return((year %% 4 == 0 && year %% 100 != 0) || (year %% 400 == 0))
}

# Function to convert local time to UTC
convert_to_utc <- function(date, time, timezone) {
  # Extract components of the date and time
  day <- as.numeric(substr(date, 1, 2))
  month <- as.numeric(substr(date, 4, 5))
  year <- as.numeric(substr(date, 7, 10))
  hour <- as.numeric(substr(time, 1, 2))
  minute <- as.numeric(substr(time, 4, 5))
  second <- as.numeric(substr(time, 7, 8))
  
  # Determine the base offset for the time zone
  offset <- switch(
    timezone,
    "Eastern time" = -5,  # UTC-5
    "Pacific time" = -8,  # UTC-8
    "UTC" = 0,            # No offset
    0                     # Default value for unknown time zones
  )
  
  # Convert local time to UTC time
  hour_utc <- hour + offset
  
  # Handle hour overflow (next or previous day)
  if (hour_utc >= 24) {
    hour_utc <- hour_utc - 24
    day <- day + 1
  } else if (hour_utc < 0) {
    hour_utc <- hour_utc + 24
    day <- day - 1
  }
  
  # Handle day/month/year overflow
  days_in_month <- c(31, ifelse(is_leap_year(year), 29, 28), 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  if (day > days_in_month[month]) {
    day <- 1
    month <- month + 1
    if (month > 12) {
      month <- 1
      year <- year + 1
    }
  } else if (day < 1) {
    month <- month - 1
    if (month < 1) {
      month <- 12
      year <- year - 1
    }
    day <- days_in_month[month]
  }
  
  # Final formatting of the date and time in UTC
  date_utc <- sprintf("%02d/%02d/%04d", day, month, year)
  time_utc <- sprintf("%02d:%02d:%02d", hour_utc, minute, second)
  
  return(paste(date_utc, time_utc))
}

# Function to safely import files
safe_read_csv <- function(file_path) {
  data <- tryCatch(
    read.csv(file_path, stringsAsFactors = FALSE),
    error = function(e) {
      stop(paste("Error while reading the file:", file_path, "\n", e))
    }
  )
  return(data)
}

# Load data
advertisers <- safe_read_csv("data/advertiser.csv")
campaigns <- safe_read_csv("data/campaigns.csv")
clicks <- read.csv("data/clicks.tsv", sep = "\t", stringsAsFactors = FALSE)
impressions <- read.csv("data/impressions.tsv", sep = "\t", stringsAsFactors = FALSE)

# Convert time zones to UTC
clicks$datetime_utc <- mapply(convert_to_utc, clicks$date, clicks$time, clicks$timezone)
impressions$datetime_utc <- mapply(convert_to_utc, impressions$date, impressions$time, impressions$timezone)

# Merge data
clicks_processed <- merge(clicks, campaigns, by.x = "campaign_id", by.y = "id", all.x = TRUE)
clicks_processed <- merge(clicks_processed, advertisers, by.x = "advertiser_id", by.y = "ID", all.x = TRUE)

impressions_processed <- merge(impressions, campaigns, by.x = "campaign_id", by.y = "id", all.x = TRUE)
impressions_processed <- merge(impressions_processed, advertisers, by.x = "advertiser_id", by.y = "ID", all.x = TRUE)

# Export processed files
write.csv(clicks_processed, "data/clicks_processed.csv", row.names = FALSE)
write.csv(impressions_processed, "data/impressions_processed.csv", row.names = FALSE)
