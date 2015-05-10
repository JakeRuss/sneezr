library("zipcode")
library("httr")
library("jsonlite")
library("stringr")
library("dplyr")
library("lubridate")

pollencast <- function(zip = NULL) {

  # Format zip code
  cleaned_zip <- clean.zipcodes(zips = zip)

  # Throw an error for zip codes returned as NA or not 5 digits
  if (nchar(cleaned_zip != 5) && is.na(cleaned_zip)) {
    stop("The provided zipcode is not acceptable.")
  }

  # Build the url for Claritin's API
  base_url <- "http://www.claritin.com/"
  app_url  <- "weatherpollenservice/weatherpollenservice.svc/getforecast/"
  api_url  <- paste0(base_url, app_url, zip)

  res <- GET(url = api_url)

  # Throw an error if the API returns non 200 status code
  stop_for_status(res)

  # Create a list of data from returned json
  tmp_json <- res %>%
    # Extract the data from response object
    content() %>%
    # Parse the json into nested list
    fromJSON() %>%
    # Un-nest and create data frames
    lapply(., data.frame, stringsAsFactors = FALSE)

  # Tidy the pollen count data
  pollen <- tmp_json$pollenForecast %>%
    mutate(city          = tolower(city),
           state         = tolower(state),
           pp            = str_trim(pp),
           timestamp     = parse_date_time(x      = timestamp,
                                           orders = "%m%d,%Y%I%M!%S%p"),
           forecast.date = seq.Date(from = as.Date(timestamp[1]),
                                    to   = as.Date(timestamp[1]) + 3,
                                    by   = 1)) %>%
    dplyr::rename(predominant = pp, pollen.ts = timestamp, pollen.count = forecast)

  # Tidy weather forcast
  weather <- tmp_json$weatherForecast %>%
    mutate(date          = parse_date_time(x      = date,
                                           orders = "%m%d,%Y%I%M!%S%p"),
           city          = tolower(city),
           state         = tolower(state),
           forecast.date = parse_date_time(x      = forecast.date,
                                           orders = "%m%d,%Y%I%M!%S%p") %>% as.Date()) %>%
    rename(weather.ts = date, temp.low = forecast.lowF, temp.high = forecast.highF,
           description.day = forecast.phraseDay, description.night = forecast.phraseNight)

  # Join pollen and weather data
  combined <- pollen %>%
    inner_join(x = ., y = weather, by = c("zip", "city", "state", "forecast.date")) %>%
  # Extract and re-order columns
  select(forecast.date, zip, city, state, pollen.count, predominant, pollen.ts,
         temp.high, temp.low, description.day, description.night, weather.ts)

  # Return the tidy data
  return(combined)
}


