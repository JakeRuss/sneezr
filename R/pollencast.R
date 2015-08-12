#'Retrieve pollen count forecast
#'
#'@param zip five digit zip code
#'@examples
#'df  <- pollencast(zip = 20238)
#'df2 <- pollencast(zip = "20238")
#'@export

pollencast <- function(zip = NULL) {

  # Format zip code
  cleaned_zip <- zipcode::clean.zipcodes(zips = zip)

  # Throw an error for zip codes returned as NA or not 5 digits
  if (nchar(cleaned_zip != 5) && is.na(cleaned_zip)) {
    stop("The provided zipcode is not acceptable.")
  }

  # Build the url for Claritin's API
  base_url <- "http://www.claritin.com/"
  api_url  <- paste0(base_url, "webservice/allergyforecast.php")

  res <- httr::POST(url = api_url, query = list(zip = cleaned_zip))

  # Throw an error if the API returns non 200 status code
  httr::stop_for_status(res)

  # Create a list of data from returned json
  tmp_json <- res %>%
    # Extract the data from response object
    httr::content(as = "text") %>%
    # Parse the json into nested list
    jsonlite::fromJSON() %>%
    # Un-nest and create data frames
    lapply(., data.frame, stringsAsFactors = FALSE)

  # Tidy the pollen count data
  pollen <- tmp_json$pollenForecast %>%
    dplyr::mutate(city          = tolower(city),
                  state         = tolower(state),
                  pp            = stringr::str_trim(pp),
                  timestamp     = lubridate::parse_date_time(x      = timestamp,
                                                             orders = "%m%d,%Y%I%M!%S%p"),
                  forecast.date = seq.Date(from = as.Date(timestamp[1]),
                                           to   = as.Date(timestamp[1]) + 3,
                                           by   = 1)) %>%
    dplyr::rename(predominant = pp, pollen.ts = timestamp, pollen.count = forecast)

  # Tidy weather forcast
  weather <- tmp_json$weatherForecast %>%
    dplyr::mutate(date          = lubridate::parse_date_time(x      = date,
                                                             orders = "%m%d,%Y%I%M!%S%p"),
                  city          = tolower(city),
                  state         = tolower(state),
                  forecast.date = lubridate::parse_date_time(x      = forecast.date,
                                                             orders = "%m%d,%Y%I%M!%S%p") %>% as.Date()) %>%
    dplyr::rename(weather.ts = date, temp.low = forecast.lowF, temp.high = forecast.highF,
           description.day = forecast.phraseDay, description.night = forecast.phraseNight)

  # Join pollen and weather data
  combined <- pollen %>%
    dplyr::inner_join(x = ., y = weather, by = c("zip", "city", "state", "forecast.date")) %>%
  # Extract and re-order columns
  dplyr::select(forecast.date, zip, city, state, pollen.count, predominant, pollen.ts,
                temp.high, temp.low, description.day, description.night, weather.ts)

  # Return the tidy data
  return(combined)
}


