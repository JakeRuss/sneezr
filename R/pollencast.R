#'Retrieve pollen count forecast
#'
#'@param zip five digit zip code
#'@param api_key API key string from Accuweather
#'@param forecast_days desired number of forecast days; 1 by default, accepts 1, 5, 10, and 15
#'@param metric Use metric system? defaults to "false"
#'@param language defaults to US English, see Accuweather API docs for supported languages
#'@param user_agent user agent string (optional); otherwise uses httr's default user agent
#'@examples
#'df  <- pollencast(zip = 20238)
#'df2 <- pollencast(zip = "20238")
#'@export

pollencast <- function(zip           = NULL,
                       api_key       = NULL,
                       forecast_days = 1,
                       metric        = "false",
                       language_code = "en-us",
                       user_agent    = NULL) {

  # Format zip code
  cleaned_zip <- zipcode::clean.zipcodes(zips = zip)

  # Throw an error for zip codes returned as NA or not 5 digits
  if (nchar(cleaned_zip) != 5 && is.na(cleaned_zip)) {
    stop("Please supply a valid 5-digit zip code.")
  }

  # Throw an error if API key not supplied
  if (is.null(api_key)) {
    stop("Please supply an Accuweather API key")
  }

  # Throw an error is forecast_days is not acceptable number
  if (!(forecast_days %in% c(1, 5, 10, 15))) {
    stop("Forecast days must be one of 1, 5, 10, or 15")
  }

  # Throw an error if metric is true or false
  if (!(tolower(metric) %in% c("true", "false"))) {
    stop("Metric must be either 'true' or 'false'")
  }

  # Build the API url for Accuweather.com
  api_url <- paste0(
    "http://dataservice.accuweather.com/forecasts/v1/daily/",
    forecast_days,
    "day/",
    cleaned_zip,
    "?apikey=",
    api_key,
    "&language=",
    language_code,
    "&details=true&metric=",
    metric)

  # Set the user_agent flexibly
  if (is.null(user_agent)) {
    ua <- httr::user_agent(httr:::default_ua())
  } else {
    ua <- httr::user_agent(user_agent)
  }

  # Make the GET request using supplied headers and api_url
  res <- httr::GET(url = api_url, ua)

  # Throw an error if the API returns non 200 status code
  httr::stop_for_status(res)

  # Create a list of data from returned json via jsonlite
  json_content <- res %>%
    # Extract the data from response object
    httr::content(as = "text") %>%
    jsonlite::fromJSON()

  forecast_dates <- json_content$DailyForecasts$Date

  tidy_pollen_data <- json_content$DailyForecasts$AirAndPollen %>%
    rlang:::set_names(forecast_dates) %>%
    dplyr::bind_rows(.id = "forecast_date") %>%
    dplyr::mutate(forecast_date    = readr::parse_datetime(
                    x      = forecast_date,
                    format = "%Y-%m-%dT%H:%M:%S%z"
                  ),
                  zip_code         = cleaned_zip,
                  forecast_created = as.POSIXlt(Sys.time(), tz = "UTC") %>% as.POSIXct()) %>%
    janitor::clean_names() %>%
    dplyr::select(zip_code, dplyr::everything())

  # Return the tidied data
  tidy_pollen_data
}


