#'Retrieve pollen count forecast
#'
#'@param zip five digit zip code
#'@param user_agent user agent string (optional); otherwise uses httr's default user agent
#'@examples
#'df  <- pollencast(zip = 20238)
#'df2 <- pollencast(zip = "20238")
#'@export

pollencast <- function(zip = NULL, user_agent = NULL) {

  # Format zip code
  cleaned_zip <- zipcode::clean.zipcodes(zips = zip)

  # Throw an error for zip codes returned as NA or not 5 digits
  if (nchar(cleaned_zip) != 5 && is.na(cleaned_zip)) {
    stop("Please supply a valid 5-digit zip code.")
  }

  # Build the API url for Pollen.com
  api_url <- paste0("https://www.pollen.com/api/forecast/current/pollen/", cleaned_zip)

  # Pollen.com validates against the referer header
  pollen_headers <- c(
               `Content-Type`    = "application/json; charset=utf-8",
               Referer           = paste0("https://www.pollen.com/forecast/current/pollen/", cleaned_zip),
               Host              = "www.pollen.com",
               Connection        = "keep-alive",
               `Accept-Language` = "en-US,en;q=0.8",
               `Accept-Encoding` = "gzip, deflate, sdch, br",
               Accept            = "application/json, text/plain, */*")

  # Set the user_agent flexibly
  if (is.null(user.agent)) {
    ua <- httr::user_agent(httr:::default_ua())
  } else {
    ua <- httr::user_agent(user.agent)
  }

  # Make the GET request using supplied headers and api_url
  res <- httr::GET(url = api_url, httr::add_headers(.headers = pollen_headers), ua)

  # Throw an error if the API returns non 200 status code
  httr::stop_for_status(res)

  # Create a list of data from returned json via jsonlite
  json_content <- res %>%
    # Extract the data from response object
    httr::content(as = "text") %>%
    jsonlite::fromJSON()

  # Tidy data from list to a data frame
  tidy_pollen_data <- dplyr::data_frame(
    forecast.date       = json_content$ForecastDate %>% as.Date(),
    zip.code            = json_content$Location$ZIP,
    city                = json_content$Location$City,
    state               = json_content$Location$State,
    pollen.date         = json_content$Location$periods$Type,
    pollen.count        = json_content$Location$periods$Index,
    pollen.trigger      = json_content$Location$periods$Triggers[[1]]$Name      %>% paste(collapse = ", "),
    pollen.trigger.type = json_content$Location$periods$Triggers[[1]]$PlantType %>% paste(collapse = ", ")) %>%

    # Manage the dates; note the case_when syntax, works for dplyr 0.5, will change in dplyr 0.6
    dplyr::mutate(pollen.date = dplyr::case_when(.$pollen.date %in% "Today"     ~ as.character(.$forecast.date),
                                                 .$pollen.date %in% "Yesterday" ~ as.character(.$forecast.date - 1),
                                                 .$pollen.date %in% "Tomorrow"  ~ as.character(.$forecast.date + 1)),
                  pollen.date = as.Date(pollen.date))

  # Return the tidied data
  tidy_pollen_data
}


