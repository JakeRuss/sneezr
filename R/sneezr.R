library("zipcode")
library("httr")
library("tidyjson")
library("dplyr")

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
  
  # Create a json object
  tmp_json <- content(res) %>% fromJSON()
  
  return(tmp_json)
  
}

tidy_json <- dat %>%
  str_replace_all(pattern = fixed("\\\\") , replacement = "")
  
  as.tbl_json()

