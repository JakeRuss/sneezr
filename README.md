# sneezr
sneezr is an R wrapper around Accuweather's Pollen Forecast

## Disclaimer

This package is not on CRAN and probably never will be, as it is for personal 
use only. The package is not endorsed by Accuweather.com and is only intended 
as a demonstration of R package building.

## Installation
```
# install.packages("devtools")
devtools::install_github("jakeruss/sneezr")
```

## Example usage
```
df <- pollencast(zip = 77002, api_key = api_key, forecast_days = 5)
```
