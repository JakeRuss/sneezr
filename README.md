# sneezr
An R wrapper for Pollen.com's Pollen Forecast API

## Disclaimer

This package is not on CRAN and probably never will be, as it is for personal 
use only. The package is not endorsed by Pollen.com and is only intended as a
demonstration of R package building.

## Installation
```
# install.packages("devtools")
devtools::install_github("jakeruss/sneezr")
```

## Example usage
```
df <- pollencast(zip = 77002)
```
