## packages need to be import
pkg <- c("RCurl", "car", "lubridate", "mice", "R306", "rmeta", "RColorBrewer", "rjson")
sapply(pkg, function(x) devtools::use_package(x))

## build documentation
devtools::document()
