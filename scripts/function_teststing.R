# testing functions using https://swcarpentry.github.io/r-novice-inflammation/02-func-R/index.html

fahrenheit_to_celsius <- function(temp_F) {
  temp_C <- (temp_F - 32) * 5 / 9
  return(temp_C)
}

# e.g.

fahrenheit_to_celsius(212)

# now make another and combine
celsius_to_kelvin <- function(temp_C) {
  temp_K <- temp_C + 273.15
  return(temp_K)
}

celsius_to_kelvin(0)

fahrenheit_to_kelvin <- function(temp_F) {
  temp_C <- fahrenheit_to_celsius(temp_F)
  temp_K <- celsius_to_kelvin(temp_C)
  return(temp_K)
}

fahrenheit_to_kelvin(32.0)

# ^ this can also look like this:

celsius_to_kelvin(fahrenheit_to_celsius(32.0))

