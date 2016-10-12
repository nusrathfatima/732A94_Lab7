#' Average departure and arrival delays
#'
#' The function calculates the average departure and arrival
#' delays of of all airports of the dataset \code{nycflights13}.
#' The function then prints a map of the United States of America and
#' visualizes the average delay per airport in a point diagram.
#' The size of the points represents the average delay time for the
#' specific airport.
#'
#'
#' @import dplyr
#' @import maps
#' @import nycflights13
#'
#' @return A map object

airport_delay <- function() {
  # Prepare airport data for join
  airport <- nycflights13::airports %>%
    mutate(ID = faa) %>%
    select(-faa, -alt, -tz, -dst)

  # calculate mean values for each departure
  dep <- nycflights13::flights %>%
    group_by(origin) %>%
    summarise(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
    arrange(avg_dep_delay) %>%
    mutate(ID = origin) %>%
    select(-origin) %>%
    right_join(airport, by = 'ID')

  # calculate mean values for each destination
  arr <- nycflights13::flights %>%
    group_by(dest) %>%
    summarise(avg_arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
    arrange(avg_arr_delay)%>%
    mutate(ID = dest) %>%
    select(-dest) %>%
    right_join(airport, by = 'ID')

  res <- map(database = 'state') %>%
    symbols(arr$lon, arr$lat,
            bg = '#e2373f', fg = '#ffffff',
            circles = sqrt(arr$avg_arr_delay),
           inches = 0.1, add = TRUE, lwd = 0.5) %>%
    symbols(dep$lon, dep$lat,
            bg = '#233d4e', fg = '#ffffff',
            circles = sqrt(dep$avg_dep_delay),
            inches = 0.1, add = TRUE, lwd = 0.5) %>%
    title('Average departure and arrival delays by US airports')

  res
}


