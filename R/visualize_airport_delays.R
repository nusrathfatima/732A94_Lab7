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
#' @import plotly
#'
#' @return A ggplot2 object

visualize_airport_delays <- function() {
  # Prepare airport data for join
  airport <- nycflights13::airports %>%
    dplyr::mutate(ID = faa) %>%
    dplyr::select(-faa, -alt, -tz, -dst)

  # Calculate the table for all delayed departures
  dep <- nycflights13::flights %>%
    dplyr::group_by(origin) %>%
    dplyr::summarise(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
    dplyr::arrange(avg_dep_delay) %>%
    dplyr::mutate(ID = origin) %>%
    dplyr::select(-origin) %>%
    dplyr::right_join(airport, by = 'ID') %>%
    dplyr:: filter(!is.na(avg_dep_delay)) %>%
    dplyr::mutate(type = ifelse(avg_dep_delay >= 0, 'behind', 'ahead')) %>%
    dplyr::mutate(avg_dep_delay = ifelse(avg_dep_delay < 0, avg_dep_delay * (-1), avg_dep_delay))

  # Calculate the table for all delayed arrivals
  arr <- nycflights13::flights %>%
    dplyr::group_by(dest) %>%
    dplyr::summarise(avg_arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
    dplyr::arrange(avg_arr_delay)%>%
    dplyr::mutate(ID = dest) %>%
    dplyr::select(-dest) %>%
    dplyr::right_join(airport, by = 'ID') %>%
    dplyr::filter(!is.na(avg_arr_delay)) %>%
    dplyr::mutate(type = ifelse(avg_arr_delay >= 0, 'behind', 'ahead')) %>%
    dplyr::mutate(avg_arr_delay = ifelse(avg_arr_delay < 0, avg_arr_delay * (-1), avg_arr_delay))

  # Calculate the min and max longitude / latidute to determine the
  # size of the maß
  min_max_lon <- c(min(dep$lon, arr$lon), max(dep$lon, arr$lon)) * 1.02
  min_max_lat <- c(min(dep$lat, arr$lat), max(dep$lat, arr$lat)) * 1.02

  # Import a world map
  us <- ggplot2::map_data('world')

  # Make some specifications of the predefined '_minimal()' theme
  # to make the plot look nicer
  theme_map <- theme_minimal() %+replace% theme(
    panel.background = element_rect(color = 'lightblue', fill = 'lightblue'),
    panel.border = element_rect(color = 'NA', fill = 'NA'),
    panel.grid = element_line(color = 'NA'),
    panel.grid.major = element_line(color = 'NA'),
    panel.grid.minor = element_line(color = 'NA'),
    legend.position = 'none'
  )

  # Create the ggplot object and return it
  ggplot2::ggplot() +
    geom_polygon(data = us, fill = '#FFFFE0', color = 'black',
                 aes(x = long, y = lat, group = group)) +
    geom_point(data = arr, aes(x = arr$lon, y = arr$lat,
                           fill = as.factor(type),
                           size = sqrt(arr$avg_arr_delay),
                           alpha = 0.7),
               shape = 21) +
    geom_point(data = dep, aes(x = dep$lon, y = dep$lat,
                               fill = as.factor(type),
                               size = sqrt(dep$avg_dep_delay),
                               alpha = 0.7),
               shape = 24) +
    coord_cartesian(xlim = min_max_lon, ylim = min_max_lat) +
    ggtitle('Average delay of flights at airports\nwithin the United States of America') +
    xlab('Longitude [°]') +
    ylab('Latitude [°]') +
    theme_map
}

