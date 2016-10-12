library(dplyr)
library(nycflights13)
library(maps)

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


map(database = 'state')
symbols(arr$lon, arr$lat,
        bg = '#e2373f', fg = '#ffffff',
        circles = sqrt(arr$avg_arr_delay),
        inches = 0.1, add = TRUE, lwd = 0.5,
        main = '')
symbols(dep$lon, dep$lat,
        bg = '#233d4e', fg = '#ffffff',
        circles = sqrt(dep$avg_dep_delay),
        inches = 0.1, add = TRUE, lwd = 0.5,
        main = '')
