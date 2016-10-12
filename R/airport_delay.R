library(nycflights13)
library(maps)

# calculate mean values for each departure
dep <- nycflights13::flights %>%
  group_by(origin) %>%
  summarise(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  arrange(avg_dep_delay) %>%
  mutate(ID = origin) %>%
  select(-origin)

# calculate mean values for each destination
arr <- nycflights13::flights %>%
  group_by(dest) %>%
  summarise(avg_arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(avg_arr_delay)%>%
  mutate(ID = dest) %>%
  select(-dest)

# Join all tables
res <- nycflights13::airports %>%
  mutate(ID = faa) %>%
  select(-faa, -alt, -tz, -dst) %>%
  left_join(dep, by = 'ID') %>%
  left_join(arr, by = 'ID')

map(database = 'world')
symbols(res$lon, res$lat,
        bg = '#e2373f', fg = '#ffffff',
        circles = sqrt(res$avg_arr_delay),
        inches = 0.03, add = TRUE, lwd = 0.5)
