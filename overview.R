library(tidyverse)

world <- read_csv('data/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv') %>%
  rename(province=`Province/State`, country=`Country/Region`, lat=`Lat`, lon=`Long`) %>%
  pivot_longer(-c(province, country, lat, lon), names_to='date', values_to='confirmed') %>%
  mutate(date = lubridate::mdy(date))

population <- tibble(
  country = c('Netherlands', 'Slovakia', 'Belgium'),
  population = c(17.4e6, 5e6, 11.4e6)
)

ggplot(
  world %>%
    filter(is.na(province)) %>%
    inner_join(population, by='country') %>%
    arrange(country, date) %>%
    mutate(
      c1m = 1e6 * confirmed / population,
      delta = c1m - lag(c1m),
      delta_7d = (c1m - lag(c1m, n=7)) / 7
    ) %>%
    filter(date >= lubridate::ymd('2020-03-01')),
  aes(date)
) +
  geom_line(aes(y = delta_7d), colour='red', size=2) +
  geom_point(aes(y = delta), size=.5) +
  facet_wrap('country', ncol = 2) +
  ylab('new confirmed cases per 1M population per day') +
  scale_x_date(date_breaks = '1 month') +
  theme_linedraw()
