library(tidyverse)
source('shared.R')

world <- read_csv('data/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv') %>%
  rename(province=`Province/State`, country=`Country/Region`, lat=`Lat`, lon=`Long`) %>%
  pivot_longer(-c(province, country, lat, lon), names_to='date', values_to='confirmed') %>%
  mutate(date = lubridate::mdy(date))

population <- tibble(
  country = c('Netherlands', 'Slovakia', 'Belgium', 'Czechia'),
  population = c(17.4e6, 5e6, 11.4e6, 10.6e6)
)

ggplot(
  world %>%
    filter(is.na(province)) %>%
    inner_join(population, by='country') %>%
    arrange(country, date) %>%
    mutate(
      c1m = 1e6 * confirmed / population,
      delta = if_else(
        country == lag(country),
        c1m - lag(c1m),
        NA_real_
      ),
      delta_7d = if_else(
        lead(country, n=3) == lag(country, n=4),
        (lead(c1m, n=3) - lag(c1m, n=4)) / 7,
        NA_real_
      )
    ),
  aes(date)
) +
  geom_line(aes(y = delta_7d), colour='red', size=1) +
  geom_point(aes(y = delta), colour='#6dae42', size=.5) +
  facet_wrap('country', ncol = 2, scales='free_y') +
  ylab('new confirmed cases per 1M population per day') +
  scale_x_date(date_breaks = '1 month', date_labels = '%b') +
  theme_kybcae
  #theme_linedraw()

ggsave('overview.png', dpi=96, width=12, height=8)
