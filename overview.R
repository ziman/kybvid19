library(tidyverse)
source('shared.R')

load <- function(fname, quantity) {
  read_csv(fname) %>%
    rename(province=`Province/State`, country=`Country/Region`, lat=`Lat`, lon=`Long`) %>%
    pivot_longer(-c(province, country, lat, lon), names_to='date', values_to='confirmed') %>%
    mutate(date = lubridate::mdy(date), quantity=quantity)
}

world <- rbind(
  load('data/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv', 'confirmed'),
  load('data/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv', 'deaths')
)

population <- tibble(
  country = c('Netherlands', 'Slovakia', 'Belgium', 'Czechia'),
  population = c(17.4e6, 5e6, 11.4e6, 10.6e6)
)

ggplot(
  world %>%
    filter(is.na(province), quantity == 'confirmed') %>%
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
  xlab(NULL) +
  ylab('new confirmed cases per 1M population per day') +
  scale_x_date(date_breaks = '1 month', date_labels = '%b') +
  theme_kybcae
  #theme_linedraw()

ggsave('be-nl-cz-sk.png', dpi=96, width=8, height=6)

ggplot(
  world %>%
    filter(is.na(province), quantity=='confirmed') %>%
    filter(country == 'Netherlands') %>%
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
  xlab(NULL) +
  ylab('new confirmed cases per 1M population per day') +
  scale_x_date(date_breaks = '1 month', date_labels = '%b') +
  theme_kybcae
#theme_linedraw()

ggsave('nl.png', dpi=96, width=5, height=4)

ggplot(
  world %>%
    filter(is.na(province), quantity=='deaths') %>%
    filter(country == 'Netherlands') %>%
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
  xlab(NULL) +
  ylab('new deaths per 1M population per day') +
  scale_x_date(date_breaks = '1 month', date_labels = '%b') +
  theme_kybcae
#theme_linedraw()

ggsave('nl-deaths.png', dpi=96, width=5, height=4)

ggplot(
  world %>%
    filter(is.na(province), quantity=='confirmed') %>%
    filter(country == 'Czechia') %>%
    arrange(country, date) %>%
    mutate(
      country = 'Česká republika',
      c1m = confirmed,
      delta = if_else(
        country == lag(country),
        c1m - lag(c1m),
        NA_real_
      ),
      delta_7d = if_else(
        lead(country, n=3) == lag(country, n=4),
        (lead(c1m, n=3) - lag(c1m, n=4)) / 7,
        NA_real_
      ),
      delta_estimate = if_else(
        is.na(lead(country, n=3)),
        delta,
        NA_real_
      )
    ),
  aes(date)
) +
  geom_line(aes(y = delta_7d), colour='red', size=1) +
  geom_line(aes(y = delta_estimate), colour='red', linetype='dashed') +
  geom_point(aes(y = delta), shape='x') +
  facet_wrap('country', ncol = 2, scales='free_y') +
  xlab(NULL) +
  ylab('počet nových potvrdených prípadov za deň') +
  scale_x_date(date_breaks = '1 month', date_labels = '%b') +
  theme_linedraw()

ggsave('cz.png', dpi=96, width=8, height=6)

