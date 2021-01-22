library(tidyverse)
#library(bbplot)
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
  country = c('Netherlands', 'Slovakia', 'Belgium', 'Czechia', 'France', 'Germany'),
  population = c(17.4e6, 5e6, 11.4e6, 10.6e6, 67e6, 83e6)
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
    filter(is.na(province), quantity == 'deaths') %>%
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
  ylab('deaths per 1M population per day') +
  scale_x_date(date_breaks = '1 month', date_labels = '%b') +
  theme_kybcae
#theme_linedraw()

ggsave('deaths-be-nl-cz-sk.png', dpi=96, width=8, height=6)

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
    filter(is.na(province), quantity=='deaths') %>%
    filter(country == 'Slovakia') %>%
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

ggsave('sk-deaths.png', dpi=96, width=5, height=4)

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

ggplot(
  world %>%
    filter(
      is.na(province),
      country %in% c('Czechia', 'Slovakia'),
      date >= lubridate::ymd('2020-08-01')
    ) %>%
    inner_join(population, by='country') %>%
    mutate(
      date = if_else(
        country == 'Czechia',
        date + 16,
        date
      )
    ) %>%
    arrange(country, quantity, date) %>%
    mutate(
      c1m = 1e6 * confirmed / population,
      delta = if_else(
        country == lag(country) & quantity == lag(quantity),
        c1m - lag(c1m),
        NA_real_
      ),
      delta_7d = if_else(
        lead(country, n=3) == lag(country, n=4)
        & lead(quantity, n=3) == lag(quantity, n=4),
        (lead(c1m, n=3) - lag(c1m, n=4)) / 7,
        NA_real_
      )
    ),
  aes(date)
) +
  geom_line(aes(y = delta_7d, colour=country), size=2) +
  geom_point(aes(y = delta, colour=country), size=.5) +
  facet_wrap('quantity', ncol = 2, scales='free_y') +
  xlab(NULL) +
  ylab('new confirmed cases per 1M population per day') +
  scale_x_date(date_breaks = '1 month', date_labels = '%b') +
  scale_colour_discrete(labels=c('CZ pred 14 dňami', 'SK')) +
  theme_minimal()
  # bbc_style()

#finalise_plot(
#  plot_name = last_plot(),
#  source = "Source: John Hopkins University",
#  save_filepath = "cz-sk.png",
#  width_pixels = 640,
#  height_pixels = 550
#)

coeffs <- tribble(
  ~country,      ~quantity,   ~k,     ~d, ~q,         ~w,
  'Czechia',     'confirmed', 2.8/50,  0,  1,          0,
  'Czechia',     'deaths',    4.0/50,  0,  1/0.2e-2,   0,
  'Netherlands', 'confirmed', 1.9/50,  0,  1,          0,
  'Netherlands', 'deaths',    1.9/50, 10,  1/0.4e-2,   0,
  'Belgium',     'confirmed', 1.9/50,  0,  1,          0,
  'Belgium',     'deaths',    1.9/50, 10,  1/0.6e-2,   0,
  'Slovakia',    'confirmed', 2.8/50,  0,  1,          0,
  'Slovakia',    'deaths',    2.8/50,  0,  1,          0
)

countries <- world %>%
  filter(is.na(province)) %>%
  filter(country == 'Czechia') %>%
  filter(date >= lubridate::ymd('2020-08-01')) %>%
  inner_join(population, by='country') %>%
  arrange(quantity, country, date) %>%
  group_by(quantity, country) %>%
  mutate(
    date_n = date - lubridate::ymd('2020-08-01'),
    c1m = confirmed, # 1e6 * confirmed / population,
    delta = c1m - lag(c1m),
    delta_7d = (lead(c1m, n=3) - lag(c1m, n=4)) / 7,
    prediction = (1.0875)**as.numeric(date_n - 26)
  ) %>%
  ungroup()

ggplot(countries %>% filter(quantity == 'deaths'), aes(date)) +
  geom_line(aes(y = delta_7d, colour=quantity), size=1) +
  geom_point(aes(y = delta, colour=quantity), size=.5) +
  geom_line(aes(y = prediction), linetype='dashed') +
  facet_wrap('country', ncol = 2, scales='free_y') +
  xlab(NULL) +
  scale_x_date(date_breaks = '1 month', date_labels = '%b') +
  theme_minimal() +
  scale_y_log10()

countries <- world %>%
  filter(is.na(province)) %>%
  filter(country == 'Czechia') %>%
  filter(date >= lubridate::ymd('2020-08-01')) %>%
  inner_join(population, by='country') %>%
  arrange(quantity, country, date) %>%
  inner_join(coeffs, by=c('country', 'quantity')) %>%
  group_by(quantity, country) %>%
  mutate(
    date_n = date - lubridate::ymd('2020-08-01'),
    c1m = q * 1e6 * confirmed / population + w,
    date = date - d,
    delta = c1m - lag(c1m),
    delta_7d = (lead(c1m, n=3) - lag(c1m, n=4)) / 7,
    delta_n = log(delta) - (k * date_n),
    delta_7d_n = log(delta_7d) - (k * date_n)
  ) %>%
  ungroup()

ggplot(countries, aes(date)) +
  geom_line(aes(y = delta_7d_n, colour=quantity), size=1) +
  geom_point(aes(y = delta_n, colour=quantity), size=.5) +
  facet_wrap('country', ncol = 2, scales='free_y') +
  xlab(NULL) +
  scale_x_date(date_breaks = '1 month', date_labels = '%b') +
  theme_minimal()

confirmed <- world %>%
  filter(
    country == 'Slovakia',
    quantity == 'confirmed'
  ) %>%
  arrange(date) %>%
  mutate(smooth = (lead(confirmed, n=3) - lag(confirmed, n=4)) / 7) %>%
  pull(smooth) %>%
  na.omit() %>%
  as.numeric()

deaths <- world %>%
  filter(
    country == 'Slovakia',
    quantity == 'deaths'
  ) %>%
  arrange(date) %>%
  mutate(smooth = (lead(confirmed, n=3) - lag(confirmed, n=4)) / 7) %>%
  pull(smooth) %>%
  na.omit() %>%
  as.numeric()
  
ccf(confirmed[40:120], deaths[40:120])
plot(confirmed[40:120])

# -----------------------------------------------------------------------------

population <- tibble(
  country = c('Netherlands', 'Slovakia', 'Belgium', 'France', 'Germany', 'Denmark'),
  population = c(17.4e6, 5e6, 11.4e6, 67e6, 83e6, 5.8e6)
)

ggplot(
  world %>%
    filter(is.na(province), quantity == 'confirmed') %>%
    filter(date >= lubridate::ymd('2020-12-01')) %>%
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
  geom_line(aes(y = delta_7d, colour=country), size=1) +
  #geom_point(aes(y = delta), colour='#6dae42', size=.5) +
  xlab(NULL) +
  ylab('new confirmed cases per 1M population per day') +
  scale_x_date(date_breaks = '1 month', date_labels = '%b') +
  ylim(0, NA) +
  theme_kybcae
#theme_linedraw()

ggsave('be-nl-cz-sk-lines.png', dpi=96, width=8, height=6)

ggplot(
  world %>%
    filter(is.na(province), quantity == 'deaths') %>%
    filter(date >= lubridate::ymd('2020-12-01')) %>%
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
  geom_line(aes(y = delta_7d, colour=country), size=1) +
  #geom_point(aes(y = delta), colour='#6dae42', size=.5) +
  xlab(NULL) +
  ylab('new confirmed cases per 1M population per day') +
  scale_x_date(date_breaks = '1 month', date_labels = '%b') +
  ylim(0, NA) +
  theme_kybcae
#theme_linedraw()

ggsave('be-nl-cz-sk-deaths-lines.png', dpi=96, width=8, height=6)