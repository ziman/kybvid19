library(tidyverse)
library(ggrepel)
require(scales)

theme_kybcae <- theme_minimal() +
  theme(
    plot.background = element_rect(fill='black'),
    panel.grid.major = element_line(colour='#333333'),
    panel.grid.minor = element_line(colour='#222222'),
    text = element_text(colour='#6DAE42', family='Tahoma', size=10)
  )

load <- function(fname) {
  xs_raw <- read_csv(fname)
  colnames(xs_raw)[1:4] <- c('province', 'country', 'lat', 'lon')
  xs <- xs_raw %>%
    pivot_longer(c(-province, -country, -lat, -lon), names_to='date') %>%
    mutate(date=as.POSIXct(strptime(date, '%m/%d/%y'))) %>%
    rename(cases=value)
}

xs <- bind_rows(
  load('data/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv') %>%
    mutate(type='confirmed'),
  load('data/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv') %>%
    mutate(type='deaths'),
  load('data/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv') %>%
    mutate(type='recovered')
) %>%
  mutate(country = case_when(
    country == 'US' ~ 'United States',
    T ~ country
  ))

countries <- read_csv('nations1.csv') %>%
  arrange(year) %>%
  group_by(country, iso2c) %>%
  summarise(population = last(population)) %>%
  ungroup() %>%
  mutate(country = case_when(
    iso2c == 'IR' ~ 'Iran',
    iso2c == 'SK' ~ 'Slovakia',
    iso2c == 'CZ' ~ 'Czechia',
    T ~ country
  ), iso2c = case_when(
    iso2c == 'GB' ~ 'UK',
    T ~ iso2c
  ))

ys <- xs %>%
  filter(
    type == 'confirmed',
    country %in% c(
#      'Iran', 'Austria',
#      'Netherlands', 'Italy', 'Germany', 'Slovakia', 'Spain',
#      'Belgium', 'France', 'United Kingdom', 'United States',
#      'Czechia', 'Switzerland',
#      'Norway', 'Denmark', 'Sweden', 'Finland'
#      'Slovakia', 'Italy'
      'Slovakia', 'Czechia', 'Italy', 'United Kingdom',
      'Netherlands', 'Spain', 'France'
    )
  ) %>%
  group_by(country, date) %>%
  summarise(cases = sum(cases)) %>%
  ungroup() %>%
  filter(cases > 0) %>%
  inner_join(
    countries,
    by='country'
  ) %>%
  mutate(
    cases_per_1meg = 1e6 * cases / population
  ) %>%
  inner_join(
    .,
    filter(., cases_per_1meg >= 1.0) %>%
      group_by(country) %>%
      summarise(first_case = min(date)) %>%
      ungroup(),
    by='country'
  ) %>%
  mutate(
    days_since_start = as.numeric(date - first_case) / 86400,
    country = paste(iso2c, ' - ', country, sep=''),
    markcol = if_else(date >= max(date) - 4*86400, country, NA_character_)
  )

last_date <- max(ys$date)

ggplot(ys %>% filter(cases > 0), aes(days_since_start, cases_per_1meg, colour=markcol, group=country)) +
  geom_abline(
    data=tibble(x=1),
    slope=log10(1.33),
    intercept=log10(1),
    linetype='dotted',
    colour='gray'
  ) +
  geom_text(
    aes(x,y), data=tibble(x=20.5, y=5e2, country=NA, markcol=NA),
    label='+33%/day', size=2.5, show.legend=F
  ) +
  geom_hline(
    data=tibble(x=1),
    yintercept = 1e6 * 8000 / 60e6,
    linetype='dashed',
    colour='red'
  ) +
  geom_text(
    aes(x,y, colour=NA, group=NA),
    data=tibble(x=0, y=133),
    colour='red',
    size=3,
    label='national lockdown in Italy',
    vjust=-0.5
  ) +
  geom_line(alpha=0.5) +
  geom_point() +
  geom_label(
    data=filter(ys, date == last_date),
    aes(label=iso2c),
    hjust=0.5,
    vjust=0.5,
    size=2.3,
    label.padding=unit(0.1, 'lines'),
    label.r=unit(0.05, 'lines'),
    show.legend=F,
    fill='black'
  ) +
  xlim(-5, 23) +
  scale_y_log10(
    labels=function(x) signif(x, 1),
    limits=c(0.3, 1000)
  ) +
  ylab('confirmed cases per 1M population') +
  xlab('days since ≥1 cases per 1M population') +
  ggtitle(paste('Up to and including', max(ys$date))) +
  scale_colour_discrete(name='country') +
  theme_kybcae

ggsave('covid.png', dpi=96, width=8, height=6)
#ggsave('covid.png', dpi=96, width=10, height=8)
#ggsave('covid-sk-it.png', dpi=96, width=7, height=5)
