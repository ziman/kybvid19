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
      'Netherlands', 'Spain', 'France', 'United States'
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
    cases_per_1meg = 1e6 * cases / population,
    marked = date >= max(date) - 7*86400
  ) %>%
  inner_join(
    .,
    filter(., marked) %>%
      group_by(country) %>%
      summarise(first_case = min(date), last_case = max(date), first_case_count = min(cases)) %>%
      ungroup(),
    by='country'
  ) %>%
  mutate(
    days_since_start = as.numeric(date - first_case) / 86400,
    days_since_end = as.numeric(date - last_case) / 86400,
    country = paste(iso2c, ' - ', country, sep=''),
    markcol = if_else(marked, country, NA_character_),
    marksize = if_else(marked, 1, 0.5)
  )

latest <- ys %>%
  mutate(rate = (cases / first_case_count) ** (1 / days_since_start)) %>%
  filter(date == max(date))

svk <- latest %>%
  filter(iso2c == 'SK')
  
ahead <- latest %>%
  filter(iso2c != 'SK') %>%
  mutate(
    days_ahead = log(cases_per_1meg / svk$cases_per_1meg) / log(svk$rate)
  )

ggplot(ys %>% filter(cases > 0, marked), aes(days_since_end)) +
  geom_abline(
    data=tibble(x=1),
    slope=log10(svk$rate),
    intercept=log10(svk$cases_per_1meg),
    linetype='dashed',
    colour='gray',
    alpha=0.5
  ) +
  geom_text(
    aes(x,y), data=tibble(x=20.5, y=5e2),
    label='+33%/day', size=2.5, show.legend=F
  ) +
  geom_hline(
    data=tibble(x=1),
    yintercept = 1e6 * 8000 / 60e6,
    linetype='dashed',
    colour='red'
  ) +
  geom_text(
    aes(x,y),
    data=tibble(x=svk$days_since_end, y=133),
    colour='red',
    size=3,
    label='national lockdown in Italy',
    vjust=-0.5
  ) +
  geom_segment(
    data=ahead,
    aes(
      x=days_since_end,
      xend=svk$days_since_end + days_ahead,
      y=cases_per_1meg,
      yend=cases_per_1meg,
      colour=country
    ),
    linetype='dotted'
  ) +
  geom_point(
    data=ahead,
    aes(
      x=svk$days_since_end + days_ahead,
      y=cases_per_1meg,
      colour=country
    ),
    shape=1,
    alpha=0.8
  ) +
  geom_line(aes(y=cases_per_1meg, colour=country), alpha=0.5) +
  geom_point(aes(y=cases_per_1meg, colour=country)) +
  geom_label(
    data=latest,
    aes(label=iso2c, y=cases_per_1meg, colour=country),
    hjust=0.5,
    vjust=0.5,
    size=2.5,
    label.padding=unit(0.1, 'lines'),
    label.r=unit(0.05, 'lines'),
    show.legend=F,
    fill='black'
  ) +
  geom_text(
    data=ahead,
    aes(
      x=svk$days_since_end + days_ahead,
      y=cases_per_1meg,
      label=paste(signif(days_ahead, 2), ' days ahead of SK', sep=''),
      colour=country
    ),
    size=3,
    hjust=0,
    nudge_x=0.2,
    show.legend=F
  ) +
  scale_y_log10(
    labels=function(x) signif(x, 1)
  ) +
  xlim(-7, 15) +
  ylab('confirmed cases per 1M population') +
  xlab('days since latest data update') +
  ggtitle(paste('Last update: ', max(ys$date))) +
  scale_colour_discrete(name='country') +
  theme_kybcae

ggsave('covid-ahead.png', dpi=96, width=8, height=6)
#ggsave('covid.png', dpi=96, width=10, height=8)
#ggsave('covid-sk-it.png', dpi=96, width=7, height=5)
