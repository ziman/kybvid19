library(tidyverse)
library(ggrepel)
require(scales)
knitr::opts_chunk$set(echo = FALSE)

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
  ))

ys <- xs %>%
  filter(
    country %in% c(
      'Iran', 'Austria',
      'Netherlands', 'Italy', 'Germany', 'Slovakia', 'Spain',
      'Belgium', 'France', 'United Kingdom', 'United States',
      'Czechia'
    )
  ) %>%
  group_by(country, date, type) %>%
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
      group_by(country, type) %>%
      summarise(first_case = min(date)) %>%
      ungroup(),
    by=c('country', 'type')
  ) %>%
  mutate(
    days_since_start = as.numeric(date - first_case) / 86400
  )

last_date <- max(ys$date)

cdata <- ys %>% filter(country == 'Netherlands')
m <- lm(log(cases_per_1meg) ~ days_since_start, data=cdata %>% filter(days_since_start >= 0, type=='confirmed'))
summary(m)

prediction <- tibble(
  days_since_start = cdata %>% pull(days_since_start),
  cases_per_1meg = exp(m$coefficients[1] + m$coefficients[2] * cdata$days_since_start),
  type='predicted'
)

ggplot(NULL, aes(days_since_start, cases_per_1meg, colour=type)) +
  geom_point(data=cdata) +
  geom_line(data=cdata) +
  geom_point(data=prediction) +
  geom_line(data=prediction, linetype='dashed') +
  theme_minimal()
  scale_y_log10()

ggplot(ys %>% filter(cases > 0), aes(days_since_start, cases_per_1meg, colour=country)) +
  geom_abline(data=tibble(x=1), slope=log10(1.33), intercept=log10(1), linetype='dashed', alpha=.5) +
  geom_text(
    aes(x,y), data=tibble(x=19, y=5e2, country=NA),
    label='+33%/day', size=2.5, show.legend=F
  ) +
  geom_hline(
    data=tibble(x=1),
    yintercept = 1e6 * 8000 / 60e6,
    linetype='dashed',
    colour='red',
    alpha=0.5
  ) +
  geom_text(
    aes(x,y, colour=NA),
    data=tibble(x=0, y=133),
    colour='red',
    size=3,
    label='national lockdown in Italy',
    vjust=1.0,
    nudge_y=0.2,
    alpha=0.5
  ) +
  geom_line(alpha=0.75) +
  geom_point() +
  geom_label_repel(
    data=filter(ys, date == last_date),
    aes(label=iso2c),
    hjust=0.5,
    vjust=0.5,
    size=2,
    label.padding=unit(0.1, 'lines'),
    label.r=unit(0.05, 'lines'),
    box.padding=0,
    show.legend=F
  ) +
  scale_y_log10(labels=function(x) signif(x, 1)) +
  xlim(-5, 23) +
  ylab('confirmed cases per 1M population') +
  xlab('days since 1 case per 1M population') +
  theme_minimal()

ggsave('doomsday.png', dpi=96, width=10, height=6)