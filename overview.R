source("shared.R")

BDAYS <- 7
FOCUS <- 'SK'
THRESHOLDS <- tibble(
  label = c('IT lockdown', 'IT lockdown'),
  per_1meg = c(6.6, 133)
)

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

wiki <- read_csv('wiki.csv') %>%
  mutate(date=lubridate::ymd(date)) %>%
  inner_join(countries, by='country')

# fix bad data
wiki[
  (wiki$iso2c == 'SK')
  & (wiki$date >= lubridate::ymd('2020-03-18'))
  & (wiki$deaths == 0),
  'deaths'
] <- 1

last_date <- wiki %>%
  group_by(country) %>%
  summarise(last_date = max(date)) %>%
  ungroup() %>%
  pull(last_date) %>%
  min()

first_date <- last_date - BDAYS

recent <- wiki %>%
  filter(first_date <= date, date <= last_date)
recent <- recent %>%
  inner_join(
    recent %>%
      filter(confirmed > 0) %>%
      group_by(iso2c) %>%
      summarise(
        min_confirmed = min(confirmed),
        min_confirmed_date = min(date),
        max_confirmed = max(confirmed)
      ) %>%
      ungroup(),
    by='iso2c'
  ) %>%
  inner_join(
    recent %>%
      filter(deaths > 0) %>%
      group_by(iso2c) %>%
      summarise(
        min_deaths = min(deaths),
        min_deaths_date = min(date),
        max_deaths = max(deaths)
      ) %>%
      ungroup(),
    by='iso2c'
  ) %>%
  mutate(
    deaths_per_1meg = 1e6 * deaths / population,
    confirmed_per_1meg = 1e6 * confirmed / population
  )

latest <- recent %>%
  filter(date == last_date) %>%
  mutate(
    rate_confirmed = (max_confirmed / min_confirmed) ** (1 / as.numeric(last_date - min_confirmed_date)),
    rate_deaths = (max_deaths / min_deaths) ** (1 / as.numeric(last_date - min_deaths_date))
  )
  
svk <- latest %>% filter(iso2c == FOCUS)
if (svk$rate_deaths == 1.0) {
  svk$rate_deaths = 1.3
}

ahead <- latest %>%
  filter(iso2c != 'SK') %>%
  mutate(
    days_ahead_confirmed = log(confirmed_per_1meg / svk$confirmed_per_1meg) / log(svk$rate_confirmed),
    days_ahead_deaths = log(deaths_per_1meg / svk$deaths_per_1meg) / log(svk$rate_deaths)
  )


ggplot(NULL) +
  geom_hline(
    data=THRESHOLDS,
    aes(yintercept=per_1meg),
    linetype='dashed',
    colour='red'
  ) +
  geom_text(
    data=THRESHOLDS,
    aes(y=per_1meg, label=label),
    x=last_date,
    colour='red'
  ) +
  geom_line(
    data=recent %>% filter(confirmed > 0),
    aes(x=date, y=confirmed_per_1meg, colour=country)
  ) +
  geom_point(
    data=recent %>% filter(confirmed > 0),
    aes(x=date, y=confirmed_per_1meg, colour=country)
  ) +
  geom_text_repel(
    data=latest,
    aes(x=date, y=confirmed_per_1meg, colour=country, label=iso2c),
    hjust=1
  ) +
  geom_line(
    data=recent %>% filter(deaths > 0),
    aes(x=date, y=deaths_per_1meg, colour=country)
  ) +
  geom_point(
    data=recent %>% filter(deaths > 0),
    aes(x=date, y=deaths_per_1meg, colour=country),
    shape='x'
  ) +
  geom_text_repel(
    data=latest,
    aes(x=date, y=deaths_per_1meg, colour=country, label=iso2c),
    hjust=1
  ) +
  scale_y_log10(
    labels=function(x) signif(x, 1)
  ) +
  theme_kybcae

+
  xlim(first_date, NA) +
  scale_y_log10(
    labels=function(x) signif(x, 1)
  ) +
  ylab(paste('per 1M population')) +
  xlab('days since latest data update') +
  ggtitle(paste('Last update: ', last_date)) +
  scale_colour_discrete(name='country') +
  theme_kybcae +
  guides(colour=F)

+
  geom_text(
    aes(x,y),
    data=tibble(x=threshold$x, y=threshold$y),
    colour='red',
    size=3,
    label=threshold$label,
    vjust=1.5,
    hjust=1
  ) +
  geom_abline(
    data=tibble(x=1),
    slope=log10(svk$rate),
    intercept=log10(svk$cases_per_1meg),
    linetype='dashed',
    colour='gray',
    alpha=0.5
  ) +
  geom_threshold_line +
  geom_threshold_label +
  geom_segment(
    data=ahead,
    aes(
      x=days_since_end,
      xend=svk$days_since_end + days_ahead,
      y=cases_per_1meg,
      yend=cases_per_1meg,
      colour=iso2c
    ),
    linetype='dotted'
  ) +
  geom_point(
    data=ahead,
    aes(
      x=svk$days_since_end + days_ahead,
      y=cases_per_1meg,
      colour=iso2c
    ),
    shape=1,
    alpha=0.8
  ) +
  geom_line(
    aes(y=cases_per_1meg, colour=iso2c),
    alpha=0.5
  ) +
  geom_point(
    aes(y=cases_per_1meg, colour=iso2c)
  ) +
  geom_text_repel(
    data=latest,
    aes(label=iso2c, y=cases_per_1meg, colour=iso2c),
    hjust=0.5,
    vjust=0.5,
    size=2.5,
    show.legend=F
  ) +
  geom_text_repel(
    data=ahead,
    aes(
      x=pmax(svk$days_since_end + days_ahead, days_since_end),
      y=cases_per_1meg,
      label=if_else(
        days_ahead > 0,
        paste(signif(days_ahead, 2), ' days ahead of ', focus, sep=''),
        paste(signif(-days_ahead, 2), ' days behind ', focus, sep='')
      ),
      colour=iso2c
    ),
    size=3,
    hjust=0,
    nudge_x=0.3,
    show.legend=F
  ) +
  xlim(-bdays, NA) +
  scale_y_log10(
    labels=function(x) signif(x, 1)
  ) +
  ylab(paste(y_label, 'per 1M population')) +
  xlab('days since latest data update') +
  ggtitle(paste('Last update: ', max(ys$date))) +
  scale_colour_discrete(name='country') +
  theme_kybcae +
  guides(colour=F)

ggsave(
  paste(rtype, '-', focus, '.svg', sep=''),
  plot=p, dpi=96, width=8, height=6, device='svg'
)

ggsave(
  paste(rtype, '-', focus, '.png', sep=''),
  plot=p, dpi=96, width=8, height=6, device='png'
)