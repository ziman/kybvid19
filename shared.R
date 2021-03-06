library(tidyverse)
library(ggrepel)
require(scales)

# extrafont::font_import()  # only once
extrafont::loadfonts()  # to get Tahoma

theme_kybcae <- theme_minimal() +
  theme(
    plot.background = element_rect(fill='black'),
    panel.grid.major = element_line(colour='#333333'),
    panel.grid.minor = element_line(colour='#222222'),
    # strip.text = element_text(colour='#6DAE42', family='Tahoma', size=10),
    strip.text = element_text(colour='#6DAE42', size=10),
    strip.background = element_rect(fill='#222222', colour='#333333'),
    #text = element_text(colour='#6DAE42', family='Tahoma', size=10)
    text = element_text(colour='#6DAE42', size=10)
  )

load_who <- function() {
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
  
  list(
    rows = xs,
    countries = countries
  )
}

load_wiki <- function() {
  xs <- read_csv('wiki.csv') %>%
    mutate(date=lubridate::ymd(date)) %>%
    pivot_longer(c(-country, -date), names_to='type', values_to='cases')

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
  
  list(
    rows = xs,
    countries = countries
  )
}

make_plot <- function(data, focus='SK', rtype = 'confirmed', y_label, countries, threshold=NULL, rate_override=NA, bdays=7) {
  xs <- data$rows %>%
    inner_join(
      data$countries,
      by='country'
    ) %>%
    select(-country) %>%
    filter(
      type == rtype,
      iso2c %in% countries,
      date <= (lubridate::now() - lubridate::hours(18))  # start showing today's numbers from 6pm
    ) %>%
    complete(type, iso2c, date) %>%
    fill(cases, population) %>%
    filter(cases > 0)  # must be done after complete+fill

  last_complete_date <- xs %>%
    group_by(iso2c) %>%
    summarise(last_date = max(date)) %>%
    ungroup() %>%
    pull(last_date) %>%
    min()
  
  ys <- xs %>%
    filter(
      date >= last_complete_date - bdays,
      date <= last_complete_date
    ) %>%
    mutate(
      cases_per_1meg = 1e6 * cases / population
    ) %>%
    inner_join(
      .,
      filter(., T) %>%
        group_by(iso2c) %>%
        summarise(first_case = min(date), last_case = max(date), first_case_count = min(cases)) %>%
        ungroup(),
      by='iso2c'
    ) %>%
    mutate(
      days_since_start = as.numeric(date - first_case),
      days_since_end = as.numeric(date - last_complete_date)
    )
  
  latest <- ys %>%
    filter(date == last_case) %>%
    mutate(
      rate = (cases / first_case_count) ** (1 / days_since_start),
      days_to_double = log(2) / log(rate)
    )
    
  svk <- latest %>%
    filter(iso2c == focus) %>%
    mutate(
      rate = replace_na(rate_override, rate),
      days_to_double = log(2) / log(rate)
    )

  ahead <- latest %>%
    filter(iso2c != focus) %>%
    mutate(
      days_ahead = log(cases_per_1meg / svk$cases_per_1meg) / log(svk$rate)
    )
  
  geom_threshold_line <- geom_blank()
  geom_threshold_label <- geom_blank()
  if (!is.null(threshold)) {
    geom_threshold_line <- 
      geom_hline(
        data=tibble(x=1),
        yintercept = threshold$y,
        linetype='dashed',
        colour='red'
      )
    geom_threshold_label <-
      geom_text(
        aes(x,y),
        data=tibble(x=last_complete_date + threshold$x, y=threshold$y),
        colour='red',
        size=3,
        label=threshold$label,
        vjust=1.5,
        hjust=1
      )
  }
  
  mult <- tibble(i = 1:10) %>%
    mutate(
      fact = 2**i,
      cases_per_1meg = svk$cases_per_1meg * fact,
      date = svk$date + svk$days_to_double * i,
      label = paste(fact, 'x\nin ', signif(svk$days_to_double*i, 2), ' days', sep='')
    ) %>%
    filter(
      date <= svk$date + max(ahead$days_ahead)
    )
  
  if (is.null(mult)) {
    mult <- tibble(
      fact = 1,
      cases_per_1meg = svk$cases_per_1meg,
      date=svk$date - 31  # HACK: move off view
    )
  }
  
  p <- ggplot(NULL) +
    geom_abline(
      data=tibble(x=1),
      slope=log10(svk$rate),
      intercept=log10(svk$cases_per_1meg) - log10(svk$rate) * as.numeric(last_complete_date - lubridate::ymd('1970-01-01')),
      linetype='dashed',
      colour='gray',
      alpha=0.5
    ) +
    geom_threshold_line +
    geom_threshold_label +
    geom_segment(
      data=mult,
      aes(x=date, y=svk$cases_per_1meg, xend=date, yend=cases_per_1meg),
      color='gray',
      linetype='dotted',
      alpha=0.5
    ) +
    geom_segment(
      data=mult %>% tail(n=1),
      aes(x=svk$date, y=svk$cases_per_1meg, xend=date, yend=svk$cases_per_1meg),
      color='gray',
      linetype='dotted',
      alpha = 0.5
    ) +
    geom_label(
      data=mult,
      aes(x=date, y=svk$cases_per_1meg, label=label),
      color='gray',
      fill='black',
      size=3,
      label.padding=unit(0.1, 'lines'),
      label.size=NA,
      alpha=0.5
    ) +
    geom_segment(
      data=ahead,
      aes(
        x=date,
        xend=svk$date + days_ahead,
        y=cases_per_1meg,
        yend=cases_per_1meg,
        colour=iso2c
      ),
      linetype='dotted'
    ) +
    geom_point(
      data=ahead,
      aes(
        x=svk$date + days_ahead,
        y=cases_per_1meg,
        colour=iso2c
      ),
      shape=1,
      alpha=0.8
    ) +
    geom_line(
      data=ys,
      aes(x=date, y=cases_per_1meg, colour=iso2c),
      alpha=0.5
    ) +
    geom_point(
      data=ys,
      aes(x=date, y=cases_per_1meg, colour=iso2c)
    ) +
    geom_text_repel(
      data=latest,
      aes(x=date, label=iso2c, y=cases_per_1meg, colour=iso2c),
      hjust=0.5,
      vjust=0.5,
      size=2.5,
      show.legend=F
    ) +
    geom_text_repel(
      data=ahead,
      aes(
        x=pmax(svk$date + days_ahead, date),
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
      nudge_x=0.5,
      show.legend=F
    ) +
    scale_y_log10(
      labels=function(x) signif(x, 1)
    ) +
    ylab(paste(y_label, 'per 1M population')) +
    xlab(NULL) +
    xlim(last_complete_date - bdays, NA) +
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
  
  return(p)
}
