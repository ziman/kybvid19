source('shared.R')

data <- load_data()

make_plot(
  data,
  focus='SK',
  rtype='confirmed',
  y_label='confirmed cases',
  countries=c(
    'Austria',
    'Netherlands', 'Italy', 'Germany', 'Slovakia', 'Spain',
    'Belgium', 'France', 'United Kingdom', 'United States',
    'Czechia', 'Switzerland',
    'Norway'
  ),
  threshold=list(
    label='IT lockdown',
    x=13,
    y=133
  )
)

make_plot(
  data,
  focus='SK',
  rtype='deaths',
  y_label='deaths',
  countries=c(
    'Austria',
    'Netherlands', 'Italy', 'Germany', 'Slovakia', 'Spain',
    'Belgium', 'France', 'United Kingdom', 'United States',
    'Czechia', 'Switzerland',
    'Norway'
  ),
  threshold=list(
    label='IT lockdown',
    x=20,
    y=6.6
  ),
  rate_override = 1.3
)

make_plot(
  data,
  focus='NL',
  rtype='confirmed',
  y_label='confirmed cases',
  countries=c(
    'Austria',
    'Netherlands', 'Italy', 'Germany', 'Slovakia', 'Spain',
    'Belgium', 'France', 'United Kingdom', 'United States',
    'Czechia', 'Switzerland',
    'Norway'
  ),
  threshold=list(
    label='IT lockdown',
    x=6,
    y=133
  )
)

make_plot(
  data,
  focus='NL',
  rtype='deaths',
  y_label='deaths',
  countries=c(
    'Austria',
    'Netherlands', 'Italy', 'Germany', 'Slovakia', 'Spain',
    'Belgium', 'France', 'United Kingdom', 'United States',
    'Czechia', 'Switzerland',
    'Norway'
  ),
  threshold=list(
    label='IT lockdown',
    x=6,
    y=6.6
  )
)
