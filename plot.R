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
