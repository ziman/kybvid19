source('shared.R')

data <- load_wiki()

# fix bad data
data$rows[
  (data$rows$country == 'Slovakia')
  & (data$rows$date >= lubridate::ymd('2020-03-18'))
  & (data$rows$type == 'deaths')
  & (data$rows$cases == 0),
  'cases'
] <- 1

make_plot(
  data,
  focus='SK',
  rtype='confirmed',
  y_label='confirmed cases',
  countries=c(
    'AT', 'IT', 'DE', 'SK', 'ES', 'BE', 'FR',
    'UK', 'US', 'CZ', 'CH', 'NO', 'NL'
  ),
  threshold=list(
    label='national lockdown in IT',
    x=15,
    y=133
  )
)

make_plot(
  data,
  focus='SK',
  rtype='deaths',
  y_label='deaths',
  countries=c(
    'AT', 'IT', 'DE', 'SK', 'ES', 'BE', 'FR',
    'UK', 'US', 'CZ', 'CH', 'NO', 'NL'
  ),
  threshold=list(
    label='national lockdown in IT',
    x=22,
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
    'AT', 'IT', 'DE', 'SK', 'ES', 'BE', 'FR',
    'UK', 'US', 'CZ', 'CH', 'NO', 'NL'
  ),
  threshold=list(
    label='national lockdown in IT',
    x=8,
    y=133
  )
)

make_plot(
  data,
  focus='NL',
  rtype='deaths',
  y_label='deaths',
  countries=c(
    'AT', 'IT', 'DE', 'SK', 'ES', 'BE', 'FR',
    'UK', 'US', 'CZ', 'CH', 'NO', 'NL'
  ),
  threshold=list(
    label='national lockdown in IT',
    x=6,
    y=6.6
  )
)

make_plot(
  data,
  focus='UK',
  rtype='confirmed',
  y_label='confirmed cases',
  countries=c(
    'AT', 'IT', 'DE', 'SK', 'ES', 'BE', 'FR',
    'UK', 'US', 'CZ', 'CH', 'NO', 'NL'
  ),
  threshold=list(
    label='national lockdown in IT',
    x=8,
    y=133
  )
)

make_plot(
  data,
  focus='UK',
  rtype='deaths',
  y_label='deaths',
  countries=c(
    'AT', 'IT', 'DE', 'SK', 'ES', 'BE', 'FR',
    'UK', 'US', 'CZ', 'CH', 'NO', 'NL'
  ),
  threshold=list(
    label='national lockdown in IT',
    x=10,
    y=6.6
  )
)
