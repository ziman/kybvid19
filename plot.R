source('shared.R')

COUNTRIES <- c(
  'AT', 'IT', 'DE', 'SK', 'ES', 'BE', 'FR',
  'UK', 'US', 'CZ', 'CH', 'NO', 'NL', 'SE'
)

data <- load_wiki()

make_plot(
  data,
  focus='SK',
  rtype='confirmed',
  y_label='confirmed cases',
  countries=COUNTRIES,
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
  countries=COUNTRIES,
  threshold=list(
    label='national lockdown in IT',
    x=22,
    y=6.6
  ),
  rate_override = 1.06
)

make_plot(
  data,
  focus='NL',
  rtype='confirmed',
  y_label='confirmed cases',
  countries=COUNTRIES,
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
  countries=COUNTRIES,
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
  countries=COUNTRIES,
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
  countries=COUNTRIES,
  threshold=list(
    label='national lockdown in IT',
    x=10,
    y=6.6
  )
)
