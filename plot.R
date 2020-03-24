source('shared.R')

data <- load_wiki()

make_plot(
  data,
  focus='SK',
  rtype='confirmed',
  y_label='confirmed cases',
  countries=c(
    'AT', 'IT', 'DE', 'SK', 'ES', 'BE', 'FR',
    'UK', 'US', 'CZ', 'CH', 'NO'
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
    'UK', 'US', 'CZ', 'CH', 'NO'
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
    'UK', 'US', 'CZ', 'CH', 'NO'
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
    'UK', 'US', 'CZ', 'CH', 'NO'
  ),
  threshold=list(
    label='national lockdown in IT',
    x=6,
    y=6.6
  )
)
