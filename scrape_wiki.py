#!/usr/bin/env python3

import re
import bs4   # type:ignore
import csv
import sys
import tqdm  # type:ignore
import httpx
import logging

SOURCES = (
    'Italy',
)

http = httpx.Client()
log = logging.getLogger(__name__)

def main():
    logging.basicConfig(level=logging.DEBUG)

    csvf = csv.writer(sys.stdout)
    csvf.writerow(('country', 'date', 'confirmed', 'deaths'))

    for country in SOURCES:
        resp = http.get(f'https://en.wikipedia.org/wiki/2020_coronavirus_pandemic_in_{country}')
        resp.raise_for_status()

        soup = bs4.BeautifulSoup(resp.text, 'lxml')
        for div in soup('div', 'barbox'):
            for tr in div('tr'):
                tds = tr('td')
                if len(tds) != 4:
                    continue

                td_date, _td_plot, td_cases, td_deaths = tds
                s_date = td_date.string.strip()
                if s_date in ('⋮', 'Date'):
                    continue

                csvf.writerow((
                    country,
                    s_date,
                    int(td_cases.span.span.string.replace(',', '')),
                    int(td_deaths.span.string.replace(',', ''))
                        if td_deaths.span.string
                        else 0
                ))

main()
