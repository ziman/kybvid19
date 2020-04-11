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
    'Austria',
    'Netherlands',
    'Germany',
    'Slovakia',
    'Spain',
    'Belgium',
    'France',
    'United Kingdom',
    'United States',
    'Czechia',
    'Switzerland',
    'Norway',
    'Sweden'
)

http = httpx.Client()
log = logging.getLogger(__name__)

def main():
    logging.basicConfig(level=logging.INFO)

    csvf = csv.writer(sys.stdout)
    csvf.writerow(('country', 'date', 'confirmed', 'deaths'))

    for country in tqdm.tqdm(SOURCES):
        slug = country.replace(' ', '_') \
            .replace('Netherlands', 'the_Netherlands') \
            .replace('United', 'the_United') \
            .replace('Czechia', 'the_Czech_Republic')

        resp = http.get(f'https://en.wikipedia.org/wiki/2020_coronavirus_pandemic_in_{slug}')
        resp.raise_for_status()

        soup = bs4.BeautifulSoup(resp.text, 'lxml')

        for div in soup('div', 'barbox'):
            for tr in div('tr'):
                tds = tr('td')
                if len(tds) < 2:
                    continue

                td_date, td_plot, *_ = tds
                if td_date.string is None:
                    continue

                s_date = td_date.string.strip()
                if not re.match(r'\d+-\d+-\d+', s_date):
                    continue

                d_deaths, d_recoveries, d_unresolved, _d1, _d2 = td_plot('div')
                deaths = int(d_deaths['title'])
                recoveries = int(d_recoveries['title'])
                unresolved = int(d_unresolved['title'])

                csvf.writerow((
                    country,
                    s_date,
                    deaths + recoveries + unresolved,  # confirmed
                    deaths,
                ))

main()
