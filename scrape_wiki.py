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

                deaths = 0
                recoveries = 0
                unresolved = 0

                for div in td_plot('div'):
                    style = div['style']
                    count = int(div['title'])
                    if 'background:Black' in style:
                        deaths = count
                    elif 'background:DimGrey' in style:
                        deaths = count
                    elif 'background:#A50026' in style:
                        deaths = count
                    elif 'background:SkyBlue' in style:
                        recoveries = count
                    elif 'background:Tomato' in style:
                        unresolved += count
                    elif 'background:OrangeRed' in style:
                        unresolved += count
                    elif 'background:Gold' in style:
                        unresolved += count
                    else:
                        raise Exception(f'unknown colour: {style} for {country}')

                csvf.writerow((
                    country,
                    s_date,
                    deaths + recoveries + unresolved,  # confirmed
                    deaths,
                ))

main()
