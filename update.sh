#!/usr/bin/env bash

set -euo pipefail

cd "$(dirname "$0")"

while true; do
	date
	(cd data; git pull)
	R -q -e 'source("covid19.R")'
	R -q -e 'source("nl.R")'
	scp covid-ahead.png functor.sk:/var/www/kybvid19
	scp covid-nl.png functor.sk:/var/www/kybvid19
	scp covid-nl-deaths.png functor.sk:/var/www/kybvid19
	scp covid-deaths.png functor.sk:/var/www/kybvid19

	sleep 3600 || break
done
