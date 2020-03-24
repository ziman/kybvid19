#!/usr/bin/env bash

set -euo pipefail

cd "$(dirname "$0")"

while true; do
	date
	(cd data; git pull)
	#R -q -e 'source("covid19.R")'
	#R -q -e 'source("nl.R")'
	R -q -e 'source("plot.R")'

	cp confirmed-SK.png covid-ahead.png
	cp confirmed-NL.png covid-nl.png
	cp deaths-NL.png covid-nl-deaths.png

	scp index.html *.png *.svg functor.sk:/var/www/kybvid19

	sleep 3600 || break
done
