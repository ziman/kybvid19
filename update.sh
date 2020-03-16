#!/usr/bin/env bash

set -euo pipefail

cd "$(dirname "$0")"

while true; do
	date
	(cd data; git pull)
	R -q -e 'source("covid19.R")'
	scp covid.png functor.sk:/var/www/kybvid19

	sleep 3600 || break
done
