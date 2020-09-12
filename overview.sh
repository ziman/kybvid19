#!/bin/bash

(cd data; git pull)
R -q -f overview.R
scp be-nl-cz-sk.png nl.png functor.sk:/var/www/kybvid19/
