#!/bin/bash

(cd data; git pull)
R -q -f overview.R
scp *.png functor.sk:/var/www/kybvid19/
