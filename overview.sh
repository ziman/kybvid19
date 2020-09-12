#!/bin/bash

(cd data; git pull)
R -q -f overview.R
scp overview.png functor.sk:/var/www/kybvid19/be-nl-cz-sk.png
