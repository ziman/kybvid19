#!/bin/bash

R -q -f overview.R
scp overview.png functor.sk:/var/www/kybvid19/
