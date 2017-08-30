## vegie?

This was an attempt to part-read really massive shapefiles with VRT, by allowing access to the SQL facilities in GDAL. 

But, it was hopelessly slow for drivers that did not support random-access. 

GDAL is now better supported for this. But, this package does a more fundamental attempt at solving the problem: 

https://github.com/hypertidy/vapour

I might resurrect this just for fun to show how VRT works, some day
