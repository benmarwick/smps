# smps 0.2.0

* update data prep function to handle missing data gracefully, no need to remove rows with zeros or NAs before sending to `prepare_data()`

* update data prep fn to divide into chunks for interpolation to handle memory better (results in a huge speed-up)

* update plot function to use geom_tile instead of raster.

# smps 0.1.0

* Basic plotting and data prep function added



