# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}

#' @title Prepare the data
#' @name Prepare data
#
# This reads in the data and ensures the diameter values are numeric and the time values are dates. We assume the input data has the first row as date-time values, and the column headings for the rest of the data are the particle diameter values.

#' @param the_data data frame with rows as day-time, columns as particle sizes
#' @param cols subset of columns to prepare, if the_data has other columns that should be ignored by this function. Default is all columns
#' @param interval_in_seconds interval to use when interpolating. Default is 200, higher is faster, but lower resolution on the plot
#' @param interval_diameter interval to use when interpolating. Default is 0.75, higher is faster, but lower resolution on the plot
#'
#' @export


prepare_data <- function(the_data,
                         cols = 1:ncol(the_data),
                         interval_in_seconds = 200,
                         interval_diameter = 0.75) {

  # focus on the untransformed values
  dat <- the_data[, cols]

  # get Diameter value from col names
  Diameter <- as.numeric(gsub("X", "", names(dat)[-1]))

  # melt the wide data into long format
  # see http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
  message("Update: converting data from wide to long format...")
  dat_long <- tidyr::gather(dat, "Diameter", "dN_dlogDp", 2:max(cols))
  message("Update: wide to long format conversion complete.")

  # we want diameter as a numeric
  dat_long$Diameter <- as.numeric(gsub("X", "", dat_long$Diameter ))
  # we want time as a date-formatted variable
  x <-  as.character(dat_long$Time)
  date_ <- as.Date(x, format = "%d/%m/%Y")
  time_ <- gsub(" ", "", substr(x, nchar(x) - 4, nchar(x)))
  dat_long$Time <- as.POSIXct(paste0(date_, " ", time_))

  # The Igor plot seems to use log dN_dlogDp values, so let's get those
  dat_long$dN_dlogDp_log <- log10(dat_long$dN_dlogDp)
  dat_long$dN_dlogDp_log <- ifelse(dat_long$dN_dlogDp_log == "NaN" |
                                     dat_long$dN_dlogDp_log == "-Inf"  , 0.001, dat_long$dN_dlogDp_log)


  # interpolate between the  values for a smoother contour
  # this takes a moment or two...

  # interpolate the log values
  message("Update: interpolating the data to give a smooth contour...")
  xo <- with(dat_long, seq(min(Time), max(Time), interval_in_seconds))
  yo <- with(dat_long, seq(min(Diameter), max(Diameter), interval_diameter))
  dat_interp_log <- with(dat_long, akima::interp(Time, Diameter, dN_dlogDp_log, xo = xo, yo = yo) )
  message("Update: interpolation complete.")

  # make log data into a data frame for ggplot
  dat_interp_log_df <-  data.frame(matrix(data = dat_interp_log$z,
                                          ncol = length(dat_interp_log$y),
                                          nrow = length(dat_interp_log$x)))
  names(dat_interp_log_df) <- dat_interp_log$y
  dat_interp_log_df$Time <- as.POSIXct(dat_interp_log$x, origin = "1970-01-01")

  # wide to long
  dat_interp_log_df_long <- tidyr::gather(dat_interp_log_df, "Diameter", "dN_dlogDp_log", 1:(ncol(dat_interp_log_df)-1))
  dat_interp_log_df_long$Diameter <- as.numeric(as.character(dat_interp_log_df_long$Diameter))


  return(dat_interp_log_df_long)
  message("Data preparation complete.")

}


#' @title Plot the data
#' @name Plot data
#
# This draws a plot of the data

#' @param the_prepared_data the output from the prepare_data function
#' @param aspect_ratio change the aspect ratio of the plot
#' @param font_size change the axis and legend text size
#' @param h how many hours apart the x-axis ticks should be
#' @param y_breaks location of major ticks on the y axis
#' @param seconds_offset offset in seconds for adjusting the log y-axis
#' @param y_axis 'linear' or 'log' (base 10) y-axis
#' @param ... so you can send other things to modify the theme and scales
#' @export
#


smps_plot <- function(the_prepared_data,
                      aspect_ratio = 1.1/5,
                      font_size = 12,
                      h = "2 hours",
                      y_breaks = 100,
                      colour_ramp = colour_ramp_igor,
                      y_axis = 'linear',
                      seconds_offset = 1000,
                      ...) {


  y_labels_breaks <-  seq(0, max(the_prepared_data$Diameter), y_breaks)

  # add tick marks every h hours
  start_date        <- min(the_prepared_data$Time)
  end_date          <-  max(the_prepared_data$Time)
  date_breaks_h     <-  seq(from = start_date, to = end_date, by = h)
  date_breaks_1_day <- seq(from = start_date, to = end_date, by = "1 day")
  multiple          <- length(date_breaks_h) / length(date_breaks_1_day)


  # set a few details for a custom theme
  mytheme <- ggplot2::theme_bw(base_size = font_size, ...) +
    ggplot2::theme(aspect.ratio = aspect_ratio, ...)

  if (y_axis == "linear"){
    # draw the plot with linear y-axis


    require(ggplot2)
    the_plot <- ggplot(the_prepared_data, aes(y = Diameter,
                                              x = Time,
                                              fill = dN_dlogDp_log)) +
      geom_raster(interpolate = TRUE)  +
      scale_fill_gradientn(name = expression(dN/dlogD[p]~cm^-3),
                           colours = colour_ramp(100),
                           labels = fill_scale_labels) +
      scale_y_continuous(expand = c(0,0),
                         labels = every_nth(y_labels_breaks, 2, inverse = TRUE),
                         breaks = y_labels_breaks,
                         ...) +
      scale_x_datetime(expand = c(0,0),
                       breaks = date_breaks_h,
                       labels = insert_minor(multiple = multiple,
                                             format(date_breaks_1_day, "%d %b"),
                                             length(date_breaks_1_day)),
                       ...) +
      xlab("Day and time") +
      ylab("Diameter (nm)") +
      mytheme

  } else {

    if(y_axis == "log") {

      # plot with log y-axis

      # Now with log axis, we need to replace the ymin and ymax
      distance <- diff((unique(the_prepared_data$Diameter)))/2
      upper <- (unique(the_prepared_data$Diameter)) + c(distance, distance[length(distance)])
      lower <- (unique(the_prepared_data$Diameter)) - c(distance[1], distance)

      # Create xmin, xmax, ymin, ymax
      the_prepared_data$xmin <- the_prepared_data$Time - seconds_offset # default of geom_raster is 0.5
      the_prepared_data$xmax <- the_prepared_data$Time + seconds_offset
      idx <- rle(the_prepared_data$Diameter)$lengths[1]
      the_prepared_data$ymin <- unlist(lapply(lower, function(i) rep(i, idx)))
      the_prepared_data$ymax <- unlist(lapply(upper, function(i) rep(i, idx)))

      # draw the plot
      require(scales)
      the_plot <- ggplot(the_prepared_data, aes(y = Diameter, x = Time,
                                                xmin=xmin, xmax=xmax,
                                                ymin=ymin, ymax=ymax,
                                                fill = dN_dlogDp_log)) +
        geom_rect()  +
        scale_fill_gradientn(name = expression(dN/dlogD[p]~cm^-3),
                             colours = colour_ramp(100),
                             labels = fill_scale_labels,
                             ...)  +
        scale_y_continuous(expand = c(0,0),
                           trans = log_trans(), breaks = base_breaks()) +
        scale_x_datetime(expand = c(0,0),
                         breaks = date_breaks_h,
                         labels = insert_minor(multiple = multiple,
                                               format(date_breaks_1_day, "%d %b"),
                                               length(date_breaks_1_day)),
                         ...) +
        xlab("Day and time") +
        ylab("Diameter (nm)") +
        mytheme


    }
    else {
      stop("please specify 'linear' or 'log for the y-axis")
    }

  }

  return(the_plot)

}


##### plotting helper functions #####

# function for minor tick marks

every_nth <- function(x, nth, empty = TRUE, inverse = FALSE)
{
  if (!inverse) {
    if(empty) {
      x[1:nth == 1] <- ""
      x
    } else {
      x[1:nth != 1]
    }
  } else {
    if(empty) {
      x[1:nth != 1] <- ""
      x
    } else {
      x[1:nth == 1]
    }
  }
}

# get visually diminishing axis ticks

base_breaks <- function(n = 10){
  function(x) {
    axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}

# insert minor tick marks
insert_minor <- function(major_labs, n_minor, multiple) {labs <-
  c( sapply( major_labs, function(x) c(x, rep("", multiple) ) ) )
labs[1:(length(labs)-n_minor)]
}



# adjust the colour ramp to match the Igor plot, please experiment with the numbers here!
colour_ramp_igor <- colorRampPalette(rev(c( rep("red", 3),
                                            rep("yellow", 1),
                                            rep("green", 1),
                                            "cyan",
                                            rep("blue", 3),
                                            "purple")))

# function to set the legend labels
fill_scale_labels <- function(x) {
  parse(text = paste0("10^",x))
}


#' SMPS dataset
#'
#' A dataset containing particle counts over three days.
#'
#' @format A data frame
#'
"my_data"
