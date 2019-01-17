#' @title Prepare the data
#' @name prepare_data
#
# This reads in the data and ensures the diameter values are numeric and the time values are dates. We assume the input data has the first column as date-time values, and the column headings for the rest of the data are the particle diameter values.

#' @param the_data data frame with rows as date-time, columns as particle sizes. The first column (and only the first column) must contain date-time values.
#' @param cols subset of columns to prepare, if the_data has other columns that should be ignored by this function. Default is all columns
#' @param interval_in_seconds interval to use when interpolating. Default is 200, higher is faster, but lower resolution on the plot
#' @param interval_diameter interval to use when interpolating. Default is 0.75, higher is faster, but lower resolution on the plot
#' @param unique_diameter_values how many unique diamater values to include in each interpolation chunk. Default is 5.
#'
#' @export
#' @importFrom  akima interp
#' @importFrom tidyr gather
#'



prepare_data <- function(the_data,
                            cols = 1:ncol(the_data),
                            interval_in_seconds = 200,
                            interval_diameter = 0.75,
                            unique_diameter_values = 5) {

  # focus on the untransformed values
  dat <- the_data[, cols]

  # get Diameter value from col names
  Diameter <- as.numeric(gsub("X", "", names(dat)[-1]))

  # interpolate between the  values for a smoother contour
  # this takes a moment or two...

  # interpolate the log values

  # split into small pieces, like time slices, interp, then recombine

  # identify what times have NA values
  na <- ifelse(complete.cases(dat),
                "ok", "not ok")
  # find length of runs that are ok and not ok
  runs <- rle(na)
  ends <- cumsum(runs$lengths) + 1
  ends[length(ends)] <- ends[length(ends)] -1
  starts <- c(1, cumsum(runs$lengths))
  runs_summary <- data.frame(cbind(starts = starts[-length(starts)],
                                   ends),
                             runs$values)
  # so we only take the rows that are ok, and interpolate on them
  # separately, then put them back together, leaving gaps between them

  can_take_these_rows <-  runs_summary[runs_summary$runs.values == "ok", ]

  list_of_sections <- vector("list", nrow(can_take_these_rows))


  # select rows that are ok
  for(i in 1:nrow(can_take_these_rows)){
    list_of_sections[[i]] <- na.omit(dat[can_take_these_rows$starts[i]:
                                           can_take_these_rows$ends[i], ])

  }


  ### loop to convert each df of good rows to long format

  list_of_sections_long <- vector("list", length(list_of_sections))

  message("Update: converting data from wide to long format...")
  for(i in 1:length(list_of_sections)){

    # melt the wide data into long format
    # see http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/

    list_of_sections_long[[i]] <-
      tidyr::gather(list_of_sections[[i]],
                    "Diameter",
                    "dN_dlogDp",
                    2:max(cols))



    # we want diameter as a numeric
    list_of_sections_long[[i]]$Diameter <-
      as.numeric(gsub("X",
                      "",
                      list_of_sections_long[[i]]$Diameter ))

    # we want time as a date-formatted variable
    x <-  as.character(list_of_sections_long[[i]]$Time)
    date_ <- as.Date(x, format = "%d/%m/%Y")
    time_ <- gsub(" ", "", substr(x, nchar(x) - 4, nchar(x)))
    list_of_sections_long[[i]]$Time <- as.POSIXct(paste0(date_, " ", time_))

    # The Igor plot seems to use log dN_dlogDp values, so let's get those
    list_of_sections_long[[i]]$dN_dlogDp_log <-
      log10(list_of_sections_long[[i]]$dN_dlogDp)

    list_of_sections_long[[i]]$dN_dlogDp_log <-
      ifelse(list_of_sections_long[[i]]$dN_dlogDp_log == "NaN" |
               list_of_sections_long[[i]]$dN_dlogDp_log == "-Inf"  ,
             0.001, list_of_sections_long[[i]]$dN_dlogDp_log)

    invisible(gc())
  }
  message("Update: wide to long format conversion complete...")

  message(paste0("Update: the data has ", nrow(can_take_these_rows),
                 " runs of consecutive measurements. I will interpolate each run separately..."))
  # loop to interpolate each good section of data
  invisible(gc())
  list_of_sections_long_interp <- vector("list", length(list_of_sections_long))


  message("Update: preparing to interpolate...")
  for(i in 1:length(list_of_sections_long)) {
    message(paste0("Update: now working on run ", i, " ..."))

    # how many rows in each diameter class?
    # We need at least two unique diameter values to interpolate
    unique_diameter_values <- unique_diameter_values # must be more than 2

    n <-  as.data.frame(table(list_of_sections_long[[i]]$Diameter))[1,2] *
      unique_diameter_values

    chunks_of_n_rows <- split(list_of_sections_long[[i]],
                              (seq(nrow(list_of_sections_long[[i]]))-1) %/% n)

    # clear memory
    invisible(gc())

    message(paste0("Update: run ", i, " will be divided into ", length(chunks_of_n_rows), " chunks for interpolation..."))

    # interpolate chunk by chunk within a good section
    interp_output <-  vector("list", length(chunks_of_n_rows))

    for(j in seq_len(length(chunks_of_n_rows))){
      print(paste0("Update: Interpolating chunk ",
                   j,
                   " of ",
                   length(chunks_of_n_rows), " chunks..."))

      x1 <- chunks_of_n_rows[[j]]
      xo <- with(x1, seq(min(Time), max(Time), interval_in_seconds))
      yo <- with(x1, seq(min(Diameter), max(Diameter), interval_diameter))
      x2 <- with(x1, akima::interp(Time,
                                   Diameter,
                                   dN_dlogDp_log,
                                   xo = xo,
                                   yo = yo) )
      x3 <-  data.frame(matrix(data = x2$z,
                               ncol = length(x2$y),
                               nrow = length(x2$x)))
      names(x3) <- x2$y
      x3$Time <- as.POSIXct(x2$x, origin = "1970-01-01")
      interp_output[[j]] <- x3
      invisible(gc())

    }

    # combine pieces into one big df, too many time columns!
    .time <- interp_output[[1]]$Time
    dat_interp_log_df <- do.call("cbind", interp_output)
    dat_interp_log_df[, names(dat_interp_log_df) == "Time"] <- NULL
    dat_interp_log_df$Time <- .time

    list_of_sections_long_interp[[i]] <- dat_interp_log_df
  }

  dat_interp_log_df <- do.call("rbind", list_of_sections_long_interp)
  .time <- dat_interp_log_df$Time
  message("Update: interpolation complete.")

  # wide to long
  dat_interp_log_df_long <- tidyr::gather(dat_interp_log_df, "Diameter", "dN_dlogDp_log", 1:(ncol(dat_interp_log_df)-1))
  dat_interp_log_df_long$Diameter <- as.numeric(as.character(dat_interp_log_df_long$Diameter))

  message("Update: Data preparation complete.")
  return(dat_interp_log_df_long)

}



#' @title Plot the data
#' @name smps_plot
#
# This draws a plot of the data

#' @param the_prepared_data the output from the prepare_data function
#' @param aspect_ratio change the aspect ratio of the plot
#' @param font_size change the axis and legend text size
#' @param h how many hours apart the x-axis ticks should be
#' @param y_breaks location of major ticks on the y axis
#' @param seconds_offset offset in seconds for adjusting the log y-axis
#' @param colour_ramp colour scale to use for the contour
#' @param y_axis 'linear' or 'log' (base 10) y-axis
#' @param days the date of a single day, to limit the plot to one day or more days. Use the form "2013-01-26" with quotation marks, or make a vector with `c("2013-01-26", "2013-01-27")`
#' @param legend_title expression or quoted character string to set the legend title
#' @param ... so you can send other things to modify the theme and scales
#' @export
#' @import ggplot2
#' @importFrom scales log_trans
#


smps_plot <- function(the_prepared_data,
                      aspect_ratio = 1.1/5,
                      font_size = 12,
                      h = "2 hours",
                      y_breaks = 100,
                      colour_ramp = colour_ramp_igor,
                      y_axis = 'linear',
                      seconds_offset = 1000,
                      days = NULL,
                      legend_title = expression(dN/dlogD[p]~cm^-3),
                      ...) {


  y_labels_breaks <-  seq(0, max(the_prepared_data$Diameter), y_breaks)

  # add tick marks every h hours



  # set a few details for a custom theme
  mytheme <- ggplot2::theme_bw(base_size = font_size, ...) +
    ggplot2::theme(aspect.ratio = aspect_ratio, ...) +
    ggplot2::theme(plot.margin = unit(c(0, 0, 0, 0), "mm"))

  if (!is.null(days)) {
    # subset the days
    data_to_plot <- the_prepared_data
    # subset days for x lim and x-axis scaling
    subset_days <- the_prepared_data[grepl(days, the_prepared_data$Time), ]

    # for custom time breaks for one or a few days
    start_date <- min(subset_days$Time)
    end_date <-  max(subset_days$Time)
    date_breaks_h     <-  seq(from = start_date, to = end_date, by = h)
    date_breaks_1_day <- seq(from = start_date, to = end_date, by = "1 day")
    multiple          <- length(date_breaks_h) / length(date_breaks_1_day)
    time_labels_breaks <- seq(start_date, end_date, h)

    # show hours on the x-axis
    date_labels <- strftime(time_labels_breaks, "%H:%S")

    if (length(days) == 1) {
      the_xlab <- format(start_date, "%d-%b")
    } else {
      the_xlab <- paste0(format(start_date, "%d-%b"), " - ", format(end_date, "%d-%b"))
    }

  } else {
    # all the days
    data_to_plot <- the_prepared_data

    # for custom time breaks for all the days
    start_date <- min(data_to_plot$Time)
    end_date <-  max(data_to_plot$Time)
    date_breaks_h     <-  seq(from = start_date, to = end_date, by = h)
    date_breaks_1_day <- seq(from = start_date, to = end_date, by = "1 day")
    multiple          <- length(date_breaks_h) / length(date_breaks_1_day)
    time_labels_breaks <- seq(start_date, end_date, h)

    # show days on the x-axis
    date_labels <- insert_minor(multiple = multiple,
                                format(date_breaks_1_day, "%d %b"),
                                length(date_breaks_1_day))
    the_xlab <- "Day and time"

  }



   # plot all the data


  if (y_axis == "linear") {
    # draw the plot with linear y-axis

    # testing...
    distance <- diff((unique(data_to_plot$Diameter)))/2
    upper <- (unique(data_to_plot$Diameter)) + c(distance, distance[length(distance)])
    lower <- (unique(data_to_plot$Diameter)) - c(distance[1], distance)

    # Create xmin, xmax, ymin, ymax
    data_to_plot$xmin <- data_to_plot$Time - seconds_offset # default of geom_raster is 0.5
    data_to_plot$xmax <- data_to_plot$Time + seconds_offset
    idx <- rle(data_to_plot$Diameter)$lengths[1]
    data_to_plot$ymin <- unlist(lapply(lower, function(i) rep(i, idx)))
    data_to_plot$ymax <- unlist(lapply(upper, function(i) rep(i, idx)))



    the_plot <- ggplot(data_to_plot, aes(y = Diameter,
                                         x = Time)) +
      geom_rect(aes(xmin=xmin, xmax=xmax,
                    ymin=ymin, ymax=ymax,
                    fill = dN_dlogDp_log))  +
      # geom_raster(interpolate = TRUE,
      #              aes(fill = dN_dlogDp_log))  +
      scale_fill_gradientn(name = legend_title,
                           colours = colour_ramp(100),
                           labels = fill_scale_labels) +
      scale_y_continuous(expand = c(0,0),
                         labels = every_nth(y_labels_breaks, 2, inverse = TRUE),
                         breaks = y_labels_breaks,
                         ...) +
      scale_x_datetime(expand = c(0,0),
                       limits = c(as.POSIXct(start_date), as.POSIXct(end_date)),
                       breaks = date_breaks_h,
                       labels = date_labels,
                       ...) +
      xlab(the_xlab) +
      ylab("Diameter (nm)") +
      mytheme

  } else {

    if(y_axis == "log") {

      # plot with log y-axis

      # Now with log axis, we need to replace the ymin and ymax
      distance <- diff((unique(data_to_plot$Diameter)))/2
      upper <- (unique(data_to_plot$Diameter)) + c(distance, distance[length(distance)])
      lower <- (unique(data_to_plot$Diameter)) - c(distance[1], distance)

      # Create xmin, xmax, ymin, ymax
      data_to_plot$xmin <- data_to_plot$Time - seconds_offset # default of geom_raster is 0.5
      data_to_plot$xmax <- data_to_plot$Time + seconds_offset
      idx <- rle(data_to_plot$Diameter)$lengths[1]
      data_to_plot$ymin <- unlist(lapply(lower, function(i) rep(i, idx)))
      data_to_plot$ymax <- unlist(lapply(upper, function(i) rep(i, idx)))

      # draw the plot

      the_plot <- ggplot(data_to_plot, aes(y = Diameter,
                                           x = Time
                                                )) +
        geom_rect(aes(xmin=xmin, xmax=xmax,
                  ymin=ymin, ymax=ymax,
                  fill = dN_dlogDp_log))  +
        scale_fill_gradientn(name = legend_title,
                             colours = colour_ramp(100),
                             labels = fill_scale_labels,
                             ...)  +
        scale_y_continuous(expand = c(0,0),
                           trans = log_trans(), breaks = base_breaks()) +
        scale_x_datetime(expand = c(0,0),
                         limits = c(as.POSIXct(start_date), as.POSIXct(end_date)),
                         breaks = date_breaks_h,
                         labels = date_labels,
                         ...) +
        xlab(the_xlab) +
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

#' @title Function for minor tick marks
#' @name every_nth
#'
#' @param x .
#' @param nth .
#' @param empty .
#' @param inverse .
#'
#' @return .
#' @export
#'
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



#' @title Get visually diminishing axis ticks
#' @name base_breaks
#'
#' @param n .
#'
#' @return .
#' @export
#'
base_breaks <- function(n = 10){
  function(x) {
    axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}

#' @title Insert minor tick marks
#' @name insert_minor
#'
#'
#' @param major_labs .
#' @param n_minor .
#' @param multiple .
#'
#' @return .
#' @export
#'
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


#' @title Function to set the legend labels
#' @name fill_scale_labels
#'
#'
#' @param x .
#'
#' @return .
#' @export
#'
fill_scale_labels <- function(x) {
  parse(text = paste0("10^",x))
}



#' @title Get the legend
#' @name get_legend
#'
#' @param the_ggplot .
#'
#' from http://stackoverflow.com/a/17470321/1036500
#'
#' @export
#' @importFrom ggplot2 ggplot_gtable ggplot_build
#'
get_legend <- function(the_ggplot){

    tmp <- ggplot_gtable(ggplot_build(the_ggplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}


#' @title Convert Excel format dates to POSIX Date-time values
#' @name reformat_date
#'
#' In Excel if we have a date like this: 16/1/2013 23:58 it's not very
#' useful in R. This function rearranges the Excel date string into
#' a POSIX date-time string such as:  "2013-01-16 23:58:00 ICT". This is
#' useful if you want to add other layers to the main plot and you
#' need to convert the Excel date format into the POSIX format that
#' we're using for the main plot.
#'
#' @param the_excel_dates .
#'
#' @export
#'
reformat_date <- function(the_excel_dates){

  x <-  as.character(the_excel_dates)
  date_ <- as.Date(x, format = "%d/%m/%Y")
  time_ <- gsub(" ", "", substr(x, nchar(x) - 4, nchar(x)))
  the_dates_converted <- as.POSIXct(paste0(date_, " ", time_))
  return(the_dates_converted)
}



#' @title Example SMPS dataset to demonstrate the package
#' @name Example SMPS dataset
#'
#'
#' A dataset containing particle counts over three days.
#'
#' @format A data frame
#'
"my_data"

