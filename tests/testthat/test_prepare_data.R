library(smps)
context("testing prepare_data")

data("my_data")

prepared_data <- prepare_data(my_data)

# dput(head(prepared_data))
expected <-
structure(list(Time = structure(c(1359187200, 1359187400, 1359187600,
                                  1359187800, 1359188000, 1359188200), class = c("POSIXct", "POSIXt"
                                  ), tzone = ""), Diameter = c(14.6, 14.6, 14.6, 14.6, 14.6, 14.6
                                  ), dN_dlogDp_log = c(4.4376332557685, 4.45576333332199, 4.47389341087547,
                                                       4.49202348842896, 4.51399838890342, 4.53597328937789)), .Names = c("Time",
                                                                                                                          "Diameter", "dN_dlogDp_log"), row.names = c(NA, 6L), class = "data.frame")

expected <- expected[ , -1]
prepared_data <- prepared_data[, -1]

test_that("prepare_data can work", {
  expect_equal(head(prepared_data), expected)
})
