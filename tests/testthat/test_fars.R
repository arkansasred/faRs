library(testthat)

expect_that(fars_map_state(100,2014),throws_error())
expect_that(fars_map_state(10,2001),throws_error())
