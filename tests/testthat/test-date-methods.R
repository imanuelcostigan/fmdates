context("Methods")

suppressPackageStartupMessages(library(lubridate))

test_that("is_eom works as expected", {
  expect_identical(is_eom(ymd("2011-02-28")), TRUE)
  expect_identical(is_eom(ymd("2012-02-28")), FALSE)
  expect_identical(is_eom(ymd("2012-02-29")), TRUE)
})

test_that("nth_nday works as expected", {
  expect_identical(nth_nday(ymd("20120601"), 2, 6), ymd("20120608"))
  expect_identical(nth_nday(ymd("20120601"), 2, "fri"), ymd("20120608"))
  expect_identical(nth_nday(ymd("20120601"), 2, "Fri"), ymd("20120608"))
  expect_identical(nth_nday(ymd("20120901"), 3, 4), ymd("20120919"))
  expect_identical(nth_nday(ymd("20121201"), 1, 2), ymd("20121203"))
  expect_identical(nth_nday(ymd("20121215"), 4, 5), ymd("20130110"))
  expect_identical(nth_nday(ymd("20121102"), 14, 3), ymd("20130205"))
})

test_that("futures_settlement:", {
  d1 <- lubridate::as_date(ymd(20160101))
  expect_identical(futures_settlement(d1, 1, 2, "fri"),
    lubridate::as_date(ymd(20160311)))
  expect_identical(futures_settlement(d1, 2, 2, "fri"),
    lubridate::as_date(ymd(20160610)))
  expect_identical(futures_settlement(d1, 1, 1, "wed", 9),
    lubridate::as_date(ymd(20160316)))
  expect_identical(futures_settlement(d1, 2, 1, "wed", 9),
    lubridate::as_date(ymd(20160615)))
  # Waiting to reimplement calendars
  # expect_identical(futures_settlement(d1, 1, 3, "wed", post_offset = days(-1),
  #   post_offset_calendar = CHZHCalendar$new()),
  #   lubridate::as_date(ymd(20160315)))
})

test_that("eom works as expected", {
  # Waiting to reimplement calendars
  # expect_identical(eom(ymd(20120203, 20140203)), ymd(20120229, 20140228))
})

test_that("Julian day works:", {
  expect_identical(to_jd(ymd(19571004) + seconds(0.81*24*60*60)), 2436116.31)
  expect_identical(to_jd(ymd_hms(20000101120000)), 2451545.0)
  expect_identical(to_jd(ymd(19000101)), 2415020.5)
})
