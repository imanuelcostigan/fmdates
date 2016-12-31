#' @include date-calendar-classes.R
NULL

#' @format A \code{\link{R6Class}} generator object
#' @rdname Calendar
#' @export
EUTACalendar <- R6::R6Class(
  classname = "EUTACalendar",
  inherit = Calendar,
  public = list(
    is_good = function (dates) {
      assertthat::assert_that(all(lubridate::year(dates) > 1998))
      a <- super$.extract_atoms(dates)
      !(private$.is_we(a$wd) |
          private$.is_ny_day(a$doy) |
          ((a$doy == a$em | a$doy == a$em - 3) & a$y >= 2000) |
          private$.is_labour_day(a$dom, a$m, a$y) |
          private$.is_xmas_day(a$dom, a$m) |
          private$.is_prudential(a$dom, a$m, a$y))
    }
  ),
  private = list(
    .tz = "Europe/Brussels",
    # EUR holiday calendar
    # http://www.ecb.europa.eu/home/html/holidays.en.html
    # Closing days (1999):
    # http://www.ecb.europa.eu/press/pr/date/1998/html/pr980903.en.html
    # Closing days (2000):
    # http://www.ecb.europa.eu/press/pr/date/1999/html/pr990715_1.en.html
    # Closing days (2001):
    # http://www.ecb.europa.eu/press/pr/date/2000/html/pr000525_2.en.html
    # Closing days ()
    # http://www.ecb.europa.eu/press/pr/date/2000/html/pr001214_4.en.html
    .is_we = function (wd) {
      wd == 1 | wd == 7
    },
    .is_ny_day = function (doy) {
      doy == 1
    },
    .is_labour_day = function (dom, m, y) {
      dom == 1 & m == 5 & y >= 2000
    },
    .is_xmas_day = function (dom, m) {
      (dom == 25 | dom == 26) & m == 12
    },
    .is_prudential = function (dom, m, y) {
      dom == 31 & m == 12 & (y == 1999 | y == 2001)
    }
  )
)
