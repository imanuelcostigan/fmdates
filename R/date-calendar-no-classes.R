#' @include date-calendar-classes.R
NULL

#' @format A \code{\link{R6Class}} generator object
#' @rdname Calendar
#' @export
NOOSCalendar <- R6::R6Class(
  classname = "NOOSCalendar",
  inherit = Calendar,
  public = list(
    is_good = function (dates) {
      a <- super$.extract_atoms(dates)
      # Weekends
      !(private$.is_we(a$wd) |
          # New years.
          private$.is_ny_day(a$doy, a$dom, a$m) |
          # Maudy Thurs, Good Friday, Easter Monday
          (a$doy == a$em | a$doy == a$em - 3 | a$doy == a$em - 4) |
          # May day
          private$.is_labour_day(a$dom, a$m) |
          # Constitution day
          private$.is_constitution_day(a$dom, a$m) |
          # Ascension day
          a$doy == a$em + 38 |
          # Pentecost
          a$doy == a$em + 48 |
          # Whit Mon
          a$doy == a$em + 49 |
          # Christmas Day, St. Stephen's day
          private$.is_xmas_day(a$dom, a$m)
      )
    }
  ),
  private = list(
    .tz = "Europe/Oslo",
    .is_we = function (wd) {
      wd == 1 | wd == 7
    },
    .is_ny_day = function (doy, dom, m) {
      doy == 1
    },
    .is_labour_day = function (dom, m) {
      dom == 1 & m == 5
    },
    .is_constitution_day = function (dom, m) {
      dom == 17 & m == 5
    },
    .is_xmas_day = function (dom, m) {
      (dom == 25 | dom == 26) & m == 12
    }
  )
)
