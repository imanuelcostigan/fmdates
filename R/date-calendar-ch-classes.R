#' @include date-calendar-classes.R
NULL

#' @format A \code{\link{R6Class}} generator object
#' @rdname Calendar
#' @export
CHZHCalendar <- R6::R6Class(
  classname = "CHZH",
  inherit = Calendar,
  public = list(
    is_good = function (dates) {
      a <- super$.extract_atoms(dates)
      # Weekends
      !(private$.is_we(a$wd) |
          # New years. No rolls
          private$.is_ny_day(a$doy, a$dom, a$m) |
          # St. Berchtold
          private$.is_st_berchtold(a$doy) |
          # Maudy Thurs, Good Friday, Easter Monday
          (a$doy == a$em | a$doy == a$em - 3 | a$doy == a$em - 4) |
          # May day
          private$.is_labour_day(a$dom, a$m) |
          # Ascension day
          a$doy == a$em + 38 |
          # Whit Mon
          a$doy == a$em + 49 |
          # National independence day
          private$.is_independence_day(a$dom, a$m) |
          # Christmas Eve, Day, St. Stephen's day
          private$.is_xmas_day(a$dom, a$m)
      )
    }
  ),
  private = list(
    .tz = "Europe/Zurich",
    .is_we = function (wd) {
      wd == 1 | wd == 7
    },
    .is_ny_day = function (doy, dom, m) {
      doy == 1 | (dom == 31 & m == 12)
    },
    .is_st_berchtold = function (doy) {
      doy == 2
    },
    .is_labour_day = function (dom, m) {
      dom == 1 & m == 5
    },
    .is_independence_day = function (dom, m) {
      dom == 1 & m == 8
    },
    .is_xmas_day = function (dom, m) {
      (dom >= 24 & dom <= 26) & m == 12
    }
  )
)
