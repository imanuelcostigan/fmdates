#' @include date-calendar-classes.R
NULL

#' @format A \code{\link{R6Class}} generator object
#' @rdname Calendar
#' @export
NZCalendar <- R6::R6Class(
  classname = "NZAUCalendar",
  inherit = Calendar,
  public = list(
    is_good = function (a) {
      !(private$.is_we(a$wd) |
          private$.is_ny_day(a$dom, a$dow, a$m) |
          private$.is_waitangi_day(a$dom, a$dow, a$y, a$m) |
          private$.is_anzac_day(a$dom, a$dow, a$y, a$m) |
          private$.is_queens_bday(a$dom, a$dow, a$m) |
          private$.is_labour_day(a$dom, a$dow, a$m) |
          private$.is_xmas_day(a$dom, a$dow, a$m) |
          private$.is_box_day(a$dom, a$dow, a$m) |
          a$doy == a$em | a$doy == a$em - 3)
    }
  ),
  private = list(
    .tz = "Pacific/Auckland",
    .is_we = function (wd) {
      wd == 1 | wd == 7
    },
    .is_ny_day = function (dom, dow, m) {
      m == 1 & (((dom == 1 | (dom == 3 & (dow == 'Mon' | dow == 'Tues')))) |
        ((dom == 2 | (dom == 4 & (dow == 'Mon' | dow == 'Tues')))))
    },
    .is_waitangi_day = function (dom, dow, y, m) {
      (dom == 6 | ((dom == 7 | dom == 8) & dow == 'Mon' & y > 2013)) & m == 2
    },
    .is_anzac_day = function (dom, dow, y, m) {
      (dom == 25 | ((dom == 26 | dom == 27) & dow == 'Mon' & y > 2013)) & m == 4
    },
    .is_queens_bday = function (dom, dow, m) {
      dom <= 7 & dow == 'Mon' & m == 6
    },
    .is_labour_day = function (dom, dow, m) {
      dom > 21 & dom <= 28 & dow == 'Mon' & m == 10
    },
    .is_xmas_day = function (dom, dow, m) {
      (dom == 25 | (dom == 27 & (dow == 'Mon' | dow == 'Tues'))) & m == 12
    },
    .is_box_day = function (dom, dow, m) {
      (dom == 26 | (dom == 28 & (dow == 'Mon'| dow == 'Tues'))) & m == 12
    }
  )
)

#' @format A \code{\link{R6Class}} generator object
#' @rdname Calendar
#' @export

NZAUCalendar <- R6::R6Class(
  classname = "NZAUCalendar",
  inherit = NZCalendar,
  public = list(
    is_good = function (dates) {
      a <- super$.extract_atoms(dates)
      super$is_good(a) & !(private$.is_au_day(a$dom, a$dow, a$m))
    },
    adjust = function (dates, bdc = "u") {
      super$.adjust(dates, bdc)
    }
  ),
  private = list(
    .tz = "Pacific/Auckland",
    .is_au_day = function (dom, dow, m) {
      ((dom >= 26 & m == 1) | (dom <= 1 & m == 2)) & dow == 'Mon'
    }
  )
)

#' @format A \code{\link{R6Class}} generator object
#' @rdname Calendar
#' @export

NZWECalendar <- R6::R6Class(
  classname = "NZWECalendar",
  inherit = NZCalendar,
  public = list(
    is_good = function (dates) {
      a <- super$.extract_atoms(dates)
      super$is_good(a) & !(private$.is_we_day(a$dom, a$dow, a$m))
    },
    adjust = function (dates, bdc = "u") {
      super$.adjust(dates, bdc)
    }
  ),
  private = list(
    .is_we_day = function (dom, dow, m) {
      (dom >= 19 & dom <= 25) & dow == 'Mon' & m == 1
    }
  )
)
