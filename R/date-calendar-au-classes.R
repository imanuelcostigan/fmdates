#' @include date-calendar-classes.R
NULL

#' @format A \code{\link{R6Class}} generator object
#' @rdname Calendar
#' @export
AUCalendar <- R6::R6Class(
  classname = "AUCalendar",
  inherit = Calendar,
  public = list(
    is_good = function (a) {
      # a is result of .extract_atoms called by sub-class
      # Gather holidays generally observed across Australia
      # http://en.wikipedia.org/wiki/Public_holidays_in_Australia
      !(private$.is_we(a$wd) |
          private$.is_ny_day(a$dom, a$dow, a$m) |
          private$.is_au_day(a$dom, a$dow, a$m) |
          private$.is_anzac_day(a$dom, a$dow, a$m) |
          private$.is_xmas_day(a$dom, a$dow, a$m) |
          # Easter holidays
          #### NEED TO FIX CASE WHEN EASTER and ANZAC OVERLAP. HAVE EXTRA HOL
          #### EG 26 Apr 2011
          a$doy == a$em | a$doy == a$em - 3)
    }
  ),
  private = list(
    .is_we = function (wd) {
      wd == 1 | wd == 7
    },
    # New Years. Next weekday a  holiday if NY falls on W/E.
    .is_ny_day = function (dom, dow, m) {
      (dom == 1 | ((dom == 2 | dom == 3) & dow == 'Mon')) & m == 1
    },
    # Australia Day. Next weekday a holiday if Aus Day falls on W/E.
    .is_au_day = function (dom, dow, m) {
      (dom == 26 | ((dom == 27 | dom == 28) & dow == 'Mon')) & m == 1
    },
    # ANZAC day. Substitute not legislated, but generally given
    .is_anzac_day = function (dom, dow, m) {
      (dom == 25 | ((dom == 26 | dom == 27) & dow == 'Mon')) & m == 4
    },
    # Christmas. Substitute generally given
    .is_xmas_day = function (dom, dow, m) {
      (dom == 25 | (dom == 27 & (dow == 'Mon' | dow == 'Tues'))) & m == 12
    }
  )
)

#' @format A \code{\link{R6Class}} generator object
#' @rdname Calendar
#' @export
AUSYCalendar <- R6::R6Class(
  classname = "AUSYCalendar",
  inherit = AUCalendar,
  public = list(
    is_good = function (dates) {
      # http://www.legislation.nsw.gov.au/maintop/view/inforce/act+115+2010+cd+0+N
      a <- super$.extract_atoms(dates)
      super$is_good(a) & !(private$.is_queens_bday(a$dom, a$dow, a$m) |
          private$.is_labour_day(a$dom, a$dow, a$m) |
          private$.is_bank_day(a$dom, a$dow, a$m) |
          private$.is_box_day(a$dom, a$dow, a$m))
    }
  ),
  private = list(
    .tz = "Australia/Sydney",
    # Queens Birthday
    .is_queens_bday = function (dom, dow, m) {
      dom > 7 & dom <= 14 & dow == 'Mon' & m == 6
    },
    # Labour Day
    .is_labour_day = function (dom, dow, m) {
      dom <= 7 & dow == 'Mon' & m == 10
    },
    # Banker holiday
    .is_bank_day = function (dom, dow, m) {
      dom <= 7 & dow == 'Mon' & m == 8
    },
    # Boxing Day. Substitute generally given
    .is_box_day = function (dom, dow, m) {
      (dom == 26 | (dom == 28 & (dow == 'Mon'| dow == 'Tues'))) & m == 12
    }
  )
)

#' @format A \code{\link{R6Class}} generator object
#' @rdname Calendar
#' @export
AUMECalendar <- R6::R6Class(
  classname = "AUMECalendar",
  inherit = AUCalendar,
  public = list(
    is_good = function (dates) {
      # http://en.wikipedia.org/wiki/Public_holidays_in_Australia
      a <- super$.extract_atoms(dates)
      super$is_good(a) & !(private$.is_queens_bday(a$dom, a$dow, a$m) |
          private$.is_labour_day(a$dom, a$dow, a$m) |
          private$.is_mc_day(a$dom, a$dow, a$m) |
          private$.is_box_day(a$dom, a$dow, a$m))
    }
  ),
  private = list(
    .tz = "Australia/Melbourne",
    # Queens Birthday
    .is_queens_bday = function (dom, dow, m) {
      dom > 7 & dom <= 14 & dow == 'Mon' & m == 6
    },
    # Labour Day
    .is_labour_day = function (dom, dow, m) {
      dom > 7 & dom <= 14 & dow == 'Mon' & m == 3
    },
    # Banker holiday
    .is_mc_day = function (dom, dow, m) {
      dom <= 7 & dow == 'Tues' & m == 11
    },
    # Boxing Day. Substitute generally given
    .is_box_day = function (dom, dow, m) {
      (dom == 26 | (dom == 28 & (dow == 'Mon'| dow == 'Tues'))) & m == 12
    }
  )
)
