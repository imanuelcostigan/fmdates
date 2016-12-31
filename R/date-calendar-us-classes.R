#' @include date-calendar-classes.R
NULL

#' @format A \code{\link{R6Class}} generator object
#' @rdname Calendar
#' @export

USNYCalendar <- R6::R6Class(
  classname = "USNYCalendar",
  inherit = Calendar,
  public = list(
    is_good = function (dates) {
      a <- super$.extract_atoms(dates)
      # http://en.wikipedia.org/wiki/New_York_State_government_holidays
      !(private$.is_we(a$wd) |
          private$.is_ny_day(a$dom, a$dow, a$m) |
          private$.is_mlk_gw_day(a$dom, a$dow, a$m) |
          private$.is_memorial_day(a$dom, a$dow, a$m) |
          private$.is_independence_day(a$dom, a$dow, a$m) |
          private$.is_labour_day(a$dom, a$dow, a$m) |
          private$.is_colombus_day(a$dom, a$dow, a$m) |
          private$.is_veterans_day(a$dom, a$dow, a$m) |
          private$.is_thanksgiving(a$dom, a$dow, a$m) |
          private$.is_xmas_day(a$dom, a$dow, a$m))
    }
  ),
  private = list(
    .tz = "America/New_York",
    .is_we = function (wd) {
      wd == 1 | wd == 7
    },
    # New Years. Sub Mon if on Sunday, and Fri if on Saturday
    .is_ny_day = function (dom, dow, m) {
      (dom == 1 & m == 1) | (dom == 31 & m == 12 & dow == 'Fri') |
        (dom == 2 & m == 1 & dow == 'Mon')
    },
    # MLK day, Washington's birthday. 3rd Mon of Jan, Feb (resp.)
    .is_mlk_gw_day = function (dom, dow, m) {
      dom > 14 & dom <= 21 & dow == 'Mon' & (m == 1 | m == 2)
    },
    # Memorial day. Last Mon of May
    .is_memorial_day = function (dom, dow, m) {
      dom > 24 & dow == 'Mon' & m == 5
    },
    # Independence Day. 4 Jul.  Sub Mon if on Sunday, and Fri if on Saturday
    .is_independence_day = function (dom, dow, m) {
      (dom == 4 | (dom == 5 & dow == 'Mon') | (dom == 3 & dom == 'Fri')) &
        m == 7
    },
    # Labour day. 1st Mon of Sep.
    .is_labour_day = function (dom, dow, m) {
      dom <= 7 & dow == 'Mon' & m == 9
    },
    # Columbus day. 2nd Mon of Oct.
    .is_colombus_day = function (dom, dow, m) {
      dom > 7 & dom <= 14 & dow == 'Mon' & m == 10
    },
    .is_veterans_day = function (dom, dow, m) {
      # Veteran's day. 11 Nov.  Sub Mon if on Sunday, and Fri if on Saturday
      (dom == 11 | (dom == 12 & dow == 'Mon') | (dom == 10 & dow == 'Fri')) &
        m == 11
    },
    # Thanksgiving. 4th Thurs of Nov
    .is_thanksgiving = function (dom, dow, m) {
      dom > 21 & dom <= 28 & dow == 'Thurs' & m == 11
    },
    # Christmas. Sub Mon if on Sunday, and Fri if on Saturday
    .is_xmas_day = function (dom, dow, m) {
      (dom == 25 | (dom == 26 & dow == 'Mon') | (dom == 24 & dow == 'Fri')) &
        m == 12
    }
  )
)
