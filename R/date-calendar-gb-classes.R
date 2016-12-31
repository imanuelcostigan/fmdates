#' @include date-calendar-classes.R
NULL

#' @format A \code{\link{R6Class}} generator object
#' @rdname Calendar
#' @export
GBLOCalendar <- R6::R6Class(
  classname = "GBLOCalendar",
  inherit = Calendar,
  public = list(
    is_good = function (dates) {
      a <- super$.extract_atoms(dates)
      # http://en.wikipedia.org/wiki/Public_holidays_in_the_United_Kingdom
      # http://en.wikipedia.org/wiki/Bank_holiday [2002, 2012 spring bank hol moved
      # to 4 Jun for Queen's jubilee]
      # http://www.timeanddate.com/holidays/uk/spring-bank-holiday#obs
      # http://www.legislation.gov.uk/ukpga/1971/80
      !(private$.is_we(a$wd) |
          private$.is_ny_day(a$dom, a$dow, a$m) |
          a$doy == a$em | a$doy == a$em - 3 |
          private$.is_bank_day(a$dom, a$dow, a$m, a$y) |
          private$.is_jubilee(a$dom, a$m, a$y) |
          private$.is_xmas_day(a$dom, a$m) |
          private$.is_box_day(a$dom, a$dow, a$m) |
          private$.is_royal_wedding(a$dom, a$m, a$y))
    }
  ),
  private = list(
    .tz = "Europe/London",
    .is_we = function (wd) {
      wd == 1 | wd == 7
    },
    # New Years. Sub next Mon if on Sat or Sunday
    .is_ny_day = function (dom, dow, m) {
      (dom == 1 | ((dom == 2 | dom == 3) & dow == 'Mon')) & m == 1
    },
    .is_bank_day = function (dom, dow, m, y) {
      # May Day bank holiday. First Mon of May. 2002/2012 spring hol moved 4 Jun.
      (dom <= 7 & m == 5 & dow == 'Mon' & y >= 1978 & (y != 2002 | y != 2012)) |
        (dom == 4 & m == 6 & (y == 2002 | y == 2012)) |
        # Spring bank hol. Last Mon of May (excl. 2002, 2012)
        (dom > 24 & m == 5 & dow == 'Mon' & (y >= 1971 & y != 2002 & y != 2012)) |
        # Spring bank holiday pushed back to 4 June for Queen's Golden and Diamond
        # Jubilee
        (dom == 4 & m == 6 & (y == 2002 | y == 2012)) |
        # Late summer bank hol. Last Mon of Aug.
        (dom > 24 & m == 8 & dow == 'Mon' & y >= 1971)
    },
    # Queen's diamond jubilee
    .is_jubilee = function (dom, m, y) {
      dom == 5 & m == 6 & y == 2012
    },
    # Christmas.
    .is_xmas_day = function (dom, m) {
      dom == 25 & m == 12
    },
    # Boxing Day. 26th December, if not a Sun.
    # 27th December in a year in which 25th or 26th December is a Sunday
    .is_box_day = function (dom, dow, m) {
      (dom == 26 | (dom == 27 & (dow == 'Mon' | dow == 'Tues'))) & m == 12
    },
    # Royal Wedding
    .is_royal_wedding = function (dom, m, y) {
      dom == 29 & m == 4 & y == 2011
    }
  )
)
