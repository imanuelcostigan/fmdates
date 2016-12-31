#' @include date-calendar-classes.R
NULL

#' @format A \code{\link{R6Class}} generator object
#' @rdname Calendar
#' @export
JPTOCalendar <- R6::R6Class(
  classname = "JPTOCalendar",
  inherit = Calendar,
  public = list(
    is_good = function (dates) {
      #   http://en.wikipedia.org/wiki/Public_holidays_in_Japan
      a <- super$.extract_atoms(dates)
      !(private$.is_we(a$wd) |
          private$.is_ny_day(a$dom, a$m) |
          private$.is_coming_day(a$dom, a$dow, a$y, a$m) |
          private$.is_foundation_day(a$dom, a$dow, a$m) |
          private$.is_equinox_day(a$ve, a$ae, a$doy, a$dow, a$m) |
          private$.is_showa_day(a$dom, a$dow, a$m) |
          private$.is_may_days(a$dom, a$dow, a$m) |
          private$.is_marine_day(a$dom, a$dow, a$y, a$m) |
          private$.is_respect_day(a$dom, a$dow, a$y, a$m) |
          private$.is_citizens_day(a$ae, a$dom, a$dow, a$y, a$m) |
          private$.is_health_day(a$dom, a$dow, a$y, a$m) |
          private$.is_culture_day(a$dom, a$dow, a$m) |
          private$.is_labour_day(a$dom, a$dow, a$m) |
          private$.is_emperors_day(a$dom, a$dow, a$m) |
          private$.is_bank_day(a$dom, a$m))
    }
  ),
  private = list(
    .tz = "Asia/Tokyo",
    .is_we = function (wd) {
      wd == 1 | wd == 7
    },
    .is_ny_day = function (dom, m) {
      # New Years. Plus two days following are bank holidays
      (dom == 1 | dom == 2 | dom == 3) & m == 1
    },
    .is_coming_day = function (dom, dow, y, m) {
      # Coming of Age Day. 2nd Mon of Jan
      # Happy Monday starts
      (((dom > 7 & dom <= 14) & m == 1 & y >= 2000 & dow == 'Mon') |
          # Before start of Happy Monday
          ((dom == 15 | (dom == 16 & dow == 'Mon')) & m == 1 & y < 2000))
    },
    .is_foundation_day = function (dom, dow, m) {
      (dom == 11 | (dom == 12 & dow == 'Mon')) & m == 2
    },
    .is_equinox_day = function (ve, ae, doy, dow, m) {
      ve <- lubridate::yday(lubridate::with_tz(ve, self$tz))
      ae <- lubridate::yday(lubridate::with_tz(ae, self$tz))
      # Vernal Equinox Day (spring)
      ((doy == ve | (doy == ve + 1 & dow == 'Mon')) & m == 3) |
        # Autumnal equinox day
        ((doy == ae | (doy == ae + 1 & dow == 'Mon')) & m == 9)
    },
    .is_showa_day = function (dom, dow, m) {
      (dom == 29 | (dom == 30 & dow == 'Mon')) & m == 4
    },
    .is_may_days = function (dom, dow, m) {
      # Constitution day, Greenery day, Children's day
      ((dom == 3 | dom == 4 | dom == 5) & m == 5) |
        (dom == 6 & (dow == 'Mon' | dow == 'Tues' | dow == 'Wed') & m == 5)
    },
    .is_marine_day = function (dom, dow, y, m) {
      ((dom > 14 & dom <= 21) & dow == 'Mon' & m == 7 & y >= 2003) |
        ((dom == 20 | (dom == 21 & dow == 'Mon')) & m == 7 & y < 2003)
    },
    .is_respect_day = function (dom, dow, y, m) {
      # Respect for aged day
      ((dom > 14 & dom <= 21) & dow == 'Mon' & m == 9 & y >= 2003) |
        ((dom == 15 | (dom == 16 & dow == 'Mon')) & m == 9 & y < 2003)
    },
    .is_citizens_day = function (ae, dom, dow, y, m) {
      ae <- lubridate::yday(lubridate::with_tz(ae, self$tz))
      # Possible Citizen's day
      dom + 1 == ae & dom > 15 & dom <= 22 & dow == 'Tues' & m == 9 & y >= 2003
    },
    .is_health_day = function (dom, dow, y, m) {
      # Health and sports day
      ((dom > 7 & dom <= 14) & dow == 'Mon' & m == 10 & y >= 2000) |
        ((dom == 10 | (dom == 11 & dow == 'Mon')) & m == 10 & y < 2000)
    },
    .is_culture_day = function (dom, dow, m) {
      # Culture day
      (dom == 3 | (dom == 4 & dow == 'Mon')) & m == 11
    },
    .is_labour_day = function (dom, dow, m) {
      # Labour Thanksgiving Day
      (dom == 23 | (dom == 24 & dow == 'Mon')) & m == 11
    },
    .is_emperors_day = function (dom, dow, m) {
      # The Emperor's Birthday
      (dom == 23 | (dom == 24 & dow == 'Mon')) & m == 12
    },
    .is_bank_day = function (dom, m) {
      # Bank holiday:
      # http://www.boj.or.jp/en/about/outline/holi.htm/
      dom == 31 & m == 12
    }
  )
)
