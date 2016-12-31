#' @include date-calendar-classes.R
NULL

#' @format A \code{\link{R6Class}} generator object
#' @rdname Calendar
#' @export
HKHKCalendar <- R6::R6Class(
  classname = "HKHKCalendar",
  inherit = Calendar,
  public = list(
    is_good = function (dates) {
      # http://www.gov.hk/en/about/abouthk/holiday/
      # https://en.wikipedia.org/w/index.php?title=Public_holidays_in_Hong_Kong&oldid=703958274
      a <- super$.extract_atoms(dates)
      cny <- lubridate::yday(chinese_new_year(lubridate::year(dates)))
      !(private$.is_we(a$wd) |
        private$.is_ny_day(a$dow, a$dom, a$m) |
        # Chinese lunar new year
        a$doy %in% cny |
        a$doy %in% (cny + 1) |
        a$doy %in% (cny + 2) |
        # Ching Ming
        a$doy == lubridate::yday(a$ve) + 15 |
        #Easter
        a$doy == a$em | a$doy == a$em - 3 |
        private$.is_labour_day(a$dow, a$dom, a$m) |
        private$.is_buddha_bday(a$dow, a$dom, a$m, a$y) |
        private$.is_dragon_boat_day(a$dow, a$dom, a$m, a$y) |
        private$.is_establishment_day(a$dow, a$dom, a$m) |
        private$.is_mid_autumnal_day(a$dow, a$dom, a$m, a$y) |
        private$.is_national_day(a$dow, a$dom, a$m) |
        private$.is_chung_yeung_festival(a$dow, a$dom, a$m, a$y) |
        private$.is_xmas_day(a$dow, a$dom, a$m) |
        private$.is_box_day(a$dow, a$dom, a$m))
    }
  ),
  private = list(
    .tz = "Asia/Hong_Kong",
    .is_we = function (wd) {
      wd == 1 | wd == 7
    },
    .is_ny_day = function (dow, dom, m) {
      # New Years.
      m == 1 & (dom == 1 | (dom == 2 & dow == "Mon"))
    },
    .is_labour_day = function (dow, dom, m) {
      # Only roll if holiday falls on Sunday (not Saturday)
      m == 5 & (dom == 1 | ((dom == 2) & dow == "Mon"))
    },
    .is_buddha_bday = function (dow, dom, m, y) {
      start_4th_lunar_month <- next_moon_phase(ISOdate(y, 4, 25, 0), "new",
        "Asia/Shanghai", FALSE)
      buddhas_bd <- start_4th_lunar_month + lubridate::days(7)
      m == lubridate::month(buddhas_bd) &
        (dom == lubridate::mday(buddhas_bd) |
            (dom == lubridate::mday(buddhas_bd) + 1 & dow == "Mon"))
    },
    .is_dragon_boat_day = function (dow, dom, m, y) {
      start_5th_lunar_month <- next_moon_phase(ISOdate(y, 5, 25, 0), "new",
        "Asia/Shanghai", FALSE)
      dragon_boat_day <- start_5th_lunar_month + lubridate::days(4)
      m == lubridate::month(dragon_boat_day) &
        (dom == lubridate::mday(dragon_boat_day) |
            (dom == lubridate::mday(dragon_boat_day) + 1 & dow == "Mon"))
    },
    .is_establishment_day = function (dow, dom, m) {
      m == 7 & (dom == 1 | (dom == 2 & dow == "Mon"))
    },
    .is_mid_autumnal_day = function (dow, dom, m, y) {
      start_8th_lunar_month <- next_moon_phase(ISOdate(y, 8, 25, 0), "new",
        "Asia/Shanghai", FALSE)
      mid_autumnal_day <- start_8th_lunar_month + lubridate::days(15)
      m == lubridate::month(mid_autumnal_day) &
        (dom == lubridate::mday(mid_autumnal_day) |
            (dom == lubridate::mday(mid_autumnal_day) + 1 & dow == "Mon"))
    },
    .is_national_day = function (dow, dom, m) {
      m == 10 & (dom == 1 | (dom == 2 & dow == "Mon"))
    },
    .is_chung_yeung_festival = function (dow, dom, m, y) {
      start_9th_lunar_month <- next_moon_phase(ISOdate(y, 9, 25, 0), "new",
        "Asia/Shanghai", FALSE)
      chung_yeung <- start_9th_lunar_month + lubridate::days(8)
      m == lubridate::month(chung_yeung) &
        (dom == lubridate::mday(chung_yeung) |
            (dom == lubridate::mday(chung_yeung) + 1 & dow == "Mon"))
    },
    .is_xmas_day = function (dow, dom, m) {
      (dom == 25 | (dom == 27 & (dow == 'Mon' | dow == 'Tues'))) & m == 12
    },
    .is_box_day = function (dow, dom, m) {
      (dom == 26 | (dom == 28 & (dow == 'Mon'| dow == 'Tues'))) & m == 12
    }
  )
)
