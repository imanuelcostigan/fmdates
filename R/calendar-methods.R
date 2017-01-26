# Generics ----------------------------------------------------------------

#' Good date checker
#'
#' Checks whether dates are business days (good days) in a given locale
#' represented by a `Calendar`.
#'
#' An `is_good` method must be written for each calendar. The default method
#' returns `TRUE` for all dates. Methods have been implemented for each of the
#' calendars inheriting from the `Calendar` class - see the method's code for
#' more details. The method implemented for the `JointCalendar` class checks
#' whether the supplied dates are good in each or any of the locales represented
#' by the joint calendar depending on the rule specified by the joint calendar.
#'
#' @param dates a vector of dates
#' @param calendar an object inheriting from either [Calendar] or
#'   [JointCalendar]. Dispatch to methods occurs on this argument.
#' @return a logical vector with `TRUE` if the date is good and `FALSE` if the
#'   date is bad
#' @examples
#' is_good(lubridate::ymd(20160126, 20160411), AUSYCalendar())
#' is_good(lubridate::ymd(20160126), USNYCalendar())
#' @export
#' @family calendar methods
#' @seealso Calendar

is_good <- function(dates, calendar) UseMethod("is_good", calendar)

#' Extract locale from calendars
#'
#' @param x an instance of a [`Calendar`] or [`JointCalendar`] object
#' @return a string representing the locale (e.g. "AUSY")
#' @examples
#' locale(AUSYCalendar())
#' locale(c(AUSYCalendar(), AUMECalendar()))
#' @export
#' @family calendar methods
locale <- function(x) UseMethod("locale", x)

# Methods -----------------------------------------------------------------

#' @export
is_good.default <- function(dates, calendar) {
  rep_len(TRUE, NROW(dates))
}

#' @export
is_good.AUSYCalendar <- function(dates, calendar) {
  # Gather holidays generally observed across Australia
  # http://en.wikipedia.org/wiki/Public_holidays_in_Australia
  a <- extract_atoms(dates, calendar)
  # Weekend
  !(a$wd == 1 | a$wd == 7 |
      # New Years. Next weekday a  holiday if NY falls on W/E.
      ((a$dom == 1 | ((a$dom == 2 | a$dom == 3) & a$dow == 'Mon')) &
          a$m == 1) |
      # Australia Day. Next weekday a holiday if Aus Day falls on W/E.
      ((a$dom == 26 | ((a$dom == 27 | a$dom == 28) & a$dow == 'Mon')) &
          a$m == 1) |
      # ANZAC day. Substitute not legislated, but generally given
      ((a$dom == 25 | ((a$dom == 26 | a$dom == 27) & a$dow == 'Mon')) &
          a$m == 4) |
      # Christmas. Substitute generally given
      ((a$dom == 25 | (a$dom == 27 & (a$dow == 'Mon' | a$dow == 'Tues'))) &
          a$m == 12) |
      # Easter holidays
      #### TODO:
      #### NEED TO FIX CASE WHEN EASTER and ANZAC OVERLAP. HAVE EXTRA HOL
      #### EG 26 Apr 2011
      (a$doy == a$em | a$doy == a$em - 3) |
      # http://www.legislation.nsw.gov.au/maintop/view/inforce/act+115+2010+cd+0+N
      # Queens Birthday
      (a$dom > 7 & a$dom <= 14 & a$dow == 'Mon' & a$m == 6) |
      # Labour Day
      (a$dom <= 7 & a$dow == 'Mon' & a$m == 10) |
      # Banker holiday
      (a$dom <= 7 & a$dow == 'Mon' & a$m == 8) |
      # Boxing Day. Substitute generally given
      ((a$dom == 26 | (a$dom == 28 & (a$dow == 'Mon'| a$dow == 'Tues'))) &
          a$m == 12))
}

#' @export
is_good.AUMECalendar <- function(dates, calendar) {
  # Gather holidays generally observed across Australia
  # http://en.wikipedia.org/wiki/Public_holidays_in_Australia
  a <- extract_atoms(dates, calendar)
  # Weekend
  !(a$wd == 1 | a$wd == 7 |
      # New Years. Next weekday a  holiday if NY falls on W/E.
      ((a$dom == 1 | ((a$dom == 2 | a$dom == 3) & a$dow == 'Mon')) &
          a$m == 1) |
      # Australia Day. Next weekday a holiday if Aus Day falls on W/E.
      (((a$dom == 26 | ((a$dom == 27 | a$dom == 28) & a$dow == 'Mon'))) &
          a$m == 1) |
      # ANZAC day. Substitute not legislated, but generally given
      ((a$dom == 25 | ((a$dom == 26 | a$dom == 27) & a$dow == 'Mon')) &
          a$m == 4) |
      # Christmas. Substitute generally given
      ((a$dom == 25 | (a$dom == 27 & (a$dow == 'Mon' | a$dow == 'Tues'))) &
          a$m == 12) |
      # Easter holidays
      #### TODO:
      #### NEED TO FIX CASE WHEN EASTER and ANZAC OVERLAP. HAVE EXTRA HOL
      #### EG 26 Apr 2011
      (a$doy == a$em | a$doy == a$em - 3) |
      # http://www.legislation.nsw.gov.au/maintop/view/inforce/act+115+2010+cd+0+N
      # Queens Birthday
      (a$dom > 7 & a$dom <= 14 & a$dow == 'Mon' & a$m == 6) |
      # Labour Day
      (a$dom > 7 & a$dom <= 14 & a$dow == 'Mon' & a$m == 3) |
      # Melb cup day
      (a$dom <= 7 & a$dow == 'Tues' & a$m == 11) |
      # Boxing Day. Substitute generally given
      ((a$dom == 26 | (a$dom == 28 & (a$dow == 'Mon'| a$dow == 'Tues'))) &
          a$m == 12))
}

#' @export
is_good.CHZHCalendar <- function(dates, calendar) {
  a <- extract_atoms(dates, calendar)
  # Weekends
  !(a$wd == 1 | a$wd == 7 |
      # New years. No rolls
      (a$doy == 1 | (a$dom == 31 & a$m == 12)) |
      # St. Berchtold
      a$doy == 2 |
      # Maudy Thurs, Good Friday, Easter Monday
      (a$doy == a$em | a$doy == a$em - 3 | a$doy == a$em - 4) |
      # May day
      a$dom == 1 & a$m == 5 |
      # Ascension day
      a$doy == a$em + 38 |
      # Whit Mon
      a$doy == a$em + 49 |
      # National independence day
      a$dom == 1 & a$m == 8 |
      # Christmas Eve, Day, St. Stephen's day
      (a$dom >= 24 & a$dom <= 26) & a$m == 12)
}

#' @export
is_good.EUTACalendar <- function(dates, calendar) {
  assertthat::assert_that(all(lubridate::year(dates) > 1998))
  a <- extract_atoms(dates, calendar)
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
  !(a$wd == 1 | a$wd == 7 | # Weekends
      # NY
      a$doy == 1 |
      # Easter
      ((a$doy == a$em | a$doy == a$em - 3) & a$y >= 2000) |
      # Labour day
      a$dom == 1 & a$m == 5 & a$y >= 2000 |
      # CHristmas
      (a$dom == 25 | a$dom == 26) & a$m == 12 |
      # Prudential day
      (a$dom == 31 & a$m == 12 & (a$y == 1999 | a$y == 2001)))
}

#' @export
is_good.GBLOCalendar <- function(dates, calendar) {
  a <- extract_atoms(dates, calendar)
  # http://en.wikipedia.org/wiki/Public_holidays_in_the_United_Kingdom
  # http://en.wikipedia.org/wiki/Bank_holiday [2002, 2012 spring bank hol moved
  # to 4 Jun for Queen's jubilee]
  # http://www.timeanddate.com/holidays/uk/spring-bank-holiday#obs
  # http://www.legislation.gov.uk/ukpga/1971/80
  !(a$wd == 1 | a$wd == 7 | # Weekend
      # NY
      (a$dom == 1 | ((a$dom == 2 | a$dom == 3) & a$dow == 'Mon')) & a$m == 1 |
      # Easter
      a$doy == a$em | a$doy == a$em - 3 |
      #### Bank day
      # May Day bank holiday. First Mon of May. 2002/2012 spring hol moved 4 Jun.
      ((a$dom <= 7 & a$m == 5 & a$dow == 'Mon' & a$y >= 1978 &
          (a$y != 2002 | a$y != 2012)) |
          (a$dom == 4 & a$m == 6 & (a$y == 2002 | a$y == 2012)) |
          # Spring bank hol. Last Mon of May (excl. 2002, 2012)
          (a$dom > 24 & a$m == 5 & a$dow == 'Mon' &
              (a$y >= 1971 & a$y != 2002 & a$y != 2012)) |
          # Spring bank holiday pushed back to 4 June for Queen's Golden and Diamond
          # Jubilee
          (a$dom == 4 & a$m == 6 & (a$y == 2002 | a$y == 2012)) |
          # Late summer bank hol. Last Mon of Aug.
          (a$dom > 24 & a$m == 8 & a$dow == 'Mon' & a$y >= 1971)) |
      ####
      # Queen's Jubilee
      a$dom == 5 & a$m == 6 & a$y == 2012 |
      # Christmas
      a$dom == 25 & a$m == 12 |
      # Boxing Day. 26th December, if not a Sun.
      # 27th December in a year in which 25th or 26th December is a Sunday
      ((a$dom == 26 | (a$dom == 27 & (a$dow == 'Mon' | a$dow == 'Tues'))) &
          a$m == 12) |
      # Royal Wedding
      a$dom == 29 & a$m == 4 & a$y == 2011)
}

#' @export
is_good.HKHKCalendar <- function(dates, calendar) {
  # http://www.gov.hk/en/about/abouthk/holiday/
  # https://en.wikipedia.org/w/index.php?title=Public_holidays_in_Hong_Kong&oldid=703958274
  a <- extract_atoms(dates, calendar)
  cny <- lubridate::yday(chinese_new_year(lubridate::year(dates)))
  start_4th_lunar_month <- next_moon_phase(ISOdate(a$y, 4, 25, 0), "new",
    "Asia/Shanghai", FALSE)
  buddhas_bd <- start_4th_lunar_month + lubridate::days(7)
  start_5th_lunar_month <- next_moon_phase(ISOdate(a$y, 5, 25, 0), "new",
    "Asia/Shanghai", FALSE)
  dragon_boat_day <- start_5th_lunar_month + lubridate::days(4)
  start_8th_lunar_month <- next_moon_phase(ISOdate(a$y, 8, 25, 0), "new",
    "Asia/Shanghai", FALSE)
  mid_autumnal_day <- start_8th_lunar_month + lubridate::days(15)
  start_9th_lunar_month <- next_moon_phase(ISOdate(a$y, 9, 25, 0), "new",
    "Asia/Shanghai", FALSE)
  chung_yeung <- start_9th_lunar_month + lubridate::days(8)
  !(a$wd == 1 | a$wd == 7 | # Weekend
      # NY
      a$m == 1 & (a$dom == 1 | (a$dom == 2 & a$dow == "Mon")) |
      # Chinese lunar new year
      a$doy %in% cny |
      a$doy %in% (cny + 1) |
      a$doy %in% (cny + 2) |
      # Ching Ming
      a$doy == lubridate::yday(a$ve) + 15 |
      # Easter
      a$doy == a$em | a$doy == a$em - 3 |
      # Labour day
      # Only roll if holiday falls on Sunday (not Saturday)
      a$m == 5 & (a$dom == 1 | ((a$dom == 2) & a$dow == "Mon")) |
      # Buddha day
      (a$m == lubridate::month(buddhas_bd) &
          (a$dom == lubridate::mday(buddhas_bd) |
              (a$dom == lubridate::mday(buddhas_bd) + 1 & a$dow == "Mon"))) |
      # Dragon boat day
      (a$m == lubridate::month(dragon_boat_day) &
          (a$dom == lubridate::mday(dragon_boat_day) |
              (a$dom == lubridate::mday(dragon_boat_day) + 1 & a$dow == "Mon"))) |
      # Establishment day
      a$m == 7 & (a$dom == 1 | (a$dom == 2 & a$dow == "Mon")) |
      # Mid autumnal day
      (a$m == lubridate::month(mid_autumnal_day) &
          (a$dom == lubridate::mday(mid_autumnal_day) |
              (a$dom == lubridate::mday(mid_autumnal_day) + 1 & a$dow == "Mon"))) |
      # National day
      a$m == 10 & (a$dom == 1 | (a$dom == 2 & a$dow == "Mon")) |
      # Chung Yeung Festival
      (a$m == lubridate::month(chung_yeung) &
          (a$dom == lubridate::mday(chung_yeung) |
              (a$dom == lubridate::mday(chung_yeung) + 1 & a$dow == "Mon")))|
      # Christmas
      ((a$dom == 25 | (a$dom == 27 & (a$dow == 'Mon' | a$dow == 'Tues'))) &
          a$m == 12) |
      # Boxing day
      ((a$dom == 26 | (a$dom == 28 & (a$dow == 'Mon'| a$dow == 'Tues'))) &
          a$m == 12))
}

#' @export
is_good.JPTOCalendar <- function(dates, calendar) {
  # http://en.wikipedia.org/wiki/Public_holidays_in_Japan
  a <- extract_atoms(dates, calendar)
  a$ve <- lubridate::yday(lubridate::with_tz(a$ve, tz(calendar)))
  a$ae <- lubridate::yday(lubridate::with_tz(a$ae, tz(calendar)))
  !(a$wd == 1 | a$wd == 7 | # Weekend
      # New Years. Plus two days following are bank holidays
      (a$dom == 1 | a$dom == 2 | a$dom == 3) & a$m == 1 |
      # Coming of Age Day. 2nd Mon of Jan
      # Happy Monday starts
      (((a$dom > 7 & a$dom <= 14) & a$m == 1 & a$y >= 2000 & a$dow == 'Mon') |
          # Before start of Happy Monday
          ((a$dom == 15 | (a$dom == 16 & a$dow == 'Mon')) &
              a$m == 1 & a$y < 2000)) |
      # Foundation day
      (a$dom == 11 | (a$dom == 12 & a$dow == 'Mon')) & a$m == 2 |
      # Vernal Equinox Day (spring)
      ((a$doy == a$ve | (a$doy == a$ve + 1 & a$dow == 'Mon')) & a$m == 3) |
      # Autumnal equinox day
      ((a$doy == a$ae | (a$doy == a$ae + 1 & a$dow == 'Mon')) & a$m == 9) |
      # Showa day
      (a$dom == 29 | (a$dom == 30 & a$dow == 'Mon')) & a$m == 4 |
      # May days
      # Constitution day, Greenery day, Children's day
      (((a$dom == 3 | a$dom == 4 | a$dom == 5) & a$m == 5) |
          (a$dom == 6 & (a$dow == 'Mon' | a$dow == 'Tues' |
              a$dow == 'Wed') & a$m == 5)) |
      # Marine day
      (((a$dom > 14 & a$dom <= 21) & a$dow == 'Mon' & a$m == 7 & a$y >= 2003) |
          ((a$dom == 20 | (a$dom == 21 & a$dow == 'Mon')) & a$m == 7 & a$y < 2003)) |
      # Respect day
      (((a$dom > 14 & a$dom <= 21) & a$dow == 'Mon' & a$m == 9 & a$y >= 2003) |
          ((a$dom == 15 | (a$dom == 16 & a$dow == 'Mon')) & a$m == 9 & a$y < 2003)) |
      # Citizens day
      (a$dom + 1 == a$ae & a$dom > 15 & a$dom <= 22 & a$dow == 'Tues' &
          a$m == 9 & a$y >= 2003) |
      # Health day
      (((a$dom > 7 & a$dom <= 14) & a$dow == 'Mon' & a$m == 10 & a$y >= 2000) |
          ((a$dom == 10 | (a$dom == 11 & a$dow == 'Mon')) &
              a$m == 10 & a$y < 2000)) |
      # Culture day
      (a$dom == 3 | (a$dom == 4 & a$dow == 'Mon')) & a$m == 11 |
      # Labour day
      (a$dom == 23 | (a$dom == 24 & a$dow == 'Mon')) & a$m == 11 |
      # Emperors day
      (a$dom == 23 | (a$dom == 24 & a$dow == 'Mon')) & a$m == 12 |
      # Bank day
      # http://www.boj.or.jp/en/about/outline/holi.htm/
      a$dom == 31 & a$m == 12)
}

#' @export
is_good.NOOSCalendar <- function(dates, calendar) {
  a <- extract_atoms(dates, calendar)
  !(a$wd == 1 | a$wd == 7 | # Weekends
      # New years.
      a$doy == 1 |
      # Maudy Thurs, Good Friday, Easter Monday
      (a$doy == a$em | a$doy == a$em - 3 | a$doy == a$em - 4) |
      # May day
      a$dom == 1 & a$m == 5 |
      # Constitution day
      a$dom == 17 & a$m == 5 |
      # Ascension day
      a$doy == a$em + 38 |
      # Pentecost
      a$doy == a$em + 48 |
      # Whit Mon
      a$doy == a$em + 49 |
      # Christmas Day, St. Stephen's day
      (a$dom == 25 | a$dom == 26) & a$m == 12)
}

#' @export
is_good.NZAUCalendar <- function(dates, calendar) {
  a <- extract_atoms(dates, calendar)
  !(a$wd == 1 | a$wd == 7 | # Weekends
      # NY
      ((((a$dom == 1 | (a$dom == 3 & (a$dow == 'Mon' | a$dow == 'Tues')))) |
          ((a$dom == 2 | (a$dom == 4 & (a$dow == 'Mon' | a$dow == 'Tues'))))) &
          a$m == 1)   |
      # Waitangi day
      ((a$dom == 6 | ((a$dom == 7 | a$dom == 8) & a$dow == 'Mon' & a$y > 2013)) &
          a$m == 2) |
      # ANZAC day
      ((a$dom == 25 | ((a$dom == 26 | a$dom == 27) & a$dow == 'Mon' & a$y > 2013))
        & a$m == 4) |
      # Queens birthday
      a$dom <= 7 & a$dow == 'Mon' & a$m == 6 |
      # Labour day
      a$dom > 21 & a$dom <= 28 & a$dow == 'Mon' & a$m == 10 |
      # Christmas
      ((a$dom == 25 | (a$dom == 27 & (a$dow == 'Mon' | a$dow == 'Tues'))) &
          a$m == 12) |
      # Boxing day
      ((a$dom == 26 | (a$dom == 28 & (a$dow == 'Mon'| a$dow == 'Tues'))) &
          a$m == 12) |
      # Easter
      a$doy == a$em | a$doy == a$em - 3 |
      # Auckland day
      ((a$dom >= 26 & a$m == 1) | (a$dom <= 1 & a$m == 2)) & a$dow == 'Mon')
}

#' @export
is_good.NZWECalendar <- function(dates, calendar) {
  a <- extract_atoms(dates, calendar)
  !(a$wd == 1 | a$wd == 7 | # Weekends
      # NY
      ((((a$dom == 1 | (a$dom == 3 & (a$dow == 'Mon' | a$dow == 'Tues')))) |
          ((a$dom == 2 | (a$dom == 4 & (a$dow == 'Mon' | a$dow == 'Tues'))))) &
          a$m == 1)   |
      # Waitangi day
      ((a$dom == 6 | ((a$dom == 7 | a$dom == 8) & a$dow == 'Mon' & a$y > 2013)) &
          a$m == 2) |
      # ANZAC day
      ((a$dom == 25 | ((a$dom == 26 | a$dom == 27) & a$dow == 'Mon' & a$y > 2013))
        & a$m == 4) |
      # Queens birthday
      a$dom <= 7 & a$dow == 'Mon' & a$m == 6 |
      # Labour day
      a$dom > 21 & a$dom <= 28 & a$dow == 'Mon' & a$m == 10 |
      # Christmas
      ((a$dom == 25 | (a$dom == 27 & (a$dow == 'Mon' | a$dow == 'Tues'))) &
          a$m == 12) |
      # Boxing day
      ((a$dom == 26 | (a$dom == 28 & (a$dow == 'Mon'| a$dow == 'Tues'))) &
          a$m == 12) |
      # Easter
      a$doy == a$em | a$doy == a$em - 3 |
      # Wellington day
      (a$dom >= 19 & a$dom <= 25) & a$dow == 'Mon' & a$m == 1)
}


#' @export
is_good.USNYCalendar <- function(dates, calendar) {
  a <- extract_atoms(dates, calendar)
  # http://en.wikipedia.org/wiki/New_York_State_government_holidays
  !(a$wd == 1 | a$wd == 7 | # Weekends
      # New Years. Sub Mon if on Sunday, and Fri if on Saturday
      ((a$dom == 1 & a$m == 1) | (a$dom == 31 & a$m == 12 & a$dow == 'Fri') |
          (a$dom == 2 & a$m == 1 & a$dow == 'Mon'))|
      # MLK day, Washington's birthday. 3rd Mon of Jan, Feb (resp.)
      a$dom > 14 & a$dom <= 21 & a$dow == 'Mon' & (a$m == 1 | a$m == 2) |
      # Memorial day. Last Mon of May
      a$dom > 24 & a$dow == 'Mon' & a$m == 5 |
      # Independence Day. 4 Jul.  Sub Mon if on Sunday, and Fri if on Saturday
      ((a$dom == 4 | (a$dom == 5 & a$dow == 'Mon') |
          (a$dom == 3 & a$dom == 'Fri')) & a$m == 7) |
      # Labour day. 1st Mon of Sep.
      a$dom <= 7 & a$dow == 'Mon' & a$m == 9 |
      # Columbus day. 2nd Mon of Oct.
      a$dom > 7 & a$dom <= 14 & a$dow == 'Mon' & a$m == 10 |
      # Veteran's day. 11 Nov.  Sub Mon if on Sunday, and Fri if on Saturday
      ((a$dom == 11 | (a$dom == 12 & a$dow == 'Mon') |
          (a$dom == 10 & a$dow == 'Fri')) & a$m == 11 )|
      # Thanksgiving. 4th Thurs of Nov
      a$dom > 21 & a$dom <= 28 & a$dow == 'Thurs' & a$m == 11 |
      # Christmas. Sub Mon if on Sunday, and Fri if on Saturday
      ((a$dom == 25 | (a$dom == 26 & a$dow == 'Mon') |
          (a$dom == 24 & a$dow == 'Fri')) & a$m == 12))
}

#' @export
is_good.JointCalendar <- function(dates, calendar) {
  m <- NROW(dates)
  n <- NROW(calendar$calendars)
  res <- matrix(nrow = m, ncol = n)
  for (i in 1:n){
    res[, i] = is_good(dates, calendar$calendars[[i]])
  }
  apply(res, 1, Reduce, f = calendar$rule)
}

#' Extract time zone from calendars
#'
#' @param x an instance of a [`Calendar`] or [`JointCalendar`] object
#' @return a string representing the time zone (e.g. "Australia/Sydney") or
#' vector of time zones in the case of joint calendars
#' @examples
#' lubridate::tz(AUSYCalendar())
#' lubridate::tz(c(AUSYCalendar(), AUMECalendar()))
#' @importFrom lubridate tz
#' @export
#' @family calendar methods
#' @name tz
tz.Calendar <- function(x) {
  x$tz
}

#' @rdname tz
#' @importFrom lubridate tz
#' @export
tz.JointCalendar <- function(x) {
  x$tzs
}

#' @export
locale.default <- function(x, ...) {
  x$locale
}

#' @export
locale.JointCalendar <- function(x, ...) {
  x$locales
}

#' @export
length.Calendar <- function(x) 1L
#' @export
length.JointCalendar <- function(x) length(x$calendars)

#' @export
c.Calendar <- function (..., recursive = FALSE) {
  calendars <- list(...)
  clengths <- sum(vapply(calendars, length, integer(1)))
  res <- vector("list", clengths)
  j <- 1
  for(i in seq_along(calendars)) {
    is_jc <- is(calendars[[i]], "JointCalendar")
    if (is_jc) {
      res[j:(j + length(calendars[[i]]) - 1)] <- calendars[[i]]$calendars
      j <- j + length(calendars[[i]])
    } else {
      res[[j]] <- calendars[[i]]
      j <- j + 1
    }
  }
  # Default join rule: all
  JointCalendar(res, all)
}

#' @export
c.JointCalendar <- function(..., recursive = FALSE) {
  calendars <- list(...)
  clengths <- sum(vapply(calendars, length, integer(1)))
  res <- vector("list", clengths)
  j <- 1
  for(i in seq_along(calendars)) {
    is_jc <- is(calendars[[i]], "JointCalendar")
    if (is_jc) {
      res[j:(j + length(calendars[[i]]) - 1)] <- calendars[[i]]$calendars
      j <- j + length(calendars[[i]])
    } else {
      res[[j]] <- calendars[[i]]
      j <- j + 1
    }
  }
  # Default join rule: all
  JointCalendar(res, all)
}

#' @export
`[.JointCalendar` <- function (x, i) {
  JointCalendar(x$calendars[i], x$rule)
}

#' @export
format.Calendar <- function(x, ...) {
  paste0("<", x$locale, "> TZ: ", x$tz)
}

#' @export
format.JointCalendar <- function(x, ...) {
  rule <- if (identical(x$rule, all)) "all" else "any"
  paste0("<JointCalendar> ", paste0(x$locales, collapse=", "), "\n",
    "   TZ: ", paste0(x$tzs, collapse = ", "), "\n",
    "   Join rule: ", rule)
}

#' @export
print.Calendar <- function(x, ...) {
  cat(format(x, ...), "\n")
  invisible(x)
}

#' @export
print.JointCalendar <- function(x, ...) {
  cat(format(x, ...) , "\n")
  invisible(x)
}

#' Calendar class checkers
#'
#' @param x object to be tested
#' @return `TRUE` if `x` inherits from `Calendar` or `JointCalendar`
#'   (`is.Calendar` and `is.JointCalendar` respectively) and `FALSE` otherwise.
#' @name is
#' @family calendar methods
#' @export
is.Calendar <- function(x) inherits(x, "Calendar")

#' @export
#' @rdname is
is.JointCalendar <- function(x) inherits(x, "JointCalendar")


# Helpers ------------------------------------------------------------------

extract_atoms <- function (dates, calendar) {
  list(
    dow = lubridate::wday(dates, TRUE, TRUE),
    dom = lubridate::mday(dates),
    doy = lubridate::yday(dates),
    y   = lubridate::year(dates),
    m   = lubridate::month(dates),
    wd  = lubridate::wday(dates),
    em  = easter_monday(lubridate::year(dates)),
    ve  = equinox(lubridate::year(dates), "mar", tz(calendar)),
    ae  = equinox(lubridate::year(dates), "sep", tz(calendar)))
}

#' Business day conventions
#'
#' A non-exported function for checking whether business day conventions
#' are valid.
#'
#' The supported day conventions are:
#' \itemize{
#'  \item u - unadjusted. No adjustments made to a date.
#'  \item f - following. The date is adjusted to the following business day.
#'  \item mf - modified following. As per following convention. However,
#'  if the following business day is in the month following the date, then the
#'  date is adjusted to the preceding business day.
#'  \item p - preceding. The date is adjusted to the preceding business day.
#'  \item mp - modified preceding. As per preceding convention. However, if
#'  the preceding business day is in the month prior to the date, then the
#'  date is adjusted to the following business day.
#'  \item ms - modified succeeding. This convention applies to Australian
#'  bank bills. Australian bank bills' maturities defined as either early
#'  (prior to the 15th) or late month (after the 15th). If the maturity date
#'  calculated straight from a bill's term crosses either the end of the month
#'  or the 15th of the month, the bill's maturity is adjusted to the preceding
#'  business day.
#' }
#'
#' @param bdc a character vector
#' @return a flag (\code{TRUE} or \code{FALSE}) if all the supplied business
#' day conventions are supported.
#' @aliases businessdayconventions
#' @family calendar methods
#' @keywords internal

is_valid_bdc <- function (bdc) {
  all(bdc %in% c('u', 'f', 'mf', 'p', 'mp', 'ms'))
}

