Calendar <- function(locale, tz) {
  function () {
    structure(list(locale = locale, tz = tz),
      class = c(paste0(locale, "Calendar"), "Calendar"))
  }
}

EmptyCalendar <- Calendar(NA, NA)
AUSYCalendar <- Calendar("AUSY", "Australia/Sydney")
AUMECalendar <- Calendar("AUME", "Australia/Melbourne")
CHZHCalendar <- Calendar("CHZH", "Europe/Zurich")
EUTACalendar <- Calendar("EUTA", "Europe/Brussels")
GBLOCalendar <- Calendar("GBLO", "Europe/London")
HKHKCalendar <- Calendar("HKHK", "Asia/Hong_Kong")
JPTOCalendar <- Calendar("JPTO", "Asia/Tokyo")
NOOSCalendar <- Calendar("NOOS", "Europe/Oslo")
NZAUCalendar <- Calendar("NZAU", "Pacific/Auckland")
NZWECalendar <- Calendar("NZWE", "Pacific/Auckland")
USNYCalendar <- Calendar("USNY", "America/New_York")


JointCalendar <- function(calendars, rule = all) {
  assertthat::assert_that(is_list_of(calendars, "Calendar"))
  locales <- vapply(calendars, locale, "character")
  is_duplicated <- duplicated(locales)
  locales <- locales[!is_duplicated]
  tzs <- vapply(calendars, tz, "character")[!is_duplicated]
  calendars <- calendars[!is_duplicated]
  structure(
    list(locales = locales, tzs = tzs, calendars = calendars, rule = rule),
    class = "JointCalendar"
  )
}