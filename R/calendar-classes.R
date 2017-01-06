#' Build a calendar
#'
#' Calendars are necessary for two reasons: they define whether a calendar day
#' is a good business day in a given locale and they are used to store the time
#' zone for the locale. Calendars can correspond to a single locale (usually a
#' city). These inherit from the `Calendar` class. The package implements a
#' number of calendars for key financial market locales such as
#' `AUSYCalendar`, `USNYCalendar` and `EUTACalendar` (TARGET). You can
#' also define a joint locale using [JointCalendar()].
#'
#' @param locale a four letter string representing an abbreviation of the
#'   locale. The package uses locale representations loosely based on
#'   [UN/LOCODE](http://www.unece.org/cefact/locode/welcome.html) (e.g.
#'   Australia/Sydney is represented by `AUSY` rather than `AU/SYD` per the
#'   LOCODE specification). The locale is used as a prefix to the calendar's
#'   S3 class in the following manner: `<locale>Calendar` (e.g. `AUSYCalendar`).
#' @param tz the time zone associated with the given `locale` using
#'   [OlsonNames()] (e.g. `Australia/Sydney`)
#' @return `Calendar()` returns a function that constructs an object inheriting
#'   from the `Calendar` class. The calendar constructors provided by the
#'   package returns an object that inherits from `Calendar`.
#' @examples
#' Calendar(NA, NA) # Defined: EmptyCalendar()
#' Calendar("AUSY", "Australia/Sydney") # Defined: AUSYCalendar()
#' @export
#' @family calendar classes

Calendar <- function(locale, tz) {
  assertthat::assert_that(assertthat::is.string(locale) || is.na(locale),
    assertthat::is.string(tz) || is.na(tz))
  function () {
    structure(list(locale = locale, tz = tz),
      class = c(paste0(locale, "Calendar"), "Calendar"))
  }
}

#' @rdname Calendar
#' @export
EmptyCalendar <- Calendar(NA, NA)
#' @rdname Calendar
#' @export
AUSYCalendar <- Calendar("AUSY", "Australia/Sydney")
#' @rdname Calendar
#' @export
AUMECalendar <- Calendar("AUME", "Australia/Melbourne")
#' @rdname Calendar
#' @export
CHZHCalendar <- Calendar("CHZH", "Europe/Zurich")
#' @rdname Calendar
#' @export
EUTACalendar <- Calendar("EUTA", "Europe/Brussels")
#' @rdname Calendar
#' @export
GBLOCalendar <- Calendar("GBLO", "Europe/London")
#' @rdname Calendar
#' @export
HKHKCalendar <- Calendar("HKHK", "Asia/Hong_Kong")
#' @rdname Calendar
#' @export
JPTOCalendar <- Calendar("JPTO", "Asia/Tokyo")
#' @rdname Calendar
#' @export
NOOSCalendar <- Calendar("NOOS", "Europe/Oslo")
#' @rdname Calendar
#' @export
NZAUCalendar <- Calendar("NZAU", "Pacific/Auckland")
#' @rdname Calendar
#' @export
NZWECalendar <- Calendar("NZWE", "Pacific/Auckland")
#' @rdname Calendar
#' @export
USNYCalendar <- Calendar("USNY", "America/New_York")


#' Joint calendars
#'
#' Sometimes the calendar governing a financial contract is defined by multiple
#' single locales. These joint calendars are represented by the `JointCalendar`
#' class.
#'
#'
#' @param calendars a list of at least one `Calendar()` objects
#' @param rule either `all` or `any` corresponding to a date being good if
#' it is good in all or any of the calendars supplied.
#' @return an object of class `JointCalendar` when using `JointCalendar()`
#' @examples
#' JointCalendar(list(AUSYCalendar(), AUMECalendar()), all)
#' JointCalendar(list(AUSYCalendar(), AUMECalendar()), any)
#' @export
#' @family calendar classes
JointCalendar <- function(calendars, rule = all) {
  assertthat::assert_that(is_list_of(calendars, "Calendar"))
  locales <- vapply(calendars, locale, "character")
  is_duplicated <- duplicated(locales)
  locales <- locales[!is_duplicated]
  tzs <- vapply(calendars, tz, "character")[!is_duplicated]
  calendars <- calendars[!is_duplicated]
  structure(
    list(
      locales = locales,
      tzs = tzs,
      calendars = calendars,
      rule = rule
    ),
    class = "JointCalendar"
  )
}