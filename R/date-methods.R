#' Checks whether dates are last day of month
#'
#' This checks whether the \code{dates} provided are the last day of a
#' month.
#'
#' @param dates a vector of dates.
#' @return a logical vector
#' @examples
#' library("lubridate")
#' is_eom(ymd(20110228)) # TRUE
#' is_eom(ymd(20120229)) # TRUE
#' @export

is_eom <- function (dates) {
  unname(lubridate::days_in_month(dates) == lubridate::mday(dates))
}

#' The end of month date
#'
#' The \code{dates} are rounded to the end of their respective months.
#'
#' @param dates a vector of dates.
#' @return a date vector with the same class as \code{dates}
#' @examples
#' library("lubridate")
#' eom(ymd(20120203, 20140203))
#' @export

eom <- function (dates) {
  lubridate::mday(dates) <- lubridate::days_in_month(dates)
  dates
}

#' \code{n^{th} n-day}
#'
#' Determines the \code{n^{th}} n-day after \code{dates}. You might want to
#' use this to determine the expiry date of futures which tend to be defined as
#' the second Friday of the expiry month. So in that case, \code{nth} is
#' \code{2} and \code{nday} is \code{6} (as Sunday has a weekday number of 1).
#'
#' @param dates a vector of dates.
#' @param nth a numeric (possibly vector) value
#' @param nday a numeric (possibly vector) value representing the weekday where
#'   Sunday is represented by the numeric value \code{1}. Case-insensitive
#'   abbreviated three letter string representations of the day will be
#'   transformed to such numeric values.
#' @return a date vector with the same class as \code{dates}
#' @examples
#' library("lubridate")
#' nth_nday(ymd("20120601"), 2, 6) # 2Fri, "2012-06-08"
#' nth_nday(ymd("20120901"), 3, 4) # 3Wed, "2012-09-19"
#' nth_nday(ymd("20121201"), 1, 2) # 1Mon, "2012-12-03"
#' nth_nday(ymd("20121215"), 4, 5) # 4Thu, "2013-01-10"
#' nth_nday(ymd("20121215"), 4, "thu") # 4Thu, "2013-01-10"
#' @export

nth_nday <- function (dates, nth, nday) {
  if (is.character(nday)) nday <- to_wday(tolower(nday))
  dates + lubridate::days((nth - 1) * 7 + (nday - lubridate::wday(dates)) %% 7)
}

to_wday <- function (wday) {
  weekdays <- stats::setNames(1:7,
    c("sun", "mon", "tue", "wed", "thu", "fri", "sat"))
  weekdays[wday]
}

quarter_end <- function (dates) {
  QE_MONTHS <- rep(c(3, 6, 9, 12), each = 3) %>% stats::setNames(1:12)
  qe_months <- QE_MONTHS[lubridate::month(dates)] %>% as.numeric()
  lubridate::month(dates) <- qe_months
  paste(lubridate::year(dates), qe_months, "01", sep = "-") %>%
    as.Date() %>%
    eom()
}

futures_settlement <- function (date, serial, nth, nday, offset = 0,
  post_offset = NULL, post_offset_calendar = NULL) {
  assertthat::assert_that(
    assertthat::is.date(date),
    assertthat::is.count(serial),
    offset == 0 || assertthat::is.count(offset), offset <= 28,
    assertthat::is.scalar(nth), assertthat::is.scalar(nday),
    !xor(is.null(post_offset), is.null(post_offset_calendar)),
    !xor(!is.null(post_offset), lubridate::is.period(post_offset)),
    !xor(!is.null(post_offset_calendar), is(post_offset_calendar, "Calendar"))
  )
  s1_start <- (date %>% quarter_end() %>% lubridate::floor_date("month")) +
    lubridate::days(offset)
  guess_first_settlement <- nth_nday(s1_start, nth, nday)
  if (!is.null(post_offset)) {
    # Usually post_offset is "2b" so "f" adjuster and not EOM is probably correct
    guess_first_settlement <-
      post_offset_calendar$shift(guess_first_settlement, post_offset, "f", FALSE)
  }
  if (guess_first_settlement <= date) {
    s1_start <- s1_start + months(3)
  }
  ref_date <- s1_start + months(3) * (serial - 1)
  res <- nth_nday(ref_date, nth, nday)
  if (!is.null(post_offset)) {
    res <- post_offset_calendar$shift(res, post_offset, "f", FALSE)
  }
  res
}

#' Roll date to mday
#'
#' This function rolls dates to a given date in the month. If the mday is
#' before the dates provided, then the dates are roll backwards - and vice
#' versa. If one of the \code{dates} is the last day of the month, then the
#' date is left unrolled. Namely, the end of month barrier is not breached.
#'
#' @param dates a vector of dates.
#' @param mday a numeric vector representing the day of the month \code{dates}
#' should be rolled to.
#' @return a date vector with the same class as \code{dates}
#' @examples
#' library("lubridate")
#' roll_date(ymd("2012-02-12"), 15)  # "2012-02-15 UTC"
#' roll_date(ymd("2012-02-12"), 29)  # "2012-02-29 UTC"
#' roll_date(ymd("2013-02-12"), 29)  # "2011-02-28 UTC"
#' @export

roll_date <- function (dates, mday) {
  last_day <- lubridate::days_in_month(dates)
  lubridate::mday(dates) <- mday * (mday <= last_day) +
    last_day * (mday > last_day)
  dates
}

# Dealing with bug in lubridate
# See https://github.com/hadley/lubridate/pull/157
c.POSIXct <- function(..., recursive = FALSE)
  .POSIXct(c(unlist(lapply(list(...), unclass))),
    tz = lubridate::tz(list(...)[[1]]))



# Period checkers --------------------------------------------------------
# See also yyy.R for assertions.


is_atomic_period <- function (period) {
  assertthat::assert_that(is(period, "Period"))
  sum(Map(methods::slot, period, methods::slotNames(period)) != 0) == 1
}

period_type <- function (period) {
  assertthat::assert_that(is_atomic_period(period))
  Find(function (x) methods::slot(period, x) != 0, methods::slotNames(period))
}

period_length <- function (period) {
  methods::slot(period, period_type(period))
}

