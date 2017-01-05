adjust <- function(dates, bdc, calendar) {
  assertthat::assert_that(is_valid_bdc(bdc), assertthat::is.scalar(bdc))
  if (identical(bdc, "u")) return (dates)

  # Setup
  is_preceding <- grepl("p$", bdc)
  direction <- +1 * (!is_preceding) - 1 * is_preceding
  is_modified <- grepl("^m", bdc)

  # Helper functions. Compacts the code below
  is_over_month_end <- function(d1, d2) {
    lubridate::month(d1) != lubridate::month(d2)
  }
  is_over_mid_month <- function(d1, d2) {
    is_early <- lubridate::mday(d1) <= 15
    is_early_and_over <- is_early & lubridate::mday(d2) > 15
    is_late_and_over <- !is_early & is_over_month_end(d1, d2)
    is_early_and_over | is_late_and_over
  }
  if (identical(bdc, "ms")) {
    is_over_barrier <- is_over_mid_month
  } else {
    is_over_barrier <- is_over_month_end
  }
  adjuster <- function(direction, is_over_barrier) {
    function(dates, is_modified) {
      dts <- dates
      to_adjust <- !is_good(dts, calendar)
      while (any(to_adjust)) {
        dts <- dts + to_adjust * lubridate::days(direction * 1)
        to_adjust <- !is_good(dts, calendar)
      }
      to_modify <- is_over_barrier(dates, dts)
      if (is_modified & any(to_modify)) {
        reverse_adjuster = adjuster(-direction, is_over_month_end)
        dts[to_modify] <- reverse_adjuster(dates[to_modify], FALSE)
      }
      dts
    }
  }

  # Adjustments
  fn <- adjuster(direction, is_over_barrier)
  fn(dates, is_modified)
}

shift <- function(dates, period, bdc = "u", calendar = EmptyCalendar(),
  eom_rule = TRUE) {
  # Period should have length one (but might have multiple period types
  # in this)
  assertthat::assert_that(assertthat::is.scalar(period))

  # Figure out which period types to loop
  ps <- methods::slotNames(period)
  # .shift only supports day, month and year slots
  ps_to_be_looped <- ps %in% c("day", "month", "year")
  is_non_zero_p <- Map(methods::slot, period,
    methods::slotNames(period)) != 0
  is_p_looped <- is_non_zero_p & ps_to_be_looped
  is_p_ignored<- is_non_zero_p & !ps_to_be_looped

  if (any(is_p_ignored)) {
    message("The following period values are not supported by shift and ",
      "are ignored: ", paste0(ps[is_p_ignored], collapse = ', '))
  }

  if (!any(is_p_looped)) {
    # Shift dates by 0 periods (i.e. adjust to good days and exit thanks)
    return(adjust(dates, bdc, calendar))
  } else {
    res <- dates
    for (p in ps[is_p_looped]) {
      recast_p <- lubridate::period(methods::slot(period, p), p)
      res <- shift_single(res, recast_p, bdc, calendar, eom_rule)
    }
  }
  return(res)
}

shift_single = function (dates, period, bdc, calendar, eom_rule) {
  # Day shift
  if (identical(period_type(period), "day")) {
    # Setup temp and counter variables
    i <- rep(0, NROW(dates))
    shift_length <- period_length(period)
    sgn <- sign(shift_length)

    # Step through days to get to shift_length good days for each date
    is_not_done <- abs(i) < abs(shift_length)
    while (any(is_not_done)) {
      dates[is_not_done] <- dates[is_not_done] + sgn * lubridate::days(1)
      to_roll <- is_good(dates, calendar)
      i[to_roll] <- i[to_roll] + sgn * 1
      is_not_done <- abs(i) < abs(shift_length)
    }
    return(dates)
  }
  # Month and year shifts are treated the same: one year = twelve months
  # Given assertions at the start, these should be the only options left
  res <- lubridate::`%m+%`(dates, period)
  if (!eom_rule) {
    return(adjust(res, bdc, calendar))
  } else {
    are_last_good_days <- identical(dates, adjust(eom(dates), "p", calendar))
    are_less_31_days <- lubridate::mday(dates) <= 30
    eom_applies <- are_last_good_days & are_less_31_days
    if (any(eom_applies))
      res[eom_applies] <- adjust(eom(res[eom_applies]), "p", calendar)
    return(res)
  }
}

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
  QE_MONTHS <- stats::setNames(rep(c(3, 6, 9, 12), each = 3), 1:12)
  qe_months <- as.numeric(QE_MONTHS[lubridate::month(dates)])
  lubridate::month(dates) <- qe_months
  eom(as.Date(paste(lubridate::year(dates), qe_months, "01", sep = "-")))
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
  s1_start <- lubridate::floor_date(quarter_end(date), "month") +
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

