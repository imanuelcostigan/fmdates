#' Adjust to good dates
#'
#' One common financial markets date arithmetic requires a date needs to be
#' rolled to the closest business day following some convention (see
#' [is_valid_bdc()] for further details). Such rolled dates can be determined by
#' calling `adjust()`.
#'
#' @param dates a vector of dates to adjust.
#' @param bdc the business day convention used to roll the `dates` if necessary
#' @param calendar an object that inherits from [`Calendar`] or [`JointCalendar`]
#' which is used to determine the goodness of `dates`
#' @return a vector of adjusted dates - good days are unadjusted
#' @examples
#' ausy <- AUSYCalendar()
#' adjust(lubridate::ymd("20120102"), "u", ausy)
#' adjust(lubridate::ymd("20120102"), "f", ausy)
#' adjust(lubridate::ymd("20120102"), "mf", ausy)
#' adjust(lubridate::ymd("20120102"), "p", ausy)
#' adjust(lubridate::ymd("20120102"), "mp", ausy)
#' adjust(lubridate::ymd("20120102"), "ms", ausy)
#' @export
#' @family calendar methods

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

#' Shifting dates to good dates
#'
#' The [adjust()] function rolls dates to the closest good dates. This function
#' shifts dates by a given [period][lubridate::period()] and adjusting the
#' resulting dates to a closest good dates following the given business day
#' convention.
#'
#' @param dates a vector of dates to shift and adjust
#' @param period an atomic instance of the [period
#'   class][lubridate::Period-class] in the sense that only one of its slots
#'   should be non-zero. It must also only be a day, month or year period type.
#' @param bdc the business day convention used to roll the `dates` if necessary
#'   (default: "u" - unadjusted)
#' @param calendar an object that inherits from [`Calendar`] or
#'   [`JointCalendar`] which is used to determine the goodness of `dates`
#'   (default: `EmptyCalendar()`)
#' @param eom_rule if one of the `dates` is the last business day of the month,
#'   is being shifted by a month or year `period` and `eom_rule` is `TRUE` then
#'   the shifted date is also the last business day of the month
#'   (default: `TRUE`)
#' @return a vector of shifted dates
#' @examples
#' library(lubridate)
#' ausy <- AUSYCalendar()
#' shift(ymd("20120229"), months(1), "u", ausy, FALSE)
#' shift(ymd("20120229"), months(1), "u", ausy, TRUE)
#' @export
#' @family calendar methods

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

