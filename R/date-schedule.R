#' Generate a date schedule
#'
#' Generate a date schedule from \code{effective_date} to
#' \code{termination_date}. This code was derived from the Quantlib method
#' Schedule::Schedule. This can be used to generate the cash flow, fixing and
#' projection dates of an interest rate swap according to certain conventions.
#'
#' @param effective_date the date at which the schedule begins. For example, the
#'   effective date of a swap. This should be \code{\link{POSIXct}}.
#' @param termination_date the date at which the schedule ends. For example, the
#'   termination date of a swap. This should be \code{\link{POSIXct}}.
#' @param tenor the frequency of the events for which dates are generated. For
#'   example, \code{month(3)} reflects events that occur quarterly. Should be an
#'   atomic \code{\link{Period-class}} of length one
#' @param first_date date of first payment for example. This defaults to
#'   \code{effective_date} as is usually the case
#' @param last_date date of last payment for example. This defaults to
#'   \code{termination_date} as is usually the case
#' @param calendar a \code{\link{Calendar}}
#' @param bdc a string representing one of the following business day
#'   conventions: "u", "f", "mf", "p", "mp", "ms" (unadjusted, following,
#'   modified following, preceding, modified preceding and modified succeeding,
#'   resp.)
#' @param stub a string representing one of the following stub types:
#'   "short_front", "short_back", "long_front", "long_back".
#' @param eom_rule a logical value defining whether the end-to-end convention
#'   applies.
#' @return an \code{Interval} vector
#' @examples
#' library (lubridate)
#' effective_date <- ymd('20120103')
#' termination_date <- ymd('20121203')
#' tenor <- months(3)
#' stub <- 'short_front'
#' bdc <- 'mf'
#' calendar <- AUSYCalendar()
#' eom_rule <- FALSE
#' generate_schedule(effective_date, termination_date, tenor, calendar,
#'  bdc, stub, eom_rule)
#' @export
#' @family calendar methods

generate_schedule <- function (effective_date, termination_date, tenor,
  calendar = EmptyCalendar(), bdc = "u", stub = "short_front", eom_rule = FALSE,
  first_date = effective_date, last_date = termination_date)
{
  # Input checking
  assertthat::assert_that(effective_date <= first_date,
    first_date < last_date,
    last_date <= termination_date,
    is_valid_stub(stub), is_atomic_period(tenor))

  # Set things up
  yrs <- year_frac(first_date, last_date, "act/365")
  suppressMessages(pa <- trunc(lubridate::years(1) / tenor))
  # Add some wiggle room
  n <- trunc(yrs * pa) + pa

  # Direction of schedule generation is backward if stub is front. Else forward
  sgn <- if (grepl("front$", stub)) -1 else +1

  # Build sequence of periods
  if (identical(sgn, -1)) {
    seed <- last_date
    period_number <- seq.int(sgn * n, 0)
  } else {
    seed <- first_date
    period_number <- seq.int(0, sgn * n)
  }

  # Generate unadjusted dates.
  if (any(period_type(tenor) %in% c("month", "year"))) {
    operator <- lubridate::`%m+%`
  } else {
    operator <- `+`
  }
  res <- operator(seed, period_number * tenor)

  # Only want unadjusted dates after the first_date which is greater than or
  # equal to the effective_date and those before the last date which is less
  # than or equal to the termination date.
  res <- res[(res >= first_date) & (res <= last_date)]

  # Add the effective / termination dates if not already there. This will
  # create a stub if that is the case. Note the assertion
  # ensures that effective_date <= first_date < last_date <= termination_date
  # so we can concatenate effective / termination dates as below.
  is_stub_created <- FALSE
  if (!(effective_date %in% res)) {
    is_stub_created <- TRUE
    # This needs the definition of c in date-methods.R for TZ to be preserved
    res <- c(effective_date, res)
  }
  if (!(termination_date %in% res)) {
    is_stub_created <- TRUE
    # This needs the definition of c in date-methods.R for TZ to be preserved
    res <- c(res, termination_date)
  }

  # EOM adjustments if necessary
  is_seed_eom <- identical(seed, adjust(eom(seed), "p", calendar))
  if (all(eom_rule, is_seed_eom)) {
    # Apply adjustments between first and last dates only (exclusive of both)
    is_eom_required <- (res > first_date) & (res < last_date)
    res[is_eom_required] <- adjust(eom(res[is_eom_required]), 'p', calendar)
  }

  # Adjust dates according to business day convention
  res <- adjust(res, bdc, calendar)

  # Check for stubs and remove relevant date when the stub is a long type.
  if (all(identical(stub, "long_back"),
    any(is_stub_created, !identical(last_date, termination_date)))) {
    res <- res[-(NROW(res) - 1)]
  }
  if (all(identical(stub, "long_front"),
    any(is_stub_created, !identical(first_date, effective_date)))) {
    res <- res[-2]
  }

  # Return!
  lubridate::interval(utils::head(res, -1), utils::tail(res, -1))
}

is_valid_stub <- function (stub) {
  all(stub %in% c("short_front", "short_back", "long_front", "long_back"))
}

