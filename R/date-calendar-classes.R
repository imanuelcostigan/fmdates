#' @include date-methods.R
NULL

#' Calendar class (R6)
#'
#' A hierarchy of classes from which a number of methods are inherited or extended.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{is_good(dates)}}{Return a logical vector where good \code{dates} return
#'  \code{TRUE}, and vice-versa. Always returns \code{TRUE} for \code{Calendar}}
#'  \item{\code{adjust(dates, bdc)}}{Adjust vector of \code{dates} according to the
#'  business day convention \code{bdc}. Will throw an error if \code{bdc} is
#'  not one of "u", "f", "mf", "p", "mp" or "ms" and if it isn't a scalar.
#'  Otherwise, will return a vector of dates with the same class as \code{dates}.}
#'  \item{\code{filter(dates)}}{A thin wrapper around \code{\link{Filter}} so
#'  that only elements of the vector \code{dates} that are good according to
#'  the calendar will be returned.}
#'  \item{\code{shift(dates, period, bdc = "u", eom_rule = TRUE)}}{Returns a
#'  vector of shifted \code{dates} with the same
#'  date class as \code{dates}. The \code{period} should be an atomic
#'  \code{\link{Period-class}} in the sense that only one of its slots should be
#'  non-zero. It must also only be one of day, month or year.
#'  If one of the \code{dates} is the last business day of the month, is being shifted
#'  by a month or year \code{period} and \code{eom_rule} is \code{TRUE} then
#'  the shifted date is also the last business day of the month.}
#'  \item{\code{locales}}{Returns the Calendar's class name as a string which
#'  is used by the \code{print} method.}
#' }
#' @section Active bindings:
#' There is one active binding \code{tz} that returns the Olsen database
#' identifier as a string if no value is passed to the method. Otherwise, it
#' sets the Calendar's time zone attribute.
#'
#' @section Fields:
#' \code{Calendar} has no fields.
#'
#' @docType class
#' @format A \code{\link{R6Class}} generator object
#' @references
#' \href{https://en.wikipedia.org/wiki/List_of_tz_database_time_zones}{Olsen database}
#' @keywords data
#' @export

Calendar <- R6::R6Class(
  classname = "Calendar",
  public = list(
    is_good = function (dates) {
      rep_len(TRUE, NROW(dates))
    },
    filter = function (dates) {
      Filter(self$is_good, dates)
    },
    adjust = function (dates, bdc) {
      assertthat::assert_that(is_valid_bdc(bdc), assertthat::is.scalar(bdc))
      if (identical(bdc, "u")) return (dates)
      # Set parameters
      is_preceding <- grepl("p$", bdc)
      direction <- +1 * (!is_preceding) - 1 * is_preceding
      is_modified <- grepl("^m", bdc)
      if (identical(bdc, "ms")) {
        barrier <- private$.mid_month
      } else{
        barrier <- private$.month_end
      }
      # Adjust
      fn <- private$.adjuster(direction, barrier)
      fn(dates, is_modified)
    },
    shift = function (dates, period, bdc = "u", eom_rule = TRUE) {
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
        return(self$adjust(dates, bdc))
      } else {
        res <- dates
        for (p in ps[is_p_looped]) {
          res <- private$.shift(res,
            lubridate::period(methods::slot(period, p), p), bdc, eom_rule)
        }
        return(res)
      }
    },
    locales = function () {
      class(self)[1]
    },
    print = function () {
      cat(paste0("<", self$locales(), "> TZ:"), self$tz, "\n")
    }
  ),
  active = list(
    tz = function (value) {
      if (missing(value)) {
        private$.tz
      } else {
        private$.tz <- value
      }
    }
  ),
  private = list(
    .tz = "",
    .month_end = function (d1, d2) {
      lubridate::month(d1) != lubridate::month(d2)
    },
    .mid_month = function (d1, d2) {
      is_early <- lubridate::mday(d1) <= 15
      is_early_and_over <- is_early & lubridate::mday(d2) > 15
      is_late_and_over <- !is_early & private$.month_end(d1, d2)
      is_early_and_over | is_late_and_over
    },
    .adjuster = function (direction = +1, barrier = private$.month_end) {
      function (dates, is_modified = TRUE) {
        dts <- dates
        to_adjust <- !self$is_good(dts)
        while (any(to_adjust)) {
          dts <- dts + to_adjust * lubridate::days(direction * 1)
          to_adjust <- !self$is_good(dts)
        }
        is_over_barrier <- barrier(dates, dts)
        if (is_modified & any(is_over_barrier)) {
          other_adjuster = private$.adjuster(-direction)
          dts[is_over_barrier] <- other_adjuster(dates[is_over_barrier], FALSE)
        }
        dts
      }
    },
    .shift = function (dates, period, bdc = "u", eom_rule = TRUE) {
      # Day shift
      if (identical(period_type(period), "day")) {
        # Setup temp and counter variables
        i <- rep(0, NROW(dates))
        shift_length <- period_length(period)
        sgn <- sign(shift_length)

        # Step through days to get to shift_length good days for each of the
        # dates
        is_not_done <- abs(i) < abs(shift_length)
        while (any(is_not_done)) {
          dates[is_not_done] <- dates[is_not_done] + sgn * lubridate::days(1)
          to_roll <- self$is_good(dates)
          i[to_roll] <- i[to_roll] + sgn * 1
          is_not_done <- abs(i) < abs(shift_length)
        }
        return(dates)
      }
      # Month and year shifts are treated the same: one year = twelve months
      # Given assertions at the start, these should be the only options left
      res <- lubridate::`%m+%`(dates, period)
      if (!eom_rule) {
        return(self$adjust(res, bdc))
      } else {
        are_last_good_days <- identical(dates, self$adjust(eom(dates), "p"))
        are_less_31_days <- lubridate::mday(dates) <= 30
        eom_applies <- are_last_good_days & are_less_31_days
        if (any(eom_applies))
          res[eom_applies] <- self$adjust(eom(res[eom_applies]), "p")
        return(res)
      }
    },
    .extract_atoms = function (dates) {
      list(
        dow = lubridate::wday(dates, TRUE, TRUE),
        dom = lubridate::mday(dates),
        doy = lubridate::yday(dates),
        y   = lubridate::year(dates),
        m   = lubridate::month(dates),
        wd  = lubridate::wday(dates),
        em  = easter_monday(lubridate::year(dates)),
        ve  = equinox(lubridate::year(dates), "mar", self$tz),
        ae  = equinox(lubridate::year(dates), "sep", self$tz))
    }
  )
)

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

is_valid_bdc <- function (bdc) {
  all(bdc %in% c('u', 'f', 'mf', 'p', 'mp', 'ms'))
}

