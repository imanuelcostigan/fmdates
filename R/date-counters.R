#' The years between two dates using the 30/360 day basis convention.
#'
#' This calculates the years between two dates using the 30/360 day basis
#' convention. This convention assumes that months consists of 30 days and
#' years consist of 360 calendar days.
#'
#' The day count is determined after making the following (ordered)
#' modifications:
#' \enumerate{
#'   \item If the start date is the 31st, set the start dates to the 30th.
#'   \item If the start date greater than the 29nd and the end date is the
#'         31st then set the end dates to the 30th.
#' }
#'
#' The year fraction is then calculated as:
#'
#' \deqn{\frac{(d_2 - d_1) + (m_2 - m_1) \times 30 + (y_2 - y_1)
#'  \times 30}{360}}
#'
#' The order of \code{date1} and \code{date2} is not important. If \code{date1}
#' is less than \code{date2} then the result will be non-negative. Otherwise,
#' the result will be negative.
#'
#' This is also known as the bond basis.
#'
#' @param date1 A date-time object.
#' @param date2 A date-time object.
#' @return A numeric value representing the number of years between
#' \code{date1} and \code{date2}.
#' @keywords internal
#' @family counter methods

thirty_360 <- function (date1, date2) {
  if (identical(date1, date2)) return (0)

  dd1 <- lubridate::mday(date1)/1
  dd2 <- lubridate::mday(date2)/1
  mm1 <- lubridate::month(date1)
  mm2 <- lubridate::month(date2)
  yy1 <- lubridate::year(date1)
  yy2 <- lubridate::year(date2)

  is_dd1_30 <- (dd1 == 31)
  dd1[is_dd1_30] <- 30
  is_dd2_30 <- (dd2 == 31) & (dd1 > 29)
  dd2[is_dd2_30] <- 30

  ((yy2 - yy1) * 360 + (mm2 - mm1) * 30 + (dd2 - dd1)) / 360
}

#' The years between two dates using the 30/360 (US) day basis convention.
#'
#' This calculates the years between two dates using the 30/360 (US) day basis
#' convention. This convention assumes that months consists of 30 days and
#' years consist of 360 calendar days.
#'
#' The day count is determined after making the following (ordered)
#' modifications:
#' \enumerate{
#'   \item If both the start and end dates are the last day of February, set
#'         the end date to the 30th.
#'   \item If the start date is the last day of February, set the start date
#'         to the 30th.
#'   \item If the end date is the 31st and the start date is either the 30th or
#'         the 30th, set the end date to the 30th
#'   \item If the start date is the 31st, set the start date to the 30th.
#' }
#'
#' The year fraction is then calculated as:
#'
#' \deqn{\frac{(d_2 - d_1) + (m_2 - m_1) \times 30 + (y_2 - y_1)
#'  \times 30}{360}}
#'
#' The order of \code{date1} and \code{date2} is not important. If \code{date1}
#' is less than \code{date2} then the result will be non-negative. Otherwise,
#' the result will be negative.
#'
#' This is also known as the EOM adjusted bond basis.
#'
#' @param date1 A date-time object.
#' @param date2 A date-time object.
#' @return A numeric value representing the number of years between
#' \code{date1} and \code{date2}.
#' @keywords internal
#' @family counter methods
thirty_360_us <- function (date1, date2)
{
  if (identical(date1, date2)) return (0)

  dd1 <- lubridate::mday(date1)/1
  dd2 <- lubridate::mday(date2)/1
  mm1 <- lubridate::month(date1)
  mm2 <- lubridate::month(date2)
  yy1 <- lubridate::year(date1)
  yy2 <- lubridate::year(date2)

  dd1_end_feb <- is_eom(date1) && lubridate::month(date1) == 2
  dd1[dd1_end_feb] <- 30

  both_end_feb <- dd1_end_feb &
    is_eom(date2) & lubridate::month(date2) == 2
  dd2[both_end_feb] <- 30

  is_both_30s <- (dd1 == 30 | dd1 == 31) & (dd2 == 31)
  dd2[is_both_30s] <- 30

  is_dd1_30 <- (dd1 == 31)
  dd1[is_dd1_30] <- 30

  ((yy2 - yy1) * 360 + (mm2 - mm1) * 30 + (dd2 - dd1)) / 360
}

#' The years between two dates using the 30/360 (EU) day basis convention.
#'
#' This calculates the years between two dates using the 30/360 (EU) day
#' basis convention. This convention assumes that months consists of 30 days
#' and years consist of 360 calendar days. This is also known as the 30/360
#' ICMA, Eurobond (ISDA 2006) and Special German basis.
#'
#' The day count is determined after making the following (ordered)
#' modifications:
#' \enumerate{
#'   \item If the start date is the 31st, set the start date to the 30th
#'   \item If the end date is 31st, set the end date to the 30th
#' }
#'
#' The year fraction is then calculated as:
#' \deqn{\frac{(d_2-d_1) + (m_2-m_1)\times 30 + (y_2-y_1)\times 360}{360}}
#'
#' The order of \code{date1} and \code{date2} is not important. If \code{date1}
#' is less than \code{date2} then the result will be non-negative. Otherwise,
#' the result will be negative.
#'
#' @param date1 A date-time object
#' @param date2 A date-time object
#' @return a numeric value representing the number of years between
#' \code{date1} and \code{date2}.
#' @keywords internal
#' @family counter methods
thirty_360_eu <- function (date1, date2)
{
  dd1 <- lubridate::mday(date1)/1
  dd2 <- lubridate::mday(date2)/1
  mm1 <- lubridate::month(date1)
  mm2 <- lubridate::month(date2)
  yy1 <- lubridate::year(date1)
  yy2 <- lubridate::year(date2)

  is_31 <- dd1 == 31
  dd1[is_31] <- 30
  is_31 <- dd2 == 31
  dd2[is_31] <- 30

  ((yy2 - yy1) * 360 + (mm2 - mm1) * 30 + (dd2 - dd1)) / 360
}

#' The years between two dates using the 30E/360 ISDA day basis convention.
#'
#' This calculates the years between two dates using the 30E/360 ISDA day
#' basis convention. This convention assumes that months consists of 30 days
#' and years consist of 360 calendar days. This is also known as the Eurobond
#' (ISDA 2000) and German basis.
#'
#' The day count is determined after making the following (ordered)
#' modifications:
#' \enumerate{
#'   \item If the start date is the last day of the month, set the start date
#'          to the 30th
#'   \item If the end date is the last day of the month, set the end date to
#'          the 30th unless the end date is the maturity date and the end
#'          month is February
#' }
#'
#' The year fraction is then calculated as:
#' \deqn{\frac{(d_2-d_1) + (m_2-m_1)\times 30 + (y_2-y_1)\times 30}{360}}
#'
#' The order of \code{date1} and \code{date2} is not important. If \code{date1}
#' is less than \code{date2} then the result will be non-negative. Otherwise,
#' the result will be negative.
#'
#' @param date1 A date-time object
#' @param date2 A date-time object
#' @param maturity_date the maturity date of the instrument
#' @return a numeric value representing the number of years between
#' \code{date1} and \code{date2}.
#' @keywords internal
#' @family counter methods
thirty_360_eu_isda <- function (date1, date2, maturity_date)
{
  dd1 <- lubridate::mday(date1)/1
  dd2 <- lubridate::mday(date2)/1
  mm1 <- lubridate::month(date1)
  mm2 <- lubridate::month(date2)
  yy1 <- lubridate::year(date1)
  yy2 <- lubridate::year(date2)

  d1_eom <- eom(date1)
  d2_eom <- eom(date2)

  is_eom <- (date1 == d1_eom)
  dd1[is_eom] <- 30
  is_d2_eo_feb <- (date2 == d2_eom) & (date2 == maturity_date) & (mm2 == 2)
  dd2[is_d2_eo_feb] <- 30

  ((yy2 - yy1) * 360 + (mm2 - mm1) * 30 + (dd2 - dd1)) / 360
}

#' The years between two dates using the 30E+/360 day basis convention.
#'
#' This calculates the years between two dates using the 30E+/360 day
#' basis convention. This convention assumes that months consists of 30 days
#' and years consist of 360 calendar days.
#'
#' The day count is determined after making the following (ordered)
#' modifications:
#' \enumerate{
#'   \item If the start date is the 31st, set the start date to the 30th
#'   \item If the end date is the 31st, set the end date to the 1st of the
#'   following month.
#' }
#'
#' The year fraction is then calculated as:
#' \deqn{\frac{(d_2-d_1) + (m_2-m_1)\times 30 + (y_2-y_1)\times 360}{360}}
#'
#' The order of \code{date1} and \code{date2} is not important. If \code{date1}
#' is less than \code{date2} then the result will be non-negative. Otherwise,
#' the result will be negative.
#'
#' @param date1 A date-time object
#' @param date2 A date-time object
#' @return a numeric value representing the number of years between
#' \code{date1} and \code{date2}.
#' @keywords internal
#' @family counter methods
thirty_360_eu_plus <- function (date1, date2)
{
  dd1 <- lubridate::mday(date1)/1
  dd2 <- lubridate::mday(date2)/1
  mm1 <- lubridate::month(date1)
  mm2 <- lubridate::month(date2)
  yy1 <- lubridate::year(date1)
  yy2 <- lubridate::year(date2)

  is_31 <- (dd1 == 31)
  dd1[is_31] <- 30
  is_31 <- (dd2 == 31)
  date2[is_31] <- lubridate::ceiling_date(date2[is_31], "month")
  dd2[is_31] <- lubridate::mday(date2[is_31])/1
  mm2[is_31] <- lubridate::month(date2[is_31])
  yy2[is_31] <- lubridate::year(date2[is_31])

  ((yy2 - yy1) * 360 + (mm2 - mm1) * 30 + (dd2 - dd1)) / 360
}

#' The years between two dates using the actual/360 day basis convention.
#'
#' This calculates the years between two dates using the actual/360 day basis
#' convention. This convention counts the number of calendars beween the start
#' and end dates and assumes a year consists of 360 days. This is also known
#' as the A/360, Act/360 or French day basis convention.
#'
#' The year fraction is calculated as:
#' \deqn{\frac{Number of calendar days}{360}}
#'
#' The order of \code{date1} and \code{date2} is not important. If \code{date1}
#' is less than \code{date2} then the result will be non-negative. Otherwise,
#' the result will be negative.
#'
#' @param date1 A date-time object
#' @param date2 A date-time object
#' @return a numeric value representing the number of years between
#' \code{date1} and \code{date2}.
#' @keywords internal
#' @family counter methods
actual_360 <- function (date1, date2) {
  as.numeric(date2 - date1) / 360
}


#' The years between two dates using the actual/365 (fixed) day basis
#' convention.
#'
#' This calculates the years between two dates using the actual/365 (fixed)
#' day  basis convention. This convention counts the number of calendars
#' between the start and end dates and assumes a year consists of 365 days.
#' This is also known as the Act/365 fixed, A/365 fixed, A/365F and English
#' day basis convention.
#'
#' The year fraction is calculated as:
#' \deqn{\frac{Number of calendar days}{365}}
#'
#' The order of \code{date1} and \code{date2} is not important. If \code{date1}
#' is less than \code{date2} then the result will be non-negative. Otherwise,
#' the result will be negative.
#'
#' @param date1 A date-time object
#' @param date2 A date-time object
#' @return a numeric value representing the number of years between
#' \code{date1} and \code{date2}.
#' @keywords internal
#' @family counter methods
actual_365 <- function (date1, date2) {
  as.numeric(date2 - date1) / 365
}


#' The years between two dates using the Actual/Actual ISDA day basis
#' convention
#'
#' This calculates the years between two dates using the Actual/Actual ISDA
#' day basis convention. This convention counts the number of calendars
#' between the start and end dates. The definition of a year is contingent on
#' whether the year is a leap year or not. The year fraction is calculated as:
#' \deqn{
#'   \frac{d_1}{{dy}_1} + \frac{d_2}{{dy}_2} + y_2 - y_1 - 1
#' }
#'
#' where:
#'
#' \itemize{
#'   \item \eqn{d_1} is the remaining days in the first date's year including
#'         both the start and end date
#'   \item \eqn{d_2} is the number of days to the second date from the start
#'         of that year
#'   \item \eqn{{dy}_1} is 366 if the first date is in a leap year. Otherwise
#'         it is 365
#'   \item \eqn{{dy}_2} is 366 if the second date is in a leap year. Otherwise
#'         it is 365
#' }
#'
#' The order of \code{date1} and \code{date2} is not important. If \code{date1}
#' is less than \code{date2} then the result will be non-negative. Otherwise,
#' the result will be negative.
#'
#' @param date1 A date-time object
#' @param date2 A date-time object
#' @return a numeric value representing the number of years between
#' \code{date1} and \code{date2}.
#' @keywords internal
#' @family counter methods
actual_actual_isda <- function (date1, date2)
{
  dib1 <- vector("numeric", NROW(date1))
  dib2 <- vector("numeric", NROW(date2))

  yy1 <- lubridate::year(date1)
  yy2 <- lubridate::year(date2)
  is_date1_ly <- lubridate::leap_year(date1)
  is_date2_ly <- lubridate::leap_year(date2)
  dib1[is_date1_ly] <- 366
  dib1[!is_date1_ly] <- 365
  dib2[is_date2_ly] <- 366
  dib2[!is_date2_ly] <- 365
  bony1 <- as.Date(ISOdate(yy1 + 1, 1, 1, 0, 0, 0))
  boy2 <- as.Date(ISOdate(yy2, 1, 1, 0, 0, 0))
  yy2 - yy1 - 1 + as.numeric(bony1 - date1) / dib1 +
    as.numeric(date2 - boy2) / dib2
}

#' The years between two dates for a given day basis convention
#'
#' This calculates the years between two dates using the given day
#' basis convention.
#'
#' The order of \code{date1} and \code{date2} is not important. If \code{date1}
#' is less than \code{date2} then the result will be non-negative. Otherwise,
#' the result will be negative. The parameters will be repeated with recycling
#' such that each parameter's length is equal to maximum length of
#' any of the parameters.
#'
#' @param date1 A vector of dates. This will be coerced to a \code{\link{Date}}
#' class.
#' @param date2 A vector of dates. This will be coerced to a \code{\link{Date}}
#' class.
#' @param day_basis The basis on which the year fraction is calculated. See
#' [is_valid_day_basis()]
#' @param maturity_date a vector of dates representing the maturity date of
#' the instrument. Only used for 30E/360 ISDA day basis.
#' @return a numeric vector representing the number of years between
#' \code{date1} and \code{date2}.
#' @examples
#' require(lubridate)
#' year_frac(ymd("2010-03-31"), ymd("2012-03-31"), "30/360us") # 2
#' year_frac(ymd("2010-02-28"), ymd("2012-03-31"), "act/360")  # 2.116667
#' year_frac(ymd("2010-02-28"), ymd("2012-03-31"), "act/365")  # 2.087671
#' year_frac(ymd("2010-02-28"), ymd("2012-03-31"), "act/actisda")  # 2.086998
#' @references \url{http://en.wikipedia.org/wiki/Day_count_convention}
#' @family counter methods
#' @export

year_frac <- function(date1, date2, day_basis, maturity_date = NULL)
{
  # Check that day basis is valid
  assertthat::assert_that(is_valid_day_basis(day_basis))

  # Convert inputs to Date (time stamp details irrelevant to these calcs)
  date1 <- lubridate::as_date(date1)
  date2 <- lubridate::as_date(date2)

  # Make sure inputs are vectors of same length. This will allow us to
  # vectorise the calculation
  max_n_dates <- max(NROW(date1), NROW(date2))
  yrs <- vector("numeric", max_n_dates)

  date1 <- rep(date1, length.out = max_n_dates)
  date2 <- rep(date2, length.out = max_n_dates)
  day_basis <- rep(day_basis, length.out = max_n_dates)
  if(!is.null(maturity_date)){
    maturity_date <- rep(maturity_date, length.out = max_n_dates)
  }

  # Prep work
  to_reverse <- date1 > date2
  if (any(to_reverse, na.rm = TRUE)) {
    yrs[to_reverse] <- -year_frac(date2[to_reverse], date1[to_reverse],
      day_basis[to_reverse], maturity_date[to_reverse])
  }

  # Get year fraction
  is_30360 <- day_basis == "30/360"
  is_30360us <- day_basis == "30/360us"
  is_30e360 <- day_basis == "30e/360"
  is_30e360isda <- day_basis == "30e/360isda"
  is_30ep360 <- day_basis == "30e+/360"
  is_act360 <- day_basis == "act/360"
  is_act365 <- day_basis == "act/365"
  is_actactisda <- day_basis == "act/actisda"

  if (any(is_30360))
    yrs[is_30360] <- thirty_360(date1[is_30360], date2[is_30360])

  if (any(is_30360us))
    yrs[is_30360us] <- thirty_360_us(date1[is_30360us], date2[is_30360us])

  if (any(is_30e360))
    yrs[is_30e360] <- thirty_360_eu(date1[is_30e360], date2[is_30e360])

  if (any(is_30e360isda))
    yrs[is_30e360isda] <- thirty_360_eu_isda(date1[is_30e360isda],
      date2[is_30e360isda], maturity_date[is_30e360isda])

  if (any(is_30ep360))
    yrs[is_30ep360] <- thirty_360_eu_plus(date1[is_30ep360], date2[is_30ep360])

  if (any(is_act360))
    yrs[is_act360] <- actual_360(date1[is_act360], date2[is_act360])

  if (any(is_act365))
    yrs[is_act365] <- actual_365(date1[is_act365], date2[is_act365])

  if (any(is_actactisda))
    yrs[is_actactisda] <- actual_actual_isda(date1[is_actactisda],
      date2[is_actactisda])

  # Return value
  yrs
}

#' Day basis conventions
#'
#' Checks whether day basis conventions are valid. Supported day basis
#' conventions are documented in [year_frac()]
#'
#' @param day_basis A character vector of day basis conventions.
#' @return will return `TRUE` for `day_basis` elements that are any of the
#'   following: \code{30/360}, \code{30/360us}, \code{30e/360},
#'   \code{30e/360isda}, \code{30e+/360}, \code{act/360}, \code{act/365} and
#'   \code{act/actisda}. Otherwise will return `FALSE`
#' @export
#' @examples
#' is_valid_day_basis(c("act/360", "act/365f"))
#' @aliases daybasisconventions
#' @family counter methods

is_valid_day_basis <- function (day_basis) {
  all(day_basis %in% c("30/360us", "30e/360", "30e/360isda", "30e+/360",
    "act/360",  "act/365", "act/actisda", "30/360"))
}
