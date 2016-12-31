#' Easter Monday day of year
#'
#' Determine the day of the year that the Western Easter Monday falls on.
#'
#' @param years a numeric vector of \code{years}
#' @return a numeric vector

easter_monday <- function (years)
{
  assertthat::assert_that(all(years >= 1901), all(years <= 2199))
  # day_number is defined in QuantLib 1.3.0.
  day_number <- c(
    98,  90, 103,  95, 114, 106,  91, 111, 102, # 1901-1909
    87, 107,  99,  83, 103,  95, 115,  99,  91, 111,   # 1910-1919
    96,  87, 107,  92, 112, 103,  95, 108, 100,  91,   # 1920-1929
    111,  96,  88, 107,  92, 112, 104,  88, 108, 100,   # 1930-1939
    85, 104,  96, 116, 101,  92, 112,  97,  89, 108,   # 1940-1949
    100,  85, 105,  96, 109, 101,  93, 112,  97,  89,   # 1950-1959
    109,  93, 113, 105,  90, 109, 101,  86, 106,  97,   # 1960-1969
    89, 102,  94, 113, 105,  90, 110, 101,  86, 106,   # 1970-1979
    98, 110, 102,  94, 114,  98,  90, 110,  95,  86,   # 1980-1989
    106,  91, 111, 102,  94, 107,  99,  90, 103,  95,   # 1990-1999
    115, 106,  91, 111, 103,  87, 107,  99,  84, 103,   # 2000-2009
    95, 115, 100,  91, 111,  96,  88, 107,  92, 112,   # 2010-2019
    104,  95, 108, 100,  92, 111,  96,  88, 108,  92,   # 2020-2029
    112, 104,  89, 108, 100,  85, 105,  96, 116, 101,   # 2030-2039
    93, 112,  97,  89, 109, 100,  85, 105,  97, 109,   # 2040-2049
    101,  93, 113,  97,  89, 109,  94, 113, 105,  90,   # 2050-2059
    110, 101,  86, 106,  98,  89, 102,  94, 114, 105,   # 2060-2069
    90, 110, 102,  86, 106,  98, 111, 102,  94, 114,   # 2070-2079
    99,  90, 110,  95,  87, 106,  91, 111, 103,  94,   # 2080-2089
    107,  99,  91, 103,  95, 115, 107,  91, 111, 103,   # 2090-2099
    88, 108, 100,  85, 105,  96, 109, 101,  93, 112,   # 2100-2109
    97,  89, 109,  93, 113, 105,  90, 109, 101,  86,   # 2110-2119
    106,  97,  89, 102,  94, 113, 105,  90, 110, 101,   # 2120-2129
    86, 106,  98, 110, 102,  94, 114,  98,  90, 110,   # 2130-2139
    95,  86, 106,  91, 111, 102,  94, 107,  99,  90,   # 2140-2149
    103,  95, 115, 106,  91, 111, 103,  87, 107,  99,   # 2150-2159
    84, 103,  95, 115, 100,  91, 111,  96,  88, 107,   # 2160-2169
    92, 112, 104,  95, 108, 100,  92, 111,  96,  88,   # 2170-2179
    108,  92, 112, 104,  89, 108, 100,  85, 105,  96,   # 2180-2189
    116, 101,  93, 112,  97,  89, 109, 100,  85, 105)    # 2190-2199
  day_number[years - 1900]
}

#' March and September equinox
#'
#' Determine the date/time of March and September Equinoxes.
#' This implements Jean Meeus' algorithm (Astronomical Algorithms
#' 1st Ed, 1991, Chapter 26).
#'
#' @inheritParams easter_monday
#' @param tz time zone for which equinox instant should be returned (default:
#' \code{"UTC"})
#' @param want_dt a flag indicating whether the returned date should be in
#' dynamical time (default: \code{FALSE})
#' @param season defines the Equinox sought. Can be \code{mar} (default) or
#' \code{sep}.
#' @return the date-time (UTC) of the Equinox (drop sub-minute precision)

equinox <- function (years, season = 'mar', tz = "UTC", want_dt = FALSE)
{
  # Check inputs
  assertthat::assert_that(season %in% c('mar', 'sep'),
    all(years >= 1000), all(years <= 3000))

  # Algorithm from Jean-Meeus Astronomical Algorithms, Chapter 26
  # Table 26.B coefficients
  coef_mar <- c(2451623.80984, 365242.37404,  0.05169, -0.00411, -0.00057)
  coef_jun <- c(2451716.56767, 365241.62603,  0.00325, 0.00888, -0.00030)
  coef_sep <- c(2451810.21715, 365242.01767, -0.11575,  0.00337,  0.00078)

  # Table 26.C constants.
  A <- c(485, 203, 199, 182, 156, 136, 77, 74, 70, 58, 52, 50, 45, 44, 29, 18,
    17, 16, 14, 12, 12, 12, 9, 8)
  B <- c(324.96, 337.23, 342.08, 27.85, 73.14, 171.52, 222.54, 296.72, 243.58,
    119.81, 297.17, 21.02, 247.54, 325.15, 60.93, 155.12, 288.79, 198.04,
    199.76, 95.39, 287.11, 320.81, 227.73, 15.45)
  C <- c(1934.136, 32964.467, 20.186, 445267.112, 45036.886, 22518.443,
    65928.934, 3034.906, 9037.513, 33718.147, 150.678, 2281.226,
    29929.562, 31555.956, 4443.417, 67555.328, 4562.452, 62894.029,
    31436.921, 14577.848, 31931.756, 34777.259, 1222.114, 16859.074)

  # Mean time
  y <- (years - 2000) / 1000
  M <- cbind(rep(1, NROW(y)), y, y ^ 2, y ^ 3, y ^ 4)
  if (identical(season, 'mar')) {
    jde0 <- as.vector(M %*% coef_mar)
  } else if (identical(season, "jun")) {
    jde0 <- as.vector(M %*% coef_jun)
  } else {
    jde0 <- as.vector(M %*% coef_sep)
  }
  # Correction
  tt <- (jde0 - 2451545) / 36525
  w <- 35999.373 * tt - 2.47
  delta_lambda <- 1 + 0.0334 * cos(rads(w)) + 0.0007 * cos(rads(2 * w))
  s <- vector("numeric", length(tt))
  for (i in seq_along(s)) {
    s[i] <- Reduce(sum, Map(function (a, b, c, t) a * cos(rads(b + c * t)),
      A, B, C, tt[i]))
  }
  # julian datetime in dynamical time (i.e. not UTC)
  jde <- jde0 + 0.00001 * s / delta_lambda
  jde_to_gregorian(jde, tz, want_dt)
}

#' Determine Gregorian date from Julian day
#'
#' @param julian_day a numeric vector
#' @return a POSIXct vector of Gregorian dates at midnight UTC time.

julian_day_to_gregorian <- function (julian_day)
{
  # Jean-Meeus Astronomical Algorithms, Chapter 7, pg 63
  z <- trunc(julian_day + 0.5)
  f <- julian_day + 0.5 - z
  alpha <- trunc((z - 1867216.25) / 36524.25)
  a <- rep(NA, NROW(julian_day))
  a[z < 2299161] <- z
  a[!(z < 2299161)] <- z + 1 + alpha - trunc(alpha / 4)
  b <- a + 1524
  c <- trunc((b - 122.1) / 365.25)
  d <- trunc(365.25 * c)
  e <- trunc((b - d) / 30.6001)
  dom <- b - d - trunc(30.6001 * e) + f
  f_dom <- dom - trunc(dom)
  m1 <- e - 1
  m2 <- rep(0, NROW(e))
  m2[e == 14 | e == 15] <-  (e - 13)[e == 14 | e == 15]
  m <- m1 * (e < 14) + m2 * (e >= 14)
  m1 <- c - 4716
  m2 <- rep(0, NROW(m))
  m2[m == 1 | m == 2] <- (c - 4715)[m == 1 | m == 2]
  y <- m1 * (m > 2) + m2 * (m <= 2)
  # reproduce same output as default values of lubridate::ymd()
  ISOdate(y, m, trunc(dom), hour = 0, tz = 'UTC') + f_dom * 24 * 60 * 60
}

jde_to_gregorian <- function (jde, tz = "UTC", want_dt = FALSE) {
  res <- julian_day_to_gregorian(jde)
  if (!want_dt) {
    # Meeus pg 71 has Delta T = TD - UT.
    res <- (res - lubridate::seconds(delta_t(res))) %>%
      lubridate::floor_date(unit = "minute")
  }
  if (tz != "UTC") {
    return (lubridate::with_tz(res, tz = tz))
  } else {
    return (res)
  }
}

to_jd <- function (dates) {
  # Only gregorian calendar is supported.
  dates_utc <- dates %>% lubridate::with_tz(tzone = "UTC")
  y <- lubridate::year(dates_utc)
  m <- lubridate::month(dates_utc)
  dom <- lubridate::mday(dates_utc)
  if (!lubridate::is.Date(dates)) {
    f_dom <- ((dates_utc - ISOdate(y, m, dom, hour = 0, tz = "UTC")) / 24) %>%
      as.numeric()
  } else {
    f_dom <- rep(0, length(dates))
  }
  dom <- dom + f_dom
  y[m <= 2] <- y[m <= 2] - 1
  m[m <= 2] <- m[m <= 2] + 12
  a <- trunc(y / 100)
  is_greg <- is_gregorian(dates)
  b <- vector("numeric", length(a))
  b[is_greg] <- 2 - a + trunc(a / 4)
  b[!is_greg] <- 0
  trunc(365.25 * (y + 4716)) + trunc(30.6001 * (m + 1)) + dom + b - 1524.5
}

is_gregorian <- function (dates) {
  cutoff <- lubridate::ymd(15821004)
  if (lubridate::is.POSIXt(dates)) {
    dates <- lubridate::as_date(dates)
  }
  dates >= cutoff
}

rads <- function (degs) {
  degs * pi / 180
}

delta_t <- function (dates) {
  assertthat::assert_that(lubridate::is.POSIXt(dates))
  dates <- lubridate::with_tz(dates, "UTC")
  # See: http://maia.usno.navy.mil/ser7/tai-utc.dat
  # Ten leap seconds before start of data contained in .leap.seconds
  # http://www.usno.navy.mil/USNO/earth-orientation/eo-info/general/date-time-def/date-and-time-definitions
  n_leap_seconds <- Map(function (x)
    lubridate::with_tz(.leap.seconds, "UTC") <= x, dates)
  n_leap_seconds <- vapply(n_leap_seconds, sum, integer(1))
  # Observed:
  # http://maia.usno.navy.mil/ser7/deltat.data
  # Quick comparison to observed suggests this code is correct to within a
  # second or two.
  32.184 + (10 + n_leap_seconds)
}

moon_phase_delta <- function (phase) {
  phase_map <- function (x) {
    switch(x, "new" = 0.0, "first" = 0.25, "full" = 0.50, "last" = 0.75)
  }
  vapply(phase, phase_map, numeric(1), USE.NAMES = FALSE)
}

mean_moon_phase <- function (k, phase) {
  # Chapter 47, Meeus
  assertthat::assert_that(
    all(k %% 1 == 0),
    assertthat::is.string(phase), phase %in% c("new", "first", "full", "last")
  )
  k <- k + moon_phase_delta(phase)
  tt <- k / 1236.85
  2451550.09765 +
    29.530588853 * k +
    0.0001337 * tt ^ 2 -
    0.000000150 * tt ^ 3 +
    0.00000000073 * tt ^ 4
}

true_moon_phase <- function (k, phase) {
  # Chapter 47, Meeus
  assertthat::assert_that(
    all(k %% 1 == 0),
    assertthat::is.string(phase), phase %in% c("new", "first", "full", "last")
  )
  jde <- mean_moon_phase(k, phase)
  k <- k + moon_phase_delta(phase)
  tt <- k / 1236.85
  ## Lunisolar corrections (47.4 - 47.7)
  ee <- 1 - 0.002516 * tt - 0.0000074 * tt ^ 2
  mm <- rads((2.5534 + 29.10535669 * k - 0.0000218 * tt ^ 2 -
      0.00000011 * tt ^ 3) %% 360)
  mmd <- rads((201.5643 + 385.81693528 * k + 0.0107438 * tt ^ 2 +
      0.00001239 * tt ^ 3 - 0.000000058 * tt ^ 4) %% 360)
  ff <- rads((160.7108 + 390.67050274 * k - 0.0016341 * tt ^ 2 -
      0.00000227 * tt ^ 3 + 0.000000011 * tt ^ 4) %% 360)
  omega <- rads((124.7746 - 1.56375580 * k + 0.0020691 * tt ^ 2 +
      0.00000215 * tt ^ 3) %% 360)
  new_moon_constant <-
    c(
      -0.40720,
      +0.17241,
      +0.01608,
      +0.01039,
      +0.00739,
      -0.00514,
      +0.00208,
      -0.00111,
      -0.00057,
      +0.00056,
      -0.00042,
      +0.00042,
      +0.00038,
      -0.00024,
      -0.00017,
      -0.00007,
      +0.00004,
      +0.00004,
      +0.00003,
      +0.00003,
      -0.00003,
      +0.00003,
      -0.00002,
      -0.00002,
      +0.00002
    )
  full_moon_constant <-
    c(
      -0.40614,
      +0.17302,
      +0.01614,
      +0.01043,
      +0.00734,
      -0.00515,
      +0.00209,
      -0.00111,
      -0.00057,
      +0.00056,
      -0.00042,
      +0.00041,
      +0.00038,
      -0.00024,
      -0.00017,
      -0.00007,
      +0.00004,
      +0.00004,
      +0.00003,
      +0.00003,
      -0.00003,
      +0.00003,
      -0.00002,
      -0.00002,
      +0.00002
    )
  qtr_phase_constant <-
    c(
      -0.62801,
      +0.17172,
      -0.01183,
      +0.00862,
      +0.00804,
      +0.00454,
      +0.00204,
      -0.00180,
      -0.0007,
      -0.0004,
      -0.00034,
      +0.00032,
      +0.00032,
      -0.00028,
      +0.00027,
      -0.00017,
      -0.00005,
      +0.00004,
      -0.00004,
      +0.00004,
      +0.00003,
      +0.00003,
      +0.00002,
      +0.00002,
      -0.00002
    )
  one_n <- rep(1, length(ee))
  new_full_phase_coefficients_a <- c(one_n, ee, one_n, one_n, ee, ee, ee ^ 2,
    one_n, one_n, ee, one_n, ee, ee, ee, rep(one_n, 11)) %>% matrix(ncol = 25)
  new_full_phase_coefficients_b <- sin(c(mmd, mm, 2 * mmd, 2 * ff, mmd - mm,
    mmd + mm, 2 * mm, mmd - 2 * ff, mmd + 2 * ff, 2 * mmd + mm, 3 * mmd,
    mm + 2 * ff, mm - 2 * ff, 2 * mmd - mm, omega, mmd + 2 * mm,
    2 * mmd - 2 * ff, 3 * mm, mmd + mm - 2 * ff, 2 * mmd + 2 * ff,
    mmd + mm + 2 * ff, mmd - mm + 2 * ff, mmd - mm - 2 * ff, 3 * mmd + mm,
    4 * mmd)) %>% matrix(ncol = 25)
  new_full_phase_coefficients <- new_full_phase_coefficients_a *
    new_full_phase_coefficients_b
  qtr_phase_coefficients_a <- c(one_n, ee, ee, one_n, one_n, ee, ee ^ 2, one_n,
    one_n, one_n, ee, ee, ee, ee ^ 2, ee, rep(one_n, 10)) %>% matrix(ncol = 25)
  qtr_phase_coefficients_b <- sin(c(mmd, mm, mmd + mm, 2 * mmd, 2 * ff,
    mmd - mm, 2 * mm, mmd - 2 * ff, mmd + 2 * ff, 3 * mmd, 2 * mmd - mm,
    mm + 2 * ff, mm - 2 * ff, mmd + 2 * mm, 2 * mmd + mm, omega,
    mmd - mm - 2 * ff, 2 * mmd + 2 * ff, mmd + mm + 2 * ff, mmd - 2 * mm,
    mmd + mm - 2 * ff, 3 * mm, 2 * mmd - 2 * ff, mmd - mm + 2 * ff,
    3 * mmd + mm)) %>% matrix(ncol = 25)
  qtr_phase_coefficients <- qtr_phase_coefficients_a * qtr_phase_coefficients_b
  ww <- 0.00306 - 0.00038 * ee * cos(mm) + 0.00026 * cos(mmd) -
    0.00002 * cos(mmd - mm) + 0.00002 * cos(mmd + mm) + 0.00002 * cos(2 * ff)
  ## Planetary corrections
  planetary_constants <- c(325, 165, 164, 126, 110, 62, 60, 56, 47, 42, 40, 37,
    35, 23) / 1e6
  a1 <- rads((299.77 + 0.107408 * k - 0.009173 * tt ^ 2) %% 360)
  a2 <- rads((251.88 + 0.016321 * k) %% 360)
  a3 <- rads((251.83 + 26.651886 * k) %% 360)
  a4 <- rads((349.42 + 36.412478 * k) %% 360)
  a5 <- rads((84.66 + 18.206239 * k) %% 360)
  a6 <- rads((141.74 + 53.303771 * k) %% 360)
  a7 <- rads((207.14 + 2.453732 * k) %% 360)
  a8 <- rads((154.84 + 7.306860 * k) %% 360)
  a9 <- rads((34.52 + 27.261239 * k) %% 360)
  a10 <- rads((207.19 + 0.121824 * k) %% 360)
  a11 <- rads((291.34 + 1.844379 * k) %% 360)
  a12 <- rads((161.72 + 24.198154 * k) %% 360)
  a13 <- rads((239.56 + 25.513099 * k) %% 360)
  a14 <- rads((331.55 + 3.592518 * k) %% 360)
  planetary_coefficients <- sin(c(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11,
    a12, a13, a14)) %>% matrix(ncol = 14)
  if (phase == "new") {
    jde <- jde + new_full_phase_coefficients %*% new_moon_constant
  } else if (phase == "full") {
    jde <- jde + new_full_phase_coefficients %*% full_moon_constant
  } else if (phase == "first") {
    jde <- jde + qtr_phase_coefficients %*% qtr_phase_constant + ww
  } else {
    jde <- jde + qtr_phase_coefficients %*% qtr_phase_constant - ww
  }
  (jde + planetary_coefficients %*% planetary_constants) %>% drop()
}

next_moon_phase <- function (dates, phase, tz = "UTC", want_dt = FALSE) {
  assertthat::assert_that(
    assertthat::is.string(phase), phase %in% c("new", "first", "full", "last"),
    assertthat::is.flag(want_dt),
    assertthat::is.string(tz)
  )
  if (lubridate::is.Date(dates)) {
    dates <- as.POSIXct(dates)
  }
  jde0 <- to_jd(dates + lubridate::seconds(delta_t(dates)))
  yrs <- (lubridate::year(dates) + lubridate::yday(dates) / 365 - 2000)
  k <- (yrs * 12.3685) %>% floor()
  guess <- mean_moon_phase(k, phase)
  is_not_after <- jde0 > guess
  while (any(jde0 > guess)) {
    k[is_not_after] <- k[is_not_after] + 1
    guess[is_not_after] <- mean_moon_phase(k[is_not_after], phase)
  }
  res <- true_moon_phase(k, phase)
  jde_to_gregorian(res, tz, want_dt)
}

chinese_new_year <- function (years) {
  # https://en.wikipedia.org/wiki/Chinese_New_Year
  soy <- as.Date(paste(years, "01", "21", sep = "-"))
  next_moon_phase(soy, "new", tz = "Asia/Shanghai")
}
