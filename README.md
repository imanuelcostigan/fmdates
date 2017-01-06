
<!-- README.md is generated from README.Rmd. Please edit that file -->
fmdates
=======

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/fmdates)](https://cran.r-project.org/package=fmdates) [![Travis build status](https://travis-ci.org/imanuelcostigan/fmdates.svg?branch=master)](https://travis-ci.org/imanuelcostigan/fmdates) [![Appveyor build status](https://ci.appveyor.com/api/projects/status/i8io32eoft5rsb6n/branch/master?svg=true)](https://ci.appveyor.com/project/imanuelcostigan/fmdates/branch/master) [![codecov](https://codecov.io/gh/imanuelcostigan/fmdates/branch/master/graph/badge.svg)](https://codecov.io/gh/imanuelcostigan/fmdates)

Motivation
----------

Over-the-counter (OTC) derivatives comprise a significant proportion of trading activity in global financial markets. Their general contractual conventions are specified in what are known as [International Swap and Derivatives Association (ISDA) definitions](http://www.isda.org/publications/isdadefslist.aspx). For example, FX and currency option transactions are governed by the *1998 FX and Currency Options Definitions* and swap transactions are governed by the *2006 ISDA Definitions*. They describe in meticulous detail, among other things, how the dates of certain financial events should be determined. This includes how dates are defined to be good or bad and how bad dates are to be adjusted to good dates. They also define how to determine the length of time between two dates.

This package implements calendars used to define locale specific business days, date adjusters and shifters, schedule generators and year fraction calculations defined by these standards.

Calendars
---------

You can determine whether dates are business days in a specific locale or specific locales:

``` r
library("lubridate")
#> 
#> Attaching package: 'lubridate'
#> The following object is masked from 'package:base':
#> 
#>     date
library("fmdates")
ausy <- AUSYCalendar()
aume <- AUMECalendar()
syme <- c(ausy, aume) # handy JointCalendar construction approach
is_good(ymd(20140404), ausy)
#> [1] TRUE
is_good(ymd(20141104), syme) # Melbourne Cup holiday
#> [1] FALSE
syme$rule <- any
is_good(ymd(20141104), syme)
#> [1] TRUE
```

Adjusters and shifters
----------------------

You can adjust (or roll) and shift dates using predefined business day conventions:

``` r
# Adjust using the modified following convention
adjust(ymd(20140404), 'mf', ausy)
#> [1] "2014-04-04"
# Shift dates
shift(ymd(20120229), months(1), 'u', ausy, FALSE) # one month
#> [1] "2012-03-29"
shift(ymd(20120229), months(1), 'mf', ausy, TRUE)  # one month with EOM rule
#> [1] "2012-03-30"
shift(ymd(20120229), years(1) + months(3), 'mf', ausy, TRUE)  # 1y3m
#> [1] "2013-05-31"
```

Schedules
---------

The preceding methods are used to generate schedules of dates required to define common financial contracts events such as cash flow exchange dates:

``` r
generate_schedule(effective_date = ymd(20120103), termination_date = ymd(20130103), 
  tenor = months(3), calendar = ausy, bdc = "mf", stub = "short_front", 
  eom_rule = FALSE)
#> [1] 2012-01-03 UTC--2012-04-03 UTC 2012-04-03 UTC--2012-07-03 UTC
#> [3] 2012-07-03 UTC--2012-10-03 UTC 2012-10-03 UTC--2013-01-03 UTC
```

Year fractions
--------------

Time lengths then usually need to be computed for each interval of such a schedule according to some day basis convention:

``` r
# 30/360us convention
year_frac(ymd("2010-03-31"), ymd("2012-03-31"), "30/360us")
#> [1] 2
# act/365 convention
year_frac(ymd("2010-02-28"), ymd("2012-03-31"), "act/365")
#> [1] 2.087671
```

More details can be found in the associated help files and the vignette (`vignette("dates", "fmdates")`)
