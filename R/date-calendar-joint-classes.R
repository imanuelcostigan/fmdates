#' @include date-calendar-classes.R
NULL

#' @format A \code{\link{R6Class}} generator object
#' @rdname Calendar
#' @export
JointCalendar <- R6::R6Class(
  classname = "JointCalendar",
  inherit = Calendar,
  public = list(
    calendars = NA,
    rule = NA,
    is_good = function (dates) {
      m <- NROW(dates)
      n <- NROW(self$calendars)
      res <- matrix(nrow = m, ncol = n)
      for (i in 1:n)
        res[, i] = self$calendars[[i]]$is_good(dates)
      apply(res, 1, Reduce, f = self$rule)
    },
    initialize = function (calendars, rule = all) {
      assertthat::assert_that(is_list_of(calendars, "Calendar"))
      self$calendars <- calendars
      self$calendars <- calendars[!duplicated(self$locales())]
      self$rule <- rule
    },
    locales = function () {
      unlist(lapply(lapply(self$calendars, class), `[[`, 1))
    },
    print = function () {
      rule <- if (identical(self$rule, all)) "all" else "any"
      cat("<JointCalendar>", paste0(self$locales(), collapse=", "), "Rule:", rule, "\n")
    }
  )
)

#' @export
c.Calendar <- function (..., recursive = FALSE) {
  calendars <- list(...)
  n <- length(calendars)
  jc_list <- list()
  for (i in seq_along(calendars)) {
    is_jc <- is(calendars[[i]], "JointCalendar")
    new_cals <- if (is_jc) calendars[[i]]$calendars else calendars[[i]]
    jc_list <- c(jc_list, new_cals)
  }
  JointCalendar$new(jc_list)
}

#' @export
`[.Calendar` <- function (x, i) {
  JointCalendar$new(x$calendars[i], x$rule)
}
