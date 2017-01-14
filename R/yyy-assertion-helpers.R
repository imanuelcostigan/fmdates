assertthat::on_failure(is_valid_bdc) <- function (call, env) {
  paste0(deparse(call$bdc), " contains invalid business day convention.")
}

assertthat::on_failure(is_valid_day_basis) <- function(call, env) {
  paste0(deparse(call$bdc), " contains invalid business day convention.")
}

assertthat::on_failure(is_list_of) <- function(call, env) {
  paste0("All elements of ", deparse(call$object), " are not objects of class ",
    deparse(call$class), ".")
}