is_list_of <- function (object, class) {
  is.list(object) && Reduce(`&&`, Map(is, object, class))
}
