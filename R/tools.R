
#' @export

deparse_to_string <- function (x) {
	paste0(deparse(x), collapse = "")
}

#' @export

stopf <- function (string, ...) {
	stop(gettextf(string, ...), call.=FALSE)
}