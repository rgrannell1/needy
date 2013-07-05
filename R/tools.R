
deparse_to_string <- function (x) {
	paste0(deparse(x), collapse = "")
}

stopf <- function (string, ...) {
	string <- paste0(string, collapse = '')
	stop(gettextf(string, ...), call.=FALSE)
}
messagef <- function (string, ...) {
	string <- paste0(string, collapse = '')
	message(gettextf(string, ...), call.=FALSE)
}
warningf <- function (string, ...) {
	string <- paste0(string, collapse = '')
	warning(gettextf(string, ...), call.=FALSE)
}

is_boolean <- function (x) {
	is.logical(x) && !is.na(x)
}
