
# I prefer haskell to fortran.

Na <- NA
True <- TRUE
Null <- NULL
False <- FALSE

deparse_to_string <- function (x) {

	if (missing(x)) {
		""
	} else {
		paste0(deparse(x), collapse = "\n")
	}	
}

stopf <- function (string, ...) {
	string <- paste0(string, collapse = '')
	values <- list(...)

	stringified <- lapply(values, function (value) {
		if (!is.character(value)) {
			deparse_to_string(value)
		} else {
			value
		}
	})

	message <- do.call(gettextf, c(list(string), stringified))
	stop(message, call. = False)
}

messagef <- function (string, ...) {
	string <- paste0(string, collapse = '')
	values <- list(...)

	stringified <- lapply(values, function (value) {
		if (!is.character(value)) {
			deparse_to_string(value)
		} else {
			value
		}
	})

	message <- do.call(gettextf, c(list(string), stringified))
	message (message, call. = False)
}

warningf <- function (string, ...) {

	string <- paste0(string, collapse = '')
	values <- list(...)

	stringified <- lapply(values, function (value) {
		if (!is.character(value)) {
			deparse_to_string(value)
		} else {
			value
		}
	})

	message <- do.call(gettextf, c(list(string), stringified))
	warning (message, call. = False)
}

is_boolean <- function (x) {
	is.logical(x) && !is.na(x)
}

