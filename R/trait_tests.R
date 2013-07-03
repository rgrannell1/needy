 
trait_tests <- ( function () { 
	# create an environment containing trait-
	# testing functions, for speed of access.

	lookup <- new.env(parent = emptyenv())
	
	lookup$any = function (value) TRUE

	# (mostly) builtin functions,
	# tht mostly test the class of the object
	lookup$array = is.array
	lookup$atomic = is.atomic
	lookup$call =  is.call
	lookup$character = is.character
	lookup$complex =  is.complex
	lookup$data.frame =  is.data.frame
	lookup$double = is.double
	lookup$environment = is.environment
	lookup$expression =  is.expression
	lookup$factor = is.factor
	lookup$finite =  
		function (value) {
			is.numeric(value) && is.finite(value)
		}
	lookup$'function' = is.function
	lookup$infinite =  
		function (value) {
			is.numeric(value) && is.infinite(value)
		}
	lookup$integer = is.integer
	lookup$language = is.language
	lookup$list = is.list
	lookup$logical = is.logical
	lookup$matrix = is.matrix
	lookup$na = 
		function (value) {
			is.vector(value) && 
			!is.expression(value) && is.na(value)
		}
	lookup$name = is.name
	lookup$nan = 
		function (value) {
			is.numeric(value) && is.nan(value)
		}
	lookup$null = is.null
	lookup$numeric = is.numeric
	lookup$object = 
		function (value) {
			# a decent test for objectness
			!is.null(attr(value, 'class'))
		}
	lookup$pairlist = is.pairlist
	lookup$primitive = is.primitive
	lookup$raw = is.raw
	lookup$recursive = is.recursive
	lookup$s4 = isS4
	lookup$symbol = is.symbol
	lookup$true = isTRUE
	lookup$table = is.table
	lookup$vector = is.vector

	# tests I find useful
	lookup$false = 
		Negate(isTRUE)
	lookup$closure = 
		function (value) {
			is.function(value) && !is.primitive(value)
		}
	lookup$whole = 
		function (value) {
			is.numeric(value) && 
			is.finite(value) &&
			( is.integer(value) || 
			(abs(round(value) - value) < .Machine$double.eps) )
		}
	lookup$positive = 
		function (value) {
			is.numeric(value) && !is.nan(value) && value > 0
		}
	lookup$nonnegative =
		function (value) {
			is.numeric(value) && !is.nan(value) && value >= 0
		}
	lookup$named = 
		function (value) {

			( is.vector(value) || is.pairlist(value) ) &&
			!is.expression(value) &&
			if (length(value) > 0) {
				all(nchar(names(value)) > 0)
			} else TRUE
		}
	lookup$boolean =
		function (value) {
			is.logical(value) && !is.na(value)
		}
	lookup$string = 
		function (value) {
			is.character(value) && length(character) == 1
		}
	lookup$listy = 
		function (value) {
			is.vector(value) || is.list(value) || is.pairlist(value)
		}

	# quantifiers
	lookup$length_zero = 
		function (value) {
			length(value) == 0
		}
	lookup$length_one = 
		function (value) {
			length(value) == 1
		}
	lookup$length_two = 
		function (value) {
			length(value) == 2
		}
	lookup$length_three = 
		function (value) {
			length(value) == 3
		}
	xParams <- function (f) {
		if (is.primitive(f)) {
			head( as.list(args(f)), -1 )
		} else {
			formals(f)
		}
	}
	lookup$nullary = 
		function (value) {
			if (!is.function (value)) {
				return (FALSE)
			}

			params <- xParams(value)

			if ("..." %in% names(params)) TRUE else {
				length(params) == 0
			}
		}
	lookup$unary =  
		function (value) {
			if (!is.function (value)) {
				return (FALSE)
			}

			params <- xParams(value)

			if ("..." %in% names(params)) TRUE else {
				length(params) == 1
			}
		}
	lookup$binary =
		function (value) {
			if (!is.function (value)) {
				return (FALSE)
			}

			params <- xParams(value)

			if ("..." %in% names(params)) TRUE else {
				length(params) == 2
			}
		}
	lookup$ternary = 
		function (value) {
			if (!is.function (value)) {
				return (FALSE)
			}

			params <- xParams(value)

			if ("..." %in% names(params)) TRUE else {
				length(params) == 3
			}
		}

	lookup$valid_traits <- ls(lookup)
	lookup

} )()
