
trait_tests <- ( function () { 
	# create a hash table containing 
	# trait-testing functions

	lookup <- new.env(parent = emptyenv())
	
	# useful for the case in which no type 
	# checking is needed, but the value should actually
	# exist
	lookup$any = 
		function (value) TRUE

	# (mostly) builtin functions,
	# tht mostly test the class of the object
	lookup$array = is.array
	lookup$atomic =  is.atomic
	lookup$call =  is.call
	lookup$character = is.character
	lookup$complex =  is.complex
	lookup$data.frame =  is.data.frame
	lookup$double = is.double
	lookup$empty.model =  is.empty.model
	lookup$environment = is.environment
	lookup$expression =  is.expression
	lookup$factor = is.factor
	lookup$finite =  is.finite
	lookup$'function' = is.function
	lookup$infinite =  is.infinite
	lookup$integer = is.integer
	lookup$language = is.language
	lookup$list = is.list
	lookup$logical =  is.logical
	lookup$matrix =  is.matrix
	lookup$na = is.na
	lookup$name = is.name
	lookup$nan = is.nan
	lookup$null = is.null
	lookup$numeric =  is.numeric
	lookup$object = 
		function (value) {
			# a decent test for objectness
			!is.null(attr(value, 'class'))
		}
	lookup$ordered = is.ordered
	lookup$pairlist =is.pairlist
	lookup$primitive = is.primitive
	lookup$raw = is.raw
	lookup$recursive = is.recursive
	lookup$s4 = isS4
	lookup$single =is.single
	lookup$symbol =is.symbol
	lookup$true = isTRUE
	lookup$table =is.table
	lookup$unsorted =is.unsorted
	lookup$vector = is.vector

	# tests I find useful
	lookup$closure = 
		function (value) {
			is.function(value) && !is.primitive(value)
		}
	lookup$whole = 
		function (value) {
			is.numeric(value) && 
			abs(round(value) - value) < .Machine$double.eps
		}
	lookup$positive = 
		function (value) {
			is.numeric(value) && value > 0
		}
	lookup$nonnegative =
		function (value) {
			is.numeric(value) >= 0
		}
	lookup$named = 
		function (value) {

			( is.vector(value) || is.pairlist(value) ) &&
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

	lookup$unary = 
		function (value) {

		}
	lookup$binary =
		function (value) {

		}
	lookup$trinary = 
		function (value) {

		}

	lookup$listed_traits <- ls(lookup)
	lookup
} )()
