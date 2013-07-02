
property_tests <- ( function () { 
	# create a hash table containing 
	# property-testing functions

	lookup <- new.env(parent = emptyenv())
	lookup$any = 
		function (value) TRUE
	lookup$array =
		is.array
	lookup$atomic = 
		is.atomic
	lookup$call = 
		is.call
	lookup$character =
		is.character
	lookup$integer =
		is.integer

	# object related predicates
	lookup$object = 
		function (value) {
			!is.null(attr(value, 'class'))
		}
	lookup$s4 = 
		isS4


	lookup$listed_properties <- ls(lookup)
	lookup
} )()




