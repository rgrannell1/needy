
trait_modifiers <- ( function () { 
	# create an environment containing trait-test modifiers.
	# these functions take trait tests, and modify them in some way.

	modify_with <- new.env(
		parent = emptyenv()
	)
	modify_with$'!' <-
		function (test) {
			function (value) {
				!test(value)
			}
		}
	modify_with$id_ <-
		function (test) {
			test
		}
	modify_with$list_of_ <-
		function (test)  {
			function (value) {

				if (!is.list(value)) {
					False
				} else {
					all( unlist(lapply(value, test)) )
				}
			}
		}
	modify_with$pairlist_of_ <-
		function (test)  {
			function (value) {

				if (!is.pairlist(value)) {
					False
				} else {
					all( unlist(lapply(value, test)) )
				}
			}
		}
	modify_with$listy_of_ <-
		function (test)  {
			function (value) {

				if (!(is.pairlist(value) || is.list(value) || is.vector(value))) {
					False
				} else {
					all( unlist(lapply(value, test)) )
				}
			}
		}

	modify_with$valid_modifiers <- ls(modify_with)
	modify_with

} )()

