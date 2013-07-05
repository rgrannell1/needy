\name{require_a}
\alias{implemented_traits}
\alias{require_a}
\title{ensure a value has a desired set of traits.}
\usage{
  require_a(traits, value, pcall = NULL)

  implemented_traits()
}
\arguments{
  \item{traits}{a character vector, with each element being
  a space-seperated string of properties to test the value
  for. See "traits". required.}

  \item{value}{an arbitrary R object to test for certain
  properties. required.}

  \item{pcall}{an call or string that provides the call to
  be displayed when an error is thrown by require_a. See
  details. optional, defaults to displaying the call to
  require_a().}
}
\description{
  ensure a value has a desired set of traits.
}
\details{
  the option \code{pcall} is included so that it is
  possible to customise where the errors seem to originate
  from. for example,

  \code{myfunc <- function (x) require_a("integer", x,
  sys.call( sys.parent(1) )) }

  will display the following if called with a string "a":

  \code{Error: myfunc("a"): the value "a" didn\'t match any
  of the following compound traits: integer}

  In this example, the user-facing function \code{myfun} is
  shown to throw the error rather than an obscure inner
  function, making debugging easier. For cases in which
  working with the call stack directly (\code{sys.call()})
  is too difficult a string can be passed to \code{pcall},
  and this string is printed in front of the error message
}
\section{Traits}{
  The \code{traits} parameter is a character vector of
  whitespace-seperated traits. For example, the following
  are syntactically valid

  \code{"integer"}

  \code{"positive integer"}

  \code{c("positive integer", "na")}

  \code{c("na", "null", "length_one pairlist")}

  while the following are not

  \code{"positive && integer" # just use whitespace to
  'and' traits}

  \code{"positive || integer" # use two elements to 'or'
  traits}

  The latter two examples, correctly implemented, would be:

  \code{"positive integer"}

  \code{c("positive", "integer")}

  As suggested above, whitespace between traits is
  interpreted as "trait a AND trait b", while seperate
  elements are intepreted as \code{ c("trait one", OR
  "trait two") } the order of traits in a compound trait is
  not significant; a \code{"positive integer"} is
  equivelant to \code{"integer positive"}.

  If a test corresponding to an atomic trait is not found,
  an error is thrown:

  \code{require_a("white-whale", 1)}

  \code{Error: require_a("white-whale", 1): unrecognised
  trait(s): (white-whale)}

  similarily, if a value doesn't have any other desired
  compound traits then an error is thrown:

  \code{require_a(c("length_one list", "null"), 1)}

  \code{Error: require_a(c("length_one list", "null"), 1):
  the value 1 didn't match any of the following compound
  traits: length_one and list, or null'}
}
\examples{

safeMap <- function (f, x) {
	# map, with verification by require_a

	pcall <- "safeMap(f, x)"
	require_a('unary function', f, pcall)
	require_a('listy', x)

	Map(f, x)
}

safeSum <- function (a, b) {

	require_a( "finite numeric", a, sys.call(sys.parent()) )
	require_a( "finite numeric", b, sys.call(sys.parent()) )

	a + b
}
}

