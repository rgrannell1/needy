\name{needs_a}
\alias{needs_a}
\title{ensure a value has a desired set of traits.}
\usage{
  needs_a(traits, value, pcall = NULL)
}
\arguments{
  \item{traits}{a character vector, with each element being
  a space-seperated string of properties to test the value
  for. See "traits". required.}

  \item{pcall}{an call or string that provides the call to
  be displayed when an error is thrown by needs_a. See
  details. optional.}
}
\description{
  ensure a value has a desired set of traits.
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

  \code{needs_a("white-whale", 1)}

  \code{Error: needs_a("white-whale", 1): unrecognised
  trait(s): (white-whale)}

  similarily, if a value doesn't have any other desired
  compound traits then an error is thrown:

  \code{needs_a(c("length_one list", "null"), 1)}

  \code{Error: needs_a(c("length_one list", "null"), 1):
  the value 1 didn't match any of the following compound
  traits: length_one and list, or null'}
}

