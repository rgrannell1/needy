needy
======

needy is a small utility library designed to make testing function
inputs less difficult. R is a dynamically typed language, but larger
projects need input checking for scalabity. 

needy offers a single function, ```require_a( )```, which lets you specify
the traits an input object should have, such as class, size,
numerical properties or number of parameters, while reducing boilerplate
code and aiding debugging.

### Usage

```require_a``` is primarily designed to reduce the amount of boilerplate 
that must be written to type-check functions. 

```javascript
Map2 <- function (f, xs) {
		
	require_a('unary function', f, xs)
	require_a('listy', xs)

	res <- vector('list', length(xs))
	for (ith in seq_along(xs)) {
		res[[ith]] <- f( xs[[ith]] )
	}
	res
}
Map2( function (a, b) a + b, 1:3 )
```
```
Error: require_a("unary function", f, xs): 
	the value function (a, b) a + b didn't match any of the following compound traits:
	unary and function
```

For a full list of implemented traits and trait modifiers,
use the aptly named ```implemented_traits()``` and ```implemented_modifiers()```.
As of version 0.3, the following traits

```
*, any, arbitary, array, atomic, binary, boolean, call, 
character, closure, complex, collection, data.frame, double, environment,
expression, factor, false, finite, function, functionable,
infinite, integer, language, length_one, length_three, length_two,
length_zero, list, listy, logical, matrix, na, name, named, nan, nonnegative,
null, nullary, number, numeric, object, pairlist, positive, primitive, raw,
recursive, s4, string, symbol, table, ternary,
true, unary, variadic, vector, whole
```

and the following trait modifiers

```
!, collection_of_, id_, list_of_, listy_of_, pairlist_of_
```

are implemented.

See the R documentation ```?require_a``` for more detailed usage information.

### Future Development

* more informative error messages: if a trait doesn't match an expectation
then a list of traits that did match will be given.

* better value deparsing: all values will be stringified using a 
custom pretty-printing library I am currently developing.

### Alternatives

I wrote needy to fit a specific use case; I have 
two large libraries (mchof and arrow, for those who are interested), and I needed
a way of reducing the amount of ```if (is.function(f)) stop()` boilerplate code,
and of standardising error messages. I find that needy works well for me, but
if this library doesn't fit your needs at the moment I recommend:

* [https://github.com/hadley/assertthat](assertthat)
* [http://cran.r-project.org/web/packages/assertive/index.html](assertive)

Assertive is currently the more mature of the two libraries, and falls more into
the category of data validation (checking if data are email addresses, hex colours, ...). Assertthat
seems to be more general, but it isn't available on CRAN currently (July 5th 2013).

### License

The MIT License

Copyright (c) 2013 Ryan Grannell

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
