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

A typical example where needy is useful is when trying to safeguard against
invalid inputs when defining a function, by defining what types of inputs it
*is* well defined for.

```IndMap``` maps a two-parameter function across a list x and x's indices 1, 2, ..., n.
It is only well defined if f is a binary (or variadic) function, and x is a list,
a vector, or a pairlist. [1]

```javascript
safeIndMap <- function (f, x) {
	
	pcall <- sys.call()
	require_a("binary function", f, pcall)
	require_a(c("vector", "pairlist"), x)
	Map(f, x, seq_along(x))
}
```

If ```safeIndMap``` is now called with a three-variable function, a descriptive error is thrown
showing the three-variable function. This error says that it was triggered by the fact
that f wasn't a binary function, which is a pretty clear error message.

```javascript
safeIndMap( function (a, b, c) a+b+c, 1:10 )

Error: safeIndMap(function(a, b, c) a + b + c, 1:10): 
	the value function (a, b, c) a + b + c didn't match any of the following compound traits:
	binary and function
```
We can be fairly confident now that if the user passes incorrect input to safeIndMap they 
should be able to figure out what went wrong quickly. 

For a full list of implemented traits, use the aptly named ```implemented_traits()```. As of version
0.1.1, the following traits are implemented.

```javascript
currently implemented traits:
 any, array, atomic, binary, boolean, call, character, closure, complex, data.frame, double, environment, expression, factor, false, finite, function, functionable, infinite, integer, language, length_one, length_three, length_two, length_zero, list, listy, logical, matrix, na, name, named, nan, nonnegative, null, nullary, numeric, object, pairlist, positive, primitive, raw, recursive, s4, string, symbol, table, ternary, true, unary, variadic, vector, whole
```
See the R documentation ```?require_a``` for more detailed usage information.

Sometimes is it more convenient to give a trait a value *cannot* have. For this reason
(as of version 0.2) traits can be negated.

```javascript
safeNotNull <- function (x) {
	
	pcall <-sys.call()
	require_a("!null", x, pcall)
	X
}
```

For a full list of implemented traits, use the aptly named ```implemented_traits()```. See
the R documentation ```?require_a``` for more detailed usage information.

### Alternatives

I wrote needy because it fits a use case I have very tidely; I have 
two large libraries (mchof and arrow, for those who are interested), and I needed
a way of reducing the amount of ```if (is.function(f)) stop()` boilerplate code,
and of standardising error messages. Needy ticks both boxes. Over times I will improve
this library substantially, but if this library doesn't fit your needs at the moment I recommend:

* [assertthat](https://github.com/hadley/assertthat)
* [assertive](http://cran.r-project.org/web/packages/assertive/index.html)

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

### Footnotes

[1] yes, pairlists aren't really used in R, but we might as well make our code as general as 
possible. The 'listy' trait is a shorthand for ```c("vector", "pairlist")```.
