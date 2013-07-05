
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

IndMap maps a two-parameter function across a list x and x's indices 1, 2, ..., n.
It is only well defined if f is a binary (or variadic) function, and x is a list,
a vector, or a pairlist. [1]

```
safeIndMap <- function (f, x) {
	
	pcall <- sys.call(sys.parent())
	require_a("binary function", f, pcall)
	require_a(c("vector", "pairlist"), x)

	Map(f, x, seq_along(x))
}
```

If safeIndMap is now called with a three-variable function, a descriptive error is thrown
showing the three-variable function. This error says that it was triggered by the fact
that f wasn't a binary function, which is a pretty clear error message.

```
safeIndMap( function (a, b, c) a+b+c, 1:10 )

Error: safeIndMap(function(a, b, c) a + b + c, 1:10): 
	the value function (a, b, c) a + b + c didn't match any of the following compound traits:
	binary and function
```

For a full list of implemented traits, use the aptly named implemented_traits().

[1] yes, pairlists aren't really used in R, but we might as well make our code as general as 
possible. The 'listy' trait is a shorthand for ```c("vector", "pairlist")```.