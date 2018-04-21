### Assignment 3

## Files

Basic explanation of the files

- ExprDiff.hs Contains the basic functions used to differentiate an expression (mostly finalized)
- ExprNorm.hs Various functions that help normalize/rewrite various expressions (mostly finalized)
- ExprParser.hs Functions to parse a string into an Expr Type (finalized)
- ExprTest.hs Various examples and tests (Currently working on)
- ExprType.hs Basic type for the Expression system (finalized)
- ExprUtil.hs External generic utility functions (finalized)


### ShortFalls

@ Curtis if you're marking this promptly at 12 AM then here's some excuses for some shortfalls

- Insufficient testing due to spending literally all my time trying to normalize expressions
- Messy code - between trying to test various things and layering fixes a lot of the code is a mess
- Lack of comments - see above

## Features

- Can perform partial differntiation of an expression
- Can simplify functions that do not contain cos,sin,ln,e^ to a normalized form (this is what took the entire week). Normalizes in a simplistic way, expanding addition within multiplication i.e. `(x+y)(x+z)` would be expanded then simplified to `x^2 + xy + xz + yz`. Note `(x+y)(x+y)` is simplified to `(x+y)^2` instead of expanding as expanding exponents is messy, especially if the exponent is a fractional number. Common terms are also grouped together i.e. `2xy + 9xy + 8 xyz` would be grouped to be `11xy + 8xyz`. Also orders things into a common order.
