### Assignment 3


[Documentation](https://chenc118.github.io/projects/Haskell.sMath/docs/index.html)

## Files

Basic explanation of the files

- ExprDiff.hs Contains the basic functions used to differentiate an expression (mostly finalized)
- ExprNorm.hs Various functions that help normalize/rewrite various expressions (mostly finalized)
- ExprParser.hs Functions to parse a string into an Expr Type (finalized)
- ExprTest.hs Various examples and tests (Currently working on)
- ExprType.hs Basic type for the Expression system (finalized)
- ExprUtil.hs External generic utility functions (finalized)

### Basic features

- Encoding of basic types, constants `Const` and variables `Var`
- Encoding of basic operations, addition `Add` and multiplication `Mult`
- Encoding of unary operations cosine `Cos`, sine `Sin`, ln `Ln`, natural exponent `NExp`
- Encoding of general exponents `Exp`
    - Note : There are not types for things like Negation, Division, secant, tan etc as they can all be expressed with these primitives making the simplification a bit easier
- Partial differentiation with respect to a variable
- Evaluation of expressions
- Simplification/partial normalization of functions
- A moderately robust parser

### Advanced Features aka parital normalization

1. Can partially normalize/rewrite a function
  - Common ordering of commutative operators
      - Addition is arranged according to the a custom sorting code found [here](https://github.com/chenc118/CS1XA3/blob/master/Assign3/ExprNorm.hs#L293).
      - Multiplication is arrange according to a custom instance declaration of Ordered found [here](https://github.com/chenc118/CS1XA3/blob/master/Assign3/ExprNorm.hs#L29)
  - Common terms in multiplication are grouped together i.e. `x^3 * x` normalizes to `x^4`
  - Common terms in addition are grouped together i.e. `2x + x` normalizes to `3x`
  - Addition is expanded out within multiplication i.e. `(x+y)(x+z)` normalizes to `x^2 + xy + xz + yz`
      - Note : there is an edge case where `(x+y)(x+y)` normalizes to `(x+y)^2` due to grouping common terms. It is not expanded due to how messy it will be for very large exponents
  - Exponents are distributed `(xy)^2` normalizes to `(x^2)(y^2)`
  - Exponents are normalized `x^2^2` normalizes to `x^4`
  - Multiplication within ln is expanded `ln(xy)` normalizes to `ln(x)+ln(y)`
  - Exponents within ln are brought down `ln(x^2)` normalizes to `2ln(x)`
  
  
  
  
### ShortFalls

- Insufficient testing due to spending literally all my time trying to normalize expressions
- Messy code - between trying to test various things and layering fixes a lot of the code is a mess
- Lack of comments - see above
