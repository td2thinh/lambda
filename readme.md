# Lambda Calculus Interpreter

This project aims to develop a somewhat sotisphicated Lambda Calculus based language.

# ROAD MAP: 

1. Interpreting pure Lambda Calculus:
- alpha conversion
- left to rightm call by value Evaluation
- normalisation using LtR-CbV
- encode I, δ, Ω, S, S K K, S I I, 0, 1, 2, 3, arithmetic operations
- tests

2. Simple Types
3. Polymorphed Lambda Calculus with more Types
4. Imperative features

# SOURCES: 
- Free and Bound Variables codes: https://github.com/kmicinski/cmsc330examples
- Dune project example, alcotest: https://github.com/mjambon/dune-starter
- CoreLib.LambdaUtils.alpha_equal is copied from Rachid BOUHMAD
- Encoding arithmetic and church integers found at wiki: https://en.wikipedia.org/wiki/Lambda_calculus#Encoding_datatypes
- Idea to improve substitution method found in : https://github.com/kmicinski/cmsc330examples as I was having some problem testing Church numerals, I found out that I need to check for free variables and alpha rename the variables in the substituting term in case there is variable capture (ie. variables of the same name but not the same) 