![workflow](https://github.com/supertom01/BabyANTLR/actions/workflows/maven.yml/badge.svg)

# One's Nightmare is Another's Demo
[BabyCobol](https://slebok.github.io/baby/) is a language that was designed to be the 
“[worst nightmare](https://doi.org/10.1145/3426425.3426933)” of compiler writers, deliberately combining 
worst features of the oldest and the ugliest programming languages ([YT](https://youtu.be/sSkIUTdfDjs)). 
Unfortunately, some of the most important software — that of banks, insurance and booking companies, military — is 
written in such languages, and in order to tackle them successfully, you need to train on smaller prey. BabyCobol is 
used in one of the CS MSc courses at UT, Software Evolution, where students treat it as a challenge and work towards its 
full implementation over the two months. For them, the goal is to implement as many features as possible, no matter the 
cost.

The goal of this project is different: take one language framework (good lists can be found on 
[this website](https://cocodo.github.io/) under "Technologies" tab or in 
[this paper](https://doi.org/10.1016/j.cl.2015.08.007) in §4), implement whatever you can in it and report on 
difficulties. Sounds easy, right? Well, perhaps all this talk about BabyCobol being a nightmare is just for show…

# Setup Instructions
1. This software requires [Apache Maven](https://maven.apache.org/) in order to function.  
2. With `mvn package` the tests are executed and a JAR file will be packaged. 
3. This JAR can then be used to run the parser over source files.

# Thesis
The final thesis itself be found on [essay.utwente.nl](https://essay.utwente.nl/91706/)

# BabyCobol Language
Beneath one will find a quick and very brief language reference of the BabyCobol language.

## Syntax
### Position-Based Syntax
| Column(s) | Description                |
|----------:|:---------------------------|
|     1 - 6 | Sequence number            |
|         7 | Line status indicator      |
|         8 | Start top-level constructs |
|   12 - 72 | Regular code               |
|       73+ | Ignored                    |

### Line Continuation
Since the language is designed to be fitted on old-fashioned punch cards, it might be the case that lines are longer 
than that they can physically fit on a given card. To indicate that a line is continued, the next line contains a
marker, a dash, in the 7th column. Indicating that it is a continuation from the previous line.

### Case Sensitivity
BabyCobol is case-*in*sensitive. However, there is one edge-case. Keywords should always be uppercase. 

### Non-reserved Keywords
Keywords are not reserved in the language and can also be used as variables, this would make the following statement
completely valid: 

```
␣␣␣␣␣␣␣␣␣␣05 END PICTURE IS 99.
```

Even though `END` is a reserved keyword.

### White Space
White spaces are ignored, but spaces are required around the - operator when used as an infix arithmetic operator.

### Lexical Imports
BabyCobol has support for verbatim imports. With the `COPY` directive these are instantly put in the file, if a compiler
error occurs, then these are shown inside the file itself and not the location into which it was copied.

Additionally, a `REPLACE` directive allows to replace a string while copying a code file. 

### Code Structure
A program consists out of *divisions*, which each consist out of *sections*. In total there are three divisions:
1. The `IDENTIFICATION_DIVISION`
   1. Contains basic program information, no real required names and values are however required.
2. The `DATA_DIVISION`
   1. Narrowed down to two basic data types
3. The `PROCEDURE_DIVISION`
   1. Mostly identical to COBOL.
   2. Consists out of arbitrarily named sections, which have *paragraphs*. Both sections and paragraphs are named and 
      callable. A paragraph consists out of *sentences* which in turn has *statements*.

#### Statement separators
Statements do not have to be, but *can* be separated by a period (`.`). 

#### Jumps
With the directive `NEXT SENTENCE` one jumps immediately to the next sentence.

## Semantics
### Data Types
There are three data types: picture clauses, occurs clauses and like clauses.

#### Picture Clause
Picture clauses are primitive type definitions which revolve around a pattern for storage and display value. For
example, the field `VAR` is defined as `VAR PICTURE IS $999V99` will have five decimal places, with each time printed 
a dollar sign as first symbol, 3 digits either a comma or period (depending on locale) and 2 more digits. The following
special characters are used in BabyCobol for such formatting:

- `9` - Any digit
- `A` - An alphabet character or a space
- `X` - Any single character
- `Z` - A leading digit, disappearing into space if zero
- `S` - A sign (+ or -, space is treated as a plus)
- `V` - A decimal separator (usually . or ,)

Any other characters are used verbatim. Any number of consequenent characters is can be replaced as `C(N)` where `C` is 
the character and `N` the amount of repetitions. Hence `999999V99` is equal to `9(6)V9(2)`, and `X(20)` is any string 
that is 20 characters long.

#### Occurs Clause
Occurs clauses are used to define arrays, the code fragment below creates an array with length 20. And puts a single
picture element in it. To reference the element back, one would have to use `ELEMENT OF ARRAY(1)`. 

```
01 ARRAY OCCURS 20.
   03 ELEMENT PICTURE IS 9(15)V99.
```

#### Like Clause
Like clauses allows to reuse the type of a variable that was already defined. In the example 
`03 NEW-FIELD LIKE ANOTHER-FIELD.` The `NEW-FIELD` variable will reuse the type of `ANOTHER-FIELD`.
A note, `OCCURS` is not treated as an essential part of the type, so using `LIKE` on `OCCURS` 

#### Arithmetic Operations
Arithmetic operations may only be performed on atomic fields, which pictures are free of any characters (so no `X` or 
`A`) fields. The same can be said for infix operators, expect for `+`, where it can be used to append strings. 
Comparisons work symbol by symbol, space-padding the shortest of the values on the left.

### Qualification
Qualified variable names can grow long, hence it is possible to only communicate a part of the path towards the field, 
the rest is automatically inferred. If a datastructure would exist where the field `FOO` has a field `BAR` which has a 
field `F`, then the full name would be `F OF BAR OF FOO`. If there wouldn't be any other competing `F`s then just the 
`F` could be enough. Or `F OF FOO`, where `BAR` is inferred. Or `F OF BAR` where the root is inferred.

If ambiguity arises, the type and context information will be used. If still ambiguity remains a proper error message 
is provided.

### Figurative Constants
BabyCobol implements three figurative constants: `SPACES`, `LOW-VALUES` and `HIGH-VALUES`. Where `MOVE SPACES X` would 
set all spaces in the `X` variable, and for every inner layer if this is a compound data structure. `LOW-VALUES` puts 
everything in the lowest possible value, so `X PICTURE IS 99`, would become `0`. But `X PICTURE IS S999` would be 
`-999`. 

### Default Values
If a unknown variable is used, then it will be instantiated with its own upper-case name. So `ACCOUNT-NAME` will become 
`"ACCOUNT-NAME"`. Where the inferred data type is `PICTURE IS X(N)`, where `N` is the length of the variable name. 

### Conditional Contractions
One can write `IF X = 2 OR 4`, instead of `IF X = 2 or X = 4`. Operator priorities work normally so 
`IF X = 42 OR 3 AND Y < X` is equal to `IF X = 42 OR (3 AND Y < X)`.

## Semantics of Statements
### Unconditional go to
The `GO TO` statement unconditionally jumps to a different procedure. The `ALTER` statement will update a `GO TO`, to 
jump to another location.

### Perform
`PERFORM` calls a procedure and returns to continue execution on the next statement. It supports a way to call several 
subsequently written paragraphs as one.

### Loop
`LOOP` will execute a single statement multiple times, and can act as a while, for or do-while loop. The clauses for
this loop do not have to occur at the start, but can appear at any point in the body.

### Name-Driven Assignment
`MOVE A TO B` will move through all the fields of `A` and map all the values from `A` to the name-matching fields in `B`
discarding the rest. Move is also used in [Figurative Constants](#figurative-constants).

### Exception Handling
The `SIGNAL` keyword allows for one error handler and catches all runtime errors.
