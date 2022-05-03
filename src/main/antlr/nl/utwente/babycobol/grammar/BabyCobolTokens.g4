lexer grammar BabyCobolTokens;

COMMENT: '*' ~('\r' | '\n')*? ('\r' | '\n') -> skip;

// Key-Words (case-sensitive)

/// Top-Level Constructs
ACCEPT: 'ACCEPT';
ADD: 'ADD';
ALTER: 'ALTER';
COPY: 'COPY';
DISPLAY: 'DISPLAY';
DIVIDE: 'DIVIDE';
EVALUATE: 'EVALUATE';
IF: 'IF';
LOOP: 'LOOP';
MOVE: 'MOVE';
MULTIPLY: 'MULTIPLY';
PERFORM: 'PERFORM';
SIGNAL: 'SIGNAL';
STOP: 'STOP';

/// Data Types
PICTURE: 'PICTURE';
OCCURS: 'OCCURS';
LIKE: 'LIKE';

/// Comparison
EQUALS: '=';
GT: '>';
LT: '<';
GE: '>=';
LE: '<=';

/// Arithmetic
PLUS: '+';
MINUS: ' - ';
TIMES_SYM: '*';
DIV: '/';
POW: '**';

/// Other keywords (usually used in combination with top-level constructs)
ADVANCING: 'ADVANCING';
AND: 'AND';
BY: 'BY';
DATA: 'DATA';
DELIMITED: 'DELIMITED';
DIVISION: 'DIVISION';
ELSE: 'ELSE';
END: 'END';
ERROR: 'ERROR';
FALSE: 'FALSE';
FILENAME: ALPHANUM+ '.' ALPHANUM+;
FROM: 'FROM';
GIVING: 'GIVING';
GO: 'GO';
HIGH_VALUES: 'HIGH-VALUES';
IDENTIFICATION: 'IDENTIFICATION';
IS: 'IS';
INTO: 'INTO';
LOW_VALUES: 'LOW-VALUES';
NEXT: 'NEXT';
NO: 'NO';
NOT: 'NOT';
OF: 'OF';
OFF: 'OFF';
ON: 'ON';
OR: 'OR';
OTHER: 'OTHER';
PROCEDURE: 'PROCEDURE';
PROCEED: 'PROCEED';
REMAINDER: 'REMAINDER';
REPLACING: 'REPLACING';
SENTENCE: 'SENTENCE';
SIZE: 'SIZE';
SPACE: 'SPACE';
SPACES: 'SPACES';
SUBTRACT: 'SUBTRACT';
THEN: 'THEN';
THROUGH: 'THROUGH';
TIMES: 'TIMES';
TO: 'TO';
TRUE: 'TRUE';
UNTIL: 'UNTIL';
VARYING: 'VARYING';
WHEN: 'WHEN';
WHILE: 'WHILE';
WITH: 'WITH';
XOR: 'XOR';

// For know skip ws
WS: [ \n\r\t]+ -> skip;

// Fragments
fragment ALPHANUM: (CHAR | [0-9]);
fragment CHAR: ([a-zA-Z] | '-');

PICTURE_REPRESENTATION: ([9AXZSV] ('(' [0-9]+ ')')?)+;
INTEGER: [0-9]+;
NAME: (ALPHANUM | '-')+;
STRING: '"' ~'"'* '"';
NO_DOT: ~'.';