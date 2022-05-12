grammar BabyCobolParser;

options {caseInsensitive = true;}

program: identificationDivision (dataDivision)? (procedureDivision)?;

//TODO: Include copy instruction support
identificationDivision: IDENTIFICATION DIVISION '.' (identifying_val '.' identifying_val '.')+;

dataDivision: DATA DIVISION '.' (declaration '.')+;

declaration: INTEGER identifier ((PICTURE IS PICTURE_REPRESENTATION) | (LIKE identifier))? (OCCURS INTEGER TIMES)?;

procedureDivision: PROCEDURE DIVISION '.' sentence* paragraph+ STOP?;

paragraph: identifier '.' sentence+;

sentence: statement+ '.';

statement: ACCEPT identifier+                                                       #AcceptStatement
//         | ADD atomic+ TO atomic (GIVING identifier)?                               #AddStatement
         | ALTER procedureName TO PROCEED TO procedureName                          #AlterStatement
         | COPY FILENAME (REPLACING replaceExpression+)?                            #CopyStatement
         | DISPLAY displayExpression* (WITH NO ADVANCING)?                          #DisplayStatement
//         | DIVIDE atomic INTO atomic+ (GIVING identifier)? (REMAINDER identifier)?  #DivideStatement
//         | EVALUATE anyExpression (whenBlock statement+)* END                       #EvaluateStatement
         | GO TO procedureName                                                      #GoToStatement
         | IF booleanExpression THEN statement+ (ELSE statement+)? END?             #IfStatement
         | LOOP loopBody* END                                                       #LoopStatement
         | MOVE moveExpression TO identifier+                                       #MoveStatement
//         | MULTIPLY atomic BY atomic+ (GIVING identifier)?                          #MultiplyStatement
         | NEXT SENTENCE                                                            #NextSentenceStatement
         | PERFORM procedureName (THROUGH procedureName)? (atomic TIMES)?           #PerformStatement
         | SIGNAL (OFF | procedureName) ON ERROR                                    #SignalStatement
         | STOP                                                                     #StopStatement
//         | SUBTRACT atomic+ FROM atomic (GIVING identifier)?                        #SubtractStatement
         | atomicExpression                                                         #AtomicStatement
         ;

atomicExpression: ADD atomic+ TO atomic (GIVING identifier)?                                #addExpression
                | DIVIDE atomic INTO atomic+ (GIVING identifier)? (REMAINDER identifier)?   #divideExpression
                | EVALUATE anyExpression (whenBlock statement+)* END                        #evaluateExpression
                | MULTIPLY atomic BY atomic+ (GIVING identifier)?                           #multiplyExpression
                | SUBTRACT atomic+ FROM atomic (GIVING identifier)?                         #substractExpression
                ;

replaceExpression: COPY_QUOTE literal COPY_QUOTE BY COPY_QUOTE literal COPY_QUOTE;

displayExpression: atomic (DELIMITED BY (SPACE | SIZE | literal))?;

moveExpression: atomic
              | HIGH_VALUES
              | LOW_VALUES
              | SPACES
              ;

whenBlock: WHEN atomic+
         | WHEN OTHER
         ;

anyExpression: arithmeticExpression
             | stringExpression
             | booleanExpression
             ;

arithmeticExpression: atomic
                    | arithmeticExpression arithmeticOperator arithmeticExpression
                    ;

stringExpression: atomic
                | stringExpression '+' stringExpression
                ;

booleanExpression: TRUE
                 | FALSE
                 | arithmeticExpression comparisonOperator arithmeticExpression
                 | NOT booleanExpression
                 | booleanExpression booleanOperator booleanExpression
                 ;

loopBody: VARYING identifier? (FROM atomic)? (TO atomic)? (BY atomic)?          #VaryingCondition
        | WHILE booleanExpression                                               #WhileCondition
        | UNTIL booleanExpression                                               #TillCondition
        | statement                                                             #GIVEANICENAME
        ;

comparisonOperator: EQUALS | GT | LT | GE | LE;

booleanOperator: OR | AND | XOR;

arithmeticOperator: PLUS | MINUS | TIMES_SYM | DIV | POW;

atomic: atomicExpression | INTEGER | identifier | STRING;

procedureName: sectionName | paragraphName | identifier;
sectionName: identifier;
paragraphName: identifier;

identifier: NAME                    //#nameIdentifier
          | NAME '(' INTEGER ')'    //#arrayIndexIdentifier
          | NAME OF identifier      //#quantifiedIdentifier
          | ACCEPT | ADD | ALTER | COPY | DISPLAY | DIVIDE | EVALUATE | IF | LOOP | MOVE | MULTIPLY | PERFORM | SIGNAL
          | STOP | PICTURE | OCCURS | LIKE | ADVANCING | AND | BY | DATA | DELIMITED | DIVISION | ELSE | END | ERROR
          | FALSE | FILENAME | FROM | GIVING | GO | HIGH_VALUES | IDENTIFICATION | IS | INTO | LOW_VALUES | NEXT | NO
          | NOT | OF | OFF | ON | OR | OTHER | PROCEDURE | PROCEED | REMAINDER | REPLACING | SENTENCE | SIZE | SPACE
          | SPACES | SUBTRACT | THEN | THROUGH | TIMES | TO | TRUE | UNTIL | VARYING | WHEN | WHILE | WITH | XOR
          ;


literal: STRING;

identifying_val: (NO_DOT | identifier | literal) (identifying_val)?;

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
COPY_QUOTE: '===';
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

WS: [ \n\r\t]+ -> channel(HIDDEN);

// Fragments
fragment ALPHANUM: (CHAR | [0-9]);
fragment CHAR: ([a-z] | '-');

PICTURE_REPRESENTATION: ([9AXZSV] ('(' INTEGER ')')?)+ ; // FIXME: Should also allow other chars -> put down verbatim!
INTEGER: [0-9]+;
NAME: (ALPHANUM | '-')+;
STRING: '"' ~'"'* '"';
NO_DOT: ~'.';