grammar  BabyCobolNoKeyword;

options {
    caseInsensitive = true;
}

program: identificationDivision (dataDivision)? (procedureDivision)?;

identificationDivision: IDENTIFICATION_DIVISION '.' (identificationDeclaration '.' identificationDeclaration '.')+;

identificationDeclaration: ~('.' | DATA_DIVISION | PROCEDURE_DIVISION)+;

dataDivision: DATA_DIVISION '.' (declaration '.')+;

declaration: INTEGER ID typeDeclaration;

typeDeclaration: ((PICTURE IS picture_repr) | (LIKE identifier))? (OCCURS INTEGER TIMES)?;

procedureDivision: PROCEDURE_DIVISION '.' sentence* paragraph+ STOP?;

paragraph: paragraphName '.' sentence+;

sentence: statement+ '.';

statement: ACCEPT identifier+                                                       #AcceptStatement
         | ALTER procedureName TO_PROCEED_TO procedureName                          #AlterStatement
         | DISPLAY displayExpression+ (WITH_NO_ADVANCING)?                          #DisplayStatement
         | GO_TO procedureName                                                      #GoToStatement
         | IF booleanExpression thenExpression elseExpression? END?                 #IfStatement
         | LOOP loopBody+ END                                                       #LoopStatement
         | MOVE moveExpression TO identifier+                                       #MoveStatement
         | NEXT_SENTENCE                                                            #NextSentenceStatement
         | PERFORM procedureName (THROUGH procedureName)? (atomic TIMES)?           #PerformStatement
         | SIGNAL (OFF | procedureName) ON_ERROR                                    #SignalStatement
         | STOP                                                                     #StopStatement
         | atomicExpression                                                         #AtomicStatement
         ;

atomicExpression: ADD atomic+ TO atomic (GIVING identifier)?                                #addExpression
                | DIVIDE atomic INTO atomic+ (GIVING identifier)? (REMAINDER identifier)?   #divideExpression
                | EVALUATE anyExpression caseExpression* END                                #evaluateExpression
                | MULTIPLY atomic BY atomic+ (GIVING identifier)?                           #multiplyExpression
                | SUBTRACT atomic+ FROM atomic (GIVING identifier)?                         #substractExpression
                ;

displayExpression: atomic (DELIMITED_BY (SPACE | SIZE | literal))?;

thenExpression: THEN statement+;

elseExpression: ELSE statement+;

moveExpression: atomic
              | HIGH_VALUES
              | LOW_VALUES
              | SPACES
              ;

caseExpression: whenBlock statement+;

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
                 | arithmeticExpression (comparisonOperator arithmeticExpression)?
                 | NOT booleanExpression
                 | booleanExpression booleanOperator booleanExpression
                 ;

loopBody: VARYING identifier? (FROM atomic)? (TO atomic)? (BY atomic)?          #VaryingCondition
        | WHILE booleanExpression                                               #WhileCondition
        | UNTIL booleanExpression                                               #TillCondition
        | statement                                                                                #NoCondition
        ;

comparisonOperator: EQUALS | GT | LT | GE | LE
                  ;

booleanOperator: OR
               | AND
               | XOR
               ;

arithmeticOperator: PLUS | MINUS | TIMES_SYM | DIV | POW;

atomic: atomicExpression | INTEGER | identifier | STRING;

procedureName: ID;

paragraphName: ID;

identifier: ID '(' INTEGER ')' #arrayIndexIdentifier
          | ID OF identifier   #quantifiedIdentifier
          | ID                 #nameIdentifier
          ;

picture_repr: (~'.')+;

literal: STRING;

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
COPY_QUOTE: '===';
FILENAME: ALPHANUM+ '.' ALPHANUM+;

STRING: '"' ~'"'* '"';

ACCEPT:     'ACCEPT';
ADD:        'ADD';
ALTER:      'ALTER';
DISPLAY:    'DISPLAY';
DIVIDE:     'DIVIDE';
EVALUATE:   'EVALUATE';
IF:         'IF';
LOOP:       'LOOP';
MOVE:       'MOVE';
MULTIPLY:   'MULTIPLY';
PERFORM:    'PERFORM';
SIGNAL:     'SIGNAL';
STOP:       'STOP';

PICTURE:    'PICTURE';
OCCURS:     'OCCURS';
LIKE:       'LIKE';

AND:        'AND';
BY:         'BY';
DELIMITED_BY: 'DELIMITED BY';
ELSE:       'ELSE';
END:        'END';
FALSE:      'FALSE';
FROM:       'FROM';
GIVING:     'GIVING';
GO_TO:      'GO TO';
HIGH_VALUES: 'HIGH-VALUES';
IS:         'IS';
INTO:       'INTO';
LOW_VALUES: 'LOW-VALUES';
NEXT_SENTENCE: 'NEXT SENTENCE';
NOT:        'NOT';
OFF:        'OFF';
OF:         'OF';
ON_ERROR:   'ON ERROR';
OR:         'OR';
OTHER:      'OTHER';
REMAINDER:  'REMAINDER';
SIZE:       'SIZE';
SPACES:     'SPACES';
SPACE:      'SPACE';
SUBTRACT:   'SUBTRACT';
THEN:       'THEN';
THROUGH:    'THROUGH';
TIMES:      'TIMES';
TO_PROCEED_TO: 'TO PROCEED TO';
TO:         'TO';
TRUE:       'TRUE';
UNTIL:      'UNTIL';
VARYING:    'VARYING';
WHEN:       'WHEN';
WHILE:      'WHILE';
WITH_NO_ADVANCING: 'WITH NO ADVANCING';
XOR:        'XOR';

IDENTIFICATION_DIVISION: 'IDENTIFICATION DIVISION';
DATA_DIVISION: 'DATA DIVISION';
PROCEDURE_DIVISION: 'PROCEDURE DIVISION';

ID: (CHAR | '-') (CHAR | INT | '-')*;
INTEGER: INT+;

// Skipped and ignored tokens
WS: [ \t\r\n]+ -> skip;
IGNORED: .;

// Fragments
fragment ALPHANUM: (CHAR | [0-9]);
fragment CHAR: [A-Z];
fragment INT: [0-9];