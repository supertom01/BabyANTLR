grammar  BabyCobol;

options {
    caseInsensitive = true;
}

@parser::header {
import java.util.Set;
import java.util.HashSet;
}

@parser::members {
private Set<String> identifiers = new HashSet<>();

public void addIdentifier(String identifier) {
    if (identifier == null) {
        return;
    }
    identifier = identifier.toLowerCase();
    identifier = identifier.replace(" ", "");
    identifiers.add(identifier);
}

public boolean isIdentifier() {
    String text = getCurrentToken().getText();
    text = text.replace(" ", "");
    boolean result = identifiers.contains(text.toLowerCase()) && !text.toUpperCase().equals(text);
    return result;
}
}

program: identificationDivision (dataDivision)? (procedureDivision)?;

identificationDivision: IDENTIFICATION_DIVISION '.' (identificationDeclaration '.' identificationDeclaration '.')+;

identificationDeclaration: ~('.' | DATA_DIVISION | PROCEDURE_DIVISION)+;

dataDivision: DATA_DIVISION '.' (declaration '.')+;

declaration: INTEGER (ID | keywords {addIdentifier($keywords.text);}) typeDeclaration;

typeDeclaration: ((PICTURE IS picture_repr) | (LIKE identifier))? (OCCURS INTEGER TIMES)?;

procedureDivision: {!isIdentifier()}? PROCEDURE_DIVISION '.' sentence* paragraph+ STOP?;

paragraph: paragraphName '.' sentence+;

sentence: statement+ '.';

statement: {!isIdentifier()}? ACCEPT identifier+                                                       #AcceptStatement
         | {!isIdentifier()}? ALTER procedureName TO_PROCEED_TO procedureName                          #AlterStatement
         | {!isIdentifier()}? DISPLAY displayExpression+ (WITH_NO_ADVANCING)?                          #DisplayStatement
         | {!isIdentifier()}? GO_TO procedureName                                                      #GoToStatement
         | {!isIdentifier()}? IF booleanExpression thenExpression elseExpression? END?                 #IfStatement
         | {!isIdentifier()}? LOOP loopBody+ END                                                       #LoopStatement
         | {!isIdentifier()}? MOVE moveExpression TO identifier+                                       #MoveStatement
         | {!isIdentifier()}? NEXT SENTENCE                                                            #NextSentenceStatement
         | {!isIdentifier()}? PERFORM procedureName (THROUGH procedureName)? (atomic TIMES)?           #PerformStatement
         | {!isIdentifier()}? SIGNAL (OFF | procedureName) ON_ERROR                                    #SignalStatement
         | {!isIdentifier()}? STOP                                                                     #StopStatement
         | atomicExpression                                                                            #AtomicStatement
         ;

atomicExpression: {!isIdentifier()}? ADD atomic+ TO atomic (GIVING identifier)?                                #addExpression
                | {!isIdentifier()}? DIVIDE atomic INTO atomic+ (GIVING identifier)? (REMAINDER identifier)?   #divideExpression
                | {!isIdentifier()}? EVALUATE anyExpression caseExpression* END                                #evaluateExpression
                | {!isIdentifier()}? MULTIPLY atomic BY atomic+ (GIVING identifier)?                           #multiplyExpression
                | {!isIdentifier()}? SUBTRACT atomic+ FROM atomic (GIVING identifier)?                         #substractExpression
                ;

displayExpression: atomic (DELIMITED_BY (SPACE | SIZE | literal))?;

thenExpression: THEN statement+;

elseExpression: ELSE statement+;

moveExpression: atomic
              | {!isIdentifier()}? HIGH_VALUES
              | {!isIdentifier()}? LOW_VALUES
              | {!isIdentifier()}? SPACES
              ;

caseExpression: whenBlock statement+;

whenBlock: {!isIdentifier()}? WHEN atomic+
         | {!isIdentifier()}? WHEN OTHER
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

booleanExpression: {!isIdentifier()}? TRUE
                 | {!isIdentifier()}? FALSE
                 | arithmeticExpression (comparisonOperator arithmeticExpression)?
                 | {!isIdentifier()}? NOT booleanExpression
                 | booleanExpression booleanOperator booleanExpression
                 ;

loopBody: {!isIdentifier()}? VARYING identifier? (FROM atomic)? (TO atomic)? (BY atomic)?          #VaryingCondition
        | {!isIdentifier()}? WHILE booleanExpression                                               #WhileCondition
        | {!isIdentifier()}? UNTIL booleanExpression                                               #TillCondition
        | statement                                                                                #NoCondition
        ;

comparisonOperator: EQUALS | GT | LT | GE | LE
                  ;

booleanOperator: {!isIdentifier()}? OR
               | {!isIdentifier()}? AND
               | {!isIdentifier()}? XOR
               ;

arithmeticOperator: PLUS | MINUS | TIMES_SYM | DIV | POW;

atomic: atomicExpression | INTEGER | identifier | STRING;

procedureName: ID | keywords;

paragraphName: ID | keywords;

identifier: (ID | {isIdentifier()}? keywords) '(' INTEGER ')' #arrayIndexIdentifier
          | (ID | {isIdentifier()}? keywords) OF identifier   #quantifiedIdentifier
          | (ID | {isIdentifier()}? keywords)                 #nameIdentifier
          ;

keywords: ACCEPT | ADD | ALTER | DISPLAY | DIVIDE | EVALUATE | IF | LOOP | MOVE | MULTIPLY | PERFORM | SIGNAL
        | STOP | PICTURE | OCCURS | LIKE | WITH_NO_ADVANCING | AND | BY | DELIMITED_BY | ELSE | END | ON_ERROR
        | FALSE | FROM | GIVING | GO_TO | HIGH_VALUES | IS | INTO | LOW_VALUES | NEXT | NOT | OF | OFF | OR | OTHER
        | TO_PROCEED_TO | REMAINDER | SENTENCE | SIZE | SPACE | SPACES | SUBTRACT | THEN | THROUGH | TIMES
        | TO | TRUE | UNTIL | VARYING | WHEN | WHILE | XOR
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

// Fragments
fragment ALPHANUM: (CHAR | [0-9]);

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
NEXT:       'NEXT';
NOT:        'NOT';
OFF:        'OFF';
OF:         'OF';
ON_ERROR:   'ON ERROR';
OR:         'OR';
OTHER:      'OTHER';
REMAINDER:  'REMAINDER';
SENTENCE:   'SENTENCE';
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

IDENTIFICATION_DIVISION: IDENTIFICATION ' ' DIVISION;
DATA_DIVISION: DATA ' ' DIVISION;
PROCEDURE_DIVISION: PROCEDURE ' ' DIVISION;

ID: (CHAR | '-') (CHAR | INT | '-')*;
INTEGER: INT+;

WS: [ \t\r\n]+ -> skip;
IGNORED: .;

fragment CHAR: [A-Z];

fragment DATA: 'DATA';
fragment DIVISION: 'DIVISION';
fragment PROCEDURE: 'PROCEDURE';
fragment IDENTIFICATION: 'IDENTIFICATION';

fragment INT: [0-9];