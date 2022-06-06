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

procedureName: ID;

paragraphName: ID;

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

ACCEPT:     A C C E P T;
ADD:        A D D;
ALTER:      A L T E R;
DISPLAY:    D I S P L A Y;
DIVIDE:     D I V I D E;
EVALUATE:   E V A L U A T E;
IF:         I F;
LOOP:       L O O P;
MOVE:       M O V E;
MULTIPLY:   M U L T I P L Y;
PERFORM:    P E R F O R M;
SIGNAL:     S I G N A L;
STOP:       S T O P;

PICTURE:    P I C T U R E;
OCCURS:     O C C U R S;
LIKE:       L I K E;

AND:        A N D;
BY:         B Y;
DELIMITED_BY: D E L I M I T E D B Y;
ELSE:       E L S E;
END:        E N D;
FALSE:      F A L S E;
FROM:       F R O M;
GIVING:     G I V I N G;
GO_TO:      G O T O;
HIGH_VALUES: H I G H ('-' ' '*) V A L U E S;
IS:         I S;
INTO:       I N T O;
LOW_VALUES: L O W ('-' ' '*) V A L U E S;
NEXT:       N E X T;
NOT:        N O T;
OFF:        O F F;
OF:         O F;
ON_ERROR:   O N E R R O R;
OR:         O R;
OTHER:      O T H E R;
REMAINDER:  R E M A I N D E R;
SENTENCE:   S E N T E N C E;
SIZE:       S I Z E;
SPACES:     S P A C E S;
SPACE:      S P A C E;
SUBTRACT:   S U B T R A C T;
THEN:       T H E N;
THROUGH:    T H R O U G H;
TIMES:      T I M E S;
TO_PROCEED_TO: T O P R O C E E D T O;
TO:         T O;
TRUE:       T R U E;
UNTIL:      U N T I L;
VARYING:    V A R Y I N G;
WHEN:       W H E N;
WHILE:      W H I L E;
WITH_NO_ADVANCING: W I T H N O A D V A N C I N G;
XOR:        X O R;

IDENTIFICATION_DIVISION: IDENTIFICATION ' ' DIVISION;
DATA_DIVISION: DATA ' ' DIVISION;
PROCEDURE_DIVISION: PROCEDURE ' ' DIVISION;

ID: (CHAR | '-') (CHAR | INT | '-')*;
INTEGER: INT+;

WS: [ \t\r\n]+ -> skip;
IGNORED: .;

fragment CHAR: A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z;

fragment DATA: D A T A;
fragment DIVISION: D I V I S I O N;
fragment PROCEDURE: P R O C E D U R E;
fragment IDENTIFICATION: I D E N T I F I C A T I O N;

fragment A: 'A';//' '*?;
fragment B: 'B';//' '*?;
fragment C: 'C';//' '*?;
fragment D: 'D';//' '*?;
fragment E: 'E';//' '*?;
fragment F: 'F';//' '*?;
fragment G: 'G';//' '*?;
fragment H: 'H';//' '*?;
fragment I: 'I';//' '*?;
fragment J: 'J';//' '*?;
fragment K: 'K';//' '*?;
fragment L: 'L';//' '*?;
fragment M: 'M';//' '*?;
fragment N: 'N';//' '*?;
fragment O: 'O';//' '*?;
fragment P: 'P';//' '*?;
fragment Q: 'Q';//' '*?;
fragment R: 'R';//' '*?;
fragment S: 'S';//' '*?;
fragment T: 'T';//' '*?;
fragment U: 'U';//' '*?;
fragment V: 'V';//' '*?;
fragment W: 'W';//' '*?;
fragment X: 'X';//' '*?;
fragment Y: 'Y';//' '*?;
fragment Z: 'Z';//' '*?;
fragment INT: [0-9];// ' '*?;