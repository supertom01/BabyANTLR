grammar  BabyCobol;

options {
    caseInsensitive = true;
}

@parser::members {
    public boolean isKeyWord() {
        String text = getCurrentToken().getText();
        return text.toUpperCase().equals(text);
    }
}

program: identificationDivision (dataDivision)? (procedureDivision)?;

identificationDivision: IDENTIFICATION_DIVISION '.' (~('.' | DATA_DIVISION | PROCEDURE_DIVISION)+ '.' ~('.' | DATA_DIVISION | PROCEDURE_DIVISION)+ '.')+;

//TODO: Include copy instruction support
//identificationDivision: identification division '.' (~('.' )+ '.' ~'.' '.')+;

dataDivision: DATA_DIVISION '.' (declaration '.')+;

declaration: INTEGER ID typeDeclaration;

typeDeclaration: ((PICTURE IS picture_repr) | (LIKE identifier))? (OCCURS INTEGER TIMES)?;

procedureDivision: PROCEDURE_DIVISION '.' sentence* paragraph+ STOP?;

paragraph: paragraphName '.' sentence+;

sentence: statement+ '.';

statement: ACCEPT identifier+                                                       #AcceptStatement
         | ALTER procedureName TO_PROCEED_TO procedureName                          #AlterStatement
         | COPY FILENAME (REPLACING replaceExpression+)?                            #CopyStatement
         | DISPLAY displayExpression* (WITH_NO_ADVANCING)?                          #DisplayStatement
         | GO_TO procedureName                                                      #GoToStatement
         | IF booleanExpression THEN statement+ (ELSE statement+)? END?             #IfStatement
         | LOOP loopBody* END                                                       #LoopStatement
         | MOVE moveExpression TO identifier+                                       #MoveStatement
         | NEXT sentence                                                            #NextSentenceStatement
         | PERFORM procedureName (THROUGH procedureName)? (atomic TIMES)?           #PerformStatement
         | SIGNAL (OFF | procedureName) ON_ERROR                                    #SignalStatement
         | STOP                                                                     #StopStatement
         | atomicExpression                                                         #AtomicStatement
         ;

atomicExpression: ADD atomic+ TO atomic (GIVING identifier)?                                #addExpression
                | DIVIDE atomic INTO atomic+ (GIVING identifier)? (REMAINDER identifier)?   #divideExpression
                | EVALUATE anyExpression (whenBlock statement+)* END                        #evaluateExpression
                | MULTIPLY atomic BY atomic+ (GIVING identifier)?                           #multiplyExpression
                | SUBTRACT atomic+ FROM atomic (GIVING identifier)?                         #substractExpression
                ;

replaceExpression: COPY_QUOTE literal COPY_QUOTE BY COPY_QUOTE literal COPY_QUOTE;

displayExpression: atomic (DELIMITED_BY (SPACE | SIZE | literal))?;

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

procedureName: ID;

paragraphName: ID;

identifier: ID                           #nameIdentifier
          | ID '(' INTEGER ')'           #arrayIndexIdentifier
          | ID OF identifier             #quantifiedIdentifier
//          | /*{!isKeyWord()}?*/ keywords #keywordIdentifier
          ;

keywords: ACCEPT | ADD | ALTER | COPY | DISPLAY | DIVIDE | EVALUATE | IF | LOOP | MOVE | MULTIPLY | PERFORM | SIGNAL
                    | STOP | PICTURE | OCCURS | LIKE | WITH_NO_ADVANCING | AND | BY | DELIMITED_BY | ELSE | END | ON_ERROR
                    | FALSE | FROM | GIVING | GO_TO | HIGH_VALUES | IS | INTO | LOW_VALUES | NEXT | NOT | OF | OFF | OR | OTHER
                    | TO_PROCEED_TO | REMAINDER | REPLACING | SENTENCE | SIZE | SPACE | SPACES | SUBTRACT | THEN | THROUGH | TIMES
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
COPY:       C O P Y;
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
REPLACING:  R E P L A C I N G;
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