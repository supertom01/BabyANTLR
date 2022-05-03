grammar BabyCobolGrammar;
import BabyCobolTokens;

program: identificationDivision (dataDivision)? (procedureDivision)?;

//TODO: Include copy instruction support
identificationDivision: IDENTIFICATION DIVISION '.' (identifying_val '.' identifying_val '.')+;

dataDivision: DATA DIVISION '.' (declaration '.')+;

declaration: INTEGER identifier ((PICTURE IS PICTURE_REPRESENTATION) | (LIKE identifier))? (OCCURS INTEGER TIMES)?;

procedureDivision: PROCEDURE DIVISION '.' sentence* paragraph+ STOP?;

paragraph: identifier '.' sentence+;

sentence: statement+ '.';

statement: ACCEPT identifier+
         | ADD atomic+ TO atomic (GIVING identifier)?
         | ALTER procedureName TO PROCEED TO procedureName
         | COPY FILENAME (REPLACING (literal BY literal)+)?
         | DISPLAY displayExpression* (WITH NO ADVANCING)?
         | DIVIDE atomic INTO atomic+ (GIVING identifier)? (REMAINDER identifier)?
         | EVALUATE anyExpression whenBlock* END
         | GO TO procedureName (OR procedureName)*
         | IF booleanExpression THEN statement+ (ELSE statement+) END?
         | LOOP loopStatement* END
         | MOVE moveExpression TO identifier+
         | MULTIPLY atomic BY atomic+ (GIVING identifier)
         | NEXT SENTENCE
         | PERFORM procedureName (THROUGH procedureName)? (atomic TIMES)?
         | SIGNAL (OFF | procedureName) ON ERROR
         | STOP
         | SUBTRACT atomic+ FROM atomic (GIVING identifier)?
         ;

displayExpression: atomic (DELIMITED BY (SPACE | SIZE | STRING))?;

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

loopStatement: VARYING identifier? (FROM atomic)? (TO atomic)? (BY atomic)?
             | WHILE booleanExpression
             | UNTIL booleanExpression
             | statement
             ;

comparisonOperator: EQUALS | GT | LT | GE | LE;

booleanOperator: OR | AND | XOR;

arithmeticOperator: PLUS | MINUS | TIMES_SYM | DIV | POW;

atomic: INTEGER | identifier | STRING;

procedureName: sectionName | paragraphName | identifier;
sectionName: identifier;
paragraphName: identifier;

identifier: NAME | NAME '(' INTEGER ')' | NAME OF identifier;

literal: STRING;

identifying_val: (NO_DOT | identifier | literal) (identifying_val)?;