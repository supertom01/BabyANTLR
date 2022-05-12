grammar BabyCobolPreProcessor;

program: line+ EOF;

line: (CODE | CONTINUATION);//  (DIVISION | LINE_REMAINDER);

fragment DIV: 'DIVISION.';
fragment DATA: 'DATA';
fragment PROCEDURE: 'PROCEDURE';
fragment IDENTIFICATION: 'IDENTIFICATION';

DIVISION: (DATA | PROCEDURE | IDENTIFICATION) ' '* DIV;

fragment SEQ_NR: . . . . . .;
COMMENT: SEQ_NR '*' ~[\r\n]* -> skip;
CONTINUATION: SEQ_NR '-';
CODE: SEQ_NR ' ';

//NAME: ([a-zA-Z] | '-')+;
//
//LINE_REMAINDER: . . . . NL;
//NL: [\n\r]+;