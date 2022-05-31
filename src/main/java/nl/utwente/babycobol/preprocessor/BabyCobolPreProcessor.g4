grammar BabyCobolPreProcessor;

options {
    caseInsensitive = true;
}

line: (~COPY)* (copy (~COPY)*?)*;

copy: COPY FILENAME (REPLACING (LITERAL BY LITERAL)+)?;

COPY: 'COPY';
FILENAME: ([A-Z0-9_-] | '\\' | '/')+ '.BC';
REPLACING: 'REPLACING';
BY: 'BY';
LITERAL: QUOTE .*? QUOTE;
WS: [\t\r\n ]+ -> skip;
OTHER: ~[ \t\r\n.]+;
DOT: '.' -> skip;

fragment QUOTE: '===';