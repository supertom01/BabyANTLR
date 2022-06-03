grammar BabyCobolPreProcessor;

options {
    caseInsensitive = true;
}

line: notCopy (copy notCopy)*;
notCopy: (~COPY)*?;

copy: COPY FILENAME (REPLACING (LITERAL BY LITERAL)+)?;

COPY: 'COPY';
FILENAME: ([A-Z0-9_-] | '\\' | '/')+ '.BC';
REPLACING: 'REPLACING';
BY: 'BY';
LITERAL: QUOTE .*? QUOTE;
WS: [\t\r\n ]+ -> channel(HIDDEN);
OTHER: ~[ \t\r\n.]+;
DOT: '.';

fragment QUOTE: '===';