package nl.utwente.babycobol.parser.errorListeners;

import nl.utwente.babycobol.preprocessor.Line;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.ATNConfigSet;
import org.antlr.v4.runtime.dfa.DFA;

import javax.management.monitor.CounterMonitor;
import java.util.Arrays;
import java.util.BitSet;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.antlr.v4.runtime.Token.EOF;

public class BabyCobolErrors extends BaseErrorListener {

    private List<Line> originalCode;

    public BabyCobolErrors(List<Line> originalCode) {
        this.originalCode = originalCode;
    }

    /**
     * Make sure that the lines as shown in the original files are pointed out correctly.
     * TODO: Make sure that the original code is being printed and not the result from the pre-processor.
     * @param recognizer
     * @param offendingSymbol
     * @param line
     * @param charPositionInLine
     * @param msg
     * @param e
     */
    @Override
    public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol, int line, int charPositionInLine, String msg, RecognitionException e) {
        // TODO: Also get original char position!
        if (line > this.originalCode.size()) {
            System.err.println(msg);
            return;
        }

        Line errorLine = this.originalCode.get(line - 1);
        int[] lines = errorLine.getLineNumbers();
         if (lines.length == 1) {
            System.err.printf("line %d:%d %s%n", lines[0], charPositionInLine, msg);
        } else {
            System.err.printf("lines %s:%d %s%n", Arrays.toString(lines), charPositionInLine, msg);
        }
        underlineError(recognizer, (Token) offendingSymbol, line, charPositionInLine);
    }

    @Override
    public void reportAmbiguity(Parser recognizer, DFA dfa, int startIndex, int stopIndex, boolean exact, BitSet ambigAlts, ATNConfigSet configs) {
        super.reportAmbiguity(recognizer, dfa, startIndex, stopIndex, exact, ambigAlts, configs);
    }

    @Override
    public void reportAttemptingFullContext(Parser recognizer, DFA dfa, int startIndex, int stopIndex, BitSet conflictingAlts, ATNConfigSet configs) {
        super.reportAttemptingFullContext(recognizer, dfa, startIndex, stopIndex, conflictingAlts, configs);
    }

    @Override
    public void reportContextSensitivity(Parser recognizer, DFA dfa, int startIndex, int stopIndex, int prediction, ATNConfigSet configs) {
        super.reportContextSensitivity(recognizer, dfa, startIndex, stopIndex, prediction, configs);
    }

    /**
     * Underlines the error, as demonstrated in "The Ultimate ANTLR 4 Reference" p. 156
     * @param recognizer         The recognizer class that tries to parse this
     * @param offendingToken     The token that cannot be parsed
     * @param line               The line on which this breaks
     * @param charPositionInLine The position of the character in the line
     */
    public void underlineError(Recognizer recognizer, Token offendingToken, int line, int charPositionInLine) {
        CommonTokenStream tokens = (CommonTokenStream) recognizer.getInputStream();
        String input = tokens.getTokenSource().getInputStream().toString();
        String[] lines = input.split("\r?\n");
        String errorLine = lines[line - 1];
        System.err.println(errorLine);
        for (int i = 0; i < charPositionInLine; i++) {
            System.err.print(" ");
        }
        int start = offendingToken.getStartIndex();
        int stop = offendingToken.getStopIndex();
        if (start >= 0 && stop >= 0) {
            for (int i = start; i < stop + 1; i++) {
                System.err.print("^");
            }
        }
        System.err.println();
    }
}
