package nl.utwente.babycobol.parser.errorListeners;

import nl.utwente.babycobol.Utils;
import nl.utwente.babycobol.preprocessor.Line;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.Token;

import java.util.List;

public class BabyCobolErrors extends BaseErrorListener {

    private List<Line> originalCode;

    public BabyCobolErrors(List<Line> originalCode) {
        this.originalCode = originalCode;
    }

    /**
     * Make sure that the lines as shown in the original files are pointed out correctly.
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

        String message = Utils.formatMessage(this.originalCode, (Token) offendingSymbol, msg);
        System.err.println(message);
    }
}
