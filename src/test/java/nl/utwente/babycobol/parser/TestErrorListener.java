package nl.utwente.babycobol.parser;

import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;

import java.util.ArrayList;
import java.util.List;

public class TestErrorListener extends BaseErrorListener {

    private List<String> errors;

    public TestErrorListener() {
        this.errors = new ArrayList<>();
    }

    public List<String> getErrors() {
        return errors;
    }

    @Override
    public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol, int line, int charPositionInLine, String msg, RecognitionException e) {
        this.errors.add(String.format("line %d:%d %s", line, charPositionInLine, msg));
    }

}
