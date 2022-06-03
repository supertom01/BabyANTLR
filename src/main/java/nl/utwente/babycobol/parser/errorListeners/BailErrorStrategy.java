package nl.utwente.babycobol.parser.errorListeners;

import org.antlr.v4.runtime.*;

/**
 * Credits: Taken from "The Definitive ANTLR Reference" page 172/173.
 */
public class BailErrorStrategy extends DefaultErrorStrategy {

    /**
     * Instead of recovering from an exception, rethrow it in a RuntimeException such that it is not caught by the rule
     * function catches. Exception e is the "cause" of the RuntimeException.
     */
    @Override
    public void recover(Parser recognizer, RecognitionException e) {
        throw new RuntimeException(e);
    }

    /**
     * Make sure that we do not attempt to recover inline; if the parser successfully recovers, it won't throw an
     * exception.
     */
    @Override
    public Token recoverInline(Parser recognizer) throws RecognitionException {
        throw new RuntimeException(new InputMismatchException(recognizer));
    }

    /** Make sure that we do not attempt to recover from problems in subrules. */
    @Override
    public void sync(Parser recognizer) {}

}
