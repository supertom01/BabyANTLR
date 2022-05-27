package nl.utwente.babycobol.parser.errorListeners;

import nl.utwente.babycobol.parser.BabyCobolLexer;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.Token;

public class Tokenizer {

    public static void tokenize(String input) {
        Lexer lexer = new BabyCobolLexer(CharStreams.fromString(input));
        for (Token token : lexer.getAllTokens()) {
            System.out.println(token);
        }
    }

    public static void main(String[] args) {
        Tokenizer.tokenize("IDENTIFICATION DIVISION" +
                "PROCEDURE DIVISION" +
                "STOP" +
                "" +
                "01 WORKING-AREA LIKE PIZZA.");
    }
}
