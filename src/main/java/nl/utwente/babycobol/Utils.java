package nl.utwente.babycobol;

import nl.utwente.babycobol.preprocessor.Line;
import org.antlr.v4.runtime.Token;

import java.util.Arrays;
import java.util.List;

public class Utils {

    public static String formatMessage(List<Line> lines, Token token, String errorMessage) {
        // Shouldn't happen but if we for some reason do not have the line numbers, then just print a default message.
        if (token.getLine() > lines.size()) {
            return String.format("line %d:%d %s", token.getLine(), token.getCharPositionInLine(),
                    errorMessage);
        }

        String message;
        Line lineObj = lines.get(token.getLine() - 1);
        int charPos = token.getCharPositionInLine() % 72;
        int line = lineObj.getLineNumbers()[token.getCharPositionInLine() / 72];
        int start = token.getStartIndex();
        int end = token.getStopIndex();
        message = String.format("[%s] line %d:%d %s%n", lineObj.getFileName(), line, charPos, errorMessage);
        String errorLine = String.format("%s%n%s%s", lineObj.toErrorString(line),
                " ".repeat(token.getCharPositionInLine()), "^".repeat(end - start + 1));

        return message + errorLine;
    }

}
