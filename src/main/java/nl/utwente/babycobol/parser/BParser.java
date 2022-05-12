package nl.utwente.babycobol.parser;

import nl.utwente.babycobol.parser.errorListeners.BabyCobolErrors;
import nl.utwente.babycobol.preprocessor.Line;
import nl.utwente.babycobol.preprocessor.PreProcessor;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.tree.ParseTree;

import java.io.File;
import java.io.IOException;
import java.util.List;

public class BParser {

    /**
     * Takes a BabyCobol source file as input, runs the preprocessor over it and afterwards runs the ANTLR parser.
     * @param sourceFile A file containing code written in BabyCobol.
     */
    public ParseTree process(File sourceFile, boolean verbose) throws IOException {
        List<Line> lines = preProcess(sourceFile);
        return parse(lines, verbose);
    }

    public List<Line> preProcess(File sourceFile) throws IOException {
        PreProcessor preProcessor = new PreProcessor(sourceFile);
        preProcessor.process();
        return preProcessor.getLines();
    }

    public ParseTree parse(List<Line> lines, boolean verbose) {
        StringBuilder code = new StringBuilder();
        for (Line line : lines) {
            code.append(line.getClean());
        }
        if (verbose) {
            System.out.println(code);
        }
        CharStream stream = CharStreams.fromString(code.toString());
        Lexer lexer = new BabyCobolParserLexer(stream);
        BabyCobolParserParser parser = new BabyCobolParserParser(new CommonTokenStream(lexer));
        parser.removeErrorListeners();
        parser.addErrorListener(new BabyCobolErrors(lines));
        return parser.program();
    }

    public static void main(String[] args) {
        File file = new File("C:\\Users\\meule\\IdeaProjects\\BabyCobol\\src\\test\\sample\\fib_bad.bc");
        BParser parser = new BParser();
        try {
            ParseTree tree = parser.process(file, true);
            System.out.println(tree);
        } catch (IOException e) {
            System.err.printf("[ERROR] Couldn't open file %s%n", e.getMessage());
            System.exit(0);
        }
    }

}
