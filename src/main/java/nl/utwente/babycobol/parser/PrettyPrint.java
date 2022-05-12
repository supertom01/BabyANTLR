package nl.utwente.babycobol.parser;

import java.io.IOException;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.tree.ParseTree;

public class PrettyPrint {

    public static void process(CharStream stream) {
        Lexer lexer = new BabyCobolParserLexer(stream);
        BabyCobolParserParser parser = new BabyCobolParserParser(new CommonTokenStream(lexer));
        ParseTree tree = parser.program();
        PrettyPrinter printer = new PrettyPrinter();
        printer.visit(tree);
        for (String line : printer.getLines()) {
            System.out.println(line);
        }

    }

    public static void processFile(String fileName) {
        try {
            System.out.printf("Processing file: %s%n", fileName);
            process(CharStreams.fromFileName(fileName));
        } catch (IOException e) {
            System.err.printf("Cannot find file: %s%n", e.getMessage());
        }
    }

    public static void processString(String code) {
        process(CharStreams.fromString(code));
    }

    public static void main(String[] args) {
        String test_code = "IDENTIFICATION DIVISION. PROGRAM ID. TEST-REPRESENTATION. AUTHOR. Tom Meulenkamp. DATA DIVISION. " +
            "1 TEST PICTURE IS 9X(5)99(2). 2 BETA LIKE TEST. 1 OMEGA OCCURS 20 TIMES. 2 ALPHA PICTURE IS A(3)XX(4)9.";
        processString(test_code);
//        processFile("C:\\Users\\meule\\IdeaProjects\\BabyCobol\\src\\test\\sample\\fib.bc");
//        processFile("C:\\Users\\meule\\IdeaProjects\\BabyCobol\\src\\test\\sample\\fib_bad.bc");
    }

}
