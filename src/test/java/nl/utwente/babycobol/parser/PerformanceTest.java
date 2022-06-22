package nl.utwente.babycobol.parser;

import nl.utwente.babycobol.BabyCobolLexer;
import nl.utwente.babycobol.BabyCobolNoKeywordLexer;
import nl.utwente.babycobol.BabyCobolNoKeywordParser;
import nl.utwente.babycobol.BabyCobolParser;
import nl.utwente.babycobol.preprocessor.Line;
import nl.utwente.babycobol.preprocessor.PreProcessor;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Lexer;

import java.io.File;
import java.io.IOException;
import java.util.List;

public class PerformanceTest {

    public static String SOURCE_DIR = System.getProperty("user.dir") + "/src/test/java/nl/utwente/babycobol/testFiles/";

    public double testKeywords(String keywordsFile, String noKeywordsFile) {
        long startTime;
        long endTime;

        File keywords = new File(SOURCE_DIR + keywordsFile);
        File noKeywords = new File(SOURCE_DIR + noKeywordsFile);

        List<Line> keywordsLines;
        List<Line> noKeywordsLines;
        // Run the pre-processor
        try {
            PreProcessor preProcessor = new PreProcessor(keywords);
            keywordsLines = preProcessor.process();
            preProcessor = new PreProcessor(noKeywords);
            noKeywordsLines = preProcessor.process();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        startTime = System.currentTimeMillis();
        // Run the parser on the keywords file
        StringBuilder code = new StringBuilder();
        for (Line line : keywordsLines) {
            code.append(line.getClean());
        }
        CharStream stream = CharStreams.fromString(code.toString());
        Lexer lexer = new BabyCobolLexer(stream);
        BabyCobolParser parser = new BabyCobolParser(new CommonTokenStream(lexer));
        TestErrorListener errorListener = new TestErrorListener();
        parser.removeErrorListeners();
        parser.addErrorListener(errorListener);
        parser.program();
        endTime = System.currentTimeMillis();
        System.out.printf("Parsing with keywords took %d ms%n", endTime - startTime);
        long diff1 = endTime - startTime;

        startTime = System.currentTimeMillis();
        // Run the parser on the keywords file
        code = new StringBuilder();
        for (Line line : noKeywordsLines) {
            code.append(line.getClean());
        }
        stream = CharStreams.fromString(code.toString());
        lexer = new BabyCobolNoKeywordLexer(stream);
        BabyCobolNoKeywordParser parser2 = new BabyCobolNoKeywordParser(new CommonTokenStream(lexer));
        errorListener = new TestErrorListener();
        parser2.removeErrorListeners();
        parser2.addErrorListener(errorListener);
        parser2.program();
        endTime = System.currentTimeMillis();
        System.out.printf("Parsing without keywords took %d ms%n", endTime - startTime);
        long diff2 = endTime - startTime;
        double percentage = 100 - ((double) diff2 / (double) diff1) * 100;
        System.out.printf("Parsing without keywords takes %.2f%% less time than with keywords.%n%n", percentage);
        return percentage;
    }

    public static void main(String[] args) {
        PerformanceTest test = new PerformanceTest();
        double t1 = test.testKeywords("input/contracted_expressions.bc", "input/contracted_expressions.bc");
        double t2 = test.testKeywords("input/copyInSameLine.bc", "input/copyInSameLine.bc");
        double t3 = test.testKeywords("input/fib.bc", "input/fib.bc");
        double t4 = test.testKeywords("input/abcFormula.bc", "input/abcFormula.bc");
        double t5 = test.testKeywords("input/keywords.bc", "noKeywords/keywords.bc");
        double t6 = test.testKeywords("input/gotolooppara.bc", "input/gotolooppara.bc");
        double t7 = test.testKeywords("input/testFile.bc", "input/testFile.bc");
        System.out.printf("On average the delay is %.2f%%%n",
                (t1 + t2 + t3 + t4 + t5 + t6 + t7) / 7);
    }

}
