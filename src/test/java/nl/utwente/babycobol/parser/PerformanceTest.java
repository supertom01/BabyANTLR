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
import org.antlr.v4.runtime.tree.ParseTree;

import java.io.File;
import java.io.IOException;
import java.util.List;

public class PerformanceTest {

    public static String SOURCE_DIR = System.getProperty("user.dir") + "/src/test/resources/";

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

        startTime = System.nanoTime();
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
        endTime = System.nanoTime();
        long diff1 = endTime - startTime;

        startTime = System.nanoTime();
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
        BabyCobolNoKeywordParser.ProgramContext tree = parser2.program();
        endTime = System.nanoTime();
        long diff2 = endTime - startTime;
        double percentage = 100 - ((double) diff2 / (double) diff1) * 100;

        System.out.printf("%-25s | %02.3f | %02.3f | %2.2f | %d %n", keywords.getName(), diff1 / 1e6, diff2 / 1e6,
                percentage, tree.procedureDivision() == null ? -1 : tree.procedureDivision().children.size());
        return percentage;
    }

    public static void main(String[] args) {
        PerformanceTest test = new PerformanceTest();
        String[] testFiles = new String[]{"input/contracted_expressions.bc", "input/copyInSameLine.bc", "input/fib.bc",
            "input/abcFormula.bc", "input/gotolooppara.bc", "input/testFile.bc", "group1/alter.baby",
            "group1/performthrough.baby", "group1/test.baby", "group4/arrayNoIndex.bcl", "group4/stopBeforeDisplays.bcl",
            "group4/stopBetweenDisplays.bcl", "group6/Vadim_test1.bcob", "group6/Vadim_test2.bcob"};
        double sum = 0.0;
        System.out.printf("%25s |  time spent  | percentage%n%25s | non-res | reserv | %n", "Filename", "");
        for (String file : testFiles) {
            sum += test.testKeywords(file, file);
        }
        System.out.printf("On average the delay is %.2f%%%n", sum / testFiles.length);
//        double t1 = test.testKeywords("input/contracted_expressions.bc", "input/contracted_expressions.bc");
//        double t2 = test.testKeywords("input/copyInSameLine.bc", "input/copyInSameLine.bc");
//        double t3 = test.testKeywords("input/fib.bc", "input/fib.bc");
//        double t4 = test.testKeywords("input/abcFormula.bc", "input/abcFormula.bc");
//        double t5 = test.testKeywords("input/keywords.bc", "noKeywords/keywords.bc");
//        double t6 = test.testKeywords("input/gotolooppara.bc", "input/gotolooppara.bc");
//        double t7 = test.testKeywords("input/testFile.bc", "input/testFile.bc");
//        System.out.printf("On average the delay is %.2f%%%n",
//                (t1 + t2 + t3 + t4 + t5 + t6 + t7) / 7);
    }

}
