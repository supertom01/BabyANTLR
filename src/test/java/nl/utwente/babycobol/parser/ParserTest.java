package nl.utwente.babycobol.parser;

import nl.utwente.babycobol.BabyCobolLexer;
import nl.utwente.babycobol.BabyCobolParser;
import nl.utwente.babycobol.data.DataStructureGenerator;
import nl.utwente.babycobol.data.Node;
import nl.utwente.babycobol.data.QualificationChecker;
import nl.utwente.babycobol.preprocessor.Line;
import nl.utwente.babycobol.preprocessor.PreProcessor;
import nl.utwente.babycobol.validation.PrettyPrinter;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.*;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.*;

public class ParserTest {

    public static String SOURCE_DIR = System.getProperty("user.dir") + "/src/test/java/nl/utwente/babycobol/testFiles/";

    private List<String> preProcessorErrors;
    private List<String> parserErrors;
    private List<String> dataStructureErrors;
    private List<String> qualificationErrors;
    private List<String> qualificationWarnings;

    public void testFile(String inputFile, String expectedFile, boolean compareFiles) {
        long startTime;
        long endTime;

        File input = new File(SOURCE_DIR + inputFile);
        File expected = new File(SOURCE_DIR + expectedFile);
        List<Line> lines;
        ParseTree program;
        Node variableRoot;
        String prettyCode;

        startTime = System.currentTimeMillis();
        // Run the pre-processor
        try {
            PreProcessor preProcessor = new PreProcessor(input);
            lines = preProcessor.process();
            this.preProcessorErrors = preProcessor.getErrors();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        endTime = System.currentTimeMillis();
        System.out.printf("Preprocessing took %d ms%n", endTime - startTime);

        startTime = System.currentTimeMillis();
        // Run the parser
        StringBuilder code = new StringBuilder();
        for (Line line : lines) {
            code.append(line.getClean());
        }
        CharStream stream = CharStreams.fromString(code.toString());
        Lexer lexer = new BabyCobolLexer(stream);
        BabyCobolParser parser = new BabyCobolParser(new CommonTokenStream(lexer));
        TestErrorListener errorListener = new TestErrorListener();
        parser.removeErrorListeners();
        parser.addErrorListener(errorListener);
        this.parserErrors = errorListener.getErrors();
        program = parser.program();
        endTime = System.currentTimeMillis();
        System.out.printf("Parsing took %d ms (%.3f lines per ms)%n", endTime - startTime, lines.size() / ((double) (endTime - startTime)));

        startTime = System.currentTimeMillis();
        // Check if sufficient qualification has been done properly.
        DataStructureGenerator generator = new DataStructureGenerator(lines);
        variableRoot = generator.visit(program);
        this.dataStructureErrors = generator.getErrors();
        endTime = System.currentTimeMillis();
        System.out.printf("Construction of the data structure took %d ms%n", endTime - startTime);

        startTime = System.currentTimeMillis();
        QualificationChecker checker = new QualificationChecker(variableRoot, lines);
        (new ParseTreeWalker()).walk(checker, program);
        endTime = System.currentTimeMillis();
        System.out.printf("Checking for sufficient qualification took %d ms%n", endTime - startTime);
        this.qualificationErrors = checker.getErrors();
        this.qualificationWarnings = checker.getWarnings();

        if (compareFiles) {
            // Run the pretty printer, its result should be equal to the contents of the expected file.
            PrettyPrinter prettyPrinter = new PrettyPrinter();
            prettyCode = prettyPrinter.process(program);

            try (BufferedReader reader = new BufferedReader(new FileReader(expected))) {
                String expectedCode = reader.lines().collect(Collectors.joining(System.lineSeparator()));
                assertEquals(expectedCode, prettyCode, "The code produced by the pretty printer is not equal to the expected code.");
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
    }

    public void testFile(String fileName, boolean compareFiles) {
        testFile("input/" + fileName, "expected/" + fileName, compareFiles);
    }

    public void testFile(String fileName) {
        testFile("input/" + fileName, "expected/" + fileName, true);
    }

    public boolean anyErrors() {
        return !(this.preProcessorErrors.isEmpty() && this.parserErrors.isEmpty() && this.dataStructureErrors.isEmpty()
                 && this.qualificationErrors.isEmpty() && this.qualificationWarnings.isEmpty());
    }

    public boolean hasError(List<String> errors, int line, String partialMessage) {
        for (String error : errors) {
            if (error.contains(String.format("line %d:", line)) &&
               error.contains(partialMessage)) {
                return true;
            }
        }
        return false;
    }

    @BeforeEach
    public void setUp() {
        this.preProcessorErrors = new ArrayList<>();
        this.parserErrors = new ArrayList<>();
        this.dataStructureErrors = new ArrayList<>();
        this.qualificationErrors = new ArrayList<>();
        this.qualificationWarnings = new ArrayList<>();
    }

    @Test
    public void testFib() {
        testFile("fib.bc");
        assertFalse(anyErrors());
    }

    @Test
    public void testKeywords() {
        testFile("keywords.bc");
        assertFalse(anyErrors());
    }

    @Test
    public void testProcedureNameKeyword() {
        testFile("keywords2.bc");
        assertFalse(anyErrors());
    }

    @Test
    public void testContractedExpression() {
        testFile("contracted_expressions.bc");
        assertFalse(anyErrors());
    }

    @Test
    public void testSufficientQualification() {
        testFile("sufficient_qualification.bc");
        assertTrue(anyErrors());
        assertTrue(this.preProcessorErrors.isEmpty());
        assertTrue(this.parserErrors.isEmpty());
        assertTrue(this.qualificationWarnings.isEmpty());
        assertEquals(1, this.dataStructureErrors.size());
        assertTrue(hasError(this.dataStructureErrors, 14, "Cannot construct a recursive data type"));
        assertEquals(1, this.qualificationErrors.size());
        assertTrue(hasError(this.qualificationErrors, 28, "Insufficient qualification"));
    }

    @Test
    public void testCopyInSameLine() {
        testFile("copyInSameLine.bc");
        assertFalse(anyErrors());
    }

    @Test
    public void testCopyTest() {
        testFile("copyTest.bc");
        assertTrue(anyErrors());
        assertTrue(hasError(this.qualificationWarnings, 2, "Implicit definition"));
        assertTrue(hasError(this.qualificationErrors, 2, "points to a field and not a container"));
    }

    @Test
    public void procedureNameTest() {
        testFile("procedureName.bc");
        assertFalse(anyErrors());
    }

    @Test
    public void abcTest() {
        testFile("abcFormula.bc");
        assertFalse(anyErrors());
    }

    @Test
    public void randomFileGroup1() {
        testFile("testFile.bc");
        assertFalse(anyErrors());
    }

    @Test
    public void goToLoopParaGroup1() {
        testFile("gotolooppara.bc");
        assertFalse(anyErrors());
    }
}
