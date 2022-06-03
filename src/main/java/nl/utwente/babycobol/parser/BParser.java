package nl.utwente.babycobol.parser;

import nl.utwente.babycobol.data.DataStructureGenerator;
import nl.utwente.babycobol.data.Node;
import nl.utwente.babycobol.data.QualificationChecker;
import nl.utwente.babycobol.exceptions.ParseException;
import nl.utwente.babycobol.parser.errorListeners.BabyCobolErrors;
import nl.utwente.babycobol.preprocessor.Line;
import nl.utwente.babycobol.preprocessor.PreProcessor;
import nl.utwente.babycobol.validation.PrettyPrinter;
import org.antlr.v4.gui.TreeViewer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

public class BParser {

    private List<Line> lines;
    private BabyCobolParser parser;

    /**
     * Takes a BabyCobol source file as input, runs the preprocessor over it and afterwards runs the ANTLR parser.
     * @param sourceFile A file containing code written in BabyCobol.
     */
    public ParseTree process(File sourceFile, String outputFile) throws IOException {
        this.lines = preProcess(sourceFile);
        return parse(lines, outputFile);
    }

    public ParseTree process(File sourceFile) throws IOException {
        return process(sourceFile, null);
    }

    public List<Line> preProcess(File sourceFile) throws IOException {
        PreProcessor preProcessor = new PreProcessor(sourceFile);
        preProcessor.process();
        for (String error : preProcessor.getErrors()) {
            System.err.println(error);
        }
        return preProcessor.getLines();
    }

    public ParseTree parse(List<Line> lines, String outputFile) {
        StringBuilder code = new StringBuilder();
        for (Line line : lines) {
            code.append(line.getClean());
        }
        if (outputFile != null) {
            try (FileWriter writer = new FileWriter(outputFile)) {
                writer.write(code.toString());
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
        CharStream stream = CharStreams.fromString(code.toString());
        Lexer lexer = new BabyCobolLexer(stream);
        this.parser = new BabyCobolParser(new CommonTokenStream(lexer));
        parser.removeErrorListeners();
        parser.addErrorListener(new BabyCobolErrors(lines));
        return parser.program();
    }

    public Node doSufficientQualification(ParseTree program) throws ParseException {
        DataStructureGenerator generator = new DataStructureGenerator(this.lines);
        Node root = generator.visit(program);
        for (String error : generator.getErrors()) {
            System.err.println(error);
        }
        QualificationChecker checker = new QualificationChecker(root, this.lines);
        (new ParseTreeWalker()).walk(checker, program);
        for (String error : checker.getErrors()) {
            System.err.println(error);
        }
        for (String warning : checker.getWarnings()) {
            System.out.println(warning);
        }
        return root;
    }

    public void visualizeTree(ParseTree tree) {
        (new TreeViewer(Arrays.asList(parser.getRuleNames()),tree)).open();
    }

    public static void main(String[] args) {
        File file = new File("C:\\Users\\meule\\IdeaProjects\\BabyCobol\\src\\test\\sample\\fib_bad.bc");
        BParser parser = new BParser();
        String outputFile = "C:/Users/meule/IdeaProjects/BabyCobol/output.bc";
        try {
            ParseTree tree = parser.process(file, outputFile);
//            parser.visualizeTree(tree);
            Node root = parser.doSufficientQualification(tree);
            PrettyPrinter printer = new PrettyPrinter(root);
            String prettyCode = printer.process(tree);
            System.out.print(prettyCode);
        } catch (IOException e) {
            System.err.printf("[ERROR] Couldn't open file %s%n", e.getMessage());
            System.exit(0);
        } catch (ParseException e) {
            System.err.println(e.getMessage());
        }
    }

}
