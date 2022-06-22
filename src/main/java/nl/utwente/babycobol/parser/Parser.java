package nl.utwente.babycobol.parser;

import nl.utwente.babycobol.BabyCobolLexer;
import nl.utwente.babycobol.BabyCobolParser;
import nl.utwente.babycobol.data.DataStructureGenerator;
import nl.utwente.babycobol.data.Node;
import nl.utwente.babycobol.data.QualificationChecker;
import nl.utwente.babycobol.exceptions.ParseException;
import nl.utwente.babycobol.parser.errorListeners.BabyCobolErrors;
import nl.utwente.babycobol.preprocessor.Line;
import nl.utwente.babycobol.preprocessor.PreProcessor;
import org.antlr.v4.gui.TreeViewer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

public class Parser {

    private List<Line> lines;
    private BabyCobolParser parser;

    /**
     * Takes a BabyCobol source file as input, runs the preprocessor over it and afterwards runs the ANTLR parser.
     * @param sourceFile A file containing code written in BabyCobol.
     */
    public ParseTree process(File sourceFile) throws IOException {
        this.lines = preProcess(sourceFile);
        return parse(lines);
    }

    public List<Line> preProcess(File sourceFile) throws IOException {
        PreProcessor preProcessor = new PreProcessor(sourceFile);
        List<Line> lines = preProcessor.process();
        for (String error : preProcessor.getErrors()) {
            System.err.println(error);
        }
        return lines;
    }

    public ParseTree parse(List<Line> lines) {
        StringBuilder code = new StringBuilder();
        for (Line line : lines) {
            code.append(line.getClean());
        }
        CharStream stream = CharStreams.fromString(code.toString());
        Lexer lexer = new BabyCobolLexer(stream);
        this.parser = new BabyCobolParser(new CommonTokenStream(lexer));
        this.parser.removeErrorListeners();
        this.parser.addErrorListener(new BabyCobolErrors(lines));
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

}
