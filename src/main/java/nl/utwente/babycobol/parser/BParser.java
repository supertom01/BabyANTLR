package nl.utwente.babycobol.parser;

import nl.utwente.babycobol.data.DataStructureGenerator;
import nl.utwente.babycobol.data.Qualification;
import nl.utwente.babycobol.data.Node;
import nl.utwente.babycobol.exceptions.ParseException;
import nl.utwente.babycobol.parser.errorListeners.BabyCobolErrors;
import nl.utwente.babycobol.preprocessor.Line;
import nl.utwente.babycobol.preprocessor.PreProcessor;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

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
        Lexer lexer = new BabyCobolLexer(stream);
        BabyCobolParser parser = new BabyCobolParser(new CommonTokenStream(lexer));
        parser.removeErrorListeners();
        parser.addErrorListener(new BabyCobolErrors(lines));
        return parser.program();
    }

    public Node doSufficientQualification(ParseTree program) throws ParseException {
        DataStructureGenerator generator = new DataStructureGenerator();
        Node root = generator.visit(program);
        for (String error : generator.getErrors()) {
            System.err.println(error);
        }
        return root;
    }

    public static void main(String[] args) {
        File file = new File("C:\\Users\\meule\\IdeaProjects\\BabyCobol\\src\\test\\sample\\sufficient_qualification2.bc");
        BParser parser = new BParser();
        try {
            ParseTree tree = parser.process(file, true);
            Node root = parser.doSufficientQualification(tree);
            System.out.println(root);
        } catch (IOException e) {
            System.err.printf("[ERROR] Couldn't open file %s%n", e.getMessage());
            System.exit(0);
        } catch (ParseException e) {
            System.err.println(e.getMessage());
        }
    }

}
