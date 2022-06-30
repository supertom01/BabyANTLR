package nl.utwente.babycobol.validation;

import nl.utwente.babycobol.BabyCobolLexer;
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
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * This class attempts to parse all files in a given folder and if there are not any errors, then the file name is
 * printed to the standard output.
 *
 * The resulting files will be used for the performance test that checks the difference in time between support for non-
 * reserved keywords and without support for non-reserved keywords.
 */
public class FileFinder {

    public static Set<String> getFilesInDir(String dir, int depth) {
        try (Stream<Path> stream = Files.walk(Paths.get(dir), depth)) {
            return stream
                    .filter(file -> !Files.isDirectory(file))
                    .map(Path::getFileName)
                    .map(Path::toString)
                    .collect(Collectors.toSet());
        } catch (IOException e) {
            return null;
        }
    }

    public static String addSpaces(List<String> codeLines) {
        StringBuilder code = new StringBuilder();
        for (String line : codeLines) {
            code.append(" ".repeat(7)).append(line).append(System.lineSeparator());
        }
        return code.toString();
    }

    public static void main(String[] args) throws IOException {

        List<String> correctPrograms = new ArrayList<>();

        if (args.length != 1) {
            System.out.println("Usage: [path to directory to be scanned]");
            return;
        }

        Set<String> files = FileFinder.getFilesInDir(args[0], 1);
        if (files == null || files.size() == 0) {
            System.out.println("Did not find any files in the directory");
            return;
        }
        for (String file : files) {
            List<String> codeLines = Files.readAllLines(Path.of(args[0] + "/" + file));
            PreProcessor preProcessor;
            if (!codeLines.get(0).startsWith("  ")) {
                preProcessor = new PreProcessor(addSpaces(codeLines), file);
            } else {
                preProcessor = new PreProcessor(codeLines, file);
            }
            List<Line> lines = preProcessor.process();
            StringBuilder code = new StringBuilder();
            if (preProcessor.hasErrors()) {continue;}
            for (Line line : lines) {
                code.append(line.getClean());
            }
            CharStream stream = CharStreams.fromString(code.toString());
            Lexer lexer = new BabyCobolLexer(stream);
            BabyCobolParser parser = new BabyCobolParser(new CommonTokenStream(lexer));
            TestErrorListener errorListener = new TestErrorListener();
            parser.removeErrorListeners();
            parser.addErrorListener(errorListener);
            ParseTree program = parser.program();
            if (!errorListener.getErrors().isEmpty()) {continue;}
            correctPrograms.add(file);
        }

        for (String program : correctPrograms) {
            System.out.println(program);
        }
    }

}
