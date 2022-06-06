package nl.utwente.babycobol;

import nl.utwente.babycobol.exceptions.ParseException;
import nl.utwente.babycobol.parser.Parser;
import nl.utwente.babycobol.validation.PrettyPrinter;
import org.antlr.v4.runtime.tree.ParseTree;

import java.io.File;
import java.io.IOException;

public class Main {

    public static String HELP_TEXT = """
            ANTLR based BabyCobol parser.
            
            Usage: options [path_to_code] {path_to_output}
            
            Options:
            -h, -help: Show this help method.
            -g, -gui : Display the parse tree.
            
            Provided paths should be absolute. Where the input path is required, the output is optional.
            The output path will produce a file with the pretty printed version of the input file.
            """;

    public static void main(String[] args) {
        if (args.length == 0) {
            System.out.println(HELP_TEXT);
            return;
        }

        boolean showTree = false;
        int i = 0;

        switch (args[0]) {
            case "-help", "-h" -> {
                System.out.println(HELP_TEXT);
                return;
            }
            case "-gui", "-g" -> {
                showTree = true;
                i = 1;
                if (args.length == 1) {
                    System.out.println(HELP_TEXT);
                    return;
                }
            }
            default -> {}
        }

        File sourceFile = new File(args[i]);
        Parser parser = new Parser();
        try {
            ParseTree parseTree = parser.process(sourceFile);
            parser.doSufficientQualification(parseTree);
            PrettyPrinter prettyPrinter = new PrettyPrinter();

            if (args.length > i + 1) {
                File outputFile = new File(args[i + 1]);
                prettyPrinter.processToFile(parseTree, outputFile);
            } else {
                String code = prettyPrinter.process(parseTree);
                System.out.print(code);
            }

            if (showTree) {
                parser.visualizeTree(parseTree);
            }
        } catch (IOException e) {
            System.err.println("Could not find source code file at the location: " + e.getMessage());
        } catch (ParseException e) {
            System.err.println(e.getMessage());
        }
    }

}
