package nl.utwente.babycobol;

import nl.utwente.babycobol.exceptions.ParseException;
import nl.utwente.babycobol.parser.Parser;
import nl.utwente.babycobol.validation.PrettyPrinter;
import org.antlr.v4.runtime.tree.ParseTree;

import java.io.File;
import java.io.IOException;

public class Main {

    public static void main(String[] args) {
        if (args.length == 0) {
            System.out.println("Usage: [path to source code] {output for pretty print file}");
            return;
        }

        File sourceFile = new File(args[0]);
        Parser parser = new Parser();
        try {
            ParseTree parseTree = parser.process(sourceFile);
            parser.doSufficientQualification(parseTree);
            PrettyPrinter prettyPrinter = new PrettyPrinter();

            if (args.length > 1) {
                File outputFile = new File(args[1]);
                prettyPrinter.processToFile(parseTree, outputFile);
            } else {
                String code = prettyPrinter.process(parseTree);
                System.out.print(code);
            }
        } catch (IOException e) {
            System.err.println("Could not find source code file at the location: " + e.getMessage());
        } catch (ParseException e) {
            System.err.println(e.getMessage());
        }
    }

}
