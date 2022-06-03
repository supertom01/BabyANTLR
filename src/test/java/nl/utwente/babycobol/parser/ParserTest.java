package nl.utwente.babycobol.parser;

import nl.utwente.babycobol.exceptions.ParseException;
import org.antlr.v4.runtime.tree.ParseTree;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions.*;

import java.io.File;
import java.io.IOException;
import java.util.List;

//public class ParserTest {
//
//    public void testFile(String filename) {
//        File file = new File(filename);
//        BParser parser = new BParser();
//        try {
//            ParseTree program = parser.process(file);
//            parser.doSufficientQualification(program);
//        } catch (IOException e) {
//
//        }
//    }
//
//    @Test
//
//}
