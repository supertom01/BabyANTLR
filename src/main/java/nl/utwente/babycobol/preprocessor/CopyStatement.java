package nl.utwente.babycobol.preprocessor;

import nl.utwente.babycobol.exceptions.ParseException;
import nl.utwente.babycobol.parser.errorListeners.BabyCobolErrors;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeProperty;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class CopyStatement extends BabyCobolPreProcessorBaseListener {

    private List<String> errors;

    private ParseTreeProperty<List<Line>> lines;

    private String workingDirectory;

    public CopyStatement(String workingDirectory) {
        this.errors = new ArrayList<>();
        this.lines = new ParseTreeProperty<>();
        this.workingDirectory = workingDirectory;
    }

    public List<Line> process(Line line) throws ParseException {
        Lexer lexer = new BabyCobolPreProcessorLexer(CharStreams.fromString(line.contentArea()));
        BabyCobolPreProcessorParser parser = new BabyCobolPreProcessorParser(new CommonTokenStream(lexer));
        parser.removeErrorListeners();
        parser.addErrorListener(new BabyCobolErrors(new ArrayList<>() {{add(line);}}));
        ParseTree lineTree = parser.line();
        (new ParseTreeWalker()).walk(this, lineTree);
        if (!this.errors.isEmpty()) {
            throw new ParseException("Failed executing copy statement");
        }
        return lines.get(lineTree);
    }

    public List<String> getErrors() {
        return errors;
    }

    @Override
    public void exitLine(BabyCobolPreProcessorParser.LineContext ctx) {
        List<Line> lines = new ArrayList<>();
        for (BabyCobolPreProcessorParser.CopyContext copy : ctx.copy()) {
            List<Line> copyLines = this.lines.get(copy);
            if (copyLines != null) {
                lines.addAll(copyLines);
            }
        }
        this.lines.put(ctx, lines);
    }

    @Override
    public void exitCopy(BabyCobolPreProcessorParser.CopyContext ctx) {
        String fileName = ctx.FILENAME().getText();
        String code = null;

        try (FileReader reader = new FileReader(fileName)) {
            BufferedReader buffer = new BufferedReader(reader);
            StringBuilder builder = new StringBuilder();
            String line;
            while ((line = buffer.readLine()) != null) {
                builder.append(line).append("\r\n");
            }
            code = builder.toString();
        } catch (FileNotFoundException e) {
            try (FileReader reader2 = new FileReader(this.workingDirectory + "\\"+ fileName)) {
                BufferedReader buffer = new BufferedReader(reader2);
                StringBuilder builder = new StringBuilder();
                String line;
                while ((line = buffer.readLine()) != null) {
                    builder.append(line).append("\r\n");
                }
                code = builder.toString();
            } catch (IOException ioException) {
                errors.add("Could not find file: " + ioException.getMessage());
            }
            if (code == null) {
                return;
            }
        } catch (IOException e) {
            errors.add("Unexpected IO error: " + e.getMessage());
            return;
        }

        // Also put this code through the pre-processor.
        PreProcessor preProcessor = new PreProcessor(code, fileName);
        preProcessor.process();
        if (preProcessor.hasErrors()) {
            this.errors.addAll(preProcessor.getErrors());
        }
        List<Line> lines = preProcessor.getLines();

        // We have to replace values in the file that we've gotten.
        if (ctx.REPLACING() != null) {
            for (int i = 0; i < ctx.LITERAL().size(); i += 2) {
                String original = ctx.LITERAL(i).getText().replaceAll("===", "");
                String replacement = ctx.LITERAL(i + 1).getText().replaceAll("===", "");
                for (int j = 0; j < lines.size(); j++) {
                    Line line = lines.get(j);
                    String newContentArea = line.contentArea().replace(original, replacement);
                    String newAreaA;
                    String newAreaB;
                    if (newContentArea.length() > 4) {
                        newAreaA = newContentArea.substring(0, 4);
                        newAreaB = newContentArea.substring(4);
                    } else {
                        newAreaA = newContentArea;
                        newAreaB = "";
                    }
                    Line newLine = new Line(line.getSequenceArea(), line.getIndicator(), newAreaA, newAreaB,
                            line.getIgnored(), line.getLineNumber(), line.getFileName(), line.getOriginalLines());
                    lines.set(j, newLine);
                }
            }
        }

        this.lines.put(ctx, lines);
    }
}
