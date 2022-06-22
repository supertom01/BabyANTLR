package nl.utwente.babycobol.preprocessor;

import nl.utwente.babycobol.BabyCobolPreProcessorBaseListener;
import nl.utwente.babycobol.BabyCobolPreProcessorLexer;
import nl.utwente.babycobol.BabyCobolPreProcessorParser;
import nl.utwente.babycobol.exceptions.ParseException;
import nl.utwente.babycobol.parser.errorListeners.BabyCobolErrors;
import nl.utwente.babycobol.parser.errorListeners.BailErrorStrategy;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.InputMismatchException;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.misc.Interval;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeProperty;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

public class CopyStatement extends BabyCobolPreProcessorBaseListener {

    private List<String> errors;

    private ParseTreeProperty<List<Line>> lines;

    private Line line;

    private String workingDirectory;
    private String fileName;

    /**
     * Parses a line in a file, that may contain a copy statement.
     * @param file The file that is currently being processed.
     */
    public CopyStatement(File file) {
        this.errors = new ArrayList<>();
        this.lines = new ParseTreeProperty<>();
        this.workingDirectory = file.getParent();
        this.fileName = file.getAbsolutePath();
    }

    public List<Line> process(Line line) throws ParseException {
        this.line = line;
        Lexer lexer = new BabyCobolPreProcessorLexer(CharStreams.fromString(line.contentArea()));
        BabyCobolPreProcessorParser parser = new BabyCobolPreProcessorParser(new CommonTokenStream(lexer));
        parser.removeErrorListeners();
        parser.setErrorHandler(new BailErrorStrategy());
        parser.addErrorListener(new BabyCobolErrors(new ArrayList<>() {{add(line);}}));
        try {
            ParseTree lineTree = parser.line();
            (new ParseTreeWalker()).walk(this, lineTree);
            if (!this.errors.isEmpty()) {
                throw new ParseException("Failed executing copy statement");
            }
            return lines.get(lineTree);
        } catch (RuntimeException e) {
            if (e.getCause() != null && e.getCause() instanceof InputMismatchException) {
                return null;
            }
            throw e;
        }
    }

    public List<String> getErrors() {
        return errors;
    }

    @Override
    public void exitLine(BabyCobolPreProcessorParser.LineContext ctx) {
        List<Line> lines = new ArrayList<>();
        for (ParseTree child : ctx.children) {
            List<Line> childLines = this.lines.get(child);
            if (childLines != null) {
                lines.addAll(childLines);
            }
        }
        this.lines.put(ctx, lines);
    }

    @Override
    public void exitNotCopy(BabyCobolPreProcessorParser.NotCopyContext ctx) {
         if (!ctx.isEmpty() && ctx.getChildCount() != 0) {
            String code = ctx.getStart().getInputStream().getText(Interval.of(ctx.getStart().getStartIndex(), ctx.getStop().getStopIndex()));
            Line line = new Line("", " ", "", code, "", this.line.getLineNumber(), this.fileName,
                    this.line.getOriginalLines());
            this.lines.put(ctx, new ArrayList<>() {{add(line);}});
        }
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
                builder.append(line).append(System.lineSeparator());
            }
            code = builder.toString();
        } catch (FileNotFoundException e) {
            try (FileReader reader2 = new FileReader(this.workingDirectory + "/"+ fileName)) {
                BufferedReader buffer = new BufferedReader(reader2);
                StringBuilder builder = new StringBuilder();
                String line;
                while ((line = buffer.readLine()) != null) {
                    builder.append(line).append(System.lineSeparator());
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
        List<Line> lines = preProcessor.process();
        if (preProcessor.hasErrors()) {
            this.errors.addAll(preProcessor.getErrors());
        }

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
