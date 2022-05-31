/**
 * @author Tom Meulenkamp (t.meulenkamp@student.utwente.nl)
 */

package nl.utwente.babycobol.preprocessor;

import nl.utwente.babycobol.exceptions.ParseException;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class PreProcessor {

    private List<Line> lines;
    private String[] source_code;
    private List<String> errors;

    /** The regex pattern that matches a single line. */
    private Pattern linePattern;

    private String fileName;

    /**
     * Reads a file and loads all the lines into the pre-processor.
     * @param codeFile      The file that should be processed.
     * @throws IOException  Thrown if we are unable to read the file.
     */
    public PreProcessor(File codeFile) throws IOException {
        this(Files.readAllLines(codeFile.toPath()).toArray(new String[]{}), codeFile.getAbsolutePath());
    }

    /**
     * Reads a code string and splits it into lines based on newlines (\n) present in the string.
     * @param code The string containing the code to be pre-processed.
     */
    public PreProcessor(String code, String fileName) {
        this(code.split("\r?\n"), fileName);
    }

    public PreProcessor(List<String> lines, String fileName) {
        this(lines.toArray(new String[0]), fileName);
    }

    /**
     * Takes on an array of code lines and constructs the pre-processor.
     * @param lines The lines that have to be pre-processed.
     */
    public PreProcessor(String[] lines, String fileName) {
        this.source_code = lines;
        this.errors = new ArrayList<>();
        this.lines = new ArrayList<>(source_code.length);
        this.fileName = fileName;

        //   1-6 : Sequence number
        //     7 : Line type
        //  8-12 : Section A
        // 13-72 : Section B
        // 73+   : Ignored
        this.linePattern = Pattern.compile("(.{6})(.)(.{4})(.{0,61})(.*)");
    }

    public List<Line> getLines() {
        return this.lines;
    }

    public boolean hasErrors() {
        return !this.errors.isEmpty();
    }

    public List<String> getErrors() {
        return this.errors;
    }

    /**
     * Insert this line into the previous set line.
     * The previous line is automatically selected.
     * @param line The line that has to be merged into the previous line.
     */
    public void handleContinuedLine(Line line) {
        if (this.lines.isEmpty()) {
            this.errors.add(String.format("Line %d: A continued line was found, without any preceding lines.",
                line.getLineNumbers()[0]));
        } else {
            Line lastLine = lines.get(lines.size() - 1);
            lines.set(lines.size() - 1, Line.mergeLines(lastLine, line));
        }
    }

    public List<Line> handleCopyStatement(Line line) {
        File file = new File(this.fileName);
        CopyStatement copyStatement = new CopyStatement(file.getParent());
        try {
            return copyStatement.process(line);
        } catch (ParseException e) {
            for (String error : copyStatement.getErrors()) {
                System.err.println(error);
            }
            return null;
        }
    }

    /**
     * Processes the lines that have been given to this class.
     * This function separates each line in five distinct sections:
     * 1. Sequence section (chars 1-6)
     * 2. Line status indicator (char 7)
     * 3. Section A (chars 8 - 12)
     * 4. Section B (chars 13 - 72)
     * 5. Ignored (chars 73+)
     *
     * It also merges continued lines into a single line, while preserving the original line numbers.
     */
    public void process() {
        for (int i = 0; i < this.source_code.length; i++) {
            Matcher matcher = this.linePattern.matcher(this.source_code[i]);
            if (matcher.matches()) {
                Line line = new Line(matcher.group(1), matcher.group(2), matcher.group(3), matcher.group(4),
                    matcher.group(5), i + 1, this.fileName);

                switch (line.getLineType()) {
                    case COMMENT -> {}
                    case CONTINUATION -> handleContinuedLine(line);
                    case NORMAL -> this.lines.add(line);
                    case INVALID -> this.errors.add(String.format("Line %d: Could not recognize \"%s\" as an line " +
                            "status indicator. Only use \" \", \"*\", \"-\".", line.getLineNumbers()[0], line.getIndicator()));
                }
            } else {
                this.errors.add(String.format("Line %d: Failed to parse", i + 1));
            }
        }

        List<Line> iterList = List.copyOf(this.lines);
        for (int i = iterList.size() - 1; i >= 0; i--) {
            List<Line> copyLines = handleCopyStatement(iterList.get(i));
            if (copyLines != null && copyLines.size() > 0) {
                this.lines.remove(i);
                this.lines.addAll(i, copyLines);
            }
        }
    }
}