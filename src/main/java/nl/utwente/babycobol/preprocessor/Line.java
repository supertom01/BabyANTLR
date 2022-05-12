/**
 * @author Tom Meulenkamp (t.meulenkamp@student.utwente.nl)
 * Inspired by Ulrich Wolffgang
 */

package nl.utwente.babycobol.preprocessor;

import java.util.Arrays;

/**
 * Represents a single BabyCobol line.
 */
public class Line {

    private final String sequenceArea;
    private final String indicator;
    private final String sectionA;
    private final String sectionB;
    private final String ignored;

    private final int lineNumber;
//    private final int[] lineNumbers;

    private final Line[] originalLines;

    private Line(String sequenceArea, String indicator, String sectionA, String sectionB, String ignored,
                int lineNumber, Line[] originalLines) {
        this.sequenceArea = sequenceArea;
        this.indicator = indicator;
        this.sectionA = sectionA;
        this.sectionB = sectionB;
        this.ignored = ignored;
        this.lineNumber = lineNumber;
        this.originalLines = originalLines;
    }

    /**
     * Constructs a line object spanning only one line.
     * @param sequenceArea
     * @param indicator
     * @param sectionA
     * @param sectionB
     * @param ignored
     * @param lineNumber
     */
    public Line(String sequenceArea, String indicator, String sectionA, String sectionB, String ignored,
                int lineNumber) {
        this(sequenceArea, indicator, sectionA, sectionB, ignored, lineNumber, null);
    }

    /**
     * Constructs a line object that used to span multiple lines, this object incorporates its original lines.
     * @param sequenceArea
     * @param indicator
     * @param sectionA
     * @param sectionB
     * @param ignored
     * @param originalLines
     */
    public Line(String sequenceArea, String indicator, String sectionA, String sectionB, String ignored,
                Line[] originalLines) {
        this(sequenceArea, indicator, sectionA, sectionB, ignored, -1, originalLines);
    }

    public String contentArea() {
        return this.sectionA + this.sectionB;
    }

    public LineType getLineType() {
        return switch (this.indicator) {
            case " " -> LineType.NORMAL;
            case "-" -> LineType.CONTINUATION;
            case "*" -> LineType.COMMENT;
            default -> LineType.INVALID;
        };
    }

    public String getIndicator() {
        return this.indicator;
    }

    public int getLineNumber() {
        return this.lineNumber;
    }

    public int[] getLineNumbers() {
        int[] nrs = new int[this.getOriginalLines().length];
        for (int i = 0; i < this.getOriginalLines().length; i++) {
            nrs[i] = this.getOriginalLines()[i].getLineNumber();
        }
        return nrs;
    }

    /**
     * If this line is a combination of two or more lines (due to line continuation), then this function returns
     * the original lines, otherwise it returns simply an array of a single element, containing itself.
     */
    public Line[] getOriginalLines() {
        return this.originalLines != null && this.originalLines.length > 0 ? this.originalLines : new Line[]{this};
    }

    public boolean isMerged() {
        return this.getOriginalLines().length > 1;
    }

    public String sectionA() {
        if (this.contentArea().length() > 4) {
            return this.contentArea().substring(0, 4);
        } else {
            return this.contentArea();
        }
    }

    public String sectionB() {
        if (this.contentArea().length() > 4) {
            return this.contentArea().substring(4);
        } else {
            return "";
        }
    }

    /**
     * Returns a cleaned string, with a consistent sequence section.
     * TODO: How do we give a nice warning?!
     */
    public String getClean() {
        String line;
        if (this.sectionA().length() == 0 || this.sectionA().charAt(0) == ' ') {
            line = " ".repeat(6) + this.indicator + " ".repeat(4) + this.sectionB();
            if (this.sectionA().replaceAll(" ", "").length() != 0) {
                System.err.printf("Warning: line %s sectionA \"%s\" was wiped, due to starting with a space %n",
                        Arrays.toString(this.getLineNumbers()), this.sectionA());
            }
        } else {
            line = " ".repeat(6) + this.indicator + this.contentArea();
        }
        line += "\r\n";
        return line;
    }

    @Override
    public String toString() {
        return this.sequenceArea + this.indicator + sectionA + sectionB + ignored;
    }

    /**
     * Appends all sectionB areas together, using the first line as a base.
     * @param lines The lines to be concatenated together.
     * @return One single line.
     */
    public static Line mergeLines(Line base, Line ...lines) {
        Line[] originalLines = new Line[lines.length + base.getOriginalLines().length];
        System.arraycopy(base.getOriginalLines(), 0, originalLines, 0, base.getOriginalLines().length);

        StringBuilder builder = new StringBuilder(base.sectionB());
        for (int i = 0; i < lines.length; i++) {
            originalLines[base.getLineNumbers().length + i] = lines[i];
            builder.append(lines[i].sectionB());
        }
        return new Line(base.sequenceArea, base.indicator, base.sectionA,
            builder.toString(), "", originalLines);
    }

}
