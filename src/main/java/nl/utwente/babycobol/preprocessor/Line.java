/**
 * @author Tom Meulenkamp (t.meulenkamp@student.utwente.nl)
 * Inspired by Ulrich Wolffgang
 */

package nl.utwente.babycobol.preprocessor;

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

    private final String fileName;

    private final Line[] originalLines;

    public Line(String sequenceArea, String indicator, String sectionA, String sectionB, String ignored,
                int lineNumber, String fileName, Line[] originalLines) {
        this.sequenceArea = sequenceArea;
        this.indicator = indicator;
        this.sectionA = sectionA;
        this.sectionB = sectionB;
        this.ignored = ignored;
        this.lineNumber = lineNumber;
        this.fileName = fileName;
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
                int lineNumber, String fileName) {
        this(sequenceArea, indicator, sectionA, sectionB, ignored, lineNumber, fileName, null);
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
                String fileName, Line[] originalLines) {
        this(sequenceArea, indicator, sectionA, sectionB, ignored, -1, fileName, originalLines);
    }

    public String getSequenceArea() {
        return this.sequenceArea;
    }

    public String getIgnored() {
        return this.ignored;
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
        return this.lineNumber != -1 ? this.lineNumber : getLineNumbers()[0];
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
     */
    public String getClean() {
        String line;
        if (this.sectionA().length() == 0) {
            line = " ".repeat(6) + this.indicator + " ".repeat(4) + this.sectionB();
        } else {
            line = " ".repeat(6) + this.indicator + this.contentArea();
        }
        line += System.lineSeparator();
        return line;
    }

    public String getFileName() {
        return this.fileName;
    }

    public String toErrorString(int lineNumber) {
        return String.format("%06d", lineNumber) + this.indicator + sectionA + sectionB + ignored;
    }

    @Override
    public String toString() {
        return String.format("%06d", this.getLineNumber()) + this.indicator + sectionA + sectionB + ignored;
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
            builder.append(lines[i].contentArea());
        }
        return new Line(base.sequenceArea, base.indicator, base.sectionA,
            builder.toString(), "", base.fileName, originalLines);
    }

}
