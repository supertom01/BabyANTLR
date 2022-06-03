package nl.utwente.babycobol.preprocessor;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

class LineTest {

    private Line comment;
    private Line code;
    private Line continuation;
    private Line invalid;
    private Line merge1;
    private Line merge2;

    @BeforeEach
    void setUp() {
        comment = new Line("123456", "*", "Hell", "o how are you doing?", "", 1, "");
        code = new Line("", " ", "INDE", "NTIFICATION DIVISION.", "", 35, "");
        continuation = new Line("123456", "-", "    ", "ST.", "", 2, "");
        invalid = new Line("123456", "?", "    ", "COMPUTE 5 * 5.", "", 3, "");
        merge1 = Line.mergeLines(comment, code);
        merge2 = Line.mergeLines(code, continuation);
    }

    @Test
    public void testGetLineType() {
        assertEquals(LineType.COMMENT, comment.getLineType());
        assertEquals(LineType.NORMAL, code.getLineType());
        assertEquals(LineType.CONTINUATION, continuation.getLineType());
        assertEquals(LineType.INVALID, invalid.getLineType());
    }

    @Test
    public void testGetLineNumbers() {
        assertArrayEquals(new int[]{1}, comment.getLineNumbers());
        assertArrayEquals(new int[]{35}, code.getLineNumbers());
        assertArrayEquals(new int[]{2}, continuation.getLineNumbers());
        assertArrayEquals(new int[]{3}, invalid.getLineNumbers());
    }

    @Test
    public void testOriginalLines() {
        assertArrayEquals(new Line[]{comment}, comment.getOriginalLines());
        assertArrayEquals(new Line[]{code}, code.getOriginalLines());
        assertArrayEquals(new Line[]{continuation}, continuation.getOriginalLines());
        assertArrayEquals(new Line[]{invalid}, invalid.getOriginalLines());
    }

    @Test
    public void testMergeLines() {
        assertTrue(merge1.isMerged());
        assertNotEquals(merge1.sectionA(), code.sectionA());
        assertEquals(merge1.sectionA(), comment.sectionA());
        assertTrue(merge1.sectionB().contains(code.sectionB()));

        assertTrue(merge2.isMerged());
        assertNotEquals(merge2.sectionA(), continuation.sectionA());
        assertEquals(merge2.sectionA(), code.sectionA());
        assertTrue(merge2.sectionB().contains(continuation.sectionB()));
        assertFalse(merge2.sectionB().contains(continuation.sectionA()));
    }

    @Test
    public void testGetLineNumbersMerged() {
        assertEquals(1, merge1.getLineNumber());
        assertEquals(35, merge2.getLineNumber());
        Set<Integer> lines1 = new HashSet<>();
        Set<Integer> lines2 = new HashSet<>();
        for (int line : merge1.getLineNumbers()) {
            lines1.add(line);
        }
        for (int line : merge2.getLineNumbers()) {
            lines2.add(line);
        }
        assertFalse(lines1.contains(-1));
        assertFalse(lines2.contains(-1));
        assertTrue(lines1.contains(code.getLineNumber()));
        assertTrue(lines1.contains(comment.getLineNumber()));
        assertTrue(lines2.contains(continuation.getLineNumber()));
        assertTrue(lines2.contains(code.getLineNumber()));
    }

    @Test
    public void testOriginalLinesMerged() {
        Set<Line> lines1 = new HashSet<>(Arrays.asList(merge1.getOriginalLines()));
        Set<Line> lines2 = new HashSet<>(Arrays.asList(merge2.getOriginalLines()));

        assertTrue(lines1.contains(code));
        assertTrue(lines1.contains(comment));
        assertTrue(lines2.contains(continuation));
        assertTrue(lines2.contains(code));
    }
}
