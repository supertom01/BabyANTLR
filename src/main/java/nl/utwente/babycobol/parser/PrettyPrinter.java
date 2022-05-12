package nl.utwente.babycobol.parser;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.antlr.v4.runtime.Token;

public class PrettyPrinter extends BabyCobolParserBaseVisitor<String> {

    private int currLineNr;
    private List<String> lines;

    private List<String> errors;

    private HashMap<String, Integer> variableNesting;
    private List<Integer> variableLevels;

    public PrettyPrinter() {
        currLineNr = 0;
        lines = new ArrayList<>();
        errors = new ArrayList<>();
        variableNesting = new HashMap<>();
        variableLevels = new ArrayList<>();
    }

    public List<String> getLines() {
        return this.lines;
    }

    private String generateLine(String code) {
        return String.format("%06d %s", currLineNr++, code);
    }

    @Override
    public String visitProgram(BabyCobolParserParser.ProgramContext ctx) {
        visit(ctx.identificationDivision());
        if (ctx.dataDivision() != null) {
            visit(ctx.dataDivision());
        }
        return null;
    }

    @Override
    public String visitIdentificationDivision(BabyCobolParserParser.IdentificationDivisionContext ctx) {
        lines.add(generateLine("IDENTIFICATION DIVISION."));
        for (int i = 0; i < ctx.identifying_val().size(); i += 2) {
            lines.add(generateLine(String.format("    %s. %s.", ctx.identifying_val(i).getText()
                , ctx.identifying_val(i + 1).getText())));
        }
        return null;
    }

    @Override
    public String visitDataDivision(BabyCobolParserParser.DataDivisionContext ctx) {
        lines.add(generateLine("DATA DIVISION."));
        for (BabyCobolParserParser.DeclarationContext dc : ctx.declaration()) {
            lines.add(generateLine(visit(dc) + "."));
        }
        return null;
    }

    @Override
    public String visitDeclaration(BabyCobolParserParser.DeclarationContext ctx) {
        BabyCobolParserParser.IdentifierContext variable = ctx.identifier(0);
        if (variableNesting.containsKey(variable.getText())) {
            Token varName = variable.getStart();
            errors.add(String.format("(%d:%d) This variable is already defined!", varName.getLine(), varName.getCharPositionInLine()));
            return null;
        }

        // Indent this sentence based on its nesting level.
        Integer level = Integer.parseInt(ctx.INTEGER(0).getText());
        variableNesting.put(variable.getText(), level);
        if (!variableLevels.contains(level)) {
            variableLevels.add(level);
        }
        String line = "    ".repeat(variableLevels.indexOf(level) + 1) + (variableLevels.indexOf(level) + 1) + " " + variable.getText().toUpperCase();

        if (ctx.PICTURE() != null) {
            line += String.format(" %s %s ", ctx.PICTURE().getText(), ctx.IS().getText());

            // Extend the shortened picture representation to its full form.
            Pattern pattern = Pattern.compile("(?<representation>[9AXZSV])(\\((?<amount>[0-9]+)\\))?");
            Matcher matcher = pattern.matcher(ctx.PICTURE_REPRESENTATION().getText());
            while (matcher.find()) {
                String representation = matcher.group("representation");
                if (matcher.group("amount") != null) {
                    representation = representation.repeat(Integer.parseInt(matcher.group("amount")));
                }
                line += representation;
            }
        } else if (ctx.LIKE() != null) {
            line += String.format(" %s %s", ctx.LIKE().getText(), ctx.identifier(1).getText());
        }

        if (ctx.OCCURS() != null) {
            line += String.format(" %s %s %s", ctx.OCCURS().getText(), ctx.INTEGER(1).getText(), ctx.TIMES());
        }
        return line;
    }

}
