package nl.utwente.babycobol.validation;

import nl.utwente.babycobol.data.Node;
import nl.utwente.babycobol.parser.BabyCobolBaseListener;
import nl.utwente.babycobol.parser.BabyCobolParser;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.misc.Interval;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeProperty;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import java.util.List;

public class PrettyPrinter extends BabyCobolBaseListener {

    public static int AREA_A_OFFSET = 7;
    public static int AREA_B_OFFSET = 12;

    private Node variableRoot;

    private ParseTreeProperty<String> lines;

    public PrettyPrinter(Node variableRoot) {
        this.lines = new ParseTreeProperty<>();
        this.variableRoot = variableRoot;
    }

    public String process(ParseTree program) {
        (new ParseTreeWalker()).walk(this, program);
        return this.lines.get(program);
    }

    @Override
    public void exitProgram(BabyCobolParser.ProgramContext ctx) {
        StringBuilder program = new StringBuilder();
        program.append(this.lines.get(ctx.identificationDivision()));
        if (ctx.dataDivision() != null) {
            program.append(this.lines.get(ctx.dataDivision()));
        }
        if (ctx.procedureDivision() != null) {
            program.append(this.lines.get(ctx.procedureDivision()));
        }
        this.lines.put(ctx, program.toString());
    }

    @Override
    public void exitIdentificationDivision(BabyCobolParser.IdentificationDivisionContext ctx) {
        StringBuilder identificationDivision = new StringBuilder();
        identificationDivision.append(" ".repeat(AREA_A_OFFSET));
        identificationDivision.append("IDENTIFICATION DIVISION.");
        identificationDivision.append(System.lineSeparator());
        for (int i = 0; i < ctx.identificationDeclaration().size(); i += 2) {
            identificationDivision.append(" ".repeat(AREA_B_OFFSET));
            identificationDivision.append(this.lines.get(ctx.identificationDeclaration(i)));
            identificationDivision.append(". ");
            identificationDivision.append(this.lines.get(ctx.identificationDeclaration(i + 1)));
            identificationDivision.append(".");
            identificationDivision.append(System.lineSeparator());
        }
        this.lines.put(ctx, identificationDivision.toString());
    }

    @Override
    public void exitIdentificationDeclaration(BabyCobolParser.IdentificationDeclarationContext ctx) {
        this.lines.put(ctx, ctx.getStart().getInputStream().getText(Interval.of(ctx.getStart().getStartIndex(), ctx.getStop().getStopIndex())));
    }

    @Override
    public void exitDataDivision(BabyCobolParser.DataDivisionContext ctx) {
        StringBuilder dataDivision = new StringBuilder();
        dataDivision.append(" ".repeat(AREA_A_OFFSET));
        dataDivision.append("DATA DIVISION.");
        dataDivision.append(System.lineSeparator());
        int currentLevel = -1;
        int extraSpaces = 0;
        for (BabyCobolParser.DeclarationContext declaration : ctx.declaration()) {
            int level = Integer.parseInt(declaration.INTEGER().getText());
            if (currentLevel == -1) {
                currentLevel = level;
            } else if (currentLevel < level) {
                currentLevel = level;
                extraSpaces++;
            } else if (currentLevel > level) {
                currentLevel = level;
                extraSpaces--;
            }
            dataDivision.append(" ".repeat(AREA_B_OFFSET));
            dataDivision.append(" ".repeat(extraSpaces * 2));
            dataDivision.append(String.format("%02d", level)).append(" ");
            if (declaration.ID() != null) {
                dataDivision.append(declaration.ID().toString().toLowerCase());
            } else {
                dataDivision.append(declaration.keywords().toString().toLowerCase());
            }
            dataDivision.append(this.lines.get(declaration.typeDeclaration()));
            dataDivision.append(".");
            dataDivision.append(System.lineSeparator());
        }
        this.lines.put(ctx, dataDivision.toString());
    }

    @Override
    public void exitTypeDeclaration(BabyCobolParser.TypeDeclarationContext ctx) {
        StringBuilder type = new StringBuilder();

        if (ctx.PICTURE() != null) {
            type.append(" PICTURE IS ");
            type.append(ctx.picture_repr().getText());
        } else if (ctx.LIKE() != null) {
            type.append(" LIKE ");
            type.append(this.lines.get(ctx.identifier()));
        }

        if (ctx.OCCURS() != null) {
            type.append(" OCCURS ");
            type.append(ctx.INTEGER().getText());
            type.append(" TIMES");
        }
        this.lines.put(ctx, type.toString());
    }

    @Override
    public void exitProcedureDivision(BabyCobolParser.ProcedureDivisionContext ctx) {
        StringBuilder procedureDivision = new StringBuilder();
        procedureDivision.append(" ".repeat(AREA_A_OFFSET));
        procedureDivision.append("PROCEDURE DIVISION.");
        procedureDivision.append(System.lineSeparator());
        for (BabyCobolParser.SentenceContext sentence : ctx.sentence()) {
            procedureDivision.append(this.lines.get(sentence));
        }
        for (BabyCobolParser.ParagraphContext paragraph : ctx.paragraph()) {
            procedureDivision.append(this.lines.get(paragraph));
        }
        procedureDivision.append(" ".repeat(AREA_A_OFFSET)).append("STOP");
        this.lines.put(ctx, procedureDivision.toString());
    }

    @Override
    public void exitParagraph(BabyCobolParser.ParagraphContext ctx) {
        StringBuilder paragraph = new StringBuilder();
        paragraph.append(" ".repeat(AREA_A_OFFSET));
        paragraph.append(this.lines.get(ctx.paragraphName()));
        paragraph.append(".");
        paragraph.append(System.lineSeparator());
        for (BabyCobolParser.SentenceContext sentence : ctx.sentence()) {
            paragraph.append(this.lines.get(sentence));
        }
        this.lines.put(ctx, paragraph.toString());
    }

    @Override
    public void exitSentence(BabyCobolParser.SentenceContext ctx) {
        StringBuilder sentence = new StringBuilder();
        for (BabyCobolParser.StatementContext statement : ctx.statement()) {
            sentence.append(" ".repeat(AREA_B_OFFSET));
            sentence.append(this.lines.get(statement));
            sentence.append(".");
            sentence.append(System.lineSeparator());
        }
        this.lines.put(ctx, sentence.toString());
    }

    @Override
    public void exitAcceptStatement(BabyCobolParser.AcceptStatementContext ctx) {
        StringBuilder accept = new StringBuilder("ACCEPT");
        for (BabyCobolParser.IdentifierContext identifier : ctx.identifier()) {
            accept.append(" ");
            accept.append(this.lines.get(identifier));
        }
        this.lines.put(ctx, accept.toString());
    }

    @Override
    public void exitAlterStatement(BabyCobolParser.AlterStatementContext ctx) {
        String alter = "ALTER " + this.lines.get(ctx.procedureName(0)) + " TO PROCEED TO " +
                       this.lines.get(ctx.procedureName(1));
        this.lines.put(ctx, alter);
    }

    @Override
    public void exitDisplayStatement(BabyCobolParser.DisplayStatementContext ctx) {
        StringBuilder display = new StringBuilder("DISPLAY");
        for (BabyCobolParser.DisplayExpressionContext displayExpression : ctx.displayExpression()) {
            display.append(" ");
            display.append(this.lines.get(displayExpression));
        }
        this.lines.put(ctx, display.toString());
    }

    @Override
    public void exitGoToStatement(BabyCobolParser.GoToStatementContext ctx) {
        this.lines.put(ctx, "GO TO " + this.lines.get(ctx.procedureName()));
    }

    // TODO: Do new lines for if statements with proper indentations
    @Override
    public void exitIfStatement(BabyCobolParser.IfStatementContext ctx) {
        StringBuilder ifStatement = new StringBuilder("IF ");
        ifStatement.append(this.lines.get(ctx.booleanExpression()));
        ifStatement.append(" ");
        ifStatement.append(this.lines.get(ctx.thenExpression()));
        if (ctx.elseExpression() != null) {
            ifStatement.append(" ");
            ifStatement.append(this.lines.get(ctx.elseExpression()));
        }
        ifStatement.append(" END");
        this.lines.put(ctx, ifStatement.toString());
    }

    // TODO: Do new lines for loops with proper indentations
    @Override
    public void exitLoopStatement(BabyCobolParser.LoopStatementContext ctx) {
        StringBuilder loop = new StringBuilder("LOOP");
        for (BabyCobolParser.LoopBodyContext body : ctx.loopBody()) {
            loop.append(" ");
            loop.append(this.lines.get(body));
        }
        loop.append(" END");
        this.lines.put(ctx, loop.toString());
    }

    @Override
    public void exitMoveStatement(BabyCobolParser.MoveStatementContext ctx) {
        StringBuilder move = new StringBuilder("MOVE ");
        move.append(this.lines.get(ctx.moveExpression()));
        move.append(" TO");
        for (BabyCobolParser.IdentifierContext identifier : ctx.identifier()) {
            move.append(" ");
            move.append(this.lines.get(identifier));
        }
        this.lines.put(ctx, move.toString());
    }

    @Override
    public void exitNextSentenceStatement(BabyCobolParser.NextSentenceStatementContext ctx) {
        this.lines.put(ctx, "NEXT SENTENCE");
    }

    @Override
    public void exitPerformStatement(BabyCobolParser.PerformStatementContext ctx) {
        StringBuilder perform = new StringBuilder("PERFORM ");
        perform.append(this.lines.get(ctx.procedureName(0)));
        if (ctx.THROUGH() != null) {
            perform.append(" THROUGH ");
            perform.append(this.lines.get(ctx.procedureName(1)));
        }
        if (ctx.TIMES() != null) {
            perform.append(this.lines.get(ctx.atomic()));
            perform.append(" TIMES");
        }
        this.lines.put(ctx, perform.toString());
    }

    @Override
    public void exitSignalStatement(BabyCobolParser.SignalStatementContext ctx) {
        StringBuilder signal = new StringBuilder("SIGNAL ");
        if (ctx.OFF() != null) {
            signal.append("OFF");
        } else {
            signal.append(this.lines.get(ctx.procedureName()));
        }
        signal.append(" ON ERROR");
        this.lines.put(ctx, signal.toString());
    }

    @Override
    public void exitStopStatement(BabyCobolParser.StopStatementContext ctx) {
        this.lines.put(ctx, "STOP");
    }

    @Override
    public void exitAtomicStatement(BabyCobolParser.AtomicStatementContext ctx) {
        this.lines.put(ctx, this.lines.get(ctx.atomicExpression()));
    }

    @Override
    public void exitAddExpression(BabyCobolParser.AddExpressionContext ctx) {
        StringBuilder add = new StringBuilder("ADD");
        List<BabyCobolParser.AtomicContext> atomicList = ctx.atomic();
        for (int i = 0; i < atomicList.size() - 1; i++) {
            add.append(" ");
            add.append(this.lines.get(ctx.atomic(i)));
        }
        add.append(" TO ");
        add.append(this.lines.get(ctx.atomic(atomicList.size() - 1)));
        if (ctx.GIVING() != null) {
            add.append(" GIVING ");
            add.append(this.lines.get(ctx.identifier()));
        }
        this.lines.put(ctx, add.toString());
    }

    @Override
    public void exitDivideExpression(BabyCobolParser.DivideExpressionContext ctx) {
        StringBuilder divide = new StringBuilder("DIVIDE ");
        List<BabyCobolParser.AtomicContext> atomicList = ctx.atomic();
        divide.append(this.lines.get(ctx.atomic(0)));
        divide.append(" INTO");
        for (int i = 1; i < atomicList.size(); i++) {
            divide.append(" ");
            divide.append(this.lines.get(ctx.atomic(i)));
        }
        boolean hasGiving = false;
        if (ctx.GIVING() != null) {
            divide.append(" GIVING ");
            divide.append(this.lines.get(ctx.identifier(0)));
            hasGiving = true;
        }
        if (ctx.REMAINDER() != null) {
            divide.append(" REMAINDER ");
            if (hasGiving) {
                divide.append(this.lines.get(ctx.identifier(1)));
            } else {
                divide.append(this.lines.get(ctx.identifier(0)));
            }
        }
        this.lines.put(ctx, divide.toString());
    }

    @Override
    public void exitEvaluateExpression(BabyCobolParser.EvaluateExpressionContext ctx) {
        StringBuilder evaluate = new StringBuilder("EVALUATE ");
        evaluate.append(this.lines.get(ctx.anyExpression()));
        for (BabyCobolParser.CaseExpressionContext caseExpression : ctx.caseExpression()) {
            evaluate.append(" ");
            evaluate.append(this.lines.get(caseExpression));
        }
        evaluate.append("END");
        this.lines.put(ctx, evaluate.toString());
    }

    @Override
    public void exitMultiplyExpression(BabyCobolParser.MultiplyExpressionContext ctx) {
        StringBuilder multiply = new StringBuilder("MULTIPLY ");
        List<BabyCobolParser.AtomicContext> atomicList = ctx.atomic();
        multiply.append(this.lines.get(ctx.atomic(0)));
        multiply.append(" BY");
        for (int i = 1; i < atomicList.size(); i++) {
            multiply.append(" ");
            multiply.append(this.lines.get(ctx.atomic(i)));
        }
        if (ctx.GIVING() != null) {
            multiply.append(" GIVING ");
            multiply.append(this.lines.get(ctx.identifier()));
        }
        this.lines.put(ctx, multiply.toString());
    }

    @Override
    public void exitSubstractExpression(BabyCobolParser.SubstractExpressionContext ctx) {
        StringBuilder subtract = new StringBuilder("SUBTRACT");
        List<BabyCobolParser.AtomicContext> atomicList = ctx.atomic();
        for (int i = 0; i < atomicList.size() - 1; i++) {
            subtract.append(" ");
            subtract.append(this.lines.get(ctx.atomic(i)));
        }
        subtract.append(" FROM ");
        subtract.append(this.lines.get(ctx.atomic(atomicList.size() - 1)));
        if (ctx.GIVING() != null) {
            subtract.append(" GIVING ");
            subtract.append(this.lines.get(ctx.identifier()));
        }
        this.lines.put(ctx, subtract.toString());
    }

    @Override
    public void exitDisplayExpression(BabyCobolParser.DisplayExpressionContext ctx) {
        StringBuilder displayExpression = new StringBuilder();
        displayExpression.append(this.lines.get(ctx.atomic()));

        if (ctx.DELIMITED_BY() != null) {
            displayExpression.append(" DELIMITED BY ");
            if (ctx.SPACE() != null) {
                displayExpression.append("SPACE");
            } else if (ctx.SIZE() != null) {
                displayExpression.append("SIZE");
            } else {
                displayExpression.append(this.lines.get(ctx.literal()));
            }
        }
        this.lines.put(ctx, displayExpression.toString());
    }

    @Override
    public void exitMoveExpression(BabyCobolParser.MoveExpressionContext ctx) {
        if (ctx.HIGH_VALUES() != null) {
            this.lines.put(ctx, "HIGH VALUES");
        } else if (ctx.LOW_VALUES() != null) {
            this.lines.put(ctx, "LOW VALUES");
        } else if (ctx.SPACES() != null) {
            this.lines.put(ctx, "SPACES");
        } else {
            this.lines.put(ctx, this.lines.get(ctx.atomic()));
        }
    }

    @Override
    public void exitWhenBlock(BabyCobolParser.WhenBlockContext ctx) {
        StringBuilder when = new StringBuilder("WHEN");
        if (ctx.OTHER() != null) {
            when.append(" OTHER");
        } else {
            for (BabyCobolParser.AtomicContext atomic : ctx.atomic()) {
                when.append(" ");
                when.append(this.lines.get(atomic));
            }
        }
        this.lines.put(ctx, when.toString());
    }

    @Override
    public void exitAnyExpression(BabyCobolParser.AnyExpressionContext ctx) {
        this.lines.put(ctx, this.lines.get(ctx.getChild(0)));
    }

    @Override
    public void exitArithmeticExpression(BabyCobolParser.ArithmeticExpressionContext ctx) {
        if (ctx.atomic() != null) {
            this.lines.put(ctx, this.lines.get(ctx.atomic()));
        } else {
            this.lines.put(ctx,
                    this.lines.get(ctx.arithmeticExpression(0)) + " " +
                    this.lines.get(ctx.arithmeticOperator()) + " " +
                    this.lines.get(ctx.arithmeticExpression(1)));
        }
    }

    @Override
    public void exitStringExpression(BabyCobolParser.StringExpressionContext ctx) {
        if (ctx.atomic() != null) {
            this.lines.put(ctx, this.lines.get(ctx.atomic()));
        } else {
            this.lines.put(ctx, this.lines.get(ctx.stringExpression(0)) + " + " +
                                this.lines.get(ctx.stringExpression(1)));
        }
    }

    @Override
    public void exitBooleanExpression(BabyCobolParser.BooleanExpressionContext ctx) {
        if (ctx.TRUE() != null) {
            this.lines.put(ctx, "TRUE");
        } else if (ctx.FALSE() != null) {
            this.lines.put(ctx, "FALSE");
        } else if (ctx.NOT() != null) {
            this.lines.put(ctx, "NOT " + this.lines.get(ctx.booleanExpression(0)));
        } else if (ctx.arithmeticExpression() != null && ctx.arithmeticExpression().size() > 0) {
            String expr = this.lines.get(ctx.arithmeticExpression(0));
            if (ctx.comparisonOperator() != null) {
                expr += " " + this.lines.get(ctx.comparisonOperator()) + " " +
                        this.lines.get(ctx.arithmeticExpression(1));
            }
            this.lines.put(ctx, expr);
        } else {
            this.lines.put(ctx, this.lines.get(ctx.booleanExpression(0)) + " " +
                    this.lines.get(ctx.booleanOperator()) + " " +
                    this.lines.get(ctx.booleanExpression(1)));
        }
    }

    @Override
    public void exitVaryingCondition(BabyCobolParser.VaryingConditionContext ctx) {
        StringBuilder varying = new StringBuilder("VARYING");
        if (ctx.identifier() != null) {
            varying.append(" ");
            varying.append(this.lines.get(ctx.identifier()));
        }
        int i = 0;
        if (ctx.FROM() != null) {
            varying.append(" FROM ");
            varying.append(this.lines.get(ctx.atomic(i)));
            i++;
        }
        if (ctx.TO() != null) {
            varying.append(" TO ");
            varying.append(this.lines.get(ctx.atomic(i)));
            i++;
        }
        if (ctx.BY() != null) {
            varying.append(" BY ");
            varying.append(this.lines.get(ctx.atomic(i)));
        }
        this.lines.put(ctx, varying.toString());
    }

    @Override
    public void exitWhileCondition(BabyCobolParser.WhileConditionContext ctx) {
        this.lines.put(ctx, "WHILE " + this.lines.get(ctx.booleanExpression()));
    }

    @Override
    public void exitTillCondition(BabyCobolParser.TillConditionContext ctx) {
        this.lines.put(ctx, "UNTIL " + this.lines.get(ctx.booleanExpression()));
    }

    @Override
    public void exitNoCondition(BabyCobolParser.NoConditionContext ctx) {
        this.lines.put(ctx, this.lines.get(ctx.statement()));
    }

    @Override
    public void exitComparisonOperator(BabyCobolParser.ComparisonOperatorContext ctx) {
        this.lines.put(ctx, ctx.getStart().getText());
    }

    @Override
    public void exitBooleanOperator(BabyCobolParser.BooleanOperatorContext ctx) {
        this.lines.put(ctx, ctx.getStart().getText().toUpperCase());
    }

    @Override
    public void exitArithmeticOperator(BabyCobolParser.ArithmeticOperatorContext ctx) {
        this.lines.put(ctx, ctx.getStart().getText());
    }

    @Override
    public void exitAtomic(BabyCobolParser.AtomicContext ctx) {
        if (ctx.atomicExpression() != null) {
            this.lines.put(ctx, this.lines.get(ctx.atomicExpression()));
        } else if (ctx.identifier() != null) {
            this.lines.put(ctx, this.lines.get(ctx.identifier()));
        } else {
            this.lines.put(ctx, ctx.getStart().getText());
        }
    }

    @Override
    public void exitProcedureName(BabyCobolParser.ProcedureNameContext ctx) {
        this.lines.put(ctx, ctx.ID().getText().toLowerCase());
    }

    @Override
    public void exitParagraphName(BabyCobolParser.ParagraphNameContext ctx) {
        this.lines.put(ctx, ctx.ID().getText().toLowerCase());
    }

    @Override
    public void exitArrayIndexIdentifier(BabyCobolParser.ArrayIndexIdentifierContext ctx) {
        String id = ctx.getStart().getText().toLowerCase();
        id += " (" + ctx.INTEGER().getText() + ")";
        this.lines.put(ctx, id);
    }

    @Override
    public void exitQuantifiedIdentifier(BabyCobolParser.QuantifiedIdentifierContext ctx) {
        String id = ctx.getStart().getText().toLowerCase();
        id += " OF " + this.lines.get(ctx.identifier());
        this.lines.put(ctx, id);
    }

    @Override
    public void exitNameIdentifier(BabyCobolParser.NameIdentifierContext ctx) {
        this.lines.put(ctx, ctx.getStart().getText().toLowerCase());
    }

    @Override
    public void exitPicture_repr(BabyCobolParser.Picture_reprContext ctx) {
        this.lines.put(ctx, ctx.getText());
    }

    @Override
    public void exitThenExpression(BabyCobolParser.ThenExpressionContext ctx) {
        StringBuilder then = new StringBuilder("THEN");
        for (BabyCobolParser.StatementContext statement : ctx.statement()) {
            then.append(" ");
            then.append(this.lines.get(statement));
        }
        this.lines.put(ctx, then.toString());
    }

    @Override
    public void exitElseExpression(BabyCobolParser.ElseExpressionContext ctx) {
        StringBuilder elseExpression = new StringBuilder("ELSE");
        for (BabyCobolParser.StatementContext statement : ctx.statement()) {
            elseExpression.append(" ");
            elseExpression.append(this.lines.get(statement));
        }
        this.lines.put(ctx, elseExpression.toString());
    }

    @Override
    public void exitCaseExpression(BabyCobolParser.CaseExpressionContext ctx) {
        StringBuilder caseExpression = new StringBuilder();
        caseExpression.append(this.lines.get(ctx.whenBlock()));
        for (BabyCobolParser.StatementContext statement : ctx.statement()) {
            caseExpression.append(" ");
            caseExpression.append(this.lines.get(statement));
        }
        this.lines.put(ctx, caseExpression.toString());
    }

    @Override
    public void exitLiteral(BabyCobolParser.LiteralContext ctx) {
        this.lines.put(ctx, ctx.STRING().getText());
    }
}
