package nl.utwente.babycobol.data;

import nl.utwente.babycobol.parser.BabyCobolBaseListener;
import nl.utwente.babycobol.parser.BabyCobolParser;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ParseTreeProperty;

import java.util.ArrayList;
import java.util.List;

public class Qualification extends BabyCobolBaseListener {

    private ParseTreeProperty<Node> nodes;
    private Node root;

    private boolean inDataDivision;

    private List<String> errors;

    public Qualification() {
        this.inDataDivision = true;
        this.nodes = new ParseTreeProperty<>();
        this.errors = new ArrayList<>();
    }

    public void addError(Token token, String errorMessage) {
        errors.add(String.format("line %d:%d %s", token.getLine(), token.getCharPositionInLine(), errorMessage));
    }

    public List<String> getErrors() {
        return errors;
    }

    public Node getRoot() {
        return root;
    }

    @Override
    public void exitDataDivision(BabyCobolParser.DataDivisionContext ctx) {
        Node root = new Node("", -1, false);
        Node prev = root;

        for (int i = 0; i < ctx.declaration().size(); i++) {
            Node variable = nodes.get(ctx.declaration(i));
            while (variable.getLevel() < prev.getLevel()) {
                prev = prev.getParent();
            }
            if (variable.getLevel() == prev.getLevel()) {
                prev.getParent().addChild(variable);
                variable.setParent(prev.getParent());
            } else {
                if (prev.isField()) {
                    addError(ctx.declaration(i).getStart(), "It is not allowed to build a data structure " +
                            "that has a field in a field.");
                    continue;
                }
                prev.addChild(variable);
                variable.setParent(prev);
            }
            prev = variable;
        }
        this.root = root;
        nodes.put(ctx, root);
    }

    @Override
    public void enterProcedureDivision(BabyCobolParser.ProcedureDivisionContext ctx) {
        this.inDataDivision = false;
    }

    /**
     * Create a new node for this declaration.
     * @param ctx the parse tree
     */
    @Override
    public void exitDeclaration(BabyCobolParser.DeclarationContext ctx) {
        String identifier = ctx.ID().getText();
        String levelString = ctx.INTEGER().getText();

        if (levelString.length() != 2) {
            addError(ctx.INTEGER().getSymbol(), "Levels should always be indicated with 2 integers");
        }

        int level = Integer.parseInt(levelString);

        Node node = new Node(identifier, level, ctx.typeDeclaration().getChildCount() != 0);

        // If the declaration has a like statement, then the structure should be copied.
        if (nodes.get(ctx.typeDeclaration()) != null) {
            Node likeNode = nodes.get(ctx.typeDeclaration());
            for (Node child : likeNode.getChildren()) {
                node.addChild(child);
            }
            node.setIsField(likeNode.isField());
        }
        nodes.put(ctx, node);
    }

    /**
     * Checks if a like statement was used for a declaration, if so we should copy the structure from the variable.
     * This can however, only happen if that variable is already defined and if it is not a container, containing
     * this field.
     * @param ctx the parse tree
     */
    @Override
    public void exitTypeDeclaration(BabyCobolParser.TypeDeclarationContext ctx) {
        if (ctx.LIKE() != null) {

        }
    }

    @Override
    public void exitNameIdentifier(BabyCobolParser.NameIdentifierContext ctx) {
        super.exitNameIdentifier(ctx);
    }

    @Override
    public void exitQuantifiedIdentifier(BabyCobolParser.QuantifiedIdentifierContext ctx) {
        super.exitQuantifiedIdentifier(ctx);
    }
}
