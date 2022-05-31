package nl.utwente.babycobol.data;

import nl.utwente.babycobol.Utils;
import nl.utwente.babycobol.parser.BabyCobolBaseVisitor;
import nl.utwente.babycobol.parser.BabyCobolParser;
import nl.utwente.babycobol.preprocessor.Line;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Constructs the data tree, that keeps track of inheritance between variables.
 * This tree will then be used to check for sufficient qualification.
 *
 * @author Tom Meulenkamp (S2267810)
 */
public class DataStructureGenerator extends BabyCobolBaseVisitor<Node> {

    /** A list of error messages, that are generated during the production of the tree. */
    private final List<String> errors;

    private final List<Line> lines;

    /** The root of the tree, this is an artificial node and contains no valid value by itself. */
    private final Node root;

    /** If set to true, then the visitor is evaluating a like statement. */
    private boolean inLikeStatement;

    public DataStructureGenerator(List<Line> lines) {
        this.lines = lines;
        this.errors = new ArrayList<>();
        this.root = new Node("", -1, false);
        this.inLikeStatement = false;
    }

    public List<String> getErrors() {
        return errors;
    }

    /**
     * Generates a new error message.
     * Based on the passed token the error line and column number are determined.
     * @param token        The token on which the error has occurred.
     * @param errorMessage The error message itself.
     */
    public void addError(Token token, String errorMessage) {
        this.errors.add(Utils.formatMessage(this.lines, token, errorMessage));
    }

    /**
     * Since identifiers are case-insensitive and could possibly contain whitespace, they are normalized.
     * @param identifier The identifier to be normalized.
     * @return A normalized value, aka. without spaces and all lower-case characters.
     */
    public static String normalizeIdentifier(String identifier) {
        return identifier.replaceAll(" ", "").toLowerCase();
    }

    @Override
    public Node visitProgram(BabyCobolParser.ProgramContext ctx) {
        if (ctx.dataDivision() != null) {
            return visit(ctx.dataDivision());
        } else {
            return root;
        }
    }

    /**
     * Construct the data structure that keeps track of the defined variables.
     * @param ctx the parse tree
     * @return The root of the data tree.
     */
    @Override
    public Node visitDataDivision(BabyCobolParser.DataDivisionContext ctx) {
        Node prev = this.root;

        for (BabyCobolParser.DeclarationContext declaration : ctx.declaration()) {
            Node variable = visit(declaration);

            // If the declaration failed to properly construct a node, skip it. An error message will later explain what
            // went wrong.
            if (variable == null) {
                continue;
            }

            // Put the node in its correct spot in the tree.
            while (variable.getLevel() < prev.getLevel()) {
                prev = prev.getParent();
            }
            if (variable.getLevel() == prev.getLevel()) {
                prev.getParent().addChild(variable);
                variable.setParent(prev.getParent());
            } else {
                if (prev.isField()) {
                    addError(declaration.getStart(), "It is not allowed to build a data structure that has" +
                            " a field in a field.");
                    continue;
                }
                prev.addChild(variable);
                variable.setParent(prev);
            }

            // If the declaration is of type LIKE, then we will have to copy the logic of the liked node.
            Node likeNode = visit(declaration.typeDeclaration());
            if (likeNode != null) {
                if (likeNode.containsNode(variable)) {
                    addError(declaration.typeDeclaration().getStart(), "Cannot construct a recursive data " +
                            "type pointing to itself.");
                } else {
                    for (Node child : likeNode.getChildren()) {
                        Node copy = Node.deepCopyNode(child);
                        copy.setParent(variable);
                        variable.addChild(copy);
                    }
                    variable.setIsField(likeNode.isField());
                }
            }

            prev = variable;
        }
        return root;
    }

    /**
     * Checks that the level consists out of two characters (01 valid, 1 invalid)
     * @param ctx the parse tree
     * @return a new node, that represents this declaration.
     */
    @Override
    public Node visitDeclaration(BabyCobolParser.DeclarationContext ctx) {
        String identifier = normalizeIdentifier(ctx.ID().getText());
        String strLevel = ctx.INTEGER().getText();
        if (strLevel.length() != 2) {
            addError(ctx.getStart(), "The level of a field should always consist out of two characters.");
        }
        int level = Integer.parseInt(strLevel);
        if (level < 0) {
            addError(ctx.getStart(), "The level of a field is not allowed to be negative.");
            return null;
        }

        return new Node(identifier, level, ctx.typeDeclaration().getChildCount() != 0);
    }

    @Override
    public Node visitTypeDeclaration(BabyCobolParser.TypeDeclarationContext ctx) {
        if (ctx.LIKE() != null) {
            this.inLikeStatement = true;
            Node node = visit(ctx.identifier());
            this.inLikeStatement = false;
            return node;
        }
        return null;
    }

    @Override
    public Node visitQuantifiedIdentifier(BabyCobolParser.QuantifiedIdentifierContext ctx) {
        String identifier = normalizeIdentifier(ctx.ID().getText());
        Node parent = visit(ctx.identifier());
        List<Node> nodes = parent.getAllNodes(identifier);
        if (nodes.size() == 0) {
            addError(ctx.ID().getSymbol(), "Could not find " + identifier + " in the container " +
                    parent.getValue());
            return null;
        } else if (nodes.size() == 1) {
            return nodes.get(0);
        } else if (ctx.getParent() instanceof BabyCobolParser.QuantifiedIdentifierContext) {
            // We could possibly still disambiguate if we only look into the containers.
            List<Node> containerNodes = parent.getAllContainerNodes(identifier);
            if (containerNodes.size() == 1) {
                return containerNodes.get(0);
            }
        }

        // If the identifier is a direct child, return that. This is possible for both containers and fields.
        if (parent.isDirectChild(identifier)) {
            return parent.getDirectChild(identifier);
        }
        addError(ctx.ID().getSymbol(), "Found ambiguous reference for the name " + identifier);
        return null;
    }

    /**
     * If we perform a like statement, then a name identifier should already be uniquely findable.
     * @param ctx the parse tree
     * @return
     */
    @Override
    public Node visitNameIdentifier(BabyCobolParser.NameIdentifierContext ctx) {
        if (inLikeStatement) {
            String identifier = normalizeIdentifier(ctx.ID().getText());
            List<Node> nodes = this.root.getAllNodes(identifier);
            if (nodes.size() == 0) {
                addError(ctx.ID().getSymbol(), "Could not find a container or field with the name " + identifier);
            } else if (nodes.size() == 1) {
                // Make sure that if the node points to a container, that it is not contained within that container.
                return nodes.get(0);
            } else {
                addError(ctx.ID().getSymbol(), "Found ambiguous reference for the name " + identifier);
            }
        }
        return null;
    }
}