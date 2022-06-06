package nl.utwente.babycobol.data;

import nl.utwente.babycobol.Utils;
import nl.utwente.babycobol.BabyCobolBaseListener;
import nl.utwente.babycobol.BabyCobolParser;
import nl.utwente.babycobol.preprocessor.Line;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ParseTreeProperty;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class QualificationChecker extends BabyCobolBaseListener {

    private final Node variables;

    private final List<Line> lines;
    private boolean procedureStarted;

    /** Stores for each given identifier context the given node with information of the variable */
    private final ParseTreeProperty<Node> variableNode;

    private final List<String> errors;
    private final List<String> warnings;

    public QualificationChecker(Node variableRoot, List<Line> lines) {
        this.variables = variableRoot;
        this.lines = lines;
        this.procedureStarted = false;
        this.variableNode = new ParseTreeProperty<>();
        this.errors = new ArrayList<>();
        this.warnings = new ArrayList<>();
    }

    /**
     * Adds a new error message.
     * @param token         The token that is most related to the error message. It is used to determine the line and
     *                          column numbers.
     * @param errorMessage  The error message to be displayed.
     */
    public void addError(Token token, String errorMessage) {
       this.errors.add(Utils.formatMessage(this.lines, token, errorMessage));
    }

    /**
     * Adds a new warning message.
     * @param token          The token that is most related to the warning message. It is used to determine the line and
     *                          column numbers.
     * @param warningMessage The warning message to be displayed.
     */
    public void addWarning(Token token, String warningMessage) {
        this.warnings.add(Utils.formatMessage(this.lines, token, warningMessage));
    }

    public List<String> getErrors() {
        return errors;
    }

    public List<String> getWarnings() {
        return warnings;
    }

    public boolean hasErrors() {
        return !this.errors.isEmpty();
    }

    public boolean hasWarnings() {
        return !this.warnings.isEmpty();
    }

    /**
     * Since identifiers can case-insensitive and whitespace ignorance is a thing, we have to make sure that we still
     * point to the same identifier. So HEY == H e Y == hey
     * @param identifier The value to normalize
     * @return An all lower-case and no whitespace containing string.
     */
    public static String normalizeIdentifier(String identifier) {
        return identifier.replaceAll(" ", "").toLowerCase();
    }

    @Override
    public void enterProcedureDivision(BabyCobolParser.ProcedureDivisionContext ctx) {
        this.procedureStarted = true;
    }

    @Override
    public void exitNameIdentifier(BabyCobolParser.NameIdentifierContext ctx) {
        // We only want to check plain identifiers when we're in the procedure division.
        // Otherwise, they are just being defined.
        // One edge-case is when qualification is used within the data division.
        if (this.procedureStarted || ctx.getParent() instanceof BabyCobolParser.QuantifiedIdentifierContext) {
            String identifier;
            Token identifierToken;
            if (ctx.ID() != null) {
                identifier = normalizeIdentifier(ctx.ID().getText());
                identifierToken = ctx.ID().getSymbol();
            } else {
                identifier = normalizeIdentifier(ctx.keywords().getText());
                identifierToken = ctx.keywords().getStart();
            }

            // Give a warning when the field is implicitly defined.
            if (!this.variables.containsValue(identifier)) {
                String errorMsg = String.format("Implicit definition for field %s.", identifier);
                addWarning(identifierToken, errorMsg);
                Node node = new Node(identifier, 1, true);
                this.variables.addChild(node);
                this.variableNode.put(ctx, node);
                return;
            }

            // Get all the nodes that can possibly match with the identifier.
            List<Node> matchingNodes = this.variables.getAllNodes(identifier);
            Node node = this.variables.getNode(identifier);

            // If there is more than one match, then there is an ambiguity and qualification is needed.
            if (matchingNodes.size() != 1) {

                // Determine if we should get a field or container node.
                List<Node> fieldNodes = this.variables.getAllFieldNodes(identifier);
                List<Node> containerNodes = this.variables.getAllContainerNodes(identifier);

                if (ctx.getParent() instanceof BabyCobolParser.QuantifiedIdentifierContext && containerNodes.size() == 1) {
                    variableNode.put(ctx, containerNodes.get(0));
                } else if (!(ctx.getParent() instanceof BabyCobolParser.QuantifiedIdentifierContext) && fieldNodes.size() == 1) {
                    variableNode.put(ctx, fieldNodes.get(0));
                } else {
                    reportInsufficientQualification(identifier, identifierToken, matchingNodes);
                }
            } else if (ctx.getParent() instanceof BabyCobolParser.QuantifiedIdentifierContext && node.isField()) {
                // One can only quantify from containers and not from fields.
                String errorMsg = "Unable to quantify from a field.";
                addError(identifierToken, errorMsg);
            } else {
                variableNode.put(ctx, node);
            }
        }
    }

    private void reportInsufficientQualification(String identifier, Token identifierToken, List<Node> matchingNodes) {
        // Collect all possible options and provide them together with an error message.
        StringBuilder availableParents = new StringBuilder();
        for (Node option : matchingNodes) {
            availableParents.append(option.getParent().getValue()).append(", ");
        }
        String options = availableParents.substring(0, availableParents.length() - 2);
        String errorMsg = String.format("Insufficient qualification for field %s, options are: %s.",
                identifier, options);
        addError(identifierToken, errorMsg);
    }

    /**
     * When we leave a quantified identifier, we have to make sure that the parent exists and that there is no option
     * for ambiguity
     * @param ctx the parse tree
     */
    @Override
    public void exitQuantifiedIdentifier(BabyCobolParser.QuantifiedIdentifierContext ctx) {
        String identifier;
        Token identifierToken;
        if (ctx.ID() != null) {
            identifier = normalizeIdentifier(ctx.ID().getText());
            identifierToken = ctx.ID().getSymbol();
        } else {
            identifier = normalizeIdentifier(ctx.keywords().getText());
            identifierToken = ctx.keywords().getStart();
        }

        String containerName = normalizeIdentifier(ctx.identifier().getText());
        Node parent = variableNode.get(ctx.identifier());

        if (parent == null) {
            // The container that's supposed to hold the identifier cannot be found.
            String errorMsg = String.format("The container %s could not be found.", containerName);
            addError(identifierToken, errorMsg);
        } else if (parent.isField()) {
            // The parent is a field, from which we cannot qualify.
            String errorMsg = String.format("The identifier %s points to a field and not a container.", containerName);
            addError(identifierToken, errorMsg);
        } else if (!parent.containsValue(identifier)) {
            // The container exists, but does not hold the identifier.
            String errorMsg = String.format("The container %s does not hold field %s.", containerName, identifier);
            addError(identifierToken, errorMsg);
        } else {
            // The container does contain the variable, now check for ambiguities.
            List<Node> fieldNodes = parent.getAllFieldNodes(identifier);
            List<Node> containerNodes = parent.getAllContainerNodes(identifier);

            boolean isPartOfQuantification = ctx.getParent() instanceof BabyCobolParser.QuantifiedIdentifierContext;
            boolean isDirectChild = parent.isDirectChild(identifier);
            if (isPartOfQuantification) {
                // We need to get a container!
                if (containerNodes.size() == 1) {
                    variableNode.put(ctx, containerNodes.get(0));
                } else if (containerNodes.size() == 0) {
                    String errorMsg = String.format("The identifier %s points to a field and not a container.", identifier);
                    addError(identifierToken, errorMsg);
                } else {
                    // TODO: Solve ambiguity by checking if it is a direct child or give an error message
                    if (isDirectChild) {
                        variableNode.put(ctx, parent.getDirectChild(identifier));
                    } else {
                        reportInsufficientQualification(identifier, identifierToken, containerNodes);
                    }
                }
            } else {
                // We need to get a field!
                if (fieldNodes.size() == 1) {
                    variableNode.put(ctx, fieldNodes.get(0));
                } else if (fieldNodes.size() == 0) {
                    String errorMsg = String.format("The identifier %s points to a container and not a field.", identifier);
                    addError(identifierToken, errorMsg);
                } else {
                    // TODO: Solve ambiguity by checking if it is a direct child or give an error message
                    if (isDirectChild) {
                        variableNode.put(ctx, parent.getDirectChild(identifier));
                    } else {
                        reportInsufficientQualification(identifier, identifierToken, fieldNodes);
                    }
                }
            }
        }
    }
}
