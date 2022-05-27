package nl.utwente.babycobol.data;

import java.util.ArrayList;
import java.util.List;

public class Node {

    private final String value;

    private final int level;

    private boolean isField;
    private final List<Node> children;
    private Node parent;

    /**
     * Construct a node with a value, level as provided by the BabyCobol code and a list of children.
     * @param value    The value of this node.
     * @param level    The level as defined in the source code.
     * @param isField  True if this node represents a field, otherwise this node is a container.
     * @param children The children of this node.
     */
    public Node(String value, int level, boolean isField, List<Node> children) {
        this.value = value;
        this.children = children;
        this.isField = isField;
        this.level = level;
    }

    /**
     * Construct a node with a value and level.
     * @param value The value of this node.
     * @param level The level of this node.
     * @param isField  True if this node represents a field, otherwise this node is a container.
     */
    public Node(String value, int level, boolean isField) {
        this(value, level, isField, new ArrayList<>());
    }

    public void setIsField(boolean isField) {
        this.isField = isField;
    }

    public boolean isField() {
        return this.isField;
    }

    public int getLevel() {
        return this.level;
    }

    /**
     * Get the depth of the current node in the tree.
     * @return The depth of the node, where the root node is on depth 0.
     */
    public int getDepth() {
        if (this.parent == null) {
            return 0;
        }
        return this.parent.getDepth() + 1;
    }

    public void addChild(Node node) {
        this.children.add(node);
    }

    public List<Node> getChildren() {
        return children;
    }

    public void setParent(Node parent) {
        this.parent = parent;
    }

    public Node getParent() {
        return parent;
    }

    public String getValue() {
        return this.value;
    }

    /**
     * Evaluates to true if one of the direct children contains the given value.
     * @param value The value to look for.
     * @return True if one of the children contains the value, otherwise false
     */
    public boolean isDirectChild(String value) {
        for (Node child : this.children) {
            if (child.getValue().equals(value)) {
                return true;
            }
        }
        return false;
    }

    public Node getDirectChild(String value) {
        for (Node child : this.children) {
            if (child.getValue().equals(value)) {
                return child;
            }
        }
        return null;
    }

    /**
     * Checks if this node or at least one of its children contains a given value.
     * @param value The value to be looked for.
     * @return True if this value is contained, otherwise false.
     */
    public boolean containsValue(String value) {
        if (this.value.equals(value)) {
            return true;
        }
        for (Node child : this.children) {
            if (child.containsValue(value)) {
                return true;
            }
        }
        return false;
    }

    public boolean containsNode(Node node) {
        if (this.equals(node)) {
            return true;
        }
        for (Node child : this.children) {
            if (child.containsNode(node)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Returns the first node that contains a given value.
     * If the node is not found, null is returned.
     * This function searches this node and all of its children.
     * @param value The value that we're looking for in this node.
     * @return A node object contain the value, or null if no matching node was found.
     */
    public Node getNode(String value) {
        if (this.value.equals(value)) {
            return this;
        }
        for (Node child : this.children) {
            Node result = child.getNode(value);
            if (result != null) {
                return result;
            }
        }
        return null;
    }

    /**
     *
     * @param value
     * @return
     */
    public List<Node> getAllNodes(String value) {
        List<Node> nodes = new ArrayList<>();
        if (this.value.equals(value)) {
            nodes.add(this);
        }
        for (Node child : this.children) {
            nodes.addAll(child.getAllNodes(value));
        }
        return nodes;
    }

    public List<Node> getAllContainerNodes(String value) {
        List<Node> nodes = new ArrayList<>();
        if (this.value.equals(value) && !this.isField) {
            nodes.add(this);
        }
        for (Node child : this.children) {
            nodes.addAll(child.getAllContainerNodes(value));
        }
        return nodes;
    }

    public List<Node> getAllFieldNodes(String value) {
        List<Node> nodes= new ArrayList<>();
        if (this.value.equals(value) && this.isField) {
            nodes.add(this);
        }
        for (Node child : this.children) {
            nodes.addAll(child.getAllFieldNodes(value));
        }
        return nodes;
    }

    /**
     * Creates a deepcopy of a node, removing the connection with the parent.
     * But keeping the connection with its children, and the children connected to it.
     * @param node
     * @return
     */
    public static Node deepCopyNode(Node node) {
        Node copy = new Node(node.getValue(), node.getLevel(), node.isField());
        for (Node child : node.getChildren()) {
            Node childCopy = deepCopyNode(child);
            childCopy.setParent(copy);
            copy.addChild(childCopy);
        }
        return copy;
    }

    @Override
    public int hashCode() {
        return value.hashCode();
    }

    @Override
    public String toString() {
        String thisNode = String.format("%2d: %s%n", this.level, this.value);
        StringBuilder children = new StringBuilder();
        for (Node child : this.children) {
            children.append("  ".repeat(child.getDepth())).append(child);
        }
        thisNode += children;
        return thisNode;
    }
}
