package ca.uwaterloo.scalacg.probe;

import net.sourceforge.gxl.*;
import probe.*;

import java.io.IOException;
import java.io.InputStream;
import java.util.*;

/**
 * Reads a call graph from a GXL file.
 * 
 * @author karim
 * 
 */
public class GXLReader extends probe.GXLReader {
	private URIs uri;
	private GXLDocument gxlDocument;
	private GXLGraph graph;

	private ArrayList<GXLEdge> edges = new ArrayList<GXLEdge>();
	private ArrayList<GXLNode> nodes = new ArrayList<GXLNode>();

	private Set<GXLNode> classes = new HashSet<GXLNode>();
	private Set<GXLNode> methods = new HashSet<GXLNode>();

	private List<GXLNode> entryPoints = new ArrayList<GXLNode>();
	private List<GXLEdge> callEdges = new ArrayList<GXLEdge>();
	private Map<GXLNode, GXLNode> declaredIn = new HashMap<GXLNode, GXLNode>();

	private Map<GXLNode, ProbeClass> nodeToClass = new HashMap<GXLNode, ProbeClass>();
	private Map<GXLNode, ProbeMethod> nodeToMethod = new HashMap<GXLNode, ProbeMethod>();

	/**
	 * Read a call graph from a GXL file.
	 */
	public CallGraph readCallGraph(InputStream file) throws IOException {
		uri = new URIs("/~olhotak/probe/schemas/callgraph.gxl");
		getGraph(file, "callgraph");
		readNodesEdges();
		sortNodes();
		sortEdges();
		createNodeMaps();

		CallGraph ret = new CallGraph();

		for (GXLNode node : entryPoints) {
			ret.entryPoints().add((ProbeMethod) nodeToMethod.get(node));
		}
		for (GXLEdge edge : callEdges) {
			if (hasAttr(edge, "context")) {
				ret.edges().add(
						new CallEdge((ProbeMethod) nodeToMethod.get(edge.getSource()), (ProbeMethod) nodeToMethod
								.get(edge.getTarget()), new CallSiteContext(getString(edge, "context"))));
			} else {
				ret.edges().add(
						new CallEdge((ProbeMethod) nodeToMethod.get(edge.getSource()), (ProbeMethod) nodeToMethod
								.get(edge.getTarget()), new ContextInsensitive()));
			}
		}

		return ret;
	}

	protected void getGraph(InputStream file, String graphName) {
		gxlDocument = null;
		try {
			gxlDocument = new GXLDocument(file);
		} catch (Exception e) {
			throw new RuntimeException("Caught exception in parsing: " + e);
		}
		graph = (GXLGraph) gxlDocument.getElement(graphName);
	}

	protected void readNodesEdges() {
		for (int i = 0; i < graph.getGraphElementCount(); i++) {
			GXLElement elem = graph.getGraphElementAt(i);
			if (elem instanceof GXLNode) {
				nodes.add((GXLNode) elem);
			} else if (elem instanceof GXLEdge) {
				edges.add((GXLEdge) elem);
			} else {
				throw new RuntimeException("unrecognized graph element " + elem);
			}
		}
	}

	protected boolean eqURI(java.net.URI u1, java.net.URI u2) {
		return u1.getFragment().equals(u2.getFragment());
	}

	protected void sortNodes() {
		for (GXLNode node : nodes) {
			if (eqURI(node.getType().getURI(), uri.uRoot())) {
				// do nothing;
			} else if (eqURI(node.getType().getURI(), uri.uClass())) {
				classes.add(node);
			} else if (eqURI(node.getType().getURI(), uri.uMethod())) {
				methods.add(node);
			} else {
				throw new RuntimeException("unrecognized node " + node + "; its id is " + node.getID());
			}
		}
	}

	protected void sortEdges() {
		for (GXLEdge edge : edges) {
			GXLNode src = (GXLNode) edge.getSource();
			GXLNode dst = (GXLNode) edge.getTarget();
			if (eqURI(edge.getType().getURI(), uri.declaredIn())) {
				declaredIn.put(src, dst);
			} else if (eqURI(edge.getType().getURI(), uri.entryPoint())) {
				entryPoints.add(dst);
			} else if (eqURI(edge.getType().getURI(), uri.calls())) {
				callEdges.add(edge);
			} else {
				throw new RuntimeException("unrecognized edge " + edge + "; its id is " + edge.getID());
			}
		}
	}

	protected void createNodeMaps() {
		for (GXLNode node : classes) {
			nodeToClass.put(node, ObjectManager.v().getClass(getString(node, "package"), getString(node, "name")));
		}

		for (GXLNode node : methods) {
			GXLNode classNode = declaredIn.get(node);
			nodeToMethod.put(
					node,
					ObjectManager.v().getMethod((ProbeClass) nodeToClass.get(classNode), getString(node, "name"),
							getString(node, "signature")));
		}
	}

	protected String getString(GXLAttributedElement elem, String key) {
		GXLAttr attr = elem.getAttr(key);
		GXLAtomicValue value = (GXLAtomicValue) attr.getValue();
		return value.getValue();
	}

	protected boolean hasAttr(GXLAttributedElement elem, String key) {
		GXLAttr attr = elem.getAttr(key);
		return attr != null;
	}
}
