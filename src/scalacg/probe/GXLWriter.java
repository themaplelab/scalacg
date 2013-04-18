package scalacg.probe;

import java.io.IOException;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import net.sourceforge.gxl.GXLDocument;
import net.sourceforge.gxl.GXLEdge;
import net.sourceforge.gxl.GXLGraph;
import net.sourceforge.gxl.GXLNode;
import net.sourceforge.gxl.GXLString;
import probe.CallGraph;
import probe.ProbeClass;
import probe.ProbeMethod;
import probe.URIs;

/**
 * Writes a call graph to a GXL file.
 * 
 * @author karim
 * 
 */
public class GXLWriter extends probe.GXLWriter {
	private URIs uri;

	private Set<ProbeMethod> methods;
	private Set<ProbeClass> classes;
	private Map<Object, Integer> idMap;

	@Override
	public void write(CallGraph cg, OutputStream file) throws IOException {
		uri = new URIs("/~olhotak/probe/schemas/callgraph.gxl");
		initializeMaps();

		// Collect up all the methods and classes appearing in the call graph.
		for (ProbeMethod method : cg.entryPoints()) {
			addMethod(method);
		}
		for (probe.CallEdge edge : cg.edges()) {
			addMethod(edge.src());
			addMethod(edge.dst());
		}

		// Assign ids to all method and class nodes.
		assignIDs();

		// Create the GXL nodes in the graph.
		GXLDocument gxlDocument = new GXLDocument();
		GXLGraph graph = new GXLGraph("callgraph");
		graph.setType(uri.uCallGraph());
		GXLNode root = new GXLNode("root");
		root.setType(uri.uRoot());
		graph.add(root);

		addClasses(graph);
		addMethods(graph);

		// Add the entry points to the GXL graph.
		for (ProbeMethod method : cg.entryPoints()) {
			GXLEdge edge = new GXLEdge("root", getId(method));
			edge.setType(uri.entryPoint());
			graph.add(edge);
		}

		// Add the call edges to the GXL graph.
		for (probe.CallEdge pe : cg.edges()) {
			final CallEdge e = (CallEdge) pe;
			GXLEdge edge = new GXLEdge(getId(e.src()), getId(e.dst()));
			edge.setType(uri.calls());

			// Here we don't care about the weight, we want the context.
			/*
			 * if (e.weight() != 0.0) { edge.setAttr("weight", new GXLFloat((float) e.weight())); }
			 */
			edge.setAttr("context", new GXLString(e.context().toString()));
			graph.add(edge);
		}

		// Write out the GXL graph.
		gxlDocument.getDocumentElement().add(graph);
		gxlDocument.write(file);
	}

	/**
	 * Initialize the maps.
	 */
	protected void initializeMaps() {
		methods = new HashSet<ProbeMethod>();
		classes = new HashSet<ProbeClass>();
	}

	/**
	 * Assign ids to all method and class nodes.
	 */
	protected void assignIDs() {
		int id = 1;
		idMap = new HashMap<Object, Integer>();
		for (ProbeMethod method : methods) {
			idMap.put(method, new Integer(id++));
		}
		for (ProbeClass cls : classes) {
			idMap.put(cls, new Integer(id++));
		}
	}

	protected void addMethod(ProbeMethod m) {
		methods.add(m);
		addClass(m.cls());
	}

	protected void addClass(ProbeClass c) {
		classes.add(c);
	}

	protected void addClasses(GXLGraph graph) {
		for (ProbeClass cls : classes) {
			addClass(graph, cls);
		}
	}

	protected void addMethods(GXLGraph graph) {
		for (ProbeMethod method : methods) {
			addMethod(graph, method);
		}
	}

	protected void addClass(GXLGraph graph, ProbeClass cl) {
		GXLNode node = new GXLNode(getId(cl));
		node.setType(uri.uClass());
		node.setAttr("package", new GXLString(cl.pkg()));
		node.setAttr("name", new GXLString(cl.name()));
		graph.add(node);
	}

	protected void addMethod(GXLGraph graph, ProbeMethod m) {
		GXLNode node = new GXLNode(getId(m));
		node.setType(uri.uMethod());
		node.setAttr("name", new GXLString(m.name()));
		node.setAttr("signature", new GXLString(m.signature()));
		graph.add(node);
		GXLEdge classEdge = new GXLEdge(getId(m), getId(m.cls()));
		classEdge.setType(uri.declaredIn());
		graph.add(classEdge);
	}

	protected String getId(ProbeClass cl) {
		Integer id = (Integer) idMap.get(cl);
		return "id" + id.toString();
	}

	protected String getId(ProbeMethod m) {
		Integer id = (Integer) idMap.get(m);
		return "id" + id.toString();
	}
}
