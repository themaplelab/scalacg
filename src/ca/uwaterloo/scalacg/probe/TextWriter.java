package ca.uwaterloo.scalacg.probe;

import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import probe.CallGraph;
import probe.ProbeClass;
import probe.ProbeMethod;

/**
 * Writes a call graph to a text file.
 * 
 * @author karim
 * 
 */
public class TextWriter extends probe.GXLWriter {
	private Set<ProbeMethod> methods;
	private Set<ProbeClass> classes;
	private Map<Object, Integer> idMap;

	@Override
	public void write(CallGraph cg, OutputStream file) throws IOException {
		PrintWriter out = new PrintWriter(file);

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

		addClasses(out);
		addMethods(out);

		// Add the entry points to the GXL graph.
		for (ProbeMethod method : cg.entryPoints()) {
			out.println("ENTRYPOINT");
			out.println(getId(method));
		}

		// Add the call edges to the GXL graph.
		for (probe.CallEdge pe : cg.edges()) {
			final CallEdge e = (CallEdge) pe;
			out.println("CALLEDGE");
			out.println(getId(e.src()));
			out.println(getId(e.dst()));
			out.println(e.context());
		}

		out.close();
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

	protected void addClasses(PrintWriter out) {
		for (ProbeClass cls : classes) {
			addClass(out, cls);
		}
	}

	protected void addMethods(PrintWriter out) {
		for (ProbeMethod method : methods) {
			addMethod(out, method);
		}
	}

	protected void addClass(PrintWriter out, ProbeClass cl) {
		out.println("CLASS");
		out.println(getId(cl));
		out.println(cl.pkg());
		out.println(cl.name());
	}

	protected void addMethod(PrintWriter out, ProbeMethod m) {
		out.println("METHOD");
		out.println(getId(m));
		out.println(m.name());
		out.println(m.signature());
		out.println(getId(m.cls()));
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
