package scalacg.probe;

import probe.CallGraph;
import probe.ProbeMethod;
import probe.Util;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.zip.GZIPInputStream;

public class CallGraphInfo {
	public static String dashLib = null;
	public static boolean dashM = false;
	public static boolean dashE = false;
	public static boolean dashG = false;

	public static void usage() {
		System.out
				.println("Usage: java -classpath callgraph-plugin.jar scalacg.probe.CallGraphInfo [options] graph.gxl");
		System.out.println("  -m : print list of reachable methods");
		System.out.println("  -e : print list of entry points");
		System.out.println("  -g : print list of call edges");
		System.out.println("  -lib file : ignore methods in packages listed in file");
		System.exit(1);
	}

	@SuppressWarnings("rawtypes")
	public static final void main(String[] args) {
		// Parse the options
		boolean doneOptions = false;
		String filename = null;
		for (int i = 0; i < args.length; i++) {
			if (!doneOptions && args[i].equals("-lib")) {
				dashLib = args[++i];
			} else if (!doneOptions && args[i].equals("-m")) {
				dashM = true;
			} else if (!doneOptions && args[i].equals("-e")) {
				dashE = true;
			} else if (!doneOptions && args[i].equals("-g")) {
				dashG = true;
			} else if (!doneOptions && args[i].equals("--")) {
				doneOptions = true;
			} else if (filename == null) {
				filename = args[i];
			} else {
				usage();
			}
		}
		if (filename == null) {
			usage();
		}

		Collection libs = Util.readLib(dashLib);

		// Read the call graph
		CallGraph graph;
		try {
			try {
				graph = new GXLReader().readCallGraph(new FileInputStream(filename));
			} catch (RuntimeException e) {
				graph = new GXLReader().readCallGraph(new GZIPInputStream(new FileInputStream(filename)));
			}
		} catch (IOException e) {
			throw new RuntimeException("caught IOException " + e);
		}

		// Get the entry points and edges
		Set<ProbeMethod> methods = new HashSet<ProbeMethod>();
		methods.addAll(graph.entryPoints());
		for (probe.CallEdge edge : graph.edges()) {
			methods.add(edge.src());
			methods.add(edge.dst());
		}

		// Print out the call graph info
		Collection ep = Util.filterLibs(libs, graph.entryPoints());
		System.out.println("Entry points     : " + Util.filterLibs(libs, ep).size());
		System.out.println("Edges            : " + Util.filterLibs(libs, graph.edges()).size());
		System.out.println("Methods          : " + Util.filterLibs(libs, methods).size());
		Collection rm = Util.filterLibs(libs, graph.findReachables());
		System.out.println("Reachable methods: " + rm.size());

		if (dashE) {
			System.out.println("Entry points: ");
			for (Iterator pmIt = ep.iterator(); pmIt.hasNext();) {
				final ProbeMethod pm = (ProbeMethod) pmIt.next();
				System.out.println(pm);
			}
		}

		if (dashM) {
			System.out.println("Reachable methods: ");
			for (Iterator pmIt = rm.iterator(); pmIt.hasNext();) {
				final ProbeMethod pm = (ProbeMethod) pmIt.next();
				System.out.println(pm);
			}
		}

		if (dashG) {
			System.out.println("Call Edges: ");
			for (Iterator edgeIt = graph.edges().iterator(); edgeIt.hasNext();) {
				final CallEdge edge = (CallEdge) edgeIt.next();
				System.out.println(edge);
			}
		}
	}

}
