package scalacg.probe;

import probe.CallGraph;
import probe.GXLReader;
import probe.ProbeMethod;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;
import java.util.zip.GZIPInputStream;

public class CallGraphDiff {

	// The arguments
	private static String superFile = null;
	private static String subFile = null;
	private static boolean dashEP = false;
	private static boolean dashCE = false;
	private static boolean dashR = false;

	@SuppressWarnings("unchecked")
	public static void main(String[] args) {
		processArguments(args);

		CallGraph superGraph = readCallGraph(superFile);
		CallGraph subGraph = readCallGraph(subFile);

		CallGraph missing = minus(superGraph, subGraph);
		CallGraph extra = minus(subGraph, superGraph);

		Set<ProbeMethod> missingReachables = minus((Set<ProbeMethod>) superGraph.findReachables(),
				(Set<ProbeMethod>) subGraph.findReachables());

		Set<ProbeMethod> extraReachables = minus((Set<ProbeMethod>) subGraph.findReachables(),
				(Set<ProbeMethod>) superGraph.findReachables());

		System.out.println("/*************************************************************************/");
		System.out.println("Info about " + superFile);
		System.out.println("/*************************************************************************/");
		System.out.println("Reachable methods: " + superGraph.findReachables().size());
		System.out.println("Entry points: " + superGraph.entryPoints().size());
		System.out.println("Call graph edges: " + superGraph.edges().size());
		System.out.println("");
		System.out.println("");

		System.out.println("/*************************************************************************/");
		System.out.println("Info about " + subFile);
		System.out.println("/*************************************************************************/");
		System.out.println("Reachable methods: " + subGraph.findReachables().size());
		System.out.println("Entry points: " + subGraph.entryPoints().size());
		System.out.println("Call graph edges: " + subGraph.edges().size());
		System.out.println("");
		System.out.println("");
		System.out.println("");
		System.out.println("");

		System.out.println("/*************************************************************************/");
		System.out.println("Missing from " + subFile);
		System.out.println("/*************************************************************************/");
		System.out.println("Reachable methods: " + missingReachables.size());
		if (dashR) {
			for (ProbeMethod method : missingReachables) {
				System.out.println(method);
			}
			System.out.println("===========================================================================");
		}

		System.out.println("Entry points: " + missing.entryPoints().size());
		if (dashEP) {
			for (ProbeMethod method : missing.entryPoints()) {
				System.out.println(method);
			}
			System.out.println("===========================================================================");
		}

		System.out.println("Call edges: " + missing.edges().size());
		if (dashCE) {
			for (probe.CallEdge edge : missing.edges()) {
				System.out.println(edge);
			}
			System.out.println("===========================================================================");
		}

		System.out.println("");
		System.out.println("");

		System.out.println("/*************************************************************************/");
		System.out.println("Missing from " + superFile);
		System.out.println("/*************************************************************************/");
		System.out.println("Reachable methods: " + extraReachables.size());
		if (dashR) {
			for (ProbeMethod method : extraReachables) {
				System.out.println(method);
			}
			System.out.println("===========================================================================");
		}

		System.out.println("Entry points: " + extra.entryPoints().size());
		if (dashEP) {
			for (ProbeMethod method : extra.entryPoints()) {
				System.out.println(method);
			}
			System.out.println("===========================================================================");
		}
		System.out.println("Call edges: " + extra.edges().size());
		if (dashCE) {
			for (probe.CallEdge edge : extra.edges()) {
				System.out.println(edge);
			}
			System.out.println("===========================================================================");
		}
		
		System.out.println("");
		System.out.println("");
		System.out.println("");
		System.out.println("");
	}

	/**
	 * Process the input arguments to the program.
	 * 
	 * @param args
	 */
	private static void processArguments(String[] args) {
		if (args.length < 2) {
			usage();
		}

		for (int i = 0; i < args.length; i++) {
			if ("-ep".equals(args[i])) {
				dashEP = true;
			} else if ("-ce".equals(args[i])) {
				dashCE = true;
			} else if ("-r".equals(args[i])) {
				dashR = true;
			} else if (superFile == null) {
				superFile = args[i];
			} else if (subFile == null) {
				subFile = args[i];
			} else {
				usage();
			}
		}
	}

	/**
	 * Print the program usage.
	 */
	private static void usage() {
		System.out
				.println("Usage: java -classpath callgraph-plugin.jar scalacg.probe.CallGraphDiff [options] supergraph.gxl subgraph.gxl");
		System.out.println("  -ce : print all edges instead of just the number of edges");
		System.out.println("  -r : print names of missing methods");
		System.out.println("  -ep : print names of missing entry points");
		System.exit(1);
	}

	/**
	 * Read in a probe call graph.
	 * 
	 * @param filename
	 * @return
	 */
	private static CallGraph readCallGraph(String filename) {
		CallGraph ret;
		try {
			try {
				ret = new GXLReader().readCallGraph(new FileInputStream(filename));
			} catch (Exception e) {
				System.out.println(e);
				ret = new GXLReader().readCallGraph(new GZIPInputStream(new FileInputStream(filename)));
			}
		} catch (IOException e) {
			throw new RuntimeException("caught IOException " + e + " on file " + filename);
		}
		return ret;
	}

	/**
	 * Compute the set difference between two call graphs
	 * 
	 * @param other
	 * @param options
	 * @param source
	 * @return
	 */
	private static CallGraph minus(CallGraph superGraph, CallGraph subGraph) {
		CallGraph result = new CallGraph();
		result.entryPoints().addAll(minus(superGraph.entryPoints(), subGraph.entryPoints()));
		result.edges().addAll(minus(superGraph.edges(), subGraph.edges()));
		return result;
	}

	/**
	 * Calculate the value of A - B.
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	private static <T> Set<T> minus(Set<T> a, Set<T> b) {
		Set<T> result = new HashSet<T>(a);
		result.addAll(a);
		result.removeAll(b);
		return result;
	}
}
