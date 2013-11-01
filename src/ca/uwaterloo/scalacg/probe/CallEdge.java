package ca.uwaterloo.scalacg.probe;

import java.util.HashSet;
import java.util.Set;

import probe.ProbeMethod;

/**
 * A call edge with context information.
 * 
 * @author karim
 * 
 */
@SuppressWarnings("rawtypes")
public class CallEdge extends probe.CallEdge {

	private CallingContext ctx;

	/**
	 * Create a call edge from source to destination without a calling context.
	 * 
	 * @param src
	 * @param dst
	 * @param context
	 */
	public CallEdge(ProbeMethod src, ProbeMethod dst) {
		super(src, dst, 0);
		this.ctx = new ContextInsensitive();
	}

	/**
	 * Create a call edge from source to destination with the given calling context.
	 * 
	 * @param src
	 * @param dst
	 * @param context
	 */
	public CallEdge(ProbeMethod src, ProbeMethod dst, CallingContext context) {
		super(src, dst, 0);
		this.ctx = context;
	}

	@Override
	public int hashCode() {
		return src().hashCode() + dst().hashCode() + ctx.hashCode();
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof CallEdge))
			return false;

		CallEdge other = (CallEdge) o;
		return src().equals(other.src()) && dst().equals(other.dst()) && ctx.equals(other.ctx);
	}

	@Override
	public String toString() {
		String context = ctx instanceof ContextInsensitive ? "" : ctx.toString() + " :: ";
		if (weight() != 0)
			return context + src().toString() + " ===> " + dst().toString() + " " + weight();
		return context + src().toString() + " ===> " + dst().toString();
	}

	@SuppressWarnings("unchecked")
	public int compareTo(CallEdge other) {
		int result = ctx.compareTo(other.ctx);
		return result == 0 ? super.compareTo(other) : result;
	}

	public CallingContext context() {
		return ctx;
	}

	/**
	 * Convert from probe.CallEdge to CallEdge
	 * 
	 * @param edges
	 * @return
	 */
	public static Set<CallEdge> probeToScalacgEdge(Set<probe.CallEdge> edges) {
		Set<CallEdge> result = new HashSet<CallEdge>();

		for (probe.CallEdge edge : edges) {
			if (edge instanceof CallEdge) {
				result.add((CallEdge) edge);
			}
		}

		return result;
	}
}