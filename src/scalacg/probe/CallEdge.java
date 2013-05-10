package scalacg.probe;

import java.util.HashSet;
import java.util.Set;

import probe.ProbeMethod;

/**
 * A call edge with context information.
 * 
 * @author karim
 * 
 */
public class CallEdge extends probe.CallEdge {

	private CallingContext ctx;

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
		if (weight() != 0)
			return ctx.toString() + " :: " + src().toString() + " ===> " + dst().toString() + " " + weight();
		return ctx.toString() + " :: " + src().toString() + " ===> " + dst().toString();
	}

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