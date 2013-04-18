package scalacg.probe;

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
		super(src, dst);
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
	
	public CallingContext context() {
		return ctx;
	}

}