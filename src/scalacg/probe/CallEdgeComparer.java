package scalacg.probe;

import java.util.Comparator;

/**
 * A comparator that compares two ProBe call edges and orders them in ascending order according to their context.
 * 
 * @author karim
 * 
 */
public class CallEdgeComparer implements Comparator<CallEdge> {
	public CallEdgeComparer() {
		super();
	}

	@Override
	public int compare(CallEdge e1, CallEdge e2) {
		return e1.compareTo(e2);
	}
}