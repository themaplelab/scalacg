package scalacg.probe;

/**
 * The calling context of a call graph edge is the abstraction used to distinguish similar edges. This could be the
 * source of the edge, its call site, etc.
 * 
 * @author karim
 * 
 */
public abstract class CallingContext {

	public abstract String toString();
	public abstract boolean equals(Object o);
	public abstract int hashCode();
	
}
