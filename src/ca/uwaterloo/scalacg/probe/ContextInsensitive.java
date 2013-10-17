package ca.uwaterloo.scalacg.probe;

public class ContextInsensitive extends CallingContext<ContextInsensitive> {
	
	@Override
	public String toString() {
		return "";
	}

	@Override
	public boolean equals(Object o) {
		return o instanceof ContextInsensitive;
	}

	@Override
	public int hashCode() {
		return "".hashCode();
	}

	@Override
	public int compareTo(ContextInsensitive o) {
		return 0;
	}

}
