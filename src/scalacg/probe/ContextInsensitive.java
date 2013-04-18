package scalacg.probe;

public class ContextInsensitive extends CallingContext {
	
	@Override
	public String toString() {
		return "";
	}

	@Override
	public boolean equals(Object o) {
		return o instanceof CallSiteContext;
	}

	@Override
	public int hashCode() {
		return "".hashCode();
	}

}
