package scalacg.probe;


public class CallSiteContext extends CallingContext {

	// The line number in the Scala source code.
	private String lineNumber; 
	
	public CallSiteContext(String lineNumber) {
		this.lineNumber = lineNumber;
	}
	
	@Override
	public String toString() {
		return lineNumber;
	}

	@Override
	public boolean equals(Object o) {
		if( !(o instanceof CallSiteContext) ) return false;
        CallSiteContext other = (CallSiteContext) o;
        if( !lineNumber.equals(other.lineNumber) ) return false;
        return true;
	}
	
	@Override
	public int hashCode() {
		return lineNumber.hashCode();
	}
}
