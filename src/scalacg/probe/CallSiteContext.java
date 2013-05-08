package scalacg.probe;

public class CallSiteContext extends CallingContext<CallSiteContext> {

	// The call site context has the form (filename : line number)
	private String context;

	public CallSiteContext(String context) {
		this.context = context;
	}

	@Override
	public String toString() {
		return context;
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof CallSiteContext))
			return false;
		CallSiteContext other = (CallSiteContext) o;
		if (!context.equals(other.context))
			return false;
		return true;
	}

	@Override
	public int hashCode() {
		return context.hashCode();
	}

	@Override
	public int compareTo(CallSiteContext other) {
		String thisSourceFile = sourceFile();
		String otherSourceFile = other.sourceFile();
		int thisLineNumber = lineNumber();
		int otherLineNumber = other.lineNumber();

		// First compare the source file names
		int result = thisSourceFile.compareTo(otherSourceFile);

		// If it's in the same file, then compare the line numbers
		return result == 0 ? (thisLineNumber < otherLineNumber ? -1 : (thisLineNumber > otherLineNumber ? 1 : 0))
				: result;
	}

	/**
	 * Get the source code line number of the call site.
	 * 
	 * @return
	 */
	public int lineNumber() {
		return Integer.parseInt(context.split(":")[1].trim().replace("line ", ""));
	}

	/**
	 * Get the source file that contains this call site from its context.
	 * 
	 * @return
	 */
	public String sourceFile() {
		return context.split(":")[0].trim();
	}
}
