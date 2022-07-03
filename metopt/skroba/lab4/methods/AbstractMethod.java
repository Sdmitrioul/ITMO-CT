package skroba.lab4.methods;

import skroba.utils.logger.Logger;

public abstract class AbstractMethod implements Method{
	protected long iterations;
	protected final double EPS;
	protected final String METHOD_NAME;
	private Logger logger;
	
	public AbstractMethod(double EPS, String METHOD_NAME) {
		this.EPS = EPS;
		this.METHOD_NAME = METHOD_NAME;
	}
	
	protected void logState(final String message) {
		if (logger != null) {
			logger.log(message + "\n");
		}
	}
	
	public void setLogger(final Logger logger) {
		this.logger = logger;
	}
	
	@Override
	public long getIterations() {
		return iterations;
	}
}
