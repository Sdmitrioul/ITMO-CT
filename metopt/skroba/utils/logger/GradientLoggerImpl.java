package skroba.utils.logger;

import skroba.utils.Pair;
import skroba.utils.Vector;

import java.util.ArrayList;
import java.util.List;

/**
 * Logger for gradients methods.
 */
public class GradientLoggerImpl implements GradientLogger {
	private Logger logger;
	private final List<Pair<Vector, Double>> data;
	
	/**
	 * Creates entity which writes data and in file in the same time.
	 * @param logger - {@link Logger} realization.
	 * @param data - given {@link List} data.
	 */
	public GradientLoggerImpl(Logger logger, List<Pair<Vector, Double>> data) {
		this.logger = logger;
		this.data = data;
	}
	
	/**
	 * Creates entity with given {@link List}.
	 * @param data - given list, to which write info.
	 */
	public GradientLoggerImpl(List<Pair<Vector, Double>> data) {
		this(null, data);
	}
	
	/**
	 * Creates entity with empty {@link ArrayList}.
	 */
	public GradientLoggerImpl() {
		data = new ArrayList<>();
	}
	
	/**
	 * Creates entity with empty {@link ArrayList} and given Logger.
	 */
	public GradientLoggerImpl(Logger logger) {
		this(logger, new ArrayList<>());
	}
	
	@Override
	public void log(final Pair<Vector, Double> pair) {
		log(pair.toString());
		data.add(pair);
	}
	
	@Override
	public void log(final String message) {
		if (logger != null) logger.log(message);
	}
	
	@Override
	public void close() {
		if (logger != null) logger.close();
	}
	
	/**
	 * Returns history of logging.
	 * @return - List<Pair<Vector, Double>>
	 */
	public List<Pair<Vector, Double>> getData() {
		return data;
	}
}
