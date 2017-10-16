package portkey;

import java.lang.instrument.Instrumentation;
import java.util.concurrent.CountDownLatch;

public class Agent {
    static {
        if (Agent.class.getClassLoader() != ClassLoader.getSystemClassLoader())
            throw new IllegalStateException("This class is expected to be loaded by the system classloader.");
    }
    private static Instrumentation instrumentation;
    private static final CountDownLatch latch = new CountDownLatch(1);
    public static Instrumentation instrumentation() throws InterruptedException {
        latch.await();
        return instrumentation;
    };
    public static boolean ready() throws InterruptedException {
        return latch.getCount() <= 0;
    };
	public static void agentmain(String args, Instrumentation instrumentation) {
		Agent.instrumentation = instrumentation;
		latch.countDown();
	}
}
