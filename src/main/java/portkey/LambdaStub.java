package portkey;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;

import clojure.java.api.Clojure;
import clojure.lang.IFn;

import com.amazonaws.services.lambda.runtime.Context;
import com.amazonaws.services.lambda.runtime.RequestStreamHandler;

public class LambdaStub implements RequestStreamHandler {
    static {
        IFn require = Clojure.var("clojure.core", "require");
        require.invoke(Clojure.read("portkey.kryo"));
        IFn unfreeze = Clojure.var("portkey.kryo", "unfreeze");
        try {
             ClassLoader classLoader = LambdaStub.class.getClassLoader();
             URL resource = classLoader.getResource("/bootstrap.kryo");
             handler = resource != null ? (IFn) ((IFn) unfreeze.invoke(resource)).invoke() : null;
        } catch (Exception e) {
            throw (e instanceof RuntimeException) ? (RuntimeException) e : new RuntimeException(e);
        }
    }

    public final static IFn handler;
    
    @Override
    public void handleRequest(InputStream input, OutputStream output, Context context) throws IOException {
        handler.invoke(input, output, context);
    }

}
