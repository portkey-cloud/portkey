package portkey;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import clojure.java.api.Clojure;
import clojure.lang.IFn;

import com.amazonaws.services.lambda.runtime.Context;
import com.amazonaws.services.lambda.runtime.RequestStreamHandler;

public class LambdaStub implements RequestStreamHandler {
    static {
        IFn require = Clojure.var("clojure.core", "require");
        require.invoke(Clojure.read("portkey.kryo"));
        IFn unfreeze = Clojure.var("portkey.kryo", "unfreeze");
        InputStream in = null;
        try {
             ClassLoader classLoader = LambdaStub.class.getClassLoader();
             handler = (RequestStreamHandler) ((IFn) unfreeze.invoke(classLoader.getResource("/bootstrap.kryo"))).invoke();
        } catch (Exception e) {
            throw (e instanceof RuntimeException) ? (RuntimeException) e : new RuntimeException(e);
        } finally {
            try {
                if (in != null) in.close();
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
    }

    public final static RequestStreamHandler handler;
    
    @Override
    public void handleRequest(InputStream input, OutputStream output, Context context) throws IOException {
        handler.handleRequest(input, output, context);
    }

}
