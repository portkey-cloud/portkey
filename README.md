# Portkey

Live-coding the Cloud!

Portkey allows, at the REPL, to expose any function as an AWS Lambda.

> Almost any inanimate object can be turned into a Portkey. Once bewitched, the object will transport anyone who grasps it to a pre-arranged destination.

> The sensation of travelling by Portkey is universally agreed to be uncomfortable, if not downright unpleasant, and can lead to nausea, giddiness and worse.

([source](https://www.pottermore.com/writing-by-jk-rowling/portkeys))

## Objective

Live at the repl:

```clj
(defn flatter [name]
  (str name " is " (rand-nth ["incredible" "awesome" "fantastic"])))

(pk/mount flatter "/hello")
=> {:url "https://api-id.execute-api.region.amazonaws.com/hello"}
```

go to "https://api-id.execute-api.region.amazonaws.com/hello?name=Rich"

spec could be leveraged so that a s/fdef would automatically turn validation on the api

Once a fn is mounted we can automatically update it each time it or one of
its direct or indirect deps change!

## Master plans

First step:

 1. Borrow Ouroboros from Powderkeg (only class access matters, var tracking is irrelevant in a first time).
 2. Starting from a fn, serialize it using Kryo and a custom SerializerFactory to log all classes traversed during serialization. We also need to log all serialized vars.
 3. Serialize these vars values and keep going until there's no new var.
 4. Consider all classes encountered during steps 2 and 3.
 5. Visit their bytecode to find all references to other classes or vars.
 6. go to 3 until no new classes
 7. package them all
 8. send to the cloud! 
 
Next steps:

 * Since we know the deps of the Lambda, we can redeploy it when a var change locally.
 * `fdef` specs could be leveraged to validate Lambda input.
 * var meta can provide hints for params naming.
 * support other backends than AWS Lambda
 * S3 buckets as Clojure references?
 
## License

Copyright © 2017 Christophe Grand

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
