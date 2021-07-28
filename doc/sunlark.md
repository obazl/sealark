# sunlark

Scheme bindings for libsealark.

The implementationn is [s7]()

## generalized ref

Example:

Suppose `node` is a Sunlark ast-node representing a rule - a function
application with a list of named arguments. The following two
expressions are equivalent ways of getting the list:

```
(ast-node-ref node :attrs)
(node :attrs)

```

The first is an ordinary Scheme function that calls implementation
routine `sunlark_node_ref_specialized`, which interprets the args; in
this case, it returns the attributes list of the node.

The latter uses s7's object-applicator mechanism. `node` is an
ast-node, which is a datum. But with s7 we can attach an "applicator"
function to such a datum, which will be invoked with it occurs in a
function-application context, e.g. as the car of a list as in this
case. The object itself will be passed as the first argument, and the
remaining elements of the list as the rest of the args. The
implementation can then be designed to do whatever makes sense; in the
case of Sunlark, we use this mechanism to support "pseudo" attributes
such as `:attrs` and `:pprint`.
