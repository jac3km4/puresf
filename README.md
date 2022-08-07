# puresf
Typesafe DSL for AWS Step Functions built with Purescript.

## features
- precise type-checking of inputs and outputs of steps using row types
- support for powerful type-level path literals

## examples
- copy the step function input and pass it to a lambda
  ```purs
  SF.pass (Path :: Path "$.copy")
    >>> SF.invoke myLambda (Path :: Path "$.copy") (Path :: Path "$.result")
  ```
- map over an array
  ```purs
  SF.map (Path :: Path "$.items") (Path :: Path "$.results")
    $ SF.invoke myLambda (Path :: Path "$.user.name") (Path :: Path "$")
  ```
- see more in [tests](test/Main.purs)
