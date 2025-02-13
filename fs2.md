`++` or `append` operator constructs a resulting stream from a left and right input stream, by pulling on the left until it is done, then pulling on the right. It is analogous to appending lists with the ++ list concatenation operator.

`(Stream('a', 'b') ++ Stream('x', 'y')).compile.toList`
 a stream of `a` and `b` characters is appended to a stream of `x` and `y`

 
The `Stream.bracket(acquire)(release)` operator constructs a stream from an `acquire` and `release` effect. The stream outputs a single element as a result of running `acquire`. When the stream terminates, the release effect is guaranteed to run, regardless of whether the stream terminates successfully, errors or is cancelled.


The `release` effect is run at the end of the stream's lifetime, not at the end of the entire program.


## reification

The act of changing something abstract (= existing as a thought or idea) into something real

In computer science, reification is the process by which an abstract idea about a program is turned into an explicit data model or other object created in a programming language.


Summary: Reification means making an abstraction into a concrete value that can be manipulated at runtime

[reification](https://ericnormand.me/article/reification)


program stack(execution stack)

