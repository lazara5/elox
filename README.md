slox
====

slox is a scripting language initially based on the Lox language as implemented in the Crafting Interpreters book by Bob Nystrom.

Differences from Lox
--------------------

* Class inheritance via **:** instead of **<**
* Function definition via **function** instead of **fun**
* Add **:** operator to access methods (handy for native methods in the Table class)

Added features
--------------

* Multiline comments
* Strings delimited with both **"** and **'**
* Support for escape sequences in strings
* **break** and **continue** in loops
* Modulo operator
* Class hierarchy starting from common Object root
* Support for native methods
* Support for native closures
* Support for anonymous functions (lambda expressions)
* Array type
* Map type (not supporting all value types as keys yet)
* Exception handling via try/catch
* Can throw exceptions from native functions
* Dispatch via computed goto
* Allow calling functions with a different number of arguments than declared
* Support for tuples (implemented on top of arrays, only usable with foreach for now)
* Generic iterators (Lua-style, both stateful and stateless)
* Can call functions from inside the VM
