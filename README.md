eLox
====

eLox is a scripting language initially based on the Lox language as implemented in the Crafting Interpreters book by Bob Nystrom.

Differences from Lox
--------------------

* Class inheritance via **:** instead of **<**
* Explicit fields in classes (declared via **local**)
* Explicit variable scope (local/global)
* Function definition via **function** instead of **fun**
* Member access via **:** (**.** now reserved for map access)
* Class-named initializers (can no longer be explicitly referred to)

Added features
--------------

* Multiline comments
* Strings delimited with both **"** and **'**
* Support for escape sequences in strings
* Support for raw strings
* Support for string literal concatenation
* Support for Python-style string formatting (WIP)
* **break** and **continue** in loops
* Modulo operator
* Shorthand operators (+=, -=, *=, /=, %=)
* Class hierarchy starting from common Object root
* Support for native methods
* Support for native closures
* Support for anonymous functions (lambda expressions)
* Support for default arguments
* Array type
* Map type with deterministic iteration order (WIP)
* Exception handling via try/catch
* Can throw exceptions from native functions
* Dispatch via computed goto
* Allow calling functions with a different number of arguments than declared
* Add **foreach** loop
* Support for tuples (implemented on top of arrays, only usable with **foreach** for now)
* Generic iterators (Java-style)
* Can call eLox functions from native code inside the VM
* Initializers are now automatically called
* Can call super initializers with custom arguments
* Module support
* Embed API (WIP)
* Variadic functions
* Support for anonymous classes
