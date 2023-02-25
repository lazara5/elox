eLox
====

eLox is a scripting language initially based on the Lox language as implemented in the Crafting Interpreters book by Bob Nystrom.

Differences from Lox
--------------------

* Class inheritance via **:** instead of **<**
* Comments via **#** instead of **//**
* Explicit fields in classes (declared via **local**)
* Explicit variable scope (local/global)
* Function definition via **function** instead of **fun**
* Member access via **:** (**.** now reserved for map access)
* Class-named initializers (can no longer be explicitly referred to)

Added features
--------------

* Multiline comments (**#\*** ... **\*#**)
* Strings delimited with both **"** and **'**
* Supports escape sequences in strings
* Raw strings
* String literal concatenation
* Supports hexadecimal literals
* Python-style string formatting
* **break** and **continue** in loops
* Modulo operator
* Shorthand operators (+=, -=, *=, /=, %=)
* **in** operator (string/array/tuple only)
* Class hierarchy starting from common **Object** root
* Supports native methods
* Supports native closures
* Anonymous functions (lambda expressions)
* Default function and method arguments
* Array type
* Map type with deterministic iteration order (WIP)
* Exception handling via try/catch
* Can throw exceptions from native functions
* Dispatch via computed goto
* Allow calling functions with a different number of arguments than declared
* **foreach** loop
* Tuples (implemented on top of arrays)
* Generic iterators (Java-style)
* Tuple unpacking
* Can call eLox functions from native code inside the VM
* Initializers are now automatically called
* Can call super initializers with custom arguments
* Module support (WIP)
* Embed API (WIP)
* Variadic functions
* Vararg expansion
* Anonymous classes
* Python-style slicing (read-only, **..** instead of **:**)
* Lua-style pattern matching
