slox
====

slox is a scripting language initially based on the Lox language as implemented in the Crafting Interpreters book by Bob Nystrom.

Differences from Lox
--------------------

* Class inheritance via : instead of <
* Function definition via **function** instead of **fun**

Added features
--------------

* Multiline comments
* Strings delimited with both **"** and **'**
* Support for escape sequences in strings
* **break** and **continue** in loops
* Modulo operator
* Class hierarchy starting from common Object root
* Support for native methods
* Array type
* Map type (not supporting all value types as keys yet)
* Exception handling via try/catch
* Dispatch via computed goto

