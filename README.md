# OCaml SocketCAN
This OCaml library provides bindings to the
[Linux SocketCAN](https://www.kernel.org/doc/Documentation/networking/can.txt)
interface.

To start programming, check the tools:
* candump is a simple can-sniffer using a blocking receive in a loop
* heartbeat uses the broadcast-manager (BCM) to regularly send packages via the kernel

The full interface can be seen in the [API documentation](http://mwweissmann.github.io/ocaml-socketcan/).

The source code of time is available under the MIT license.

This library is originally written by [Markus Weissmann](http://www.mweissmann.de/)
