# OCaml SocketCAN
This OCaml library provides bindings to the
[Linux SocketCAN](https://www.kernel.org/doc/Documentation/networking/can.txt)
interface.

To start programming, check the tools:
* candump is a simple can-sniffer using a blocking receive in a loop
* heartbeat uses the broadcast-manager (BCM) to regularly send packages via the kernel

The full interface can be seen in the [API documentation](http://mwweissmann.github.io/ocaml-socketcan/).

To compile OCaml-SocketCAN you need to have the Linux header files on your system. OCaml-SocketCAN also requires the [OCaml posix-time](https://github.com/mwweissmann/ocaml-posix-time) library for timestamps on CAN frames.

The source code of time is available under the MIT license.

This library is originally written by [Markus Weissmann](http://www.mweissmann.de/)
