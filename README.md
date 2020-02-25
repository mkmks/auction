Vickrey auction
===============

What is it
----------

This package contains three implementations of a sealed-bid, second-price
auction, also known as a Vickrey auction. One of these implementations is also
integrated into an example command-line application.

How to build
------------

This package uses SBT. To produce the .jar, run no more than the following
command:

	sbt package

How to test
-----------

The tests for this package are generated randomly from properties specified with
ScalaCheck which integrates nicely with SBT:

	sbt test

How to use
----------

To use any of the three implementations in your own code, it suffices to import
the `Vickrey` object:

	import org.mkmks.auction.Vickrey

To run the example command-line application: 

	sbt "run inputfile"
	
For example,

	sbt "run examples/Example1.csv"

Each line of the input file must contain bid prices from a single bidder given
as integer numbers (that is, without fractional parts) and separated by
commas. Empty lines are interpreted as bidders who are present during the
auction but don't make bids.

Discussion
----------

We provide three implementations at once to illustrate our design process. Each
implementation has a name: there is a "little", a "seasoned" and a "reasoned"
one, which is a reference to the once-popular series of books on programming in
Scheme written by Daniel Friedman. Just as the book series introduces new
concepts, building gradually on older ones, and culminates in an exercise in
mechanized reasoning, we discover and write down, step by step, the properties
that a robust Vickrey auction implementation ought to have.


Future work
-----------

At the moment, our example application can only read bid information from CSV
files but the groundwork has been laid to extend it into a highly concurrent web
service. The application and the "reasoned" Vickrey implementation are built on
top of the `fs2` streams library which has support for TCP/UDP networking. There
exist HTTP frameworks and message queue clients that are written using `fs2` and
can be readily integrated into our application.
