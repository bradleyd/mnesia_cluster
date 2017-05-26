# MnesiaCluster

hacking tools around making mnesia cluster easier

### The goal:

* When a node is deployed it will detect if it is the first node in the cluster.  If so, then it will create the tables and schema.

* If the node is not the first, it will ask the other node(s) to add it to the existing cluster.

* If the node already is apart of the cluster (think re-deploy) then just start and wait for tables.


### TODO

* use intelligent service discovery to only ask nodes that are mnesia cluster nodes

* TESTS

* Better error handling when things get weird

* Handle mnesia system events like node down etc.


## Installation

DONT USE
