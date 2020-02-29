# bspcheck

bspcheck is a Frama-C plugin. It aims at checking correctness of BSP Programs.
Is it part of an ongoing effort on the verification of textually aligned SPMD Programs and is highly experimental.

## Installation 

bspcheck requires Frama-c to be installed. 
To install the plugin

make
make install


## Use 

The following command produces a file cfg.dot that describes a control flow graph such that
- Textually aligned program points are blue
- Other program points are red

frama-c -bspcheck <C file>

To produce the graph

dot -Tpdf cfg.dot -o cfg.pdf

## References 

Safe Usage of Registers in BSPlib
SAC 2019, Apr 2019, Limassol, Cyprus
Wadoud Bousdira, Arvid Jakobsson, Frederic Dabrowski,

Safe Usage of Registers in BSPlib (Preprint)
2019
Arvid Jakobsson, Frederic Dabrowski, Wadoud Bousdira,

A denotational semantics of textually aligned SPMD Program
Journal of Logical and Algebraic Methods in Programming, Elsevier, In press
Frederic Dabrowski,

A Denotational Semantics of Textually Aligned SPMD Programs
International Symposium on Formal Approaches to Parallel and Distributed Systems (4PAD 2018), Jul 2018, Orléans, France
Frédéric Dabrowski,

Textual Alignment in SPMD Programs
SAC '18: Proceedings of the 33rd Annual ACM Symposium on Applied Computing, Apr 2018, Pau, France
Frederic Dabrowski,

Replicated Synchronization for Imperative BSP Programs
International Conference on Computational Science (ICCS), 2017, Zürich, Switzerland
Arvid Jakobsson, Frederic Dabrowski, Wadoud Bousdira, Frédéric Loulergue, Gaetan Hains, 
