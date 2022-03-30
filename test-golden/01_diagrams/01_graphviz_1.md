---
title: GraphViz
---

# GraphViz

We try out the GraphViz integration...

``` {.dot style="width: 20em"}
digraph {
  graph [rankdir=BT, margin=0];

  B -> A;
  C -> A;

  D -> B
  E -> B
  F -> B

  G -> C
}
```
