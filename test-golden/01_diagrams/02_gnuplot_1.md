---
title: Gnuplot
---

# Gnuplot

We try out the Gnuplot integration...

``` {.gnuplot table="foo" style="width: 20em"}
set xtic 1
plot $foo using 1:2 with lines title "foo"
```

::::: {data-table="foo" .hidden}
   X    Y
---- ----
   1   10
   2   12
   3    9
   4    3
   5   10
:::::
