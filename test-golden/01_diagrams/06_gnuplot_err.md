---
title: Gnuplot error
---

# Gnuplot

We try out the Gnuplot integration when things go wrong...

## An invalid script given to gnuplot

``` {.gnuplot table="foo"}
set xtic 1
plott $foo using 1:2 with lines title "foo"
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

## An invalid data table reference, and the table itself fails to parse.

``` {.gnuplot table="broken"}
set xtic 1
plott $broken using 1:2 with lines title "broken"
```

::::: {data-table="borken" .hidden}
   X      Y
------- -----
**foo** _bar_
:::::
