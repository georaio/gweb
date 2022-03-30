---
title: Gnuplot External Data
---

# Gnuplot

We try out the Gnuplot integration using an external file data source...

``` {.gnuplot file-foo="03_gnuplot_2.dat" style="width: 20em"}
set xtic 1
plot foo using 1:2 with lines title "foo"
```
