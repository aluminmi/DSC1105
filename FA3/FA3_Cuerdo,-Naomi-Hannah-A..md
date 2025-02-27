FA3_CUERDO, Naomi Hannah A.
================
Cuerdo, Naomi Hannah A.
2025-02-26

``` r
data("diamonds")
```

1.  Create a histogram on the diamonds dataset, for example with
    ggplot() + geom_histogram(aes(x = carat), data = diamonds)

Re-write this using the layer function like we did in class. Hint: if
you don’t know what the default values for some of the aspects of the
plot, examine p\$layers.

``` r
histo_layer <- ggplot(diamonds, aes(x = carat)) +
  layer(
    geom = "bar",
    stat = "bin",
    position = "stack",
    mapping = aes(y = after_stat(count))
  )

histo_layer
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](FA3_Cuerdo,-Naomi-Hannah-A._files/figure-gfm/layer%20function-1.png)<!-- -->

2.  Remember that a histogram is a plot with stat_bin and geom_bar.
    Modify your histogram code so that it uses a different geom, for
    example geom_line or geom_point. This should be simple once you have
    the layer specification of a histogram.

``` r
histo_layer2 <- ggplot(diamonds, aes(x = carat)) +
  layer(
    geom = "point",
    stat = "bin",
    position = "identity",
    mapping = aes(y = after_stat(count))
  )

histo_layer2
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](FA3_Cuerdo,-Naomi-Hannah-A._files/figure-gfm/histogram%20modify%20using%20geom_point-1.png)<!-- -->

The graph uses points to visualize the data instead of bars.

We can also use lines for visualization:

``` r
histo_layer3 <- ggplot(diamonds, aes(x = carat)) +
  layer(
    geom = "line",
    stat = "bin",
    position = "identity",
    mapping = aes(y = after_stat(count))
  )

histo_layer3
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](FA3_Cuerdo,-Naomi-Hannah-A._files/figure-gfm/histogram%20modify%20using%20geom_line-1.png)<!-- -->
3. In your histogram (the one plotted with bars that you created in
question 1), add an aesthetic mapping from one of the factor variables
(maybe color or clarity) to the fill or color aesthetic.

``` r
histo_layer4 <- ggplot(diamonds, aes(x = carat, fill = color)) +
  layer(
    geom = "bar",
    stat = "bin",
    position = "stack",
    mapping = aes(y = after_stat(count))
  )

histo_layer4
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](FA3_Cuerdo,-Naomi-Hannah-A._files/figure-gfm/aesthetic%20mapping-1.png)<!-- -->

4.  What is the default position adjustment for a histogram? Try
    changing the position adjustment in the histogram you created in
    question 3 to something different (hint: try dodge).

``` r
histo_layer5 <- ggplot(diamonds, aes(x = carat, fill = color)) +
  layer(
    geom = "bar",
    stat = "bin",
    position = "dodge",
    mapping = aes(y = after_stat(count))
  )

histo_layer5
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](FA3_Cuerdo,-Naomi-Hannah-A._files/figure-gfm/histo%20layer%205-1.png)<!-- -->

By default, histograms use **position = “stack”**, but changing it to
**dodge** will place the bars next to each other instead of stacking
them.
