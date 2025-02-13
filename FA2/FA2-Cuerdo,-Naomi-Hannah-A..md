FA2 - Cuerdo, Naomi Hannah A.
================
Cuerdo, Naomi Hannah A.
2025-02-12

``` r
data <-read.csv("C:/Users/naomi/Downloads/cytof_one_experiment.csv")
data(example_gymnastics_2)
str(data)
```

    ## 'data.frame':    50000 obs. of  35 variables:
    ##  $ NKp30        : num  0.188 1.035 3 4.3 -0.439 ...
    ##  $ KIR3DL1      : num  3.616 1.7 6.141 -0.221 -0.504 ...
    ##  $ NKp44        : num  -0.561 -0.289 1.903 0.243 -0.153 ...
    ##  $ KIR2DL1      : num  -0.294 -0.48 0.482 -0.483 0.751 ...
    ##  $ GranzymeB    : num  2.48 3.26 4.28 3.35 3.19 ...
    ##  $ CXCR6        : num  -0.1447 -0.0339 1.9465 0.9262 -0.0589 ...
    ##  $ CD161        : num  -0.315 -0.411 -0.502 3.877 1.091 ...
    ##  $ KIR2DS4      : num  1.945 3.8025 -0.3201 -0.1697 -0.0503 ...
    ##  $ NKp46        : num  4.082 3.734 4.559 4.483 0.838 ...
    ##  $ NKG2D        : num  2.62 -0.483 -0.507 1.927 -0.458 ...
    ##  $ NKG2C        : num  -0.357 -0.468 2.619 -0.311 0.922 ...
    ##  $ X2B4         : num  -0.271 -0.559 -0.455 1.635 1.242 ...
    ##  $ CD69         : num  3.85 2.91 3.11 3.05 2.64 ...
    ##  $ KIR3DL1.S1   : num  -0.255 -0.291 3.661 0.287 0.422 ...
    ##  $ CD2          : num  5.353 4.313 5.597 -0.5 -0.548 ...
    ##  $ KIR2DL5      : num  -0.509 3.777 0.813 0.361 1.064 ...
    ##  $ DNAM.1       : num  0.881 1.541 1.001 1.266 0.872 ...
    ##  $ CD4          : num  -0.3235 -0.1321 -0.5993 -0.1257 -0.0711 ...
    ##  $ CD8          : num  -0.282 0.916 1.838 0.767 -0.106 ...
    ##  $ CD57         : num  3.33 2.49 3.99 2 3.43 ...
    ##  $ TRAIL        : num  -0.608 -0.503 -0.275 -0.513 -0.143 ...
    ##  $ KIR3DL2      : num  -0.3067 -0.5432 2.0649 2.1125 -0.0251 ...
    ##  $ MIP1b        : num  1.25 2.87 4.1 3.37 -0.31 ...
    ##  $ CD107a       : num  -0.13 -0.189 -0.2 -0.572 -0.107 ...
    ##  $ GM.CSF       : num  -0.431 -0.163 3.189 0.913 -0.604 ...
    ##  $ CD16         : num  4 4.41 6 5.82 4.01 ...
    ##  $ TNFa         : num  0.9014 1.9359 -0.0234 -0.6079 -0.6199 ...
    ##  $ ILT2         : num  -0.386 2.9839 -0.5211 -0.0438 1.1827 ...
    ##  $ Perforin     : num  6.43 6.81 5.1 5.84 4.89 ...
    ##  $ KIR2DL2.L3.S2: num  1.2271 -0.0414 -0.1671 -0.5175 -0.3625 ...
    ##  $ KIR2DL3      : num  2.66066 3.8413 -0.00969 -0.59299 -0.39812 ...
    ##  $ NKG2A        : num  -0.522 4.677 -0.473 -0.406 -0.544 ...
    ##  $ NTB.A        : num  4.35 3.47 5.63 4.6 3.61 ...
    ##  $ CD56         : num  2.9 3.78 5.7 6.07 1.97 ...
    ##  $ INFg         : num  -0.384 2.719 2.532 2.456 3.147 ...

**1. Use pivot_longer to reshape the dataset into one that has two
columns, the first giving the protein identity and the second giving the
amount of the protein in one of the cells. The dataset you get should
have 1750000 rows (50000 cells in the original dataset times 35
proteins).**

``` r
cytof_long <- data %>%
  pivot_longer(cols = everything(), names_to = "Protein", values_to = "Amount")

dim(cytof_long)
```

    ## [1] 1750000       2

From the code above, it shows that the dataset has already reshaped into
1,750,000 rows.

**2. Use group_by and summarise to find the median protein level and the
median absolute deviation of the protein level for each marker. (Use the
R functions median and mad).**

``` r
protein_summary <- cytof_long %>%
  group_by(Protein) %>%
  summarise(
    Median = median(Amount, na.rm = TRUE),
    MAD = mad(Amount, na.rm = TRUE)
  )
protein_summary
```

    ## # A tibble: 35 × 3
    ##    Protein  Median   MAD
    ##    <chr>     <dbl> <dbl>
    ##  1 CD107a  -0.122  0.609
    ##  2 CD16     5.12   0.874
    ##  3 CD161    0.726  1.69 
    ##  4 CD2      3.95   1.68 
    ##  5 CD4     -0.204  0.395
    ##  6 CD56     5.71   0.998
    ##  7 CD57     3.07   1.99 
    ##  8 CD69     4.59   1.02 
    ##  9 CD8      2.40   2.29 
    ## 10 CXCR6   -0.0581 0.727
    ## # ℹ 25 more rows

**3. Make a plot with mad on the x-axis and median on the y-axis. This
is known as a spread location (s-l) plot. What does it tell you about
the relationship betwen the median and the mad?**

``` r
ggplot(protein_summary, aes(x = MAD, y = Median)) +
  geom_point() + theme_minimal() + 
  labs(title = "Spread-Location Plot",
       x = "Median Absolute Deviation (MAD)",
       y = "Median Protein Level")
```

![](FA2-Cuerdo,-Naomi-Hannah-A._files/figure-gfm/S-L%20Plot-1.png)<!-- -->

The **Spread-Location (S-L) Plot** visualizes the relationship between
the **median protein expression levels** and **median absolute deviation
(MAD)** across different proteins.

The plot shows a **partial positive correlation**, which means that
proteins with higher median expression often have higher variability.
Some proteins remain **low in both median and MAD**, while others have
**low median but moderate MAD**, indicating fluctuations. A few outliers
exist on the graph.

**4. Using either pivot_longer on its own or pivot_longer in combination
with separate, reshape the dataset so that it has columns for country,
event, year, and score.**

``` r
gymnastics_long <- example_gymnastics_2 %>%
  pivot_longer(cols = -country, names_to = "event_year", values_to = "score") %>%
  separate(event_year, into = c("event", "year"), sep = "_")

gymnastics_long
```

    ## # A tibble: 12 × 4
    ##    country       event year  score
    ##    <chr>         <chr> <chr> <dbl>
    ##  1 United States vault 2012   48.1
    ##  2 United States floor 2012   45.4
    ##  3 United States vault 2016   46.9
    ##  4 United States floor 2016   46.0
    ##  5 Russia        vault 2012   46.4
    ##  6 Russia        floor 2012   41.6
    ##  7 Russia        vault 2016   45.7
    ##  8 Russia        floor 2016   42.0
    ##  9 China         vault 2012   44.3
    ## 10 China         floor 2012   40.8
    ## 11 China         vault 2016   44.3
    ## 12 China         floor 2016   42.1
