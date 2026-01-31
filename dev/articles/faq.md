# FAQ

## What is H3 and how does resolution work?

H3 is a geospatial indexing system developed by Uber that divides the
world into hexagonal cells. Unlike traditional grids (squares), hexagons
provide more uniform distances between neighboring cells and better
represent spatial relationships.

The **resolution** parameter controls hexagon size across 16 levels
(0-15). Lower resolutions create larger hexagons suitable for regional
analysis, while higher resolutions create smaller hexagons that reveal
neighborhood-level detail. Each resolution level reduces hexagon area by
approximately 7×.

Learn more at [h3geo.org](https://h3geo.org/).

### Visual comparison: São Paulo at different resolutions

Below we show the same municipality (São Paulo - SP) at resolutions 1
through 9. Notice how finer resolutions reveal spatial patterns that are
smoothed out at coarser scales.

The base function used in this example:

``` r
library(cnefetools)

tracts_to_h3(
    code_muni = 3550308,  # São Paulo
    h3_resolution = res   # 1 to 9
  )
```

    #> Warning: package 'mapview' was built under R version 4.3.3

### H3 resolution = 1 (Average Hexagon Area = 609,788 km²)

![plot of chunk
unnamed-chunk-5](faq_files/figure-html/unnamed-chunk-5-1.png)

plot of chunk unnamed-chunk-5

### H3 resolution = 2 (Average Hexagon Area = 86,802 km²)

![plot of chunk
unnamed-chunk-6](faq_files/figure-html/unnamed-chunk-6-1.png)

plot of chunk unnamed-chunk-6

### H3 resolution = 3 (Average Hexagon Area = 12,393 km²)

![plot of chunk
unnamed-chunk-7](faq_files/figure-html/unnamed-chunk-7-1.png)

plot of chunk unnamed-chunk-7

### H3 resolution = 4 (Average Hexagon Area = 1,770 km²)

![plot of chunk
unnamed-chunk-8](faq_files/figure-html/unnamed-chunk-8-1.png)

plot of chunk unnamed-chunk-8

### H3 resolution = 5 (Average Hexagon Area = 253 km²)

![plot of chunk
unnamed-chunk-9](faq_files/figure-html/unnamed-chunk-9-1.png)

plot of chunk unnamed-chunk-9

### H3 resolution = 6 (Average Hexagon Area = 36 km²)

![plot of chunk
unnamed-chunk-10](faq_files/figure-html/unnamed-chunk-10-1.png)

plot of chunk unnamed-chunk-10

### H3 resolution = 7 (Average Hexagon Area = 5 km²)

![plot of chunk
unnamed-chunk-11](faq_files/figure-html/unnamed-chunk-11-1.png)

plot of chunk unnamed-chunk-11

### H3 resolution = 8 (Average Hexagon Area = 0.7 km²)

![plot of chunk
unnamed-chunk-12](faq_files/figure-html/unnamed-chunk-12-1.png)

plot of chunk unnamed-chunk-12

### H3 resolution = 9 (Average Hexagon Area = 0.1 km²)

![plot of chunk
unnamed-chunk-13](faq_files/figure-html/unnamed-chunk-13-1.png)

plot of chunk unnamed-chunk-13
