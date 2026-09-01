# Aggregating CNEFE address counts

In February 2024, IBGE released CNEFE data from the 2022 Census showing
that Brazil has more religious temples (579,800) than educational
establishments (264,400) and health facilities (247,500) combined. This
finding sparked widespread media coverage and public debate (see
[Agência
Brasil](https://agenciabrasil.ebc.com.br/geral/noticia/2024-02/brasil-tem-mais-estabelecimentos-religiosos-que-escolas-e-hospitais)).

However, these figures represent national or state-level aggregates.
What about the **intra-urban** distribution? Are there neighborhoods
with a particularly high concentration of religious establishments
relative to educational establishments?
[`cnefe_counts()`](https://pedreirajr.github.io/cnefetools/dev/reference/cnefe_counts.md)
allows us to answer these questions by aggregating CNEFE address counts
to H3 hexagonal cells or user-provided polygons such as neighborhoods.

In this article, we compute an Index of Concentration at the Extremes
(ICE) comparing educational and religious establishments for the
municipality of Porto Alegre, using both H3 resolution 8 hexagons and
official neighborhood boundaries.

Before computing ICE, let’s read the CNEFE data for Porto Alegre as
point geometries and visualize the spatial distribution of educational
and religious establishments:

### Attaching libraries:

``` r

library(cnefetools)
library(geobr)
library(dplyr)
library(sf)
library(mapview)
```

### Reading CNEFE data to visualize educational and religious establishments:

``` r

poa_cnefe <- read_cnefe(
  code_muni = 4314902, # IBGE code for Porto Alegre)
  cache = T,
  output = 'sf',
  verbose = F
  )

poa_cnefe_edurel <- poa_cnefe |> 
  filter(COD_ESPECIE %in% c(4,8)) |> # Select only educational (4) and religious (8) facilities
  mutate(
    est_type = factor(ifelse(COD_ESPECIE == 4,'Educational','Religious'))
         )

mapview(
  poa_cnefe_edurel, 
  zcol = 'est_type', 
  layer.name = 'Establishment type',
  col.regions = c("blue","red"), # Blue (educational) and Red (religious)
  cex = 2,
  burst = T
)   
```

## The Index of Concentration at the Extremes (ICE)

ICE was originally proposed by Booth & Crouter (2001) to measure spatial
concentration between two groups. We adapt it here to compare
educational (`addr_type4`) and religious (`addr_type8`) establishments:
``` math
\text{ICE} = \frac{n_{\text{educational}} - n_{\text{religious}}}{n_{\text{educational}} + n_{\text{religious}}}
```

The index ranges from -1 to +1:

- **+1**: all establishments are educational (maximum concentration of
  educational establishments)
- **0**: equal number of educational and religious establishments
- **-1**: all establishments are religious (maximum concentration of
  temples)

## Aggregating counts to H3 hexagons

We use
[`cnefe_counts()`](https://pedreirajr.github.io/cnefetools/dev/reference/cnefe_counts.md)
with `polygon_type = "hex"` and `h3_resolution = 8` to aggregate address
counts to H3 hexagonal cells. Resolution 8 cells have an average area of
approximately 0.74 km².

``` r
poa_hex_counts <- cnefe_counts(
  code_muni = 4314902, # IBGE code for Porto Alegre
  polygon_type = "hex",
  h3_resolution = 8
)
#> Warning: The `polygon_type` argument of `cnefe_counts()` is deprecated as of cnefetools
#> 0.3.0.
#> The aggregation mode is now inferred from `polygon`.
#> ℹ Pass an <sf> object to `polygon` for user polygons, or leave it `NULL` for an
#>   H3 grid.
#> ℹ The deprecated feature was likely used in the cnefetools package.
#>   Please report the issue at <https://github.com/pedreirajr/cnefetools/issues>.
#> This warning is displayed once per session.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
#> 
ℹ Step 1/3: Ensuring the CNEFE data file...

                                            
ℹ Using cached file: C:\Users\jorge\AppData\Local/R/cache/R/cnefetools/2022/4314902_PORTO_ALEGRE.csv.gz
#> ℹ Step 1/3: Ensuring the CNEFE data file...

✔ Step 1/3 (CNEFE data ready) [208ms]      
#> 
ℹ Step 2/3: Building full H3 grid over municipality boundary...

✔ Step 2/3 (H3 grid built) [3.8s]                              
#> 
ℹ Step 3/3: Counting address species per hexagon...

✔ Step 3/3 (Addresses counted) [1.7s]              

head(poa_hex_counts)
#> Simple feature collection with 6 features and 9 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -51.10675 ymin: -30.26442 xmax: -51.04588 ymax: -30.23249
#> Geodetic CRS:  WGS 84
#>            id_hex addr_type1 addr_type2 addr_type3 addr_type4 addr_type5
#> 1 88a9010c33fffff          0          0          0          0          0
#> 2 88a9010c37fffff          0          0          0          0          0
#> 3 88a9010c81fffff          5          0          0          0          0
#> 4 88a9010c83fffff          0          0          0          0          0
#> 5 88a9010c85fffff        179          0          0          1          0
#> 6 88a9010c87fffff        444          0          0          0          1
#>   addr_type6 addr_type7 addr_type8                       geometry
#> 1          0          0          0 POLYGON ((-51.0592 -30.2644...
#> 2          0          0          0 POLYGON ((-51.05004 -30.263...
#> 3          0          0          0 POLYGON ((-51.09506 -30.247...
#> 4          0          0          0 POLYGON ((-51.10091 -30.241...
#> 5         25          6          2 POLYGON ((-51.0859 -30.2469...
#> 6         27          6          5 POLYGON ((-51.09175 -30.240...
```

The output contains columns `addr_type1` through `addr_type8`,
corresponding to CNEFE address types. The ones we are interested in are:

- `addr_type4`: Educational establishments
- `addr_type8`: Religious establishments

## Computing ICE for H3 hexagons

We compute ICE only for hexagons that have at least one educational or
religious establishment:

``` r

poa_hex_ice <- poa_hex_counts |>
  filter(addr_type4 > 0 | addr_type8 > 0) |>
  mutate(
    ice = (addr_type4 - addr_type8) / (addr_type4 + addr_type8)
  )

summary(poa_hex_ice$ice)
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#> -1.00000 -1.00000 -0.39231 -0.28022  0.07692  1.00000
```

## Mapping H3 results

We use a diverging color scale: blue indicates higher concentration of
educational establishments (+1), white indicates balance (0), and red
indicates higher concentration of religious establishments (-1).

``` r

mapview(
  poa_hex_ice,
  zcol = "ice",
  col.regions = colorRampPalette(c("red", "white", "blue")),
  layer.name = "ICE (H3 res. 8)"
)
```

## Aggregating counts to neighborhoods

Now we use official neighborhood boundaries from the [`geobr`
package](https://github.com/ipeaGIT/geobr). The
[`cnefe_counts()`](https://pedreirajr.github.io/cnefetools/dev/reference/cnefe_counts.md)
function accepts user-provided polygons via the `polygon` argument.

``` r

# Load Porto Alegre neighborhoods
poa_neighborhoods <- read_neighborhood(year = 2022, simplified = FALSE) |>
  filter(name_muni == "Porto Alegre")

nrow(poa_neighborhoods)
#> [1] 94
```

``` r
poa_neigh_counts <- cnefe_counts(
  code_muni = 4314902,
  polygon_type = "user",
  polygon = poa_neighborhoods
)
#> 
ℹ Step 1/2: Ensuring data and preparing polygon...

                                                   
ℹ Using cached file: C:\Users\jorge\AppData\Local/R/cache/R/cnefetools/2022/4314902_PORTO_ALEGRE.csv.gz
#> ℹ Step 1/2: Ensuring data and preparing polygon...

✔ Step 1/2 (Data and polygon ready) [275ms]       
#> 
ℹ Step 2/2: Counting addresses per polygon...
#> Warning: Polygon coverage: "100.0%" of CNEFE points captured.
#> ℹ 762110 of 762239 points are within the provided polygon.
#> ℹ 129 points fell outside the polygon and were not counted.
#> 
✔ Step 2/2 (Addresses counted) [3.5s]        

head(poa_neigh_counts)
#> Simple feature collection with 6 features and 22 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -51.24266 ymin: -30.07413 xmax: -51.19591 ymax: -30.03231
#> Geodetic CRS:  SIRGAS 2000
#> # A tibble: 6 × 23
#>   code_muni name_muni    code_neighborhood name_neighborhood code_district
#>       <dbl> <chr>                    <dbl> <chr>                     <dbl>
#> 1   4314902 Porto Alegre        4314902001 Medianeira            431490205
#> 2   4314902 Porto Alegre        4314902002 Praia de Belas        431490205
#> 3   4314902 Porto Alegre        4314902003 Cidade Baixa          431490205
#> 4   4314902 Porto Alegre        4314902004 Menino-Deus           431490205
#> 5   4314902 Porto Alegre        4314902005 Farroupilha           431490205
#> 6   4314902 Porto Alegre        4314902006 Santa Cecília         431490205
#> # ℹ 18 more variables: name_district <chr>, code_subdistrict <dbl>,
#> #   name_subdistrict <chr>, code_state <dbl>, abbrev_state <chr>,
#> #   name_state <chr>, code_region <dbl>, name_region <chr>, year <dbl>,
#> #   geometry <MULTIPOLYGON [°]>, addr_type1 <int>, addr_type2 <int>,
#> #   addr_type3 <int>, addr_type4 <int>, addr_type5 <int>, addr_type6 <int>,
#> #   addr_type7 <int>, addr_type8 <int>
```

## Computing ICE by neighborhood

``` r

poa_neigh_ice <- poa_neigh_counts |>
  filter(addr_type4 > 0 | addr_type8 > 0) |>
  mutate(
    ice = (addr_type4 - addr_type8) / (addr_type4 + addr_type8)
  )

summary(poa_neigh_ice$ice)
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#> -1.00000 -0.44444 -0.20000 -0.07657  0.40741  1.00000
```

## Mapping neighborhood results

``` r

mapview(
  poa_neigh_ice,
  zcol = "ice",
  col.regions = colorRampPalette(c("red", "white", "blue")),
  layer.name = "ICE (Neighborhoods)"
)
```

## Comparing spatial resolutions

The H3 hexagonal grid offers several desirable topological properties
for spatial analysis: all cells have the same area and shape, every cell
has exactly six neighbors at equal distances, and the grid avoids the
orientation artifacts that arise with square grids. These properties
make hexagonal grids particularly well-suited for spatial statistics,
neighborhood-based indicators, and smooth interpolation across space.

On the other hand, user-provided polygons, such as neighborhoods or
health districts, carry administrative or institutional meaning that is
directly relevant to policy-making and domain-specific research. By
supporting any polygon geometry,
[`cnefe_counts()`](https://pedreirajr.github.io/cnefetools/dev/reference/cnefe_counts.md)
allows researchers and practitioners to aggregate CNEFE data to
whichever spatial unit best fits their study design.

Both approaches demonstrate how
[`cnefe_counts()`](https://pedreirajr.github.io/cnefetools/dev/reference/cnefe_counts.md)
enables intra-urban analysis, moving beyond aggregate national
statistics to reveal spatial patterns within cities.

## References

Booth, A.; Crouter, A. C. (Eds.). (2001). *Does It Take a Village?
Community Effects on Children, Adolescents, and Families*. Psychology
Press.
