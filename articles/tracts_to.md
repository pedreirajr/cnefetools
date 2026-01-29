# Dasymetric interpolation with tracts_to\_\* functions

Census data in Brazil are published at the census tract level. However,
many research and planning applications require data at different
spatial granularities, such as hexagonal grids for standardized spatial
analysis, or custom polygons such as traffic zones for transportation
modeling.

[`tracts_to_h3()`](https://pedreirajr.github.io/cnefetools/reference/tracts_to_h3.md)
and
[`tracts_to_polygon()`](https://pedreirajr.github.io/cnefetools/reference/tracts_to_polygon.md)
perform **dasymetric interpolation** using CNEFE dwelling points as
ancillary data. Instead of assuming uniform distribution within tracts,
they allocate tract-level values to individual dwelling points
identified in the CNEFE and then aggregate these to the target spatial
units. The allocation rule depends on the variable type:

- **Sum variables** (e.g., `pop_ph`, `race_preta`, `age_70m`): the tract
  total is divided equally among eligible dwelling points. The target
  spatial unit (e.g., hexagons, traffic zones, or any user-supplied
  polygon) receives the sum of all point-level shares that fall within
  it.
- **Average variables** (`avg_inc_resp`): the tract-level average is
  assigned (not split) to each eligible dwelling point. The target
  spatial unit receives the average of all assigned values within it.

This approach preserves the heterogeneous distribution of dwellings
within each tract, yielding more accurate estimates than simple areal
interpolation.

This article demonstrates both functions:
[`tracts_to_h3()`](https://pedreirajr.github.io/cnefetools/reference/tracts_to_h3.md)
for transferring census variables to an H3 hexagonal grid in Fortaleza,
and
[`tracts_to_polygon()`](https://pedreirajr.github.io/cnefetools/reference/tracts_to_polygon.md)
for transferring census variables to traffic zones in São Paulo.

The currently supported variables (and their respective IBGE census
tract codes) can be found using the `tracts_variables_ref` dataframe
that accompanies the package:

``` r
library(cnefetools)

tracts_variables_ref |>
  knitr::kable()
```

| var_cnefetools | code_var_ibge | desc_var_ibge                                                         | table_ibge |
|:---------------|:--------------|:----------------------------------------------------------------------|:-----------|
| pop_ph         | V00005        | Domicilios Particulares Permanentes Ocupados, Quantidade de moradores | Domicilios |
| pop_ch         | V00007        | Domicilios Coletivos Com Morador, Quantidade de moradores             | Domicilios |
| male           | V01007        | Sexo masculino                                                        | Pessoas    |
| female         | V01008        | Sexo feminino                                                         | Pessoas    |
| age_0_4        | V01031        | 0 a 4 anos                                                            | Pessoas    |
| age_5_9        | V01032        | 5 a 9 anos                                                            | Pessoas    |
| age_10_14      | V01033        | 10 a 14 anos                                                          | Pessoas    |
| age_15_19      | V01034        | 15 a 19 anos                                                          | Pessoas    |
| age_20_24      | V01035        | 20 a 24 anos                                                          | Pessoas    |
| age_25_29      | V01036        | 25 a 29 anos                                                          | Pessoas    |
| age_30_39      | V01037        | 30 a 39 anos                                                          | Pessoas    |
| age_40_49      | V01038        | 40 a 49 anos                                                          | Pessoas    |
| age_50_59      | V01039        | 50 a 59 anos                                                          | Pessoas    |
| age_60_69      | V01040        | 60 a 69 anos                                                          | Pessoas    |
| age_70m        | V01041        | 70 anos ou mais                                                       | Pessoas    |
| race_branca    | V01317        | Cor ou raca e branca                                                  | Pessoas    |
| race_preta     | V01318        | Cor ou raca e preta                                                   | Pessoas    |
| race_parda     | V01320        | Cor ou raca e parda                                                   | Pessoas    |
| race_amarela   | V01319        | Cor ou raca e amarela                                                 | Pessoas    |
| race_indigena  | V01321        | Cor ou raca e indigena                                                | Pessoas    |

## Setup

``` r
library(dplyr) 
library(ggplot2)
library(sf)
library(mapview)
library(leafsync)
```

## Part 1: Census variables on H3 hexagons (Fortaleza)

We interpolate three census variables to H3 resolution 8 hexagons for
Fortaleza (IBGE code 2304400):

- `pop_ph`: population in private households
- `avg_inc_resp`: average income of household heads
- `race_preta`: population self-identified as Black (*preta*)

``` r
ftl_h3 <- tracts_to_h3(
  code_muni = 2304400,
  h3_resolution = 8,
  vars = c("pop_ph", "avg_inc_resp", "race_preta")
)
#> ℹ Processing code 2304400
#> ℹ Step 1/6: connecting to DuckDB and loading extensions...
#> ✔ Spatial extension loaded
#> ℹ Step 1/6: connecting to DuckDB and loading extensions...
✔ Step 1/6 (DuckDB ready) [552ms]
#> 
#> ℹ Step 2/6: preparing census tracts in DuckDB...
#> ℹ Using cached file: 'sc_23.parquet'
#> ℹ Step 2/6: preparing census tracts in DuckDB...
✔ Step 2/6 (tracts ready) [308ms]
#> 
#> ℹ Step 3/6: preparing CNEFE points in DuckDB...
#> ℹ Using cached file: C:\Users\jorge\AppData\Local/R/cache/R/cnefetools/2304400_FORTALEZA.zip
#> ℹ Step 3/6: preparing CNEFE points in DuckDB...
✔ Step 3/6 (CNEFE points ready) [3.8s]
#> 
#> ℹ Step 4/6: spatial join (points to tracts) and allocation prep...
#> ✔ Step 4/6 (join and allocation prep) [2.5s]
#> 
#> ℹ Step 5/6: aggregating allocated values to H3 cells...
#> ✔ Step 5/6 (hex aggregation) [224ms]
#> 
#> ℹ Step 6/6: building H3 grid and joining results...
#> ✔ Step 6/6 (sf output) [793ms]
#> 
#> 
#> ── Dasymetric interpolation diagnostics ──
#> 
#> ! Unallocated total for population from private households (pop_ph): "2169"
#>   ("0.09%" of total)
#> ! Unallocated total for race_preta (race_preta): "74" ("0.04%" of total)
#> ! avg_inc_resp assigned to 1032500 of 1033953 eligible points
#> ! avg_inc_resp is "NA" in 128 tracts
#> ! Unmatched CNEFE points (no tract): 194
#> ! Tracts with "NA" totals: pop_ph "NA" in 128; race_preta "NA" in 200
#> ! Tracts with no eligible dwellings: pop_ph in 6 tracts; race_preta in 6 tracts

head(ftl_h3)
#> Simple feature collection with 6 features and 4 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -38.64091 ymin: -3.845923 xmax: -38.50523 ymax: -3.711455
#> Geodetic CRS:  WGS 84
#>            id_hex     pop_ph avg_inc_resp race_preta
#> 1 8880104cddfffff 13321.3310     1911.185 1180.05315
#> 2 88801045e5fffff 13383.8123     1590.995  897.03672
#> 3 8880107b65fffff 13345.7094     2121.699 1061.31442
#> 4 88801045ddfffff   890.9914     1467.918   89.11701
#> 5 8880104c1dfffff  7417.4240     8685.882  257.48227
#> 6 88801040b5fffff 11225.1574     1277.496 1048.97185
#>                         geometry
#> 1 POLYGON ((-38.5506 -3.72179...
#> 2 POLYGON ((-38.59024 -3.7848...
#> 3 POLYGON ((-38.56852 -3.7249...
#> 4 POLYGON ((-38.63505 -3.7927...
#> 5 POLYGON ((-38.50925 -3.7464...
#> 6 POLYGON ((-38.52856 -3.8459...
```

### Mapping the results

We use
[`leafsync::sync()`](https://rdrr.io/pkg/leafsync/man/latticeView.html)
to display the three variables side by side:

``` r
map_pop <- mapview(
  ftl_h3,
  zcol = "pop_ph",
  layer.name = "Population"
)

map_inc <- mapview(
  ftl_h3,
  zcol = "avg_inc_resp",
  layer.name = "Avg. income"
)

map_race <- mapview(
  ftl_h3,
  zcol = "race_preta",
  layer.name = "Black pop."
)

sync(map_pop, map_inc, map_race, ncol = 3)
```

### Exploring the relationship between income and racial composition

We can examine the relationship between average household head income
and the proportion of Black population in each hexagon:

``` r
ftl_h3_plot <- ftl_h3 |>
  st_drop_geometry() |>
  filter(pop_ph > 0) |>
  mutate(pct_preta = race_preta / pop_ph)

ggplot(ftl_h3_plot, aes(x = avg_inc_resp, y = pct_preta)) +
  geom_point(alpha = 0.3, size = 3) +
  geom_smooth(method = "loess", se = TRUE, color = "steelblue") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Average income of household heads (R$)",
    y = "Black population share (race_preta / pop_ph)"
  ) +
  theme_minimal()
```

![](tracts_to_files/figure-html/unnamed-chunk-6-1.png)

As expected, the plot reveals a strong negative correlation between
these two variables: hexagons with a higher share of Black population
tend to have substantially lower average household income. This pattern
reflects deep-rooted racial income inequality, a persistent feature of
the Brazilian social landscape.

## Part 2: Census variables on traffic zones (São Paulo)

Transportation planning often requires census-derived variables at the
traffic zone level rather than the census tract level. The [`odbr`
package](https://github.com/hsvab/odbr/) provides traffic zone
geometries from origin-destination surveys conducted in Brazilian
metropolitan areas. Since the OD survey for São Paulo covers the entire
metropolitan region (39 municipalities), we filter only the traffic
zones within the municipality of São Paulo.

``` r
library(odbr)

# Load traffic zones for the São Paulo metropolitan region
sp_zones <- read_map(
  city = "Sao Paulo",
  year = 2017,
  geometry = "zone"
)

# Filter zones within the municipality of São Paulo
sp_zones_muni <- sp_zones |>
  filter(NomeMunici == "São Paulo") |> 
  sf::st_make_valid() # Correcting invalid geometries in advance

nrow(sp_zones_muni)
#> [1] 342
```

We interpolate two variables:

- `pop_ph`: population in private households
- `age_70m`: population aged 70 years or older

``` r
sp_zones_census <- tracts_to_polygon(
  code_muni = 3550308,
  polygon = sp_zones_muni, 
  vars = c("pop_ph", "age_70m")
)
#> ℹ Processing municipality code 3550308...
#> ℹ Step 1/6: aligning CRS...
#> ℹ Input CRS: "EPSG:22523" | Output CRS: "EPSG:22523"
#> ℹ Step 1/6: aligning CRS...
✔ Step 1/6 (CRS alignment) [660ms]
#> 
#> ℹ Step 2/6: connecting to DuckDB and loading extensions...
#> ✔ Spatial extension loaded
#> ℹ Step 2/6: connecting to DuckDB and loading extensions...
✔ Step 2/6 (DuckDB ready) [504ms]
#> 
#> ℹ Step 3/6: preparing census tracts in DuckDB...
#> ℹ Using cached file: 'sc_35.parquet'
#> ℹ Step 3/6: preparing census tracts in DuckDB...
#> Warning: "0.02%" of polygon area falls outside census tracts of municipality 3550308.
#> ℹ Only CNEFE points within census tracts will be used for interpolation.
#> ✔ Step 3/6 (tracts ready) [5.2s]
#> 
#> ℹ Step 4/6: preparing CNEFE points in DuckDB...
#> ℹ Using cached file: C:\Users\jorge\AppData\Local/R/cache/R/cnefetools/3550308_SAO_PAULO.zip
#> ℹ Step 4/6: preparing CNEFE points in DuckDB...
✔ Step 4/6 (CNEFE points ready) [18s]
#> 
#> ℹ Step 5/6: spatial join (points to tracts) and allocation...
#> Warning: Polygon coverage: "99.99%" of CNEFE dwelling points captured.
#> ℹ 4993321 of 4993594 points are within the provided polygon.
#> ℹ 273 points fell outside the polygon and were not counted.
#> ✔ Step 5/6 (join and allocation) [1m 7.6s]
#> 
#> ℹ Step 6/6: aggregating allocated values to polygons...
#> ✔ Step 6/6 (polygon aggregation) [474ms]
#> 
#> 
#> ── Dasymetric interpolation diagnostics ──
#> 
#> ! Unallocated total for population from private households (pop_ph): "86998"
#>   ("0.76%" of total)
#> ! Unallocated total for age_70m (age_70m): "5397" ("0.58%" of total)
#> ! Unmatched CNEFE points (no tract) = 2935
#> ! Tracts with "NA" totals: pop_ph "NA" in 622; age_70m "NA" in 1316.
#> ! Tracts with no eligible dwellings: pop_ph in 324 tracts; age_70m in 297
#>   tracts

head(sp_zones_census)
#> Simple feature collection with 6 features and 9 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 331820.5 ymin: 7393922 xmax: 334180.5 ymax: 7396311
#> Projected CRS: Corrego Alegre 1970-72 / UTM zone 23S
#> # A tibble: 6 × 10
#>   NumeroZona NomeZona      NumeroMuni NomeMunici NumDistrit NomeDistri Area_ha_2
#>        <dbl> <chr>              <dbl> <chr>           <dbl> <chr>          <dbl>
#> 1          1 Sé                    36 São Paulo          80 Sé              57.1
#> 2          2 Parque Dom P…         36 São Paulo          80 Sé             114. 
#> 3          3 Praça João M…         36 São Paulo          80 Sé              47.8
#> 4          4 Ladeira da M…         36 São Paulo          67 República       75.1
#> 5          5 República             36 São Paulo          67 República       75.0
#> 6          6 Santa Ifigên…         36 São Paulo          67 República       82.9
#> # ℹ 3 more variables: geom <MULTIPOLYGON [m]>, pop_ph <dbl>, age_70m <dbl>
```

### Mapping the elderly population share

We compute the share of population aged 70+ in each traffic zone. This
indicator is relevant for identifying areas where transport planning
should prioritize accessibility for people with reduced mobility, such
as low-floor buses, accessible sidewalks, and demand-responsive transit
services.

``` r
sp_zones_ratio <- sp_zones_census |>
  filter(pop_ph > 0) |>
  mutate(pct_70m = age_70m / pop_ph)

mapview(
  sp_zones_ratio,
  zcol = "pct_70m",
  layer.name = "Share of pop. aged 70+"
)
```

## Notes

Notice how
[`tracts_to_polygon()`](https://pedreirajr.github.io/cnefetools/reference/tracts_to_polygon.md)
is particularly useful in transportation research, where census
variables (such as population by age group and income) are essential
inputs for travel demand models (especially in Trip Generation models
from 4-Step modeling). However, traffic zone geometries rarely align
with census tract boundaries. Dasymetric interpolation via CNEFE
dwelling points provides a principled method to bridge this spatial
mismatch, transferring census data to custom spatial units and
supporting more accurate trip generation and distribution models.

### Hint to scaling to multiple municipalities

The `odbr` traffic zones cover the entire São Paulo metropolitan region
(39 municipalities). To interpolate census data for all municipalities,
you can use
[`purrr::map()`](https://purrr.tidyverse.org/reference/map.html) to
apply
[`tracts_to_polygon()`](https://pedreirajr.github.io/cnefetools/reference/tracts_to_polygon.md)
to each municipality and then combine the results with
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html).

### Future versions

In future versions of the package,
[`tracts_to_polygon()`](https://pedreirajr.github.io/cnefetools/reference/tracts_to_polygon.md)
will support municipality-independent operation, allowing users to pass
polygons that span multiple municipalities without the need for
per-municipality iteration.

### Interpolation diagnostics

Both functions print detailed diagnostics after each run, so the user
can assess the quality of the interpolation. For example, the
[`tracts_to_h3()`](https://pedreirajr.github.io/cnefetools/reference/tracts_to_h3.md)
call for Fortaleza above produces:

    ── Dasymetric interpolation diagnostics ──
    ! Unallocated total for population from private households (pop_ph): "2169" ("0.09%" of total)
    ! Unallocated total for race_preta (race_preta): "74" ("0.04%" of total)
    ! avg_inc_resp assigned to 1032500 of 1033953 eligible points
    ! avg_inc_resp is "NA" in 128 tracts
    ! Unmatched CNEFE points (no tract): 194
    ! Tracts with "NA" totals: pop_ph "NA" in 128; race_preta "NA" in 200
    ! Tracts with no eligible dwellings: pop_ph in 6 tracts; race_preta in 6 tracts

And the
[`tracts_to_polygon()`](https://pedreirajr.github.io/cnefetools/reference/tracts_to_polygon.md)
call for São Paulo:

    ── Dasymetric interpolation diagnostics ──
    ! Unallocated total for population from private households (pop_ph): "86998" ("0.76%" of total)
    ! Unallocated total for age_70m (age_70m): "5397" ("0.58%" of total)
    ! Unmatched CNEFE points (no tract) = 2935
    ! Tracts with "NA" totals: pop_ph "NA" in 622; age_70m "NA" in 1316
    ! Tracts with no eligible dwellings: pop_ph in 324 tracts; age_70m in 297 tracts

These warnings report the amount of unallocated population (and its
percentage of the census total), the number of CNEFE points that could
not be matched to a tract, and the number of tracts with missing values
or no eligible dwellings. In both examples, the unallocated shares are
small (below 1%), indicating that the interpolation preserves most of
the original census totals. Users should inspect these diagnostics to
decide whether the loss is acceptable for their application.

### Ecological fallacy caveat

Dasymetric interpolation distributes tract-level aggregates to dwelling
points under the assumption that each dwelling within a tract receives
an equal share of the total. This means that the method does not recover
true individual- or dwelling-level values; it redistributes a group
statistic. Drawing conclusions about individuals from such aggregated
data constitutes an **ecological fallacy** (the error of attributing
group-level patterns to individuals).

The risk is greater when the source census tracts are large, since
larger tracts are more likely to contain internally heterogeneous
populations, making the uniform allocation assumption less realistic.
Users should interpret interpolated values as spatial estimates of
aggregate quantities, not as precise measurements at the dwelling or
individual level.
