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
$$\text{ICE} = \frac{n_{\text{educational}} - n_{\text{religious}}}{n_{\text{educational}} + n_{\text{religious}}}$$

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
#> ℹ Step 1/3: Ensuring ZIP and inspecting archive...
#> ℹ Using cached file: C:\Users\jorge\AppData\Local/R/cache/R/cnefetools/4314902_PORTO_ALEGRE.zip
#> ℹ Step 1/3: Ensuring ZIP and inspecting archive...
✔ Step 1/3 (CNEFE ZIP ready) [157ms]
#> 
#> ℹ Step 2/3: Building full H3 grid over municipality boundary...
#> ✔ Step 2/3 (H3 grid built) [3.9s]
#> 
#> ℹ Step 3/3: Counting address species per hexagon...
#> ✔ Step 3/3 (Addresses counted) [1.5s]

head(poa_hex_counts)
#> Simple feature collection with 6 features and 9 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -51.1276 ymin: -30.24817 xmax: -51.08759 ymax: -30.22164
#> Geodetic CRS:  WGS 84
#>            id_hex addr_type1 addr_type2 addr_type3 addr_type4 addr_type5
#> 1 88a9010c83fffff          0          0          0          0          0
#> 2 88a9010c87fffff        444          0          0          0          1
#> 3 88a9010c8bfffff          0          0          0          0          0
#> 4 88a9010c91fffff          5          0          0          0          0
#> 5 88a9010c93fffff         11          0          0          0          0
#> 6 88a9010c95fffff         43          0          0          0          0
#>   addr_type6 addr_type7 addr_type8                       geometry
#> 1          0          0          0 POLYGON ((-51.10091 -30.241...
#> 2         27          6          5 POLYGON ((-51.09175 -30.240...
#> 3          0          0          0 POLYGON ((-51.10422 -30.248...
#> 4          2          0          0 POLYGON ((-51.11591 -30.236...
#> 5          1          0          0 POLYGON ((-51.12176 -30.230...
#> 6          2          0          0 POLYGON ((-51.10675 -30.235...
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
#> -1.00000 -1.00000 -0.38462 -0.27384  0.07692  1.00000
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
#> ℹ Step 1/2: Ensuring data and preparing polygon...
#> ℹ Using cached file: C:\Users\jorge\AppData\Local/R/cache/R/cnefetools/4314902_PORTO_ALEGRE.zip
#> ℹ Step 1/2: Ensuring data and preparing polygon...
✔ Step 1/2 (Data and polygon ready) [407ms]
#> 
#> ℹ Step 2/2: Counting addresses per polygon...
#> Warning: Polygon coverage: "100.0%" of CNEFE points captured.
#> ℹ 762110 of 762239 points are within the provided polygon.
#> ℹ 129 points fell outside the polygon and were not counted.
#> ✔ Step 2/2 (Addresses counted) [3s]

head(poa_neigh_counts)
#> Simple feature collection with 6 features and 26 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -51.30325 ymin: -30.17167 xmax: -51.10924 ymax: -29.9323
#> Geodetic CRS:  SIRGAS 2000
#>   code_muni    name_muni name_neighborhood code_neighborhood code_subdistrict
#> 1   4314902 Porto Alegre Aberta dos Morros        4314902080      43149020500
#> 2   4314902 Porto Alegre         Agronomia        4314902035      43149020500
#> 3   4314902 Porto Alegre          Anchieta        4314902055      43149020500
#> 4   4314902 Porto Alegre       Arquipélago        4314902033      43149020500
#> 5   4314902 Porto Alegre       Auxiliadora        4314902045      43149020500
#> 6   4314902 Porto Alegre            Azenha        4314902008      43149020500
#>   name_subdistrict code_district name_district code_urban_concentration
#> 1             <NA>     431490205  Porto Alegre                  4314902
#> 2             <NA>     431490205  Porto Alegre                  4314902
#> 3             <NA>     431490205  Porto Alegre                  4314902
#> 4             <NA>     431490205  Porto Alegre                  4314902
#> 5             <NA>     431490205  Porto Alegre                  4314902
#> 6             <NA>     431490205  Porto Alegre                  4314902
#>   name_urban_concentration code_immediate name_immediate code_intermediate
#> 1          Porto Alegre/RS         430001   Porto Alegre              4301
#> 2          Porto Alegre/RS         430001   Porto Alegre              4301
#> 3          Porto Alegre/RS         430001   Porto Alegre              4301
#> 4          Porto Alegre/RS         430001   Porto Alegre              4301
#> 5          Porto Alegre/RS         430001   Porto Alegre              4301
#> 6          Porto Alegre/RS         430001   Porto Alegre              4301
#>   name_intermediate code_state        name_state code_region name_region
#> 1      Porto Alegre         43 Rio Grande do Sul           4         Sul
#> 2      Porto Alegre         43 Rio Grande do Sul           4         Sul
#> 3      Porto Alegre         43 Rio Grande do Sul           4         Sul
#> 4      Porto Alegre         43 Rio Grande do Sul           4         Sul
#> 5      Porto Alegre         43 Rio Grande do Sul           4         Sul
#> 6      Porto Alegre         43 Rio Grande do Sul           4         Sul
#>   addr_type1 addr_type2 addr_type3 addr_type4 addr_type5 addr_type6 addr_type7
#> 1       4249          1          1          8          2        235        147
#> 2       1219          3          1         11          1        113         22
#> 3        294          1          0          3          1        446         16
#> 4       2851          1          1         12          2        286         62
#> 5       5464          3          0          9         45        576         57
#> 6       8150         10          0         17         31       1236         39
#>   addr_type8                           geom
#> 1         15 MULTIPOLYGON (((-51.20221 -...
#> 2          5 MULTIPOLYGON (((-51.15367 -...
#> 3          1 MULTIPOLYGON (((-51.15517 -...
#> 4         28 MULTIPOLYGON (((-51.26181 -...
#> 5          1 MULTIPOLYGON (((-51.1865 -3...
#> 6          5 MULTIPOLYGON (((-51.21153 -...
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
