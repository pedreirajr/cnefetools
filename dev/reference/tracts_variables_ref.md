# Reference table for tracts_to\_\* function variables

A data frame that maps variable names used in
[`tracts_to_h3()`](https://pedreirajr.github.io/cnefetools/dev/reference/tracts_to_h3.md)
and
[`tracts_to_polygon()`](https://pedreirajr.github.io/cnefetools/dev/reference/tracts_to_polygon.md)
to the official IBGE census tract dataset codes and descriptions.

## Usage

``` r
tracts_variables_ref
```

## Format

A data frame with 20 rows and 4 columns:

- var_cnefetools:

  Variable name used in cnefetools functions.

- code_var_ibge:

  Official IBGE variable code from the census tract aggregates.

- desc_var_ibge:

  Official IBGE variable description in Portuguese.

- table_ibge:

  Name of the IBGE census tract table where the variable is found
  (Domicilios or Pessoas).

## Source

IBGE - Censo Demografico 2022, Agregados por Setores Censitarios.

## Examples

``` r
# View the reference table
tracts_variables_ref
#>    var_cnefetools code_var_ibge
#> 1          pop_ph        V00005
#> 2          pop_ch        V00007
#> 3            male        V01007
#> 4          female        V01008
#> 5         age_0_4        V01031
#> 6         age_5_9        V01032
#> 7       age_10_14        V01033
#> 8       age_15_19        V01034
#> 9       age_20_24        V01035
#> 10      age_25_29        V01036
#> 11      age_30_39        V01037
#> 12      age_40_49        V01038
#> 13      age_50_59        V01039
#> 14      age_60_69        V01040
#> 15        age_70m        V01041
#> 16    race_branca        V01317
#> 17     race_preta        V01318
#> 18     race_parda        V01320
#> 19   race_amarela        V01319
#> 20  race_indigena        V01321
#> 21         n_resp        V06001
#> 22   avg_inc_resp        V06004
#>                                                                                                                         desc_var_ibge
#> 1                                                               Domicilios Particulares Permanentes Ocupados, Quantidade de moradores
#> 2                                                                           Domicilios Coletivos Com Morador, Quantidade de moradores
#> 3                                                                                                                      Sexo masculino
#> 4                                                                                                                       Sexo feminino
#> 5                                                                                                                          0 a 4 anos
#> 6                                                                                                                          5 a 9 anos
#> 7                                                                                                                        10 a 14 anos
#> 8                                                                                                                        15 a 19 anos
#> 9                                                                                                                        20 a 24 anos
#> 10                                                                                                                       25 a 29 anos
#> 11                                                                                                                       30 a 39 anos
#> 12                                                                                                                       40 a 49 anos
#> 13                                                                                                                       50 a 59 anos
#> 14                                                                                                                       60 a 69 anos
#> 15                                                                                                                    70 anos ou mais
#> 16                                                                                                               Cor ou raca e branca
#> 17                                                                                                                Cor ou raca e preta
#> 18                                                                                                                Cor ou raca e parda
#> 19                                                                                                              Cor ou raca e amarela
#> 20                                                                                                             Cor ou raca e indigena
#> 21                                                               Pessoas responsaveis em domicilios particulares permanentes ocupados
#> 22 Valor do rendimento nominal medio mensal das pessoas responsaveis com rendimentos por domicilios particulares permanentes ocupados
#>          table_ibge
#> 1        Domicilios
#> 2        Domicilios
#> 3           Pessoas
#> 4           Pessoas
#> 5           Pessoas
#> 6           Pessoas
#> 7           Pessoas
#> 8           Pessoas
#> 9           Pessoas
#> 10          Pessoas
#> 11          Pessoas
#> 12          Pessoas
#> 13          Pessoas
#> 14          Pessoas
#> 15          Pessoas
#> 16          Pessoas
#> 17          Pessoas
#> 18          Pessoas
#> 19          Pessoas
#> 20          Pessoas
#> 21 ResponsavelRenda
#> 22 ResponsavelRenda

# Find the IBGE code for a specific variable
tracts_variables_ref[tracts_variables_ref$var_cnefetools == "pop_ph", ]
#>   var_cnefetools code_var_ibge
#> 1         pop_ph        V00005
#>                                                           desc_var_ibge
#> 1 Domicilios Particulares Permanentes Ocupados, Quantidade de moradores
#>   table_ibge
#> 1 Domicilios
```
