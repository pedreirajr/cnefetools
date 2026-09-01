# cnefetools: Access and Analysis of Brazilian CNEFE Address Data

Download, cache and read municipality-level address data from the
Cadastro Nacional de Enderecos para Fins Estatisticos (CNEFE) of the
2022 Brazilian Census, published by the Instituto Brasileiro de
Geografia e Estatistica (IBGE)
<https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/>.
Beyond data access, provides spatial aggregation of addresses,
computation of land-use mix indices, and dasymetric interpolation of
census tract variables using CNEFE dwelling points as ancillary data.
Results can be produced on 'H3' hexagonal grids or user-supplied
polygons, and heavy operations leverage a 'DuckDB' backend with
extensions for fast, in-process execution.

## Package options

`cnefetools.duckdb_config` takes a named list of DuckDB settings,
applied to every connection the package opens:

    options(cnefetools.duckdb_config = list(threads = 4, memory_limit = "4GB"))

Left unset, DuckDB sizes itself against the whole machine, taking one
thread per logical core and a `memory_limit` of 80% of installed RAM.
That is the right default on a dedicated machine and the wrong one on a
shared node, a laptop running other work, or a CI runner.

Names are passed to DuckDB's `SET` verbatim, so any setting DuckDB
accepts works, not only these two. An unrecognised name raises an error
naming it.

Going over `memory_limit` makes DuckDB spill to its temporary directory
rather than fail, so a low value costs time, not correctness.

The download cache location is set through the `CNEFETOOLS_CACHE_DIR`
environment variable rather than an option. See
[`clear_cache_muni()`](https://pedreirajr.github.io/cnefetools/dev/reference/clear_cache_muni.md)
and
[`clear_cache_tracts()`](https://pedreirajr.github.io/cnefetools/dev/reference/clear_cache_tracts.md).

## See also

Useful links:

- <https://github.com/pedreirajr/cnefetools>

- <https://pedreirajr.github.io/cnefetools/>

- Report bugs at <https://github.com/pedreirajr/cnefetools/issues>

## Author

**Maintainer**: Jorge Ubirajara Pedreira Junior
<jorge.ubirajara@ufba.br>
([ORCID](https://orcid.org/0000-0002-8243-5395)) \[copyright holder\]

Authors:

- Jorge Ubirajara Pedreira Junior <jorge.ubirajara@ufba.br>
  ([ORCID](https://orcid.org/0000-0002-8243-5395)) \[copyright holder\]

- Bruno Mioto <brunomioto97@gmail.com>

Other contributors:

- Kaio Cunha Pedreira <kaiocp7@gmail.com> \[contributor\]
