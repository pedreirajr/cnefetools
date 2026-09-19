#' Reference table for tracts_to_* function variables
#'
#' A data frame that maps variable names used in [tracts_to_h3()] and
#' [tracts_to_polygon()] to the census tract variable codes and descriptions.
#'
#' The variable codes are the ones used by the censobr package, which
#' repackages the IBGE census tract aggregates and is where the census tract
#' assets take their attributes from (see `data-raw/sc_assets_build.R` in the
#' package repository), and each table corresponds to a censobr dataset. They
#' don't always match the file and column names on the IBGE FTP server.
#'
#' @format A data frame with 22 rows and 4 columns:
#' \describe{
#'   \item{var_cnefetools}{Variable name used in cnefetools functions.}
#'   \item{code_var_ibge}{Variable code in the census tract aggregates, as used by censobr.}
#'   \item{desc_var_ibge}{Official IBGE variable description in Portuguese.}
#'   \item{table_ibge}{Census tract table where the variable is found
#'     (Domicilios, Pessoas, or ResponsavelRenda), which correspond to the
#'     censobr datasets Domicilio, Pessoas and ResponsavelRenda.}
#' }
#'
#' @source IBGE - Censo Demografico 2022, Agregados por Setores Censitarios,
#'   as repackaged by the censobr package.
#'
#' @examples
#' # View the reference table
#' tracts_variables_ref
#'
#' # Find the IBGE code for a specific variable
#' tracts_variables_ref[tracts_variables_ref$var_cnefetools == "pop_ph", ]
#'
"tracts_variables_ref"
