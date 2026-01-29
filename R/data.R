#' Reference table for tracts_to_* function variables
#'
#' A data frame that maps variable names used in [tracts_to_h3()] and
#' [tracts_to_polygon()] to the official IBGE census tract dataset codes
#' and descriptions.
#'
#' @format A data frame with 20 rows and 4 columns:
#' \describe{
#'   \item{var_cnefetools}{Variable name used in cnefetools functions.}
#'   \item{code_var_ibge}{Official IBGE variable code from the census tract aggregates.}
#'   \item{desc_var_ibge}{Official IBGE variable description in Portuguese.}
#'   \item{table_ibge}{Name of the IBGE census tract table where the variable is found
#'     (Domicilios or Pessoas).}
#' }
#'
#' @source IBGE - Censo Demografico 2022, Agregados por Setores Censitarios.
#'
#' @examples
#' # View the reference table
#' tracts_variables_ref
#'
#' # Find the IBGE code for a specific variable
#' tracts_variables_ref[tracts_variables_ref$var_cnefetools == "pop_ph", ]
#'
"tracts_variables_ref"
