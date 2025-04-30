
#' Thésaurus des diagnostics de la FEDORU
#'
#' Thésaurus utilisé dans le kit panorama 2024
#'
#' @format ## `Thesaurus_Fedoru`
#' Un tibble de 22 567 lignes et 5 colonnes
#' \describe{
#'   \item{diagnostic_code}{Code Cim10}
#'   \item{diagnostic_libelle}{Diagnostic associé au code cim10}
#'   \item{type_urgence_libelle}{Classification FEDORU du diagnostic}
#'   \item{discipline_libelle}{Discipline associée au diagnostic}
#'   \item{pathologie_libelle}{Pathologie associée au diagnostic}
#' }
"Thesaurus_Fedoru"

#' Thésaurus des diagnostics SARA
#'
#' Thésaurus utilisé par SARA dans eCerveau
#'
#' @format ## `tab_CIM10_eCerveau`
#' Un tibble de 42 883 lignes et 11 colonnes
#' \describe{
#'   \item{code_diag}{Code Cim10}
#' }
"tab_CIM10_eCerveau"
