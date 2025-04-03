
#' Format grand nombres
#'
#' @param val Numeric
#'
#' @returns Une chaine de caractère contenant le nombre donné en entrée avec un espace en séparateur des milliers
#' @export
#'
fct_f_big <- function(val){#format for big numbers (inserts a big mark)
  val_ok <- format(val, big.mark = " ")
  return(val_ok)
}

#' Transformation minutes en heures
#'
#' @param d_min numeric
#'
#' @returns Retourne une durée en minute au format charactère HH:MM
#' @export
#'
#' @importFrom dplyr if_else
#'
fct_min_as_hour <- function(d_min){
  hour = d_min%/%60
  minute = d_min%%60
  minute = if_else(nchar(minute) == 1, paste0("0", minute), as.character(minute))
  hour_min = paste0(hour, "h", minute)
  return(hour_min)
}

