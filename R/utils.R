
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


#' Nombre patients presents une heure données
#'
#' @param d_entree POSIXct date/heure d'entrée
#' @param d_sortie POSIXct date/heure d'entrée
#' @param heure Heure à tester (Entier entre 0 à 23)
#' @param avg_etab Booléen : Le nombre de présent doit-il être au total ou en moyenne par établissement
#' @param id_etab character : Vecteur contenant un identifiant unique des établissements présents
#'
#' @returns numeric
#'
#' @importFrom lubridate is.POSIXct date hour
#' @import dplyr
#'
fct_nb_presents <- function(d_entree, d_sortie, heure, avg_etab = FALSE, id_etab = NULL){
  #check variables of
  if(!is.POSIXct(d_entree) | !is.POSIXct(d_sortie)){
    stop("d_entree et d_sortie doivent \u00eatre des POSIXct")
  }
  if(!is.numeric(heure) | heure < 0 | heure > 23){
    stop("heure doit \u00eatre un numeric entre 0 et 23")
  }
  if(avg_etab & is.null(id_etab)){
    stop("ID_ETAB ne peut \u00eatre nul si avg_etab est TRUE")
  }

  #calcul du dénominateur (nb de jours * nb etab)
  if(avg_etab){
    n_etab <- length(unique(id_etab))#nb etab dans la table
  } else {
    n_etab = 1
  }
  n_jours <- length(unique(date(d_entree)))
  denom = n_etab*n_jours

  #Calcul du nombre de patients présents
  base <- tibble(d_entree, d_sortie) %>%
    mutate(nb_jours_present = as.numeric(difftime(date(d_sortie), date(d_entree), units = "days")),#0 = 1 seul jour
           H_ENTREE = hour(d_entree),
           H_SORTIE = hour(d_sortie))

  nb_present_h <- base %>%
    filter((nb_jours_present == 0 & H_ENTREE == heure & H_SORTIE == heure) |#entrée et sortie à l'heure testée
             (nb_jours_present == 0 & H_ENTREE < heure & H_SORTIE >= heure) |#entrée avant l'heure et sorti après
             (nb_jours_present == 1 & (H_ENTREE < heure | H_SORTIE >= heure)) |
             (nb_jours_present >= 2)) %>% #présent de 00h00 à 23h59
    nrow()

  nb_present_h_avg = round(nb_present_h/denom)

  return(nb_present_h_avg)
}



