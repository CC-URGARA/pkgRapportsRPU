

#' Page de garde
#'
#' @description
#' Génère la page de garde des rapports RPU. ATTENTION : La police utilisée ne fonctionne que dans un environnement utilisant showtext::showtext_auto
#'
#'
#' @param nom_etab character : Nom de l'établissement
#' @param date_debut character DMY : date de début du rapport
#' @param date_fin character DMY : date de fin du rapport
#'
#' @returns un plot
#' @export
#'
#' @import ggplot2
#' @importFrom png readPNG
#' @importFrom stringr str_replace
#' @importFrom lubridate dmy month year
#' @importFrom ggpubr background_image
#' @importFrom dplyr tibble
#' @importFrom sysfonts font_add
#'
fct_pageGarde <- function(nom_etab, date_debut, date_fin){
  #Fond de la page de garde
  img <- readPNG(system.file("img/Page_garde_vierge.png", package = "pkgRapportsRPU"))

  #Mise en forme du nom du centre
  nom_etab_break <- str_replace(nom_etab, "( -)? Urgences", "\nUrgences")
  if(nchar(nom_etab_break) > 40 & grepl(" - ", nom_etab_break)){
    nom_etab_break <- str_replace(nom_etab_break, " - ", "\n")
  }

  #Mise en forme de la date du rapport
  lab_date <- paste0(month(dmy(date_debut), label = T, abbr = F), "-",
                     month(dmy(date_fin), label = T, abbr = F), " ",
                     year(dmy(date_debut)))


  # Chargement de la police d'écriture
  font_path <- system.file("fonts", "Co_Bold.otf", package = "pkgRapportsRPU")
  font_add("CO_bold", font_path)

  #Génération de la page de garde
  pageGarde = ggplot(tibble()) +
    background_image(img) +
    geom_text(aes(label = "Rapport", x = 102, y = 28), color = "white", size = 17, hjust = 1,
              fontface = "bold", family = "CO_bold") +
    geom_text(aes(label = "quadrimestriel", x = 102, y = 20), color = "white", size = 17, hjust = 1,
              fontface = "bold", family = "CO_bold") +
    geom_text(aes(label = lab_date, x = 102, y = 11), color = "white", size = 13, hjust = 1,
              fontface = "bold", family = "CO_bold") +
    geom_text(aes(label = nom_etab_break, x = 102, y = 3), color = "#15ace1", size = 7, hjust = 1,
              fontface = "bold") +
    coord_cartesian(xlim = c(0, 100), ylim = c(0,100)) +
    theme_void()
  return(pageGarde)
}

