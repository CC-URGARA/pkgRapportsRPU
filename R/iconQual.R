

#' Créer dossier icon qualité
#'
#' @param path Chemin vers le dossier ou créer le dossier "iconQualiteDonnee et les 100 icons
#'
#' @returns NULL
#' @export
#'
#' @importFrom dplyr if_else
#' @importFrom ggplot2 ggsave
#'
fct_create_icon_qual_folder = function(path){
  #Check si le dossier existe
  if(!dir.exists(path)){stop(paste0("Le dossier ", path, " n'existe pas"))}

  #check si le chemin fini par un / ou pas
  last_char <- substr(path, nchar(path), nchar(path))
  suffix_path = if_else(last_char == "/", "", "/")#ajout si nécessaire

  #Création du dossier où les icons irons
  path_folder_icon = paste0(path, suffix_path, "iconQualiteDonnee")
  dir.create(path_folder_icon)

  #Création des icons et sauvegarde en png
  for(p in 0:100){
    ggsave(plot = fct_make_icon_qual(p),
           filename = paste0("iconQualData_", p, ".png"),
           path = path_folder_icon,
           height = 70, width = 70, units = "px", dpi = 70)
  }

}


#' Icon qualité des données
#'
#' @param p Numeric entre 0 et 100 => Nombre à insérer au centre du cercle
#'
#' @returns un ggplot
#'
#' @import ggplot2
#' @importFrom grDevices colorRampPalette
#' @importFrom dplyr tibble
#'
fct_make_icon_qual <- function(p){
  if(!is.numeric(p)){stop("p doit etre numeric")}
  if(p < 0 | p > 100){stop("p doit etre compris entre 0 et 100")}
  # Créer une palette de couleurs allant du bleu au rouge
  paletteGenerator <- colorRampPalette(c("#A50026",
                                         "#D73027", "#F46D43", "#FDAE61",
                                         # "#FEE08B",
                                         # "#D9EF8B",
                                         "#A6D96A", "#66BD63", "#1A9850",
                                         "#006837"))
  palette = paletteGenerator(101)
  # Générer 10 couleurs intermédiaires
  p_round = round(p)
  bg_col = palette[p_round + 1]
  lab = paste0(round(p), "%")


  plot_icon = ggplot(tibble()) +
    geom_point(aes(x = 0, y = 0), color = bg_col, size = 30) +
    geom_text(aes(label = lab, x = 0, y = 0), color = "white", size = 6) +
    coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1)) +
    theme_void() +
    theme(
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA),
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.box.background = element_rect(fill = "transparent", colour = NA)
    )
  return(plot_icon)
}
