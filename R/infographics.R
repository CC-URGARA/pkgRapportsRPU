#' Infographic nid d'abeille
#'
#' @param base RPU (établissement ou groupe)
#' @param pediatrique Booléen : Rapport pédiatrique T/F
#' @param tab_activite Valeur retournée par la fonction
#' @param type character : Etab ou Groupe selon si la base fournie est celle du groupe ou de l'étab
#'
#' @returns Un graphique
#' @export
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom png readPNG
#' @importFrom ggpubr background_image
#' @importFrom stats median
#'
fct_infographic_resume <- function(base, pediatrique = FALSE, tab_activite, type){
  if(!type %in% c("Etab", "Groupe")){stop("Type doit \u00eatre Etab ou Groupe")}
  #indicateurs :
  if(pediatrique){
    if(type == "Etab"){
      img <- readPNG(system.file("img/infog_CC_vierge_ped_etab.png", package = "pkgRapportsRPU"))
      part_age_var = paste0(round(mean(base$age_inf2, na.rm = T)*100), "%")
    } else if(type == "Groupe") {
      img <- readPNG(system.file("img/infog_CC_vierge_ped_groupe.png", package = "pkgRapportsRPU"))
      part_age_var = paste0(round(mean(base$age_inf2, na.rm = T)*100), "%")
    }
  } else if(!pediatrique){
    if(type == "Etab"){
      img <- readPNG(system.file("img/infog_CC_vierge_general_etab.png", package = "pkgRapportsRPU"))
      part_age_var = paste0(round(mean(base$age_geq75, na.rm = T)*100), "%")
    } else if(type == "Groupe") {
      img <- readPNG(system.file("img/infog_CC_vierge_general_groupe.png", package = "pkgRapportsRPU"))
      part_age_var = paste0(round(mean(base$age_geq75, na.rm = T)*100), "%")
    }
  } else stop("erreur type ou pedia")

  n_etab <- length(unique(base$NOM_ETAB))#nb etab dans la table

  part_CCMU12 = paste0(round(mean(base$CCMU_1_2, na.rm = T)*100), "%")
  nb_passages = format(round(nrow(base)/n_etab), big.mark = " ")
  part_15inf = paste0(round(mean(base$age_inf15, na.rm = T)*100), "%")
  # part_2inf = paste0(round(mean(base$age_inf2, na.rm = T)*100), "%")
  duree_passage = round(median(base$duree_passage_min, na.rm = T))
  tab_activite_max = tab_activite %>% filter(activite == "max")
  tab_activite_min = tab_activite %>% filter(activite == "min")
  activite_max = paste0(tab_activite_max$n ," pr\u00e9sents \u00e0 " , tab_activite_max$Heure, "h")
  activite_min = paste0(tab_activite_min$n ," pr\u00e9sents \u00e0 " , tab_activite_min$Heure, "h")
  part_hospit = paste0(round(mean(base$hospit, na.rm = T)*100), "%")

  tab_coord_chiffres <-
    tibble(x = c(46, -1, -52, -2, 32, -2, -2, -52),
           y = c(77, 50, 23, -5, -32, -53, -70, -85),
           lab = c(part_age_var, part_CCMU12, nb_passages, part_15inf,
                   duree_passage, activite_max, activite_min, part_hospit),
           size = c(100, 100, 85, 100, 50, 30, 30, 100))


  infographic <- ggplot(tab_coord_chiffres, aes(x = x, y = y)) +
    background_image(img) +
    geom_text(aes(label = lab, size = size), fontface = "bold", hjust = 0.5) +
    scale_size_continuous(range = c(3, 10)) +
    coord_cartesian(xlim = c(-100, 100), ylim = c(-100,100)) +
    theme_void() +
    theme(legend.position = "none")
  return(infographic)
}
