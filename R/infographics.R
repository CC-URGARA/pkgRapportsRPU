#' Infographic nid d'abeille
#'
#' @param base RPU (établissement ou groupe)
#' @param pediatrique Booléen : Rapport pédiatrique T/F
#' @param tab_activite Valeur retournée par la fonction
#' @param type character : Etab ou Groupe selon si la base fournie est celle du groupe ou de l'étab
#' @param excl_orient_non_pec Booléen : Les fugues, réorientations, partis sans attendre et sorties contre avis doivent-il être exclus des délais et diag ?
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
fct_infographic_resume <- function(base, pediatrique = FALSE, tab_activite, type,
                                   excl_orient_non_pec){
  if(!type %in% c("Etab", "Groupe")){stop("Type doit \u00eatre Etab ou Groupe")}

  #Exclusion des REO/SCAM/PSO/FUGUE si necessaire
  if(excl_orient_non_pec){
    base = base %>%
      mutate(duree_passage_min = if_else(ORIENTATION %in% c("FUGUE", "REO", "PSA", "SCAM"),
                                         NA_real_, duree_passage_min)
      )
  }

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




#' Inforgraphie évolution
#'
#' @param RPU_evol Table d'évolition entre l'année n-1 et n. Créé par la fonction fct_make_tab_evol_RPU
#'
#' @returns Un ggplot
#' @export
#'
#' @importFrom png readPNG
#' @import dplyr
#' @import ggplot2
#'
fct_infographic_evol = function(RPU_evol){
  #Chargement du fond de plot
  img <- readPNG(system.file("img/infog_evol_vierge.png", package = "pkgRapportsRPU"))

  #Définition des coordonnées dans le plot
  tab_coord_chiffres <-
    tibble(Indicateur = c("n_RPU", "n_RPU_quotidien", "n_present_15h", "n_present_5h",
                          "tx_hospit", "tx_mutation", "tx_transfert", "tx_RAD",
                          "med_duree_passage", "tx_duree_passage_sup_4h",
                          "sex_ratio", "moy_age", "tx_age_inf2", "tx_age_inf15", "tx_age_geq75",
                          "tx_nuit", "tx_weekend", "tx_PERSO", "tx_SANITAIRE", "tx_CCMU1_2",
                          "tx_DP_medico_chir", "tx_DP_trauma", "tx_DP_psy", "tx_DP_toxico", "tx_DP_autre"),
           x = c(rep(-85, 4), -81, -71, -71, -81, -76, -76, rep(54, 5), rep(67, 5), rep(49, 5)),
           y = c(58, 52, 45, 38.5, -5, -11, -17.8, -24, -68, -75, 60, 53.5, 47, 40, 34, 3, -4, -17, -24, -36, -72, -78, -84, -91, -97))

  #Création des labels à mettre sur le plot + merge avec les coordonnées
  tab_plot = RPU_evol %>%
    ungroup() %>%
    select(Indicateur, val_indicateur_n2, lab) %>%
    mutate(lab_clean = paste0(val_indicateur_n2, " (", lab, ")")) %>%
    right_join(tab_coord_chiffres, by = "Indicateur")

  #Année à mettre sur le plot
  annee_plot = unique(RPU_evol$annee_n2)
  if(length(annee_plot)>1){stop("Plusieurs ann\u00e9es dans les donn\u00e9es")}

  #Création du plot
  plot_evol <- ggplot(tab_plot, aes(x = x, y = y)) +
    background_image(img) +
    geom_text(aes(label = lab_clean), size = 2, hjust = 1, color = "#1b5c9a") +#fontface = "bold"
    annotate(geom = "text", label = annee_plot, x = -26.5, y = 58,
             size = 2.2, hjust = 1, color = "#1b5c9a", fontface = "bold") +
    scale_size_continuous(range = c(3, 10)) +
    coord_cartesian(xlim = c(-100, 100), ylim = c(-100,100)) +
    theme_void() +
    theme(legend.position = "none")
  return(plot_evol)
}





#' table evolution infographie
#'
#' @param RPU_n1 RPU de l'année de référence
#' @param RPU_n2 RPU de l'année "active" du rapport
#' @param group_by Façon dont sont regroupées les résultats : "none" pour population globale, "etab" pour groupe par étab et "group" pour grouper par "group" de taille d'étab
#' @param excl_orient_non_pec Booléen : Les fugues, réorientations, partis sans attendre et sorties contre avis doivent-il être exclus des délais et diag ?
#'
#' @returns un tibble
#' @export
#'
#' @import dplyr
#'
fct_make_tab_evol_RPU <- function(RPU_n1, RPU_n2, group_by = "etab", excl_orient_non_pec){
  #Calcul des indicateurs tracés pour l'évolution
  tab_indic_evol_n1 = fct_calc_indic_evol(RPU = RPU_n1, group_by = group_by,
                                          excl_orient_non_pec = excl_orient_non_pec)
  tab_indic_evol_n2 = fct_calc_indic_evol(RPU = RPU_n2, group_by = group_by,
                                          excl_orient_non_pec = excl_orient_non_pec)

  #Liste des indicateurs dont l'\u00e9volution et est pouventage d'évolution
  list_indic_evol_pourc = c("n_RPU", "n_present_15h", "n_present_5h", "moy_age", "n_RPU_quotidien")#liste des variables dont l'\u00e9volution est en pourcentage d'augmentation

  #Variables utilisées pour joindre les deux années
  if(group_by == "etab"){
    join_var = c("COD_FIN", "NOM_ETAB", "Indicateur")
  } else if(group_by == "group"){
    join_var = c("group", "Indicateur")
  } else if(group_by == "none"){
    join_var = c("Indicateur")
  } else stop("group_by doit \u00eatre etab ou group ou none")

  tab_indic_evol = left_join(tab_indic_evol_n1, tab_indic_evol_n2,
                             by = join_var, suffix = c("_n1", "_n2")) %>%
    mutate(
      val_indicateur_n1 = as.numeric(val_indicateur_n1),
      val_indicateur_n2 = as.numeric(val_indicateur_n2),
      evol = if_else(Indicateur %in% list_indic_evol_pourc,
                     ((val_indicateur_n2 - val_indicateur_n1)/val_indicateur_n1) * 100,
                     val_indicateur_n2 - val_indicateur_n1),
      evol = round(evol, 1),
      evol_type = if_else(Indicateur %in% list_indic_evol_pourc, "pourc_aug", "aug_points"),
      signe = if_else(evol >= 0, "+", ""),
      suffix_lab = if_else(evol_type == "pourc_aug", "%", ""),
      lab = paste0(signe, trimws(format(evol, decimal.mark = ",")), suffix_lab),
      lab = if_else(is.na(signe) | is.na(evol), "XX", lab),
      val_indicateur_n2_lab = if_else(is.na(val_indicateur_n2), "XX",
                                      if_else(Indicateur %in% list_indic_evol_pourc | Indicateur %in% c("med_duree_passage", "n_jours_an"),
                                              as.character(trimws(format(round(val_indicateur_n2), big.mark = " "))),
                                              as.character(trimws(format(val_indicateur_n2, decimal.mark = ","))))),
      val_indicateur_n1_lab = if_else(is.na(val_indicateur_n1), "XX",
                                      if_else(Indicateur %in% list_indic_evol_pourc | Indicateur %in% c("med_duree_passage", "n_jours_an"),
                                              as.character(trimws(format(round(val_indicateur_n1), big.mark = " "))),
                                              as.character(trimws(format(val_indicateur_n1, decimal.mark = ",")))))) %>%
    ungroup()

  return(tab_indic_evol)
}



#' Calcul des indicateurs traceur d'évolution
#'
#' @param RPU base RPU avec variables intermédiaires déjà calculées
#' @param group_by Façon dont sont regroupées les résultats : "none" pour population globale, "etab" pour groupe par étab et "group" pour grouper par "group" de taille d'étab
#' @param excl_orient_non_pec Booléen : Les fugues, réorientations, partis sans attendre et sorties contre avis doivent-il être exclus des délais et diag ?
#'
#' @returns Un tibble
#' @export
#'
#' @import dplyr
#' @importFrom stats median
#' @importFrom tidyr pivot_longer
#'
fct_calc_indic_evol = function(RPU, group_by = "etab", excl_orient_non_pec = TRUE){
  #Check format base
  if(any(!c("SORTIE", "ENTREE", "MODE_SORTIE", "ORIENTATION", "hospit", "duree_passage_min", "SEXE", "age",
            "age_inf2", "age_inf15", "age_geq75", "nuit", "weekend", "TRANSPORT", "CCMU_1_2",
            "DP_type", "annee", "NOM_ETAB", "COD_FIN") %in% names(RPU))){
    stop('Les colonnes "SORTIE", "ENTREE", "MODE_SORTIE", "ORIENTATION", "hospit", "duree_passage_min", "SEXE", "age",
         "age_inf2", "age_inf15", "age_geq75", "nuit", "weekend", "TRANSPORT", "CCMU_1_2",
         "DP_type", "annee", "NOM_ETAB", "COD_FIN" doivent etre presentes dans la base RPU')
  }

  if(!group_by %in% c("etab", "group", "none")){
    stop("group_by doit \u00eatre \'etab\', \'group\' ou \'none\'")
  }

  #Exclusion des REO/PSA/SCAM/FUGUE si nécessaire
  if(excl_orient_non_pec){
    RPU = RPU %>%
      mutate(
        duree_passage_min = if_else(ORIENTATION %in% c("FUGUE", "REO", "PSA", "SCAM"), NA_real_, duree_passage_min),
        DP_type = if_else(ORIENTATION %in% c("FUGUE", "REO", "PSA", "SCAM"), NA_character_, DP_type)
        )
  }

  #groupe par etab, group ou rien
  if(group_by == "etab"){
    base_grp = RPU %>% group_by(annee, NOM_ETAB, COD_FIN)
  } else if(group_by == "group"){
    base_grp = RPU %>% group_by(annee, group)
  } else if(group_by == "none"){
    base_grp = RPU %>% group_by(annee)
  } else stop("group_by doit \u00eatre etab ou group ou none")

  tab_indic_evol = base_grp %>%
    mutate(
      across(.cols = c(MODE_SORTIE, SEXE),
             .fns = function(x){
               if_else(x %in% c("NC", "I") , NA_character_, x)
             })
    ) %>%
    # group_by(annee, NOM_ETAB, COD_FIN) %>%
    summarise(
      n_RPU = n(),
      n_present_15h = if_else(sum(!is.na(SORTIE)) >= 30,
                              fct_nb_presents(d_entree = ENTREE, d_sortie = SORTIE, heure = 15),
                              NA_real_),
      n_present_5h = if_else(sum(!is.na(SORTIE)) >= 30,
                             fct_nb_presents(d_entree = ENTREE, d_sortie = SORTIE, heure = 5),
                             NA_real_),
      tx_hospit = if_else(sum(!is.na(MODE_SORTIE)) >= 30,
                          round(mean(hospit, na.rm = T)*100, 1),
                          NA_real_),
      tx_mutation = if_else(sum(!is.na(MODE_SORTIE)) >= 30,
                            round(mean(MODE_SORTIE == "Mutation", na.rm = T)*100, 1),
                            NA_real_),
      tx_transfert = if_else(sum(!is.na(MODE_SORTIE)) >= 30,
                             round(mean(MODE_SORTIE == "Transfert", na.rm = T)*100, 1),
                             NA_real_),
      tx_RAD = if_else(sum(!is.na(MODE_SORTIE)) >= 30,
                       round(mean(MODE_SORTIE == "Domicile", na.rm = T)*100, 1),
                       NA_real_),
      med_duree_passage = if_else(sum(!is.na(duree_passage_min)) >= 30,
                                  median(duree_passage_min, na.rm = T),
                                  NA_real_),
      tx_duree_passage_sup_4h = if_else(sum(!is.na(duree_passage_min)) >= 30,
                                        round(mean(duree_passage_min > 4*60, na.rm = T)*100, 1),
                                        NA_real_),
      sex_ratio = if_else(sum(!is.na(SEXE)) >= 30,
                          round(sum(SEXE == "F", na.rm = T)/sum(SEXE == "M", na.rm = T), 1),
                          NA_real_),
      moy_age = if_else(sum(!is.na(age)) >= 30,
                        round(mean(age, na.rm = T)),
                        NA_real_),
      tx_age_inf2 = if_else(sum(!is.na(age)) >= 30,
                            round(mean(age_inf2, na.rm = T)*100, 1),
                            NA_real_),
      tx_age_inf15 = if_else(sum(!is.na(age)) >= 30,
                             round(mean(age_inf15, na.rm = T)*100, 1),
                             NA_real_),
      tx_age_geq75 = if_else(sum(!is.na(age)) >= 30,
                             round(mean(age_geq75, na.rm = T)*100, 1),
                             NA_real_),
      tx_nuit = if_else(sum(!is.na(nuit)) >= 30,
                        round(mean(nuit, na.rm = T)*100, 1),
                        NA_real_),
      tx_weekend = if_else(sum(!is.na(weekend)) >= 30,
                           round(mean(weekend, na.rm = T)*100, 1),
                           NA_real_),
      tx_PERSO = if_else(sum(!is.na(TRANSPORT)) >= 30,
                         round(mean(TRANSPORT == "PERSO", na.rm = T)*100, 1),
                         NA_real_),
      tx_SANITAIRE = if_else(sum(!is.na(TRANSPORT)) >= 30,
                             round(mean(TRANSPORT != "PERSO", na.rm = T)*100, 1),
                             NA_real_),
      tx_CCMU1_2 = if_else(sum(!is.na(CCMU_1_2)) >= 30,
                           round(mean(CCMU_1_2, na.rm = T)*100, 1),
                           NA_real_),
      tx_DP_medico_chir = if_else(sum(!is.na(DP_type)) >= 30,
                                  round(mean(DP_type == "M\u00e9dico-chirurgical", na.rm = T)*100, 1),
                                  NA_real_),
      tx_DP_trauma = if_else(sum(!is.na(DP_type)) >= 30,
                             round(mean(DP_type == "Traumatologique", na.rm = T)*100, 1),
                             NA_real_),
      tx_DP_psy = if_else(sum(!is.na(DP_type)) >= 30,
                          round(mean(DP_type == "Psychiatrique", na.rm = T)*100, 1),
                          NA_real_),
      tx_DP_toxico = if_else(sum(!is.na(DP_type)) >= 30,
                             round(mean(DP_type == "Toxicologique", na.rm = T)*100, 1),
                             NA_real_),
      tx_DP_autre = if_else(sum(!is.na(DP_type)) >= 30,
                            round(mean(DP_type == "Autre recours", na.rm = T)*100, 1),
                            NA_real_)
    ) %>%
    mutate(
      n_jours_an = if_else(as.numeric(annee) %% 4 == 0, 366, 365),#Marche jusqu'à 2100
      n_RPU_quotidien = round(n_RPU/n_jours_an)
    ) %>%
    pivot_longer(cols = c(n_RPU:n_RPU_quotidien), names_to = "Indicateur", values_to = "val_indicateur")

  return(tab_indic_evol)
}






#' Inforgraphie description des personnes âgées
#'
#' @param RPU Une table au format RPU utilisé dans les rapports (non filtré sur les PA)
#' @param nb_PA_region Nombre de personnes âgées en ARA à afficher dans le graphique
#' @param lab_annee label des années à afficher. Calculé automatiquement sur les données si "NULL"
#' @returns Un ggplot
#' @export
#'
#' @importFrom png readPNG
#' @importFrom lubridate date wday
#' @importFrom stats median
#' @importFrom purrr map_df
#' @import dplyr
#' @import ggplot2
#'
fct_infographic_descript_PA = function(RPU, nb_PA_region, lab_annee = NULL){
  #Chargement du fond de plot
  img <- readPNG(system.file("img/infog_descript_PA_vierge.png", package = "pkgRapportsRPU"))

  #Selection des PA
  RPU_PA = RPU %>%
    filter(age_geq75) %>%
    mutate(
      across(c(GRAVITE, TRANSPORT, SEXE, MODE_SORTIE), function(col){
        if_else(col %in% c("NC", "I"), NA_character_, as.character(col))
      })
    ) %>%
    left_join(Thesaurus_Fedoru %>% select(diagnostic_code, type_urgence_libelle) %>% unique,
              by = c("DP_clean" = "diagnostic_code"), relationship = "many-to-one")

  #Calcul des informations à afficher dans le plot
  ##calc lab_annee
  lab_annee = ifelse(!is.null(lab_annee), lab_annee, paste(unique(range(RPU$annee)), sep = "-"))

  ##tab des indicateurs
  nb_jours = length(unique(date(RPU$ENTREE)))
  tab_info_graph = tibble(
    #Donnes regionales
    # lab_annee = lab_annee,
    nb_PA_region = fct_f_big(as.numeric(nb_PA_region)),
    nb_RPU_PA = fct_f_big(nrow(RPU_PA)),
    part_PA = paste0(round(nrow(RPU_PA)/nrow(RPU)*100, 1), "%"),
    moy_quot_passages = fct_f_big(round(nrow(RPU_PA)/nb_jours)),
    moy_present_15h = fct_f_big(fct_nb_presents(d_entree = RPU_PA$ENTREE, d_sortie = RPU_PA$SORTIE,
                                                heure = 15)),
    moy_present_6h = fct_f_big(fct_nb_presents(d_entree = RPU_PA$ENTREE, d_sortie = RPU_PA$SORTIE,
                                                      heure = 6)),
    #mode de sortie
    tx_hpt = paste0(round(mean(RPU_PA$hospit, na.rm = T)*100, 1), "%"),
    tx_mut = paste0(round(mean(RPU_PA$MODE_SORTIE == "Mutation", na.rm = T)*100, 1), "%"),
    tx_transf = paste0(round(mean(RPU_PA$MODE_SORTIE == "Transfert", na.rm = T)*100, 1), "%"),
    tx_RAD = paste0(round(mean(RPU_PA$MODE_SORTIE == "Domicile", na.rm = T)*100, 1), "%"),

    #DMS
    med_duree_passage = fct_f_big(round(median(RPU_PA$duree_passage_min, na.rm = T))),
    med_duree_passage_inf4h = paste0(round(mean(RPU_PA$duree_passage_min < 4*60, na.rm = T)*100, 1), "%"),

    #Patients
    sex_ratio = round(sum(RPU_PA$SEXE == "M", na.rm = T)/sum(RPU_PA$SEXE == "F", na.rm = T), 1),
    moy_age = round(mean(RPU_PA$age, na.rm = T), 1),
    age_sup90 = paste0(round(mean(RPU_PA$age > 90, na.rm = T)*100, 1), "%"),

    #Arrivée
    tx_nuit = paste0(round(mean(RPU_PA$H_ENTREE >= 20 | RPU_PA$H_ENTREE < 8, na.rm = T)*100, 1), "%"),
    tx_weekend = paste0(round(mean(wday(RPU_PA$ENTREE, week_start = 1) %in% c(6, 7))*100, 1), "%"),
    tx_TRANS_PERSO = paste0(round(mean(RPU_PA$TRANSPORT == "PERSO", na.rm = T)*100, 1), "%"),
    tx_TRANS_SANI = paste0(round(mean(RPU_PA$TRANSPORT != "PERSO", na.rm = T)*100, 1), "%"),
    tx_CCMU1_2 = paste0(round(mean(RPU_PA$GRAVITE == "1" | RPU_PA$GRAVITE == "2", na.rm = T)*100, 1), "%"),

    #DP
    tx_DP_medicochir = paste0(round(mean(RPU_PA$type_urgence_libelle == "M\u00e9dico-chirurgical", na.rm = T)*100, 1), "%"),
    tx_DP_trauma = paste0(round(mean(RPU_PA$type_urgence_libelle == "Traumatologique", na.rm = T)*100, 1), "%"),
    tx_DP_psy = paste0(round(mean(RPU_PA$type_urgence_libelle == "Psychiatrique", na.rm = T)*100, 1), "%"),
    tx_DP_toxico = paste0(round(mean(RPU_PA$type_urgence_libelle == "Toxicologique", na.rm = T)*100, 1), "%"),
    tx_DP_autre = paste0(round(mean(RPU_PA$type_urgence_libelle == "Autre recours", na.rm = T)*100, 1), "%")
  )
  #correction des données non calculables en NC
  tab_info_graph <- map_df(tab_info_graph, function(col){
    if_else(is.na(col) | is.nan(col) |
              grepl("NA", col)| grepl("NaN", col), "NC", as.character(col))
    })

  #Définition des coordonnées dans le plot
  tab_coord_chiffres <-
    tibble(Indicateur = c("nb_PA_region", "nb_RPU_PA", "part_PA", "moy_quot_passages", "moy_present_15h", "moy_present_6h",
                          "tx_hpt", "tx_mut", "tx_transf", "tx_RAD",
                          "med_duree_passage", "med_duree_passage_inf4h",
                          "sex_ratio", "moy_age", "age_sup90",
                          "tx_nuit", "tx_weekend", "tx_TRANS_PERSO", "tx_TRANS_SANI", "tx_CCMU1_2",
                          "tx_DP_medicochir", "tx_DP_trauma", "tx_DP_psy", "tx_DP_toxico", "tx_DP_autre"),
           x = c(rep(-74, 6),
                 rep(-76, 4),
                 rep(-71.2, 2),
                 rep(45, 3),
                 rep(58, 5),
                 rep(40.5, 5)),
           y = c(51.5, 46, 40.5, 35.1, 29.5, 24.1,
                 -11.9, -17.3, -23, -28.4,
                 -73.9, -79.6,
                 54.7, 49.2, 43.8,
                 -1.9, -7.5, -18.9, -24.2, -35.7,
                 -76.7, -82.2, -87.6, -93.1, -98.6))

  #Création des labels à mettre sur le plot + merge avec les coordonnées
  tab_plot = tab_info_graph %>%
    mutate_all(as.character) %>%
    pivot_longer(everything(), names_to = "Indicateur", values_to = "val_indicateur") %>%
    right_join(tab_coord_chiffres, by = "Indicateur")

  #Création du plot
  plot_descript_PA <- ggplot(tab_plot, aes(x = x, y = y)) +
    background_image(img) +
    geom_text(aes(label = val_indicateur), size = 2, hjust = 1, color = "#345885") +
    annotate(geom = "text", label = lab_annee, x = -17, y = 60,
             size = 3.9, hjust = 1, color = "#345885", fontface = "bold") +
    scale_size_continuous(range = c(3, 10)) +
    coord_cartesian(xlim = c(-100, 100), ylim = c(-100,100)) +
    theme_void() +
    theme(legend.position = "none")

  return(plot_descript_PA)
}
