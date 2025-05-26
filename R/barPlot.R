
#' barplot simple
#'
#' @param base_groupe RPU du groupe dont l'étab actif (var etab_actif)
#' @param var_interet Character : Variable a représenter
#' @param ylab Nom de l'axe y du graphique
#' @param titre_graph Titre du graphique
#' @param titre_tab Titre de la table
#' @param lab_var_tab Nom de la variable dans la table
#'
#' @returns Un plot et une table
#' @export
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom kableExtra kbl kable_classic_2 kable_styling
#' @importFrom ggpubr theme_pubclean
#' @importFrom scales label_percent
#' @importFrom stats reorder
fct_make_barplot <- function(base_groupe, var_interet,
                             ylab = "", titre_graph = "",
                             titre_tab = "", lab_var_tab = ""){
  # lab_etab <- unique(base_groupe$NOM_ETAB[base_groupe$etab_actif %in% "1"])
  lab_groupe <- unique(base_groupe$group)

  #calcule des chiffres du graph
  tab_plot <- base_groupe %>%
    select(var_interet = all_of(var_interet), NOM_ETAB, etab_actif) %>%
    group_by(NOM_ETAB, etab_actif, .drop = F) %>%
    summarise(y = mean(as.numeric(var_interet), na.rm = T),
              n_noNA = sum(!is.na(var_interet)),
              n = sum(as.numeric(var_interet), na.rm = T),
              y = if_else(n_noNA == 0, 0, y, missing = 0)#Si aucune données on met 0
              ) %>%
    ungroup() %>%
    mutate(NOM_ETAB = reorder(NOM_ETAB, y),
           effectif_bas = n_noNA < 30)

  vec_var_interet <- pull(base_groupe, all_of(var_interet))
  mean_groupe = mean(vec_var_interet, na.rm = T)

  #réalisation du graph
  plot <- ggplot(tab_plot, aes(x = NOM_ETAB, y = y, fill = etab_actif)) +
    geom_col(width = 0.75, aes(alpha = effectif_bas)) +
    geom_hline(aes(yintercept = mean_groupe, linetype = "Moyenne du groupe"), linewidth = 1, color = col_secblue) +
    scale_y_continuous(name = ylab, labels = label_percent(), breaks = pretty) +
    scale_x_discrete(name = "Etablissement") +
    scale_fill_manual(name = "", values = c("0" = col_glob, "1" = col_etab)) +
    scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 1)) +
    scale_linetype_manual(name = "", values = c("Moyenne du groupe" = 2)) +
    labs(title = titre_graph) +
    guides(fill = "none", alpha = "none") +
    theme_pubclean() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.key = element_blank())

  #Calcule des chiffres de la table
  tab_etab <- base_groupe %>%
    filter(etab_actif %in% "1") %>%
    select(var_interet = all_of(var_interet)) %>%
    summarise(Groupe = "Service",
              "Nombre de passages" = format(n(), big.mark = " "),
              "Dont renseign\u00e9s" = format(sum(!is.na(var_interet)), big.mark = " "),
              var_interet = paste0(format(sum(var_interet, na.rm = T), big.mark = " "), " (",
                                   round(mean(var_interet, na.rm = T)*100, 1), "%)"))

  tab_group <- base_groupe %>%
    select(var_interet = all_of(var_interet)) %>%
    summarise(
      Groupe = "Groupe",
      "Nombre de passages" = format(n(), big.mark = " "),
      "Dont renseign\u00e9s" = format(sum(!is.na(var_interet)), big.mark = " "),
      var_interet = paste0(format(sum(var_interet, na.rm = T), big.mark = " "), " (",
                           round(mean(var_interet, na.rm = T)*100, 1), "%)"))

  if(sum(base_groupe$etab_actif == 1, na.rm = T) > 0){
    tab_glob <-  bind_rows(tab_etab, tab_group)
  } else {
    tab_glob <- tab_group
  }

  tab_glob_kbl <- tab_glob %>%
    kbl(caption = titre_tab, col.names = c("", "Nombre de\npassages", "Dont renseign\u00e9s", lab_var_tab),
        align = "c", format = "latex") %>%
    kable_classic_2() %>%
    kable_styling(latex_options = c("HOLD_position"))


  return(list(plot = plot,
              data = tab_glob_kbl))
}


#' barplot simple sur l'âge
#'
#' @param base_groupe RPU du groupe dont l'étab actif (var etab_actif)
#' @param titre_tab titre de la table
#'
#' @returns Un plot et une table
#' @export
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom kableExtra kbl kable_classic_2 kable_styling
#' @importFrom ggpubr theme_pubclean
#' @importFrom stats reorder
fct_make_barplot_age <- function(base_groupe, titre_tab = ""){
  #Calcul de la var d'intérêt (age >= 75 parmi les adultes)
  base_groupe = base_groupe %>%
    mutate(age_geq75_adult = if_else(as.numeric(age) >= 18, age_geq75, NA))

  #récupération nom etab/groupe
  lab_groupe <- unique(base_groupe$group)

  #calcule des chiffres du graph
  tab_plot <- base_groupe %>%
    mutate(age_geq75_adult = if_else(as.numeric(age) >= 18, age_geq75, NA)) %>%
    group_by(NOM_ETAB, etab_actif, .drop = F) %>%
    summarise(y = mean(as.numeric(age_geq75_adult), na.rm = T),
              n_noNA = sum(!is.na(age_geq75_adult)),
              n = sum(as.numeric(age_geq75_adult), na.rm = T)) %>%
    ungroup() %>%
    mutate(NOM_ETAB = reorder(NOM_ETAB, y),
           effectif_bas = n_noNA < 30)

  mean_groupe = mean(base_groupe$age_geq75_adult, na.rm = T)

  #réalisation du graph
  plot <- ggplot(tab_plot, aes(x = NOM_ETAB, y = y, fill = etab_actif)) +
    geom_col(width = 0.75, aes(alpha = effectif_bas)) +
    geom_hline(aes(yintercept = mean_groupe, linetype = "Moyenne du groupe"), linewidth = 1, color = col_secblue) +
    scale_y_continuous(name = "Patients adultes > 75 ans (%)", labels = percent, breaks = pretty) +
    scale_x_discrete(name = "Etablissement") +
    scale_fill_manual(name = "", values = c("0" = col_glob, "1" = col_etab)) +
    scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 1)) +
    scale_linetype_manual(name = "", values = c("Moyenne du groupe" = 2)) +
    guides(fill = "none", alpha = "none") +
    theme_pubclean() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.key = element_blank())

  #Calcule des chiffres de la table
  tab_etab <- base_groupe %>%
    filter(etab_actif %in% "1") %>%
    summarise(Groupe = "Service",
              "Nombre de passages" = format(n(), big.mark = " "),
              "Patients \u00e2g\u00e9s" = paste0(format(sum(age_geq75, na.rm = T), big.mark = " "), " (",
                                       round(mean(age_geq75, na.rm = T)*100, 1), "%)"),

              "Nombre de passages adultes" = format(sum(as.numeric(age) >= 18, na.rm = T), big.mark = " "),
              "Patients \u00e2g\u00e9s parmi les adultes" = paste0(format(sum(age_geq75_adult, na.rm = T), big.mark = " "), " (",
                                                         round(mean(age_geq75_adult, na.rm = T)*100, 1), "%)"))

  tab_group <- base_groupe %>%
    summarise(Groupe = "Groupe",
              "Nombre de passages" = format(n(), big.mark = " "),
              "Patients \u00e2g\u00e9s" = paste0(format(sum(age_geq75, na.rm = T), big.mark = " "), " (",
                                       round(mean(age_geq75, na.rm = T)*100, 1), "%)"),

              "Nombre de passages adultes" = format(sum(as.numeric(age) >= 18, na.rm = T), big.mark = " "),
              "Patients \u00e2g\u00e9s parmi les adultes" = paste0(format(sum(age_geq75_adult, na.rm = T), big.mark = " "), " (",
                                                         round(mean(age_geq75_adult, na.rm = T)*100, 1), "%)"))
  if(sum(base_groupe$etab_actif == "1", na.rm = T) > 0){
    tab_glob <-  bind_rows(tab_etab, tab_group)
  } else {
    tab_glob <- tab_group
  }

  tab_glob_kbl <- tab_glob %>%
    kbl(caption = titre_tab, col.names = c("", "Nombre de\npassages", "Patients \u00e2g\u00e9s", "Nombre de\npassages adultes", "Patients \u00e2g\u00e9s parmi\nles adultes"),
        align = "c", format = "latex") %>%
    kable_classic_2() %>%
    kable_styling(latex_options = c("HOLD_position"))


  return(list(plot = plot,
              data = tab_glob_kbl))
}


#' barplot bicolor
#'
#' @param base_etab RPU de l'établissement
#' @param base_groupe RPU du groupe de l'établissement
#' @param var_interet Character : Variable a représenter
#' @param label_var_interet Nom de la variable à représenter
#' @param na.rm Booléen : Retirer les données manquantes
#' @param titre Titre du graphique
#' @param make_table Booléen : Le tableau de données doit-il être généré
#' @param rm.empty.levels Booléen : Retirer les niveaux vides d'un facteur
#' @param pedia Booléen : le rapport est-il pédiatrique
#' @param in.freq.order Booléen : Ordonner le graphique par fréquence décroissante (TRUE) ou selon les niveaux du facteurs (FALSE)
#'
#' @returns un graphique et une table
#' @export
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom ggpubr theme_pubclean
#' @importFrom stats relevel na.omit
#' @importFrom forcats fct_infreq fct_relevel fct_inorder
#' @importFrom scales percent_format
#' @importFrom stringr str_remove_all
#'
fct_make_bardodge <- function(base_etab, base_groupe, var_interet, label_var_interet,
                              na.rm = FALSE, titre = "", make_table = FALSE,
                              rm.empty.levels = FALSE, pedia, in.freq.order = TRUE){
  # renommage de la variable à ploter afin de pouvoir l'invoquer plus facilement par la suite
  base_groupe <- base_groupe %>%
    rename(var_interet = all_of(var_interet)) %>%
    mutate(groupe = "Groupe")
  base_etab <- base_etab %>%
    rename(var_interet = all_of(var_interet)) %>%
    mutate(groupe = "Service")

  if(rm.empty.levels){
    base_groupe$var_interet = factor(base_groupe$var_interet)
    base_etab$var_interet = factor(base_etab$var_interet, levels = levels(base_groupe$var_interet))
  }

  base <- bind_rows(base_groupe, base_etab)
  base$groupe <- relevel(factor(base$groupe), ref = "Service")

  if(in.freq.order){#Organise var_interet par ordre décroissant dans le service
    order_service = levels(fct_infreq(base_etab$var_interet))#modalité présentes dans le service par ordre décroissant
    levels_tot = levels(fct_infreq(base$var_interet))#tous les niveaux possibles
    order_modalite = c(order_service, setdiff(levels_tot, order_service))#ajout à la fin des niveaux présent au glob mais pas dans le service
    base$var_interet = factor(base$var_interet, levels = order_modalite)
  }

  #Gestion des NC
  if(na.rm){
    base$var_interet = recode_factor(base$var_interet, "NC" = NA_character_)
  }

  tab_plot <- base %>%
    group_by(var_interet,
             groupe, .drop = F) %>%
    count() %>%
    ungroup()
  if(na.rm){tab_plot = na.omit(tab_plot)}
  tab_plot <- tab_plot %>%
    group_by(groupe, .drop = F) %>%
    mutate(p = n/sum(n),
           p_lab = paste0(round(p*100, 1), "%"),
           var_interet = reorder(var_interet, desc(p)),
           var_interet = fct_relevel(var_interet, "NC", after = Inf),
           groupe_lab = paste0(groupe, "\n(n = ", fct_f_big(sum(n)), ")"),
           groupe_lab = fct_inorder(groupe_lab))

  vec_color_fill = c(col_etab, col_glob)
  names(vec_color_fill) = unique(tab_plot$groupe_lab)


  plot <- ggplot(tab_plot, aes(x = var_interet, y = p, fill = groupe_lab)) +
    geom_col(position = position_dodge2(width = 1, preserve = "single")) +
    scale_fill_manual(name = "", values = vec_color_fill) + #c(col_glob, col_etab)) +#suppose que les groupes sont dans l'ordre alphabetique
    scale_y_continuous(name = "Pourcentage de patients", breaks = pretty,
                       labels = percent_format(accuracy = 1)) +
    scale_x_discrete(name = label_var_interet, expand = c(0.1, 0)) +
    coord_cartesian(expand = T) +
    guides(fill = guide_legend(nrow = 1)) +
    labs(title = titre) +
    theme_pubclean() +
    theme(legend.position = "bottom")

  #table retournée à la fin
  if(!make_table){
    tab_adulte = NULL
    tab_ped = NULL
  } else {
    #adulte
    tab_adulte_n = table(base$groupe, base$var_interet)
    tab_adulte_p = round(prop.table(tab_adulte_n, 1)*100, 1)

    tab_adulte = paste0(trimws(format(tab_adulte_n, big.mark = " ")), " (",
                        tab_adulte_p, "%)") %>%
      str_remove_all(" \\(NaN%\\)") %>%
      matrix(nrow = nrow(tab_adulte_n), ncol = ncol(tab_adulte_n), byrow = F)
    rownames(tab_adulte) = row.names(tab_adulte_n)
    colnames(tab_adulte) = colnames(tab_adulte_n)

    if(na.rm){
      tab_adulte_n_NA = table(base$groupe, base$var_interet, useNA = "al")
      col_adulte_n_NA <- fct_f_big(tab_adulte_n_NA[1:2, ncol(tab_adulte_n_NA)])
      tab_adulte <- cbind(tab_adulte, NC = col_adulte_n_NA)
    }

    #pedia
    if(pedia){
      base_ped = base %>% filter(age_inf2)
    } else {
      base_ped = base %>% filter(age_inf15)
    }

    tab_ped_n = table(base_ped$groupe, base_ped$var_interet)
    tab_ped_p = round(prop.table(tab_ped_n, 1)*100, 1)

    tab_ped = paste0(trimws(format(tab_ped_n, big.mark = " ")), " (",
                     tab_ped_p, "%)") %>%
      str_remove_all(" \\(NaN%\\)") %>%
      matrix(nrow = nrow(tab_ped_n), ncol = ncol(tab_ped_n), byrow = F)
    rownames(tab_ped) = row.names(tab_ped_n)
    colnames(tab_ped) = colnames(tab_ped_n)

    if(na.rm){
      tab_ped_n_NA = table(base_ped$groupe, base_ped$var_interet, useNA = "al")
      col_ped_n_NA <- fct_f_big(tab_ped_n_NA[1:2, ncol(tab_ped_n_NA)])
      tab_ped <- cbind(tab_ped, NC = col_ped_n_NA)
    }
  }


  return(
    list(plot = plot,
         data_adulte = tab_adulte,
         data_pedia = tab_ped)
  )
}



#' barplot position fill
#'
#' @param base_etab RPU de l'établissement
#' @param base_groupe RPU du groupe de l'établissement
#' @param var_interet Character : Variable a représenter
#' @param label_var_interet Nom de la variable à représenter
#' @param titre Titre du graphique
#' @param make_table Booléen : Le tableau de données doit-il être généré
#' @param na.rm Booléen : Retirer les données manquantes
#' @param pedia Booléen : le rapport est-il pédiatrique
#' @param excl_orient_non_pec Booléen : Les patients réorientés/fugues/sortie sans ou contre avis médical doivent-ils être exclus ?
#' @param palette_color_manual palette de couleur donnée à "value" dans "scale_fill_manual"
#'
#' @returns un plot et deux tables
#' @export
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom ggpubr theme_pubclean
#' @importFrom stats relevel na.omit
#' @importFrom scales percent_format
#' @importFrom stringr str_remove_all
#'
fct_make_barfill <- function(base_etab, base_groupe, var_interet, label_var_interet, titre = "",
                             make_table = FALSE, na.rm = FALSE, pedia, excl_orient_non_pec = F,
                             palette_color_manual = NULL){
  # renommage de la variable à ploter afin de pouvoir l'invoquer plus facilement par la suite
  base_groupe <- base_groupe %>%
    rename(var_interet = all_of(var_interet)) %>%
    mutate(groupe = "Groupe")
  base_etab <- base_etab %>%
    rename(var_interet = all_of(var_interet)) %>%
    mutate(groupe = "Service")

  #Gestion du cas 100% NC => Erreur pour le moment
  if(all(base_etab$var_interet == "NC" | is.na(base_etab$var_interet))){
    stop(paste0("La variable ", var_interet, " est manquante pour 100% des patients"))
  }

  #Création de la base avec l'étab actif deux fois
  base <- bind_rows(base_groupe, base_etab)
  base$groupe <- relevel(factor(base$groupe), ref = "Service")

  #Suppression des FUGUE REO SCAM et PSA si excl_orient_non_pec = T
  if(excl_orient_non_pec){
    base = base %>%
      filter(!ORIENTATION %in% c("FUGUE", "REO", "PSA", "SCAM"))
  }

  #Gestion des NC
  if(na.rm){
    base$var_interet = recode_factor(base$var_interet, "NC" = NA_character_)
  }

  #Table du plot
  tab_plot <- base %>%
    group_by(var_interet,
             groupe, .drop = F) %>%
    count() %>%
    na.omit() %>%
    ungroup(var_interet) %>%
    mutate(p = round(n/sum(n)*100),
           groupe_lab = paste0(groupe, "\n(n = ", fct_f_big(sum(n)), ")"))
  # palette <- RColorBrewer::brewer.pal(length(levels(factor(tab_plot$var_interet))), "Set1")

  #plot
  plot <- ggplot(tab_plot, aes(x = groupe_lab, y = n, fill = var_interet)) +
    geom_col(position = position_fill(reverse = T), alpha = 0.9) +
    scale_fill_manual(values = pal_UrgAra(),
                      limits = levels(tab_plot$var_interet),
                      name = label_var_interet) +
    scale_y_continuous(name = "Pourcentage de patients", breaks = seq(0, 1, by = 0.1), labels = percent_format(accuracy = 1, suffix = "")) +
    scale_x_discrete(name = "") +
    coord_flip() +
    guides(fill = guide_legend(nrow = 1)) +
    labs(title = titre) +
    theme_pubclean() +
    theme(legend.position = "bottom",
          axis.text.y = element_text(hjust = 0.5),
          panel.background = element_rect(fill='transparent'),
          plot.background = element_rect(fill='transparent', color=NA)) #transparent plot bg

  if(!is.null(palette_color_manual)){#Changement de la palette de couleur pour une manuelle
    plot = plot + scale_fill_manual(name = label_var_interet,
                                    values = palette_color_manual)
  }

  #table retournée à la fin
  if(!make_table){
    tab_adulte = NULL
    tab_ped = NULL
  } else {
    #adulte
    tab_adulte_n = table(base$groupe, base$var_interet)
    tab_adulte_p = round(prop.table(tab_adulte_n, 1)*100, 1)

    tab_adulte = paste0(trimws(format(tab_adulte_n, big.mark = " ")), " (",
                        tab_adulte_p, "%)") %>%
      str_remove_all(" \\(NaN%\\)") %>%
      matrix(nrow = nrow(tab_adulte_n), ncol = ncol(tab_adulte_n), byrow = F)
    rownames(tab_adulte) = row.names(tab_adulte_n)
    colnames(tab_adulte) = colnames(tab_adulte_n)

    if(na.rm){
      tab_adulte_n_NA = table(base$groupe, base$var_interet, useNA = "al")
      col_adulte_n_NA <- fct_f_big(tab_adulte_n_NA[1:2, ncol(tab_adulte_n_NA)])
      tab_adulte <- cbind(tab_adulte, NC = col_adulte_n_NA)
    }

    #pedia
    if(pedia){
      base_ped = base %>% filter(age_inf2)
    } else{
      base_ped = base %>% filter(age_inf15)
    }

    tab_ped_n = table(base_ped$groupe, base_ped$var_interet)
    tab_ped_p = round(prop.table(tab_ped_n, 1)*100, 1)

    tab_ped = paste0(trimws(format(tab_ped_n, big.mark = " ")), " (",
                     tab_ped_p, "%)") %>%
      str_remove_all(" \\(NaN%\\)") %>%
      matrix(nrow = nrow(tab_ped_n), ncol = ncol(tab_ped_n), byrow = F)
    rownames(tab_ped) = row.names(tab_ped_n)
    colnames(tab_ped) = colnames(tab_ped_n)

    if(na.rm){
      tab_ped_n_NA = table(base_ped$groupe, base_ped$var_interet, useNA = "al")
      col_ped_n_NA <- fct_f_big(tab_ped_n_NA[1:2, ncol(tab_ped_n_NA)])
      tab_ped <- cbind(tab_ped, NC = col_ped_n_NA)
    }

  }

  return(
    list(plot = plot,
         data_adulte = tab_adulte,
         data_pedia = tab_ped)
  )
}




#' barplot position fill par etablissement
#'
#' @param base_groupe RPU du groupe de l'établissement
#' @param var_interet Character : Variable a représenter
#' @param label_var_interet Nom de la variable à représenter
#' @param grouping_var Nom de la variable de groupement (en axe y)
#' @param label_grouping Label a afficher pour l'axe Y sur le graphique
#' @param titre Titre du graphique
#' @param make_table Booléen : Le tableau de données doit-il être généré
#' @param na.rm Booléen : Retirer les données manquantes
#' @param pedia Booléen : le rapport est-il pédiatrique
#' @param excl_orient_non_pec Booléen : Les patients réorientés/fugues/sortie sans ou contre avis médical doivent-ils être exclus ?
#' @param palette_color_manual palette de couleur donnée à "value" dans "scale_fill_manual"
#' @param anonyme Boolèen : Les valeurs de la variable de groupement doivent-elles être cachées sur le graphique ?
#'
#' @returns un plot et deux tables
#' @export
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom ggpubr theme_pubclean
#' @importFrom stats relevel na.omit
#' @importFrom scales percent_format
#' @importFrom stringr str_remove_all
#'
fct_make_barfill_par_etab <- function(base_groupe, var_interet, label_var_interet,
                                      grouping_var, label_grouping, titre = "",
                                      make_table = FALSE, na.rm = FALSE, pedia, excl_orient_non_pec = F,
                                      palette_color_manual = NULL, anonyme = TRUE){
  # renommage de la variable à ploter afin de pouvoir l'invoquer plus facilement par la suite
  base <- base_groupe %>%
    rename(var_interet = all_of(var_interet),
           groupe = all_of(grouping_var))

  #Suppression des FUGUE REO SCAM et PSA si excl_orient_non_pec = T
  if(excl_orient_non_pec){
    base = base %>%
      filter(!ORIENTATION %in% c("FUGUE", "REO", "PSA", "SCAM"))
  }

  #Gestion des NC
  if(na.rm){
    base$var_interet = recode_factor(base$var_interet, "NC" = NA_character_)
  }

  #Table du plot
  tab_plot <- base %>%
    group_by(var_interet,
             groupe, .drop = F) %>%
    count() %>%
    na.omit() %>%
    ungroup(var_interet) %>%
    mutate(p = round(n/sum(n)*100),
           groupe_lab = paste0(groupe, "\n(n = ", fct_f_big(sum(n)), ")"))

  #plot
  plot <- ggplot(tab_plot, aes(x = groupe_lab, y = n, fill = var_interet)) +
    geom_col(position = position_fill(reverse = T), alpha = 0.9) +
    scale_fill_manual(values = pal_UrgAra(),
                      limits = levels(tab_plot$var_interet),
                      name = label_var_interet) +
    scale_y_continuous(name = "Pourcentage de patients", breaks = seq(0, 1, by = 0.1), labels = percent_format(accuracy = 1, suffix = "")) +
    scale_x_discrete(name = label_grouping) +
    coord_flip() +
    guides(fill = guide_legend(nrow = 1)) +
    labs(title = titre) +
    theme_pubclean() +
    theme(legend.position = "bottom",
          axis.text.y = element_text(hjust = 0.5),
          panel.background = element_rect(fill='transparent'),
          plot.background = element_rect(fill='transparent', color=NA)) #transparent plot bg

  if(anonyme){  #Anonymisation
    plot = plot +
      theme(
        axis.text.y = element_blank(),
        axis.ticks = element_blank())
  }

  if(!is.null(palette_color_manual)){#Changement de la palette de couleur pour une manuelle
    plot = plot + scale_fill_manual(name = label_var_interet,
                                    values = palette_color_manual)
  }

  #table retournée à la fin
  if(!make_table){
    tab = NULL
  } else {
    #adulte
    tab_n = table(base$var_interet)
    tab_p = round(prop.table(tab_n)*100, 1)

    tab = paste0(trimws(format(tab_n, big.mark = " ")), " (",
                 tab_p, "%)") %>%
      str_remove_all(" \\(NaN%\\)") %>%
      as.data.frame() %>%
      t()
    rownames(tab) = "Groupe"
    colnames(tab) = names(tab_n)

    if(na.rm){
      nNA = format(sum(is.na(base$var_interet)), big.mark = " ")
      tab = cbind(tab, "NC" = nNA)
    }
  }

  return(
    list(plot = plot,
         data = tab)
  )
}
