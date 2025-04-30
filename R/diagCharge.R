#' Figure et table diag de charge
#'
#' @param base_etab RPU de l'établissement
#' @param base_groupe RPU du groupe
#' @param borne_min Début de la période
#' @param borne_max Fin de la période
#' @param duree_max Durée maximale autorisée
#' @param pedia booléen : rapport pédia ou non
#' @param outlier_handling How should values above max_LOS be handled. If outlier_handling = "cap" (default), these values will be replaced by max_LOS. If outlier_handling = "remove", these values will be removed_handling
#'
#' @returns un plot et 2 table et 2 infos pour l'infographie
#' @export
#'
#' @import rUrgAra
#' @import dplyr
#' @importFrom tidyr pivot_wider
#'
#'
fct_diagramme_charge <- function(base_etab, base_groupe, borne_min, borne_max,
                                 duree_max, pedia, outlier_handling){
  res_whale_etab = rUrgAra::plot_diag_charge(base_etab, entry = "ENTREE", exit = "SORTIE",
                                             from = borne_min, to = borne_max, max_LOS = duree_max,
                                             outlier_handling = outlier_handling
                                             )
  res_whale_groupe <- rUrgAra::plot_diag_charge(base_groupe, entry = "ENTREE", exit = "SORTIE",
                                                strata = "NOM_ETAB",
                                                from = borne_min, to = borne_max, max_LOS = duree_max,
                                                outlier_handling = outlier_handling)

  n_jours <- res_whale_etab$tab$n_days[1]

  #création des tableaux
  tab_whale_etab = res_whale_etab$tab
  tab_whale_groupe = res_whale_groupe$tab

  ##pop globale
  tab_export_etab <- tab_whale_etab %>%
    mutate(Heure = factor(Hour, levels = 1:24)) %>%
    group_by(Heure, .drop = F) %>%
    summarise(n = round(sum(n_avg, na.rm = T))) %>%
    mutate(Heure = paste0(as.numeric(as.character(Heure)), "h")) %>%
    pivot_wider(names_from = Heure, values_from = n) %>%
    mutate(Population = "Service", .before = "1h")

  tab_export_groupe <- tab_whale_groupe %>%
    mutate(Heure = factor(Hour, levels = 1:24)) %>%
    group_by(Heure, .drop = F) %>%
    summarise(n = round(sum(n_avg, na.rm = T))) %>%
    mutate(Heure = paste0(as.numeric(as.character(Heure)), "h")) %>%
    pivot_wider(names_from = Heure, values_from = n) %>%
    mutate(Population = "Groupe", .before = "1h")

  tab_export <- bind_rows(tab_export_etab, tab_export_groupe)

  #Pop pédiatrique
  if(pedia){
    base_etab_pedia = base_etab %>%
      filter(age_inf2)
    base_groupe_pedia = base_groupe %>%
      filter(age_inf2)
  } else{
    base_etab_pedia = base_etab %>%
      filter(age_inf15)
    base_groupe_pedia = base_groupe %>%
      filter(age_inf15)
  }


  res_whale_etab_pedia <- rUrgAra::plot_diag_charge(base_etab_pedia, entry = "ENTREE", exit = "SORTIE",
                                                    strata = "NOM_ETAB",
                                                    from = borne_min, to = borne_max, max_LOS = duree_max)
  res_whale_groupe_pedia <- rUrgAra::plot_diag_charge(base_groupe_pedia, entry = "ENTREE", exit = "SORTIE",
                                                      strata = "NOM_ETAB",
                                                      from = borne_min, to = borne_max, max_LOS = duree_max)

  tab_export_etab_ped <-  res_whale_etab_pedia$tab %>%
    mutate(Heure = factor(Hour, levels = 1:24)) %>%
    group_by(Heure, .drop = F) %>%
    summarise(n = round(sum(n_avg, na.rm = T))) %>%
    mutate(Heure = paste0(as.numeric(as.character(Heure)), "h")) %>%
    pivot_wider(names_from = Heure, values_from = n) %>%
    mutate(Population = "Service", .before = "1h")

  tab_export_groupe_ped <- res_whale_groupe_pedia$tab %>%
    mutate(Heure = factor(Hour, levels = 1:24)) %>%
    group_by(Heure, .drop = F) %>%
    summarise(n = round(sum(n_avg, na.rm = T))) %>%
    mutate(Heure = paste0(as.numeric(as.character(Heure)), "h")) %>%
    pivot_wider(names_from = Heure, values_from = n) %>%
    mutate(Population = "Groupe", .before = "1h")

  tab_export_ped <- bind_rows(tab_export_etab_ped, tab_export_groupe_ped)

  #tab infographics en conclusion
  ##etab
  tab_effectifs_h_etab <- tab_whale_etab %>%
    group_by(Heure = Hour, .drop = F) %>%
    summarise(n = round(sum(n_avg))) %>%
    arrange(Heure)

  #Séparer les deux actions permet de facilement prendre le premier max et premier min s'il y en a plusieurs
  tab_infographic_max_etab <- tab_effectifs_h_etab %>%
    filter(n == max(n)) %>%
    slice(1) %>%
    mutate(activite = "max")

  tab_infographic_min_etab <- tab_effectifs_h_etab %>%
    filter(n == min(n)) %>%
    slice(1) %>%
    mutate(activite = "min")

  tab_infographic_etab <- bind_rows(tab_infographic_max_etab, tab_infographic_min_etab)

  ##Groupe
  tab_effectifs_h_groupe <- tab_whale_groupe %>%
    group_by(Heure = Hour, .drop = F) %>%
    summarise(n = round(sum(n_avg))) %>%
    arrange(Heure)

  #Séparer les deux actions permet de facilement prendre le premier max et premier min s'il y en a plusieurs
  tab_infographic_max_groupe <- tab_effectifs_h_groupe %>%
    filter(n == max(n)) %>%
    slice(1) %>%
    mutate(activite = "max")

  tab_infographic_min_groupe <- tab_effectifs_h_groupe %>%
    filter(n == min(n)) %>%
    slice(1) %>%
    mutate(activite = "min")

  tab_infographic_groupe <- bind_rows(tab_infographic_max_groupe, tab_infographic_min_groupe)

  #export
  return(list(plot = res_whale_etab$plot,
              data_adulte = tab_export,
              data_ped = tab_export_ped,
              tab_infographic_etab = tab_infographic_etab,
              tab_infographic_groupe = tab_infographic_groupe))
}
