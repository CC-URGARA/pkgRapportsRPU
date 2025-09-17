#couleurs utilisées dans la librairie
col_glob = "#125486"#"#93CDDD"
col_etab = "#e91b1d"
col_secblue = "#0070C0"

#Variables globales utilisées dans des évaluations non standards
globalVariables(unique(c(
  # fct_diagramme_charge:
  "age_inf15", "age_inf2", "Heure", "Hour", "n_avg",
  # fct_make_bardodge:
  "age_inf15", "age_inf2", "col_etab", "col_glob", "groupe", "groupe_lab", "p",
  # fct_make_barfill:
  "age_inf15", "age_inf2", "groupe", "groupe_lab", "ORIENTATION",
  # fct_make_barplot:
  "col_etab", "col_glob", "effectif_bas", "etab_actif", "n_noNA", "NOM_ETAB", "percent", "y",
  # fct_pyramide_age:
  "age_class", "COD_FIN", "lab", "NOM_ETAB", "p", "SEXE",
  # fct_duree_passage:
  "duree_passage_min", "Maximum", "y_heure",
  # fct_infographic_resume:
  "activite", "size", "x", "Thesaurus_Fedoru", "diagnostic_code",
  "type_urgence_libelle", "val_indicateur",
  # fct_tab_diag:
  "DP_lab", "DP_recod", "n_p",
  # fct_make_barplot_age:
  "age", "age_geq75", "age_geq75_adult",
  # fct_calc_indic_evol:
  "annee", "CCMU_1_2", "DP_type", "ENTREE", "group", "hospit", "MODE_SORTIE", "n_jours_an", "n_RPU", "n_RPU_quotidien", "nuit", "SORTIE", "TRANSPORT", "weekend",
  # fct_infographic_evol:
  "Indicateur", "lab_clean", "val_indicateur_n2",
  # fct_make_tab_evol_RPU:
  "evol", "evol_type", "Indicateur", "signe", "suffix_lab", "val_indicateur_n1", "val_indicateur_n2",
  # fct_nb_presents:
  "H_ENTREE", "H_SORTIE", "nb_jours_present"
)))
