#' Lollipop durées de passages
#'
#' @param base_groupe RPU du groupe
#' @param titre_tab Titre du tableau
#' @param excl_orient_non_pec Booléen : Les patients réorientés/fugues/sortie sans ou contre avis médical doivent-ils être exclus ?
#'
#' @returns une liste
#' @export
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom kableExtra kbl kable_classic_2 kable_styling
#' @importFrom ggpubr theme_pubclean
#' @importFrom stats median quantile
#'
fct_duree_passage <- function(base_groupe, titre_tab, excl_orient_non_pec){
  # exclusion des orientation correspondant à des pat non pris en charge
  if(excl_orient_non_pec){
    base_groupe = base_groupe %>%
      filter(!ORIENTATION %in% c("FUGUE", "REO", "PSA", "SCAM"))
  }

  med_groupe = median(base_groupe$duree_passage_min, na.rm = T)/60

  tab_plot <- base_groupe %>%
    group_by(NOM_ETAB, etab_actif, .drop = F) %>%
    summarise(y = median(duree_passage_min, na.rm = T),
              n = sum(!is.na(duree_passage_min))) %>%
    ungroup() %>%
    mutate(NOM_ETAB = reorder(NOM_ETAB, y),
           y_heure = y/60,
           effectif_bas = n < 30)

  tab_plot_limit <- tab_plot %>%
    filter(y_heure <= 24)

  # plot
  plot <- ggplot(tab_plot_limit, aes(x=NOM_ETAB, y=y_heure)) +
    geom_segment(aes(x=NOM_ETAB, xend=NOM_ETAB, y=med_groupe, yend=y_heure, alpha = effectif_bas)) +
    geom_hline(aes(yintercept = med_groupe, color = "M\u00e9diane du groupe"), linewidth = 2) +
    geom_point(aes(color=etab_actif, alpha = effectif_bas), size=4, show.legend = F) +
    scale_color_manual(name = "", values = c("0" = col_glob, "1" = col_etab, "M\u00e9diane du groupe" = "#0070C0"),
                       breaks = c("M\u00e9diane du groupe")) +
    scale_y_continuous(name = "Dur\u00e9e m\u00e9diane de passage (h)", breaks = seq(0, 500, by = 1),
                       limits = c(0, 24)) +
    scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 1)) +
    coord_cartesian(ylim = c(0, min(c(24, max(tab_plot_limit$y_heure) + 1)))) +
    guides(alpha = "none") +
    theme_pubclean() +
    theme(
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      legend.key = element_blank(),
    ) +
    xlab("")


  #Calcule des chiffres de la table
  tab_etab <- base_groupe %>%
    filter(etab_actif %in% "1") %>%
    summarise(Groupe = "Service",
              N = fct_f_big(n()),
              "M\u00e9diane" = fct_min_as_hour(round(quantile(duree_passage_min, na.rm = T, probs = 0.5))),
              "Q25%" = fct_min_as_hour(round(quantile(duree_passage_min, na.rm = T, probs = 0.25))),
              "Q75%" = fct_min_as_hour(round(quantile(duree_passage_min, na.rm = T, probs = 0.75))),
              Minimum = fct_min_as_hour(round(min(duree_passage_min, na.rm = T))),
              Maximum = round(max(duree_passage_min, na.rm = T))) %>%
    mutate(Maximum = if_else(Maximum > 129600, "> 89 jours",
                             if_else(Maximum > 1440,
                                     paste(round(Maximum/1440, 1), "jours"),
                                     fct_min_as_hour(Maximum))))

  tab_group <- base_groupe %>%
    summarise(Groupe = "Groupe",
              N = fct_f_big(n()),
              "M\u00e9diane" = fct_min_as_hour(round(quantile(duree_passage_min, na.rm = T, probs = 0.5))),
              "Q25%" = fct_min_as_hour(round(quantile(duree_passage_min, na.rm = T, probs = 0.25))),
              "Q75%" = fct_min_as_hour(round(quantile(duree_passage_min, na.rm = T, probs = 0.75))),
              Minimum = fct_min_as_hour(round(min(duree_passage_min, na.rm = T))),
              Maximum = round(max(duree_passage_min, na.rm = T))) %>%
    mutate(Maximum = if_else(Maximum > 129600, "> 89 jours",
                             if_else(Maximum > 1440,
                                     paste(round(Maximum/1440, 1), "jours"),
                                     fct_min_as_hour(Maximum))))

  tab_glob <- bind_rows(tab_etab, tab_group) %>%
    kbl(caption = titre_tab, col.names = c("", "N", "M\u00e9diane", "Q25%", "Q75%", "Minimum", "Maximum"),
        align = "c", format = "latex") %>%
    kable_classic_2() %>%
    kable_styling(latex_options = c("HOLD_position"))

  #export
  return(list(
    plot = plot,
    data = tab_glob,
    med_etab = tab_plot$y_heure[tab_plot$etab_actif == "1"]))
}
