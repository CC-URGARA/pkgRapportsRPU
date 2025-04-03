#' Pyramide des ages
#'
#' @param base base contenant les variables suivantes (SEXE, age_class, base, n). Age_classe contient les valeurs suivantes c("00_02", "03_10", "11_17", "18_24", "25_39", "40_54", "55_64", "65_79", "80_P")
#' @param titre_tab Titre du tableau à retourner
#' @param bassin_calc Booléen indiquant si le bassin sanitaire était calculable
#'
#' @returns un plot et une table
#' @export
#'
#' @import tidyr
#' @import dplyr
#' @import ggplot2
#' @importFrom kableExtra kbl kable_classic_2 kable_styling add_header_above collapse_rows
#' @importFrom forcats fct_recode
#' @importFrom ggpubr theme_pubclean get_legend
#' @importFrom gridExtra arrangeGrob
fct_pyramide_age <- function(base, titre_tab, bassin_calc){
  tab_plot <- base %>%
    group_by(base, SEXE, .drop = F) %>%
    mutate(n = as.numeric(n),
           p = n/sum(n),
           age_class = factor(age_class,
                              levels = c("00_02", "03_10", "11_17", "18_24", "25_39", "40_54", "55_64", "65_79", "80_P")),
           age_class = fct_recode(age_class,
                                  "00-02" = "00_02",
                                  "03-10" = "03_10",
                                  "11-17" = "11_17",
                                  "18-24" = "18_24",
                                  "25-39" = "25_39",
                                  "40-54" = "40_54",
                                  "55-64" = "55_64",
                                  "65-79" = "65_79",
                                  " > 80" = "80_P")) %>% ungroup() %>%
    complete(COD_FIN, NOM_ETAB, SEXE, age_class, base, fill = list("n" = 0, "p" = 0))

  #Si le bassin est non fiable, les données sont supprimées
  if(!bassin_calc){
    tab_plot = tab_plot %>%
      mutate(
        n = if_else(base == "Bassin", NA, n),
        p = if_else(base == "Bassin", NA, p)
      )
  }

  max_y <- max(tab_plot$p, na.rm = T)

  ## Plotting
  gg <- ggplot(tab_plot, aes(x=age_class))

  gg.male <- gg +
    geom_col(data=subset(tab_plot,SEXE == 'M' & base == "RPU"),
             aes(y = p), color = "blue", linewidth = 1, alpha = 0) +
    geom_col(data=subset(tab_plot,SEXE == 'M' & base == "Bassin"),
             aes(y = p), fill = "blue", alpha = 0.5) +
    scale_y_continuous('', labels = scales::percent, limits = c(0, max_y + 0.05)) +
    scale_x_discrete('') +
    coord_flip() +
    labs(title = "Hommes") +
    theme_pubclean() +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_text(hjust = -1))

  gg.female <-  gg +
    geom_col(data=subset(tab_plot,SEXE == 'F' & base == "RPU"),
             aes(y = p),  color = "red", alpha = 0, linewidth = 1) +
    geom_col(data=subset(tab_plot,SEXE == 'F' & base == "Bassin"),
             aes(y = p), fill = "red", alpha = 0.5) +
    scale_y_continuous('', labels = scales::percent, trans = 'reverse', limits = c(max_y + 0.05, 0)) +
    scale_x_discrete('', labels = NULL) +
    coord_flip() +
    labs(title = "Femmes") +
    theme_pubclean() +
    theme(axis.ticks.y = element_blank())

  #création d'un plot vide contenant uniquement la légende voulue
  lab_legend = c("Service : Femmes", "Bassin : Femmes", "Service : Hommes",  "Bassin : Hommes")
  plot_legend <- ggplot(tibble(), aes(x = 0, y = 0)) +
    # geom_col(aes(fill = c("F_RPU", "F_Bassin", "M_RPU", "M_Bassin"), color = c("F_RPU", "F_Bassin", "M_RPU", "M_Bassin"),
    #              linewidth = c("F_RPU", "F_Bassin", "M_RPU", "M_Bassin"), alpha = c("F_RPU", "F_Bassin", "M_RPU", "M_Bassin"))) +
    geom_col(aes(fill = c("F_RPU", "F_Bassin", "M_RPU", "M_Bassin"), color = c("F_RPU", "F_Bassin", "M_RPU", "M_Bassin"),
                 linewidth = c("F_RPU", "F_Bassin", "M_RPU", "M_Bassin"), alpha = c("F_RPU", "F_Bassin", "M_RPU", "M_Bassin"))) +
    scale_fill_manual(name = "",
                      values = c("F_RPU" = "#FF0000", "F_Bassin" = "#FFFFFF", "M_RPU" = "#0000FF", "M_Bassin" = "#FFFFFF"),
                      labels = lab_legend) +
    scale_color_manual(name = "",
                       values = c("F_RPU" = "#FF0000", "F_Bassin" = "#FF0000", "M_RPU" = "#0000FF", "M_Bassin" = "#0000FF"),
                       labels = lab_legend) +
    scale_discrete_manual(name = "", aesthetics = "linewidth",
                          values = c("F_RPU" = 0, "F_Bassin" = 1, "M_RPU" = 0, "M_Bassin" = 1),
                          labels = lab_legend) +
    scale_alpha_manual(name = "",
                       values = c("F_RPU" = 0.5, "F_Bassin" = 0, "M_RPU" = 0.5, "M_Bassin" = 0),
                       labels = lab_legend) +
    theme_pubclean()

  legend <- get_legend(plot_legend, position = "bottom")

  ## Plutting it together
  lay = rbind(c(1, 2),
              c(3, 3))
  gg.final <- arrangeGrob(gg.female,
                          gg.male,
                          legend,
                          layout_matrix = lay,
                          widths=c(0.45,0.55),
                          heights = c(0.92, 0.08),
                          ncol=2)

  #Calcule des chiffres de la table
  tab <- tab_plot %>%
    ungroup() %>%
    select(base, age_class, n, SEXE) %>%
    mutate(SEXE = fct_recode(SEXE,
                             "Hommes" = "M",
                             "Femmes" = "F"),
           base = fct_recode(base,
                             "Patients du service" = "RPU",
                             "Population du bassin sanitaire" = "Bassin"),
           n = as.numeric(n)) %>%#trimws(format(, big.mark = " "
    group_by(base, SEXE) %>%
    mutate(p = round(n/sum(n)*100, 1),
           lab = trimws(paste0(fct_f_big(n), " (", p, "%)"))) %>%
    ungroup() %>%
    select(base, SEXE, age_class, lab) %>%
    pivot_wider(names_from = age_class, values_from = lab) %>%
    arrange(desc(base), SEXE)

  vec_names_col <- names(tab)
  vec_names_col[1:2] <- ""
  vec_names_col[vec_names_col == " > 80"] <- "> 80"

  #Nettoyage des "NA (NA%)
  tab = tab %>%
    mutate_if(is.character,
              function(col){
                if_else(col %in% "NA (NA%)", "X", col)
              })

  tab_kb <- tab %>%
    kbl(caption = titre_tab,
        align = "c", format = "latex",
        col.names = vec_names_col) %>%
    kable_classic_2() %>%
    kable_styling(latex_options = c("HOLD_position", "scale_down")) %>%
    add_header_above(c("", "", "Tranche d\'\u00e2ge" = 9)) %>%
    collapse_rows(1:2, row_group_label_position = "stack")

  ##Export
  list(plot = gg.final,
       data = tab_kb)
}
