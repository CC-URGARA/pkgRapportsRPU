#' tableau des diagnostics principaux
#'
#' @param base_etab RPU de l'établissement
#' @param base_groupe RPU du groupe de l'établissement
#' @param excl_orient_non_pec Booléen : Les patients réorientés/fugues/sortie sans ou contre avis médical doivent-ils être exclus ?
#'
#' @returns Une liste
#' @export
#'
#' @import dplyr
#' @importFrom stringr str_detect
fct_tab_diag <- function(base_etab, base_groupe, excl_orient_non_pec = FALSE){
  if(excl_orient_non_pec){
    base_etab = base_etab %>%
      filter(!ORIENTATION %in% c("FUGUE", "REO", "PSA", "SCAM"))

    base_groupe = base_groupe %>%
      filter(!ORIENTATION %in% c("FUGUE", "REO", "PSA", "SCAM"))
  }

  #tab etab
  tab_effectifs_DP_etab <- base_etab %>%
    filter(DP_recod != "NC") %>%
    group_by(DP_recod, .drop = TRUE) %>%
    count() %>%
    ungroup() %>%
    mutate(
      n_p = paste0(trimws(format(n, big.mark = " ")), " (",
                   round(n/sum(n)*100, 1), "%)")) %>%
    select(-n, "V1" = DP_recod, "V2" = n_p) %>%
    add_row(V1 = "Codage du diagnostic principal", V2 =  "", .before = 1)

  tab_effectifs_DP_etab <- bind_rows(tab_effectifs_DP_etab,
                                     tibble("V1" = "NC",
                                            "V2" = fct_f_big(sum(base_etab$DP_recod %in% "NC"))))

  top_diag_etab <- base_etab %>%
    filter(DP_recod %in% "DP hors traumatologie") %>%
    group_by(DP_lab, .drop = F) %>%
    count() %>%
    arrange(desc(n)) %>%
    ungroup() %>%
    mutate(
      n_p = paste0(trimws(format(n, big.mark = " ")), " (",
                   round(n/sum(n)*100, 1), "%)")) %>%
    top_n(5, n) %>%
    slice_head(n = 5) %>%#Ajouté pour gérer les cas ou il y a des égalités et donc plus de 5 lignes dans top5
    select(-n, "V1" = DP_lab, "V2" = n_p) %>%
    add_row(V1 = "Diagnostics principaux hors trauma les plus cod\u00e9s", V2 =  "", .before = 1)

  tab_etab = bind_rows(tab_effectifs_DP_etab, top_diag_etab)

  Z53_top5_etab = any(str_detect(top_diag_etab$V1, "^Z53"))

  #tab du groupe
  tab_effectifs_DP_groupe <- base_groupe %>%
    filter(DP_recod != "NC") %>%
    group_by(DP_recod, .drop = TRUE) %>%
    count() %>%
    ungroup() %>%
    mutate(
      n_p = paste0(trimws(format(n, big.mark = " ")), " (",
                   round(n/sum(n)*100, 1), "%)")) %>%
    select(-n, "V1" = DP_recod, "V2" = n_p) %>%
    add_row(V1 = "Codage du diagnostic principal", V2 =  "", .before = 1)

  tab_effectifs_DP_groupe <- bind_rows(tab_effectifs_DP_groupe,
                                       tibble("V1" = "NC",
                                              "V2" = fct_f_big(sum(base_groupe$DP_recod %in% "NC"))))

  top_diag_groupe <- base_groupe %>%
    filter(DP_recod %in% "DP hors traumatologie") %>%
    group_by(DP_lab, .drop = F) %>%
    count() %>%
    arrange(desc(n)) %>%
    ungroup() %>%
    mutate(
      n_p = paste0(trimws(format(n, big.mark = " ")), " (",
                   round(n/sum(n)*100, 1), "%)")) %>%
    top_n(5, n) %>%
    slice_head(n = 5) %>%#Ajouté pour gérer les cas ou il y a des égalités et donc plus de 5 lignes dans top5
    select(-n, "V1" = DP_lab, "V2" = n_p) %>%
    add_row(V1 = "Diagnostics principaux hors trauma les plus cod\u00e9s", V2 =  "", .before = 1)

  tab_groupe = bind_rows(tab_effectifs_DP_groupe, top_diag_groupe)
  #export
  return(list(tab_etab = tab_etab,
              tab_groupe = tab_groupe,
              Z53_top5_etab = Z53_top5_etab))
}
