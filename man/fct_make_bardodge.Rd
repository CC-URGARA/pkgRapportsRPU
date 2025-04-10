% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/barPlot.R
\name{fct_make_bardodge}
\alias{fct_make_bardodge}
\title{barplot bicolor}
\usage{
fct_make_bardodge(
  base_etab,
  base_groupe,
  var_interet,
  label_var_interet,
  na.rm = FALSE,
  titre = "",
  make_table = FALSE,
  rm.empty.levels = FALSE,
  pedia,
  in.freq.order = TRUE
)
}
\arguments{
\item{base_etab}{RPU de l'établissement}

\item{base_groupe}{RPU du groupe de l'établissement}

\item{var_interet}{Character : Variable a représenter}

\item{label_var_interet}{Nom de la variable à représenter}

\item{na.rm}{Booléen : Retirer les données manquantes}

\item{titre}{Titre du graphique}

\item{make_table}{Booléen : Le tableau de données doit-il être généré}

\item{rm.empty.levels}{Booléen : Retirer les niveaux vides d'un facteur}

\item{pedia}{Booléen : le rapport est-il pédiatrique}

\item{in.freq.order}{Booléen : Ordonner le graphique par fréquence décroissante (TRUE) ou selon les niveaux du facteurs (FALSE)}
}
\value{
un graphique et une table
}
\description{
barplot bicolor
}
