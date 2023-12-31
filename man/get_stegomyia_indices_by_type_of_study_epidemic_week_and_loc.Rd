% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/get_stegomyia_indices_by_type_of_study_epidemic_week_and_loc.R
\name{get_stegomyia_indices_by_type_of_study_epidemic_week_and_loc}
\alias{get_stegomyia_indices_by_type_of_study_epidemic_week_and_loc}
\title{Get stegomyia indices by type of study, date and geo}
\usage{
get_stegomyia_indices_by_type_of_study_epidemic_week_and_loc(
  df,
  st = "Verificacion",
  ew = "2",
  var
)
}
\arguments{
\item{df}{the dataframe with information.}

\item{st}{The type of study selected. By default, it is set to
"Verificacion".}

\item{ew}{The date used to calculate the stegomyia indices. By default,
it is set to "2".}

\item{var}{The geographic variable used to calculate the stegomyia indices.}
}
\value{
The dataframe with stegomyia indices of the selection of type of study and
geographic variable.
}
\description{
The Stegomyia indices are calculated for each sampling. Once the data have
been loaded with function "load_rwa_data" and changed type data of function
"clean_raw_data", select study type, date and geographic variable using the
following
 formulas:
      - Container Index(CI): (number of infected containers/total number of
      containers) * 100.
      - House Index (HI): (number of infected houses /total number of
      houses) * 100.
      - Breteau Index (BI): (number of positive containers/number of houses
      explored) * 100.
}
