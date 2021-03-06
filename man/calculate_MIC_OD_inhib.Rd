% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_MIC_OD_inhib.R
\name{calculate_MIC_OD_inhib}
\alias{calculate_MIC_OD_inhib}
\title{Convert OD into inhibition percentage}
\usage{
calculate_MIC_OD_inhib(processed_OD_data_list, negative_control_col = 12,
  positive_control_col = 11, highest_conc_col = 1, lowest_conc_col = 10,
  number_of_lines = 8, species)
}
\arguments{
\item{processed_OD_data_list}{List of data.frame. The first column of each
data.frame being the denomination of what is in the line and the following
columns being OD values read by the plate reader. The output of extract_MIC_OD_data
is already in the correct format.}

\item{negative_control_col}{Number of the column that contains the negative control.
Defaults to 12}

\item{positive_control_col}{Number of the column that contains the positive control.
Defaults to 11}

\item{lowest_conc_col}{Number of the column that contains the lowest concentration
of tested antibiotic. Defaults to 10}

\item{number_of_lines}{Number of lines used by plate. Defaults to 8}

\item{higest_conc_col}{Number of the column that contains the highest concentration
of tested antibiotic. Defaults to 1}
}
\description{
Calculate inhibition percentage in each well in order to determine the MIC for each row
}
\examples{
calculate_MIC_OD_inhib()
}
\keyword{MIC,}
\keyword{OD,}
\keyword{inhibition}
\keyword{microdilution,}
