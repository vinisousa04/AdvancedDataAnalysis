% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/boxplot_by_categories.R
\name{boxplot_by_categories}
\alias{boxplot_by_categories}
\title{boxplot_by_categories}
\usage{
boxplot_by_categories(df, quantitative, categorical, id = NULL)
}
\arguments{
\item{df}{A data frame.}

\item{quantitative}{The quoted name of the column with the quantitative
variable.}

\item{categorical}{The quoted name of the column with the categorical
variable, i.e. the variables that divides the data in groups.}

\item{id}{The quoted name name of the column with the individual variables,
i.e. the observations. It is set as default to NULL for the cases where there
is no column identifying the indioviduals in the data set.}
}
\value{
Boxplot of the quantitative variable for each group in the
categorical variable. And also idintify the outlier, when they exist
is the data.
}
\description{
This is a wrapper for a ggplot2 boxplot for a variable with different groups,
and also adds the labels for outliers, if any.
}
\examples{
boxplot_by_categories(df=data, quantitative="pressure", categorical="blood_type",id="patient_name")

}
