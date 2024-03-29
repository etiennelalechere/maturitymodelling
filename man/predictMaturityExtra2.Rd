% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predictMaturityExtra2.R,
%   R/predictMaturityExtra2_sauv.R
\name{predictMaturityExtra2}
\alias{predictMaturityExtra2}
\title{Predict maturity from raw metric maps without any extrapolation}
\usage{
predictMaturityExtra(model,modelName,dir)

predictMaturityExtra(model,modelName,dir)
}
\arguments{
\item{maturityDB.selec}{matrix or data frame, metrics database to define ranges}

\item{model}{predictive model}

\item{metricsDir}{character, directory name to load metric maps}

\item{outputDir}{character, directory name to export maturity maps}

\item{outputModelName}{character used to define output names}
}
\description{
Filter metrics to predict NA in case of extrapolation

Filter metrics to predict NA in case of extrapolation
}
\examples{
# not run: predictMaturityExtra2(maturityDB.selec,model,metricsDir,outputDir,outputModelName)
# not run: predictMaturityExtra2(maturityDB.selec,model,metricsDir,outputDir,outputModelName)
}
