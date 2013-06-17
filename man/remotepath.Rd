\name{remotepath}
\alias{remotepath}
\title{Find paths on server of resources defined by urls}
\usage{
  remotepath(url)
}
\arguments{
  \item{url}{Character vector of one or more urls}
}
\value{
  character vector of paths
}
\description{
  Does not attempt to parse queries etc
}
\examples{
remotepath("http://cran.r-project.org/web/packages/scrapeR/scrapeR.pdf")
}
\author{
  jefferis
}
\seealso{
  \code{\link{parseURI}}
}

