\name{fetch_page}
\alias{fetch_page}
\title{Cache a single remote page if not already present}
\usage{
  fetch_page(url, cacheloc, localpathfn = remotepath,
    cookie = NA, header = FALSE, Verbose = TRUE,
    maxtime = NA, curlopts = NA, DryRun = FALSE,
    ReturnCMD = FALSE)
}
\arguments{
  \item{url}{Remote URL}

  \item{cacheloc}{Root of local cache}

  \item{localpathfn}{function that converts remote url to
  (relative) path}

  \item{cookie}{Optional cookie file (e.g. from Mozilla
  Firefox)}

  \item{header}{Whether to download header rather than
  content of URL}

  \item{Verbose}{Whether to be verbose}

  \item{maxtime}{Maximum time in seconds to wait for a
  response}

  \item{curlopts}{Additional options for curl}

  \item{DryRun}{Say what we would do rather than doing it}

  \item{ReturnCMD}{Return resultant shell command rather
  than running it.}
}
\value{
  NULL if already exists / on failure, otherwise path to
  local file
}
\description{
  Cache a single remote page if not already present
}
\examples{
\dontrun{
fetch_page("https://github.com/jefferis/gscraper/blob/master/README.md",cacheloc=mycache<-tempfile())
}
}
\author{
  jefferis
}
\seealso{
  \code{\link{remotepath}}

  Other fetchpage:
  \code{\link{fetch_missing_pages_parallel}}
}

