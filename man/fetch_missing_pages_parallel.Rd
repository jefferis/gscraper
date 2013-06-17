\name{fetch_missing_pages_parallel}
\alias{fetch_missing_pages_parallel}
\title{Download missing pages in parallel to local cache directory}
\usage{
  fetch_missing_pages_parallel(urls, cacheloc,
    fetchpagefn = fetch_page, njobs = 10,
    jobdir = tempfile("fmppjobs"), jobstem = "fmppjob",
    RunJobs = TRUE, ...)
}
\arguments{
  \item{urls}{Character vector of URLs}

  \item{cacheloc}{Path to local cache}

  \item{fetchpagefn}{Function that actually fetches the
  pages}

  \item{njobs}{Number of download jobs to run in parallel}

  \item{jobdir}{Directory that will contain download job
  scripts}

  \item{jobstem}{Filename prefix for shell scripts}

  \item{RunJobs}{Whether jobs are actualy run}

  \item{...}{Additional parameters passed to fetchpagefn}
}
\value{
  character vector of paths to job files
}
\description{
  Download missing pages in parallel to local cache
  directory
}
\details{
  parallel downloads are based on asynchronous shell
  scripts
}
\author{
  jefferis
}
\seealso{
  Other fetchpage: \code{\link{fetch_page}}
}

