\name{extract_links}
\alias{extract_links}
\title{Extract the links from text of a web page}
\usage{
  extract_links(body, linktype = "href", regex,
    fixed = FALSE, rooturl = attr(body, "url"),
    absolute = TRUE)
}
\arguments{
  \item{body}{Raw text of web page}

  \item{linktype}{class of link to find (e.g. href,src)}

  \item{regex}{Regular expression to filter links}

  \item{fixed}{Whether regular expression is fixed}

  \item{rooturl}{Base url for expansion of relative links}

  \item{absolute}{Whether to convert relative urls to
  absolute}
}
\value{
  character verctor of urls
}
\description{
  Extract the links from text of a web page
}
\details{
  The baseurl is normally just the original url (although a
  different url is sometime explicitly specified in the
  html body).
}
\author{
  jefferis
}
\seealso{
  \code{\link{grep},\link{getHTMLLinks}}
}

