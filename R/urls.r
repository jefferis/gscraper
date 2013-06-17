#' Find paths on server of resources defined by urls
#' 
#' Does not attempt to parse queries etc
#' @param url Character vector of one or more urls
#' @return character vector of paths
#' @author jefferis
#' @export
#' @seealso \code{\link{parseURI}}
#' @examples
#' remotepath("http://cran.r-project.org/web/packages/scrapeR/scrapeR.pdf")
remotepath<-function(url){
  # simple fn to return the path of a file on the remote server
  # http://server.com/some/path/file -> some/path/file
  sub("^[a-z]+://([^/]+)/+(.*)","\\2",url)
}
