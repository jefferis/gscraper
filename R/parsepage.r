#' Parse an http header
#' 
#' @param header Header text
#' @return Named character vector
#' @author jefferis
#' @export
parseurlheader<-function(header){
  if(regexpr("OK",header[1])<0) return(NULL)
  tt=header[grep(":",header)]
  tt=sub("\\r","",tt)
  names=sub("^([^:]+): .*","\\1",tt)
  values=sub("^[^:]+: (.*)","\\1",tt)
  l=list()
  l[names]=values
  l
}

#' Extract the links from text of a web page
#' 
#' @details The baseurl is normally just the original url (although a different
#' url is sometime explicitly specified in the html body).
#' @details absolute depends on getRelativeUrl from the \code{XML} package.
#' @param body Raw text of web page
#' @param linktype class of link to find (e.g. href,src)
#' @param regex Regular expression to filter links
#' @param fixed Whether regular expression is fixed
#' @param rooturl Base url for expansion of relative links
#' @param absolute Whether to convert relative urls to absolute
#' @return character verctor of urls
#' @author jefferis
#' @export
#' @seealso \code{\link{grep},\link{getHTMLLinks}}
#' @importFrom XML getRelativeURL
extract_links<-function(body,linktype="href",regex,fixed=FALSE,
    rooturl=attr(body,'url'),absolute=TRUE){
  t2=body[grep(linktype,body,fixed=fixed)]
  t3=unlist(strsplit(t2,"><")) # split lines with multiple html fields
  t4=t3[grep(linktype,t3,fixed=fixed)] # just keep the ones that still match linktype
  links=sub(paste(".*",linktype,"=\"([^\"]+).*",sep=""),"\\1",t4)
  
  if(!missing(regex)) links=links[grep(regex,links,fixed=fixed)]
  
  if(absolute && !is.null(rooturl)) {
    links=getRelativeURL(links,rooturl)
  }
  links
}
