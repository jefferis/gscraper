#' Cache a single remote page if not already present
#' 
#' @param url Remote URL
#' @param cacheloc Root of local cache
#' @param localpathfn function that converts remote url to (relative) path
#' @param cookie Optional cookie file (e.g. from Mozilla Firefox)
#' @param header Whether to download header rather than content of URL
#' @param Verbose Whether to be verbose
#' @param maxtime Maximum time in seconds to wait for a response
#' @param curlopts Additional options for curl
#' @param DryRun Say what we would do rather than doing it
#' @param ReturnCMD Return resultant shell command rather than running it.
#' @return NULL if already exists / on failure, otherwise path to local file
#' @author jefferis
#' @export
#' @seealso \code{\link{remotepath}}
#' @family fetchpage
#' @examples
#' \dontrun{
#' fetch_page("https://github.com/jefferis/gscraper/blob/master/README.md",cacheloc=mycache<-tempfile())
#' }
fetch_page<-function(url, cacheloc, localpathfn=remotepath, cookie=NA, header=FALSE, 
    Verbose=TRUE, maxtime=NA,curlopts=NA,DryRun=FALSE,ReturnCMD=FALSE){
  
  cachedpath=file.path(cacheloc,localpathfn(url))
  if(header) 
    cachedpath=paste(cachedpath,sep=".","header")
  cacheddir=dirname(cachedpath)
  if(!file.exists(cacheddir)) dir.create(cacheddir,recursive=TRUE)
  
  # else 
  if(header) cmd="curl --head"
  else cmd="curl"
  
  if(file.exists(cachedpath)){
    if(ReturnCMD) return(NULL) # no download required, so return empty
    if(Verbose) cat("Using cached file\n")
  } else {
    if(Verbose) cat("Fetching web page",ifelse(header," header",""),sep="","\n")
    if(!is.na(cookie)) cmd=paste(cmd,"-b",cookie)
    cmd=paste(cmd,"-o",shQuote(cachedpath))
    if(!is.na(maxtime)) cmd=paste(cmd,"--max-time",maxtime)
    if(!is.na(curlopts)) cmd=paste(cmd,curlopts)
    cmd=paste(cmd,shQuote(url))
    if(Verbose||DryRun) cat(cmd,sep="","\n")
    if(ReturnCMD) return(cmd)
    if(!DryRun) {
      rval=system(cmd)
      if(rval>0) {
        warning("download failed for: ",url)
        return(NULL)
      }
    }
  }
  if(file.exists(cachedpath)) {
    return(cachedpath)
  }
  else return(NULL)
}

#' Download missing pages in parallel to local cache directory
#' 
#' @details parallel downloads are based on asynchronous shell scripts
#' @param urls Character vector of URLs 
#' @param cacheloc Path to local cache
#' @param fetchpagefn Function that actually fetches the pages
#' @param njobs Number of download jobs to run in parallel
#' @param jobdir Directory that will contain download job scripts
#' @param jobstem Filename prefix for shell scripts
#' @param RunJobs Whether jobs are actualy run
#' @param ... Additional parameters passed to fetchpagefn
#' @return character vector of paths to job files
#' @author jefferis
#' @export
#' @family fetchpage
fetch_missing_pages_parallel<-function(urls,cacheloc,fetchpagefn=fetch_page,
    njobs=10,jobdir=tempfile('fmppjobs'),jobstem="fmppjob",RunJobs=TRUE,...){
  
  # This function takes a list of urls and downloads the missing ones using 
  # njobs parallel scripts calleing curl 
  # The curl commands are built by FCFetchPages and should be identical to those
  # run in the standard fashion
  
  # make list of cmds and trim it down to unique non-NULL commands 
  cmds=sapply(urls,fetchpagefn,cacheloc,Verbose=F,ReturnCMD=TRUE,...)
  cmds=unique(unlist(cmds))
  if(is.null(cmds)) {
    message("No missing pages!")
    return(NULL)
  }
  
  njobs=min(njobs,length(cmds))
  jobs=character(njobs)
  if(!file.exists(jobdir)) dir.create(jobdir,recursive=TRUE)
  for(i in seq(njobs)){
    jobs[i]=file.path(jobdir,paste(jobstem,sep="",i,".sh"))
    cat(paste(cmds[seq(from=i,by=njobs,to=length(cmds))],collapse="\n"),file=jobs[i])
    if(RunJobs) system(paste("sh",shQuote(jobs[i])),wait=FALSE)
  }
  jobs
}
