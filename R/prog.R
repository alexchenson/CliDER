#' download climate data
#'
#' @param p number representing the percent progress
#' @param recurs determines whether or not the progress bar will print back onto the line
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' r <- download_climate('RTMA',date,4,out_dir = ".")
#' }
#'
prog <- function(p,recurs = FALSE){
  p <- floor(p)
  i <- 100-p
  comp <- strrep("#",p)
  inc <- strrep("~",i)
  if(recurs == TRUE){
    bar <- paste0("\r{{",comp,inc,"}} : ",p,"% Complete")
    cat(bar)
    utils::flush.console()
  }
  else{
    bar <- paste0("{{",comp,inc,"}} : ",p,"% Complete")
    print(bar)
  }
}

