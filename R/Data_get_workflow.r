#' helper to work with bulk download processing  workflow


#' @author ILO / bescond  
#' @keywords ILO, microdataset, processing
#' @examples
#' ## Not run:

#' ## End(**Not run**)
#' @export
#' @rdname Data_get_workflow
Data_get_workflow <- function(){

init <- getwd()
setwd(ilo:::path$data)

options(warn = -1)
readxl:::read_excel('./_Admin/0_WorkFlow.xlsx', sheet = 'file')
					
					
					
}

