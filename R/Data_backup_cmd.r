#' backup do for Master files
#'
#' not longer used 
#'
#'
#' @author ILO / bescond  
#' @keywords ILO, microdataset, preprocessing
#' @examples
#' ## Not run:
#'
#' ## End(**Not run**)
#' @export
#' @rdname Data_get_workflow
Data_backup_cmd <- function(wd){

test <- ilo:::path$data
master <- Data_get_workflow() %>% distinct(repo, project) %>% 
				mutate(init = paste0(test, repo, '/', project, '/do'), 
					   backup = paste0(wd, 'iloData/inst/doc'))
				
				

	for (i in 1:nrow(master)){
		try(file.copy(from = master$init[i], to = master$backup[i],copy.mode = TRUE, copy.date = TRUE, overwrite  = TRUE, recursive = TRUE), silent = TRUE)
	}

	file.copy(	from = paste0(ilo:::path$data, '_Admin/0_WorkFlow.xlsx'), 
				to = paste0(wd, 'iloData/inst/doc/0_WorkFlow.xlsx'),copy.mode = TRUE, copy.date = TRUE, overwrite  = TRUE)
	


}
