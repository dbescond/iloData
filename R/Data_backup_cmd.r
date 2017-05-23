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
Data_backup_cmd <- function(){

wd <-  ilo:::path$data

master <- Data_get_workflow() %>% distinct(repo, project) %>% 
				mutate(init = paste0(wd, repo, '/', project, '/do'), 
					   backup = paste0(wd, '_Admin/CMD/iloData/inst/doc'))
				
				

	for (i in 1:nrow(master)){
		try(file.copy(from = master$init[i], to = master$backup[i],copy.mode = TRUE, copy.date = TRUE, recursive = TRUE), silent = TRUE)
	}

try(file.copy(from = paste0(ilo:::path$data, '_Admin/0_WorkFlow.xlsx'), to = paste0(ilo:::path$data, '_Admin/CMD/iloData/inst/doc/0_WorkFlow.xlsx'), copy.mode = TRUE, copy.date = TRUE), silent = TRUE)


}
