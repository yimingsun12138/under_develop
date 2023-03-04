#' generate chat session id.
generate_session_id <- function(){
  
  #load required package
  library(uuid)
  
  #generate session id
  indi <- TRUE
  while(indi){
    session_id <- paste('session',UUIDgenerate(),sep = ':')
    temp <- ls(envir = globalenv())
    if(!(session_id %in% temp)){
      indi <- FALSE
    }
  }
  
  #return
  return(session_id)
}