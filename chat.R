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

#' Initialize chat session
Init_chat_session <- function(global = NULL,
                              model = 'gpt-3.5-turbo-0301',
                              simplify = TRUE,
                              import_histroy = NULL,
                              export_history = FALSE,
                              temperature = 0.7,
                              top_p = 1,
                              n = 1,
                              max_tokens = 2048,
                              presence_penalty = 0,
                              frequency_penalty = 0){
  
  #check parameter
  
}