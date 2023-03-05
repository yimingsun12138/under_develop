#' Define chat_session class.
setClass(Class = 'chat_session',slots = c(session_id = 'character',history = 'list'))

#' Initialize chat_session class.
setMethod(f = 'initialize',
          signature = signature(.Object = 'chat_session'),
          definition = function(.Object,session_id = NULL,global = NULL){
            
            #check parameter
            if(!(is.null(global) | class(global) == 'character')){
              stop('system global parameter must be NULL or a string!')
            }
            .Object@session_id <- session_id
            .Object@history <- list(list(`role` = 'system',`content` = global))
            return(.Object)
          })

#' Filter chat_session history if necessary
setGeneric(name = 'filter_chat_history',def = function(.Object){standardGeneric(f = 'filter_chat_history')})

setMethod(f = 'filter_chat_history',
          signature = signature(.Object = 'chat_session'),
          definition = function(.Object){
            #filter NULL content
            if(.Object@history[[1]]$role == 'system' & is.null(.Object@history[[1]]$content)){
              .Object@history <- tail(.Object@history,n = -1)
            }
            #return
            return(.Object)
          })

#' add chat_session history
setGeneric(name = 'add_chat_history',def = function(.Object,role,prompt_content){standardGeneric(f = 'add_chat_history')})

setMethod(f = 'add_chat_history',
          signature = signature(.Object = 'chat_session'),
          definition = function(.Object,role,prompt_content){
            #check parameter
            if(!(role %in% c('system','user'))){
              stop('prompt role can only be system or user!')
            }
            .Object@history <- append(.Object@history,list(list(`role` = role,`content` = prompt_content)))
            return(.Object)
          })

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