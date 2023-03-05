#' Define chat_session class.
setClass(Class = 'chat_session',slots = c(session_id = 'character',history = 'list',archive = 'list'))

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
            .Object@archive <- .Object@history
            return(.Object)
          })

#' Filter chat_session history if necessary
setGeneric(name = 'filter_chat_history',def = function(.Object){standardGeneric(f = 'filter_chat_history')})

setMethod(f = 'filter_chat_history',
          signature = signature(.Object = 'chat_session'),
          definition = function(.Object){
            #filter NULL content
            if(.Object@history[[1]]$role == 'system' & is.null(.Object@history[[1]]$content)){
              .Object@history <- tail(x = .Object@history,n = -1)
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
            if(!(role %in% c('system','user','assistant'))){
              stop('prompt role can only be system, user or assistant!')
            }
            if(class(prompt_content) != 'character'){
              stop('prompt content can only be a string!')
            }
            #add prompt
            .Object@history <- append(.Object@history,list(list(`role` = role,`content` = prompt_content)))
            .Object@archive <- append(.Object@archive,tail(x = .Object@history,n = 1))
            return(.Object)
          })

#' export chat_session archive
setGeneric(name = 'export_chat_history',def = function(.Object){standardGeneric(f = 'export_chat_history')})

setMethod(f = 'export_chat_history',
          signature = signature(.Object = 'chat_session'),
          definition = function(.Object){
            chat_history <- base::lapply(X = .Object@archive,FUN = function(x){
              prompt_content <- paste(x$role,x$content,sep = ' : \n')
              return(prompt_content)
            })
            chat_history <- base::do.call(what = paste,args = list(chat_history,collapse = '\n\n'))
            cat(chat_history)
          })