chat_func <- function(prompt_content,
                      role = 'user',
                      model = '%s',
                      temperature = %f,
                      top_p = %f,
                      n = %d,
                      max_tokens = %d,
                      presence_penalty = %f,
                      frequency_penalty = %f,
                      export_history = FALSE){
  
  #load required library
  require(httr)
  
  #check role
  if(!(role %%in%% c('system','user'))){
    stop('prompt role can only be system or user!')
  }
  
  #whether export history
  if(export_history){
    return(export_chat_history(.Object = `%s`))
  }
  
  #add prompt
  temp_session <- add_chat_history(.Object = `%s`,role = role,prompt_content = prompt_content)
  
  #filter history while processing http POST
  indi <- TRUE
  forcement <- FALSE
  
  while(indi){
    #filter chat history
    temp_session <- filter_chat_history(.Object = temp_session,force = forcement)
    chat_history <- temp_session@history
    
    #create http body
    http_body <- list(`model` = model,
                      `temperature` = temperature,
                      `top_p` = top_p,
                      `n` = n,
                      `max_tokens` = max_tokens,
                      `presence_penalty` = presence_penalty,
                      `frequency_penalty` = frequency_penalty,
                      `messages` = chat_history)
    
    #request
    if(exists(x = 'OpenAI_organization')){
      request_POST <- httr::POST(
        url = URL_chat,
        httr::add_headers(`Content-Type` = 'application/json'),
        httr::add_headers(`Authorization` = paste('Bearer',OpenAI_API_key,sep = ' ')),
        httr::add_headers(`OpenAI-Organization` = OpenAI_organization),
        body = http_body,
        encode = 'json'
      )
    }else{
      request_POST <- httr::POST(
        url = URL_chat,
        httr::add_headers(`Content-Type` = 'application/json'),
        httr::add_headers(`Authorization` = paste('Bearer',OpenAI_API_key,sep = ' ')),
        body = http_body,
        encode = 'json'
      )
    }
    
    requset_content <- httr::content(x = request_POST)
    
    #length exceeded?
    if(!(is.null(requset_content$choices))){
      indi <- FALSE
      forcement <- FALSE
    }else if(is.null(requset_content$choices) & requset_content$error$code == 'context_length_exceeded'){
      indi <- TRUE
      forcement <- TRUE
    }else{
      stop('requset_content wrong!')
    }
  }
  
  #process content
  for(i in 1:n){
    temp_session <- add_chat_history(.Object = temp_session,
                                     role = requset_content$choices[[i]]$message$role,
                                     prompt_content = gsub(pattern = '^\\n\\n',replacement = '',x = requset_content$choices[[i]]$message$content,fixed = FALSE))
    cat(paste0('ChatGPT:\\n',gsub(pattern = '^\\n\\n',replacement = '',x = requset_content$choices[[i]]$message$content,fixed = FALSE),'\\n\\n'))
  }
  
  #export to global env
  `%s` <<- temp_session
  
}