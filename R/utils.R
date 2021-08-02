library(Bergm) # main 
library(ergm) # datasets

# FETCH THE DATASETS
get_datasets <- function(loadenv=NULL) {
  df_ergm_package <- data(package="ergm")$results
  df_bergm_package <- data(package="Bergm")$results
  
  df_package <- rbind(df_bergm_package, df_ergm_package)
  
  net_datasets <- c()
  
  for (i in 1:nrow(df_package)) {
    s <- as.character(df_package[i,3])
    s_ <- str_match_all(s, "\\((.*?)\\)")[[1]][2]
    if (is.na(s_)) s_ <- s
    net_datasets <- c(net_datasets, s_)
  }
  
  net_names <- lapply(df_package[,3], function(x) str_split(x, " ")[[1]][1])
  net_names <- unlist(net_names)
  
  ds_dict <- list()
  
  for (k in 1:length(net_names)) {
    data(list=c(net_datasets[k]))
    if (network::is.network(get(net_names[k]))) {
      ds_dict[net_names[k]] <- net_datasets[k] 
    }
  }
  
  if(is.environment(loadenv)) {
    for (e in ls(loadenv)) { 
      if(network::is.network(get(e, envir=loadenv))) { 
        ds_dict[e] <- e
      }
    }
  }
  
  ds_dict
}

## PARSE THE CONFIG DATA
prepare_config <- function(config_data, args=config_data$default_vals, replace_args=list()) {
  has_args <- stringr::str_detect(config_data$template, "(\\{NUMBER\\}|\\{BOOL\\}|\\{VERTEX\\}|\\{STRING\\})")
  my_args <- args
  arg_names <- names(my_args) # names of the arguments
  str_out <- config_data$template
  arg_type_list <- NULL
  
  if (has_args) {
    arg_type_list <- stringr::str_match_all(config_data$template, "(\\{NUMBER\\}|\\{BOOL\\}|\\{VERTEX\\}|\\{STRING\\})")[[1]][,1]
    if (length(my_args)==length(arg_type_list)) {
      my_args_transformed <- lapply(1:length(my_args), function(i) {
        switch(arg_type_list[i], 
               "{NUMBER}"=as.numeric(ifelse(any(names(replace_args)==arg_names[i]),replace_args[arg_names[i]],my_args[i])), 
               "{BOOL}"=as.logical(ifelse(any(names(replace_args)==arg_names[i]),replace_args[arg_names[i]],my_args[i])), 
               "{VERTEX}"=as.character(paste0('"',ifelse(any(names(replace_args)==arg_names[i]),replace_args[arg_names[i]],my_args[i]),'"')),
               "{STRING}"=as.character(ifelse(any(names(replace_args)==arg_names[i]),replace_args[arg_names[i]],my_args[i])))
      })
      
      for (a in 1:length(my_args_transformed)) {
        str_out <- stringr::str_replace(str_out, "(\\{NUMBER\\}|\\{BOOL\\}|\\{VERTEX\\}|\\{STRING\\})", as.character(my_args_transformed[a]))
      }
    } else {
      warning(paste("The number of the arguments doesn't match. Expecting ", paste(arg_type_list, collapse=" ")),".")
    }
  }
  
  str_out <- stringr::str_replace_all(str_out, '\"NULL\"', "NULL")
  
  controls <- arg_type_list
  names(controls) <- arg_names
  list(str_out=ifelse(str_out=="",config_data$name,str_out), controls=controls)
}

extract_categories <- function(n.net) {
  new_cats <- names(n.net$val[[1]])
  new_cats <- new_cats[-c(which(new_cats=='vertex.names'),which(new_cats=='na'))]
  new_cats
}