#' Customer message log level
#' 
#' @param ... The message(s)
#' @param level The severity
#' 
#' @details 0 = everything, 1 = debug, 2=normal, 3=important
myMessage <- function(..., level = 2){
  
  compare_level <- getOption("googleAuthR.verbose")
  
  if(level >= compare_level){
    message(...)
  }

}



#' Idempotency
#'
#' A random code to ensure no repeats
#'
#' @return A random 15 digit hash
#'
#' @export
idempotency <- function(){
  paste(sample(c(LETTERS, letters, 0:9), 15, TRUE),collapse="")
}


#' Converts RFC3339 to as.Date
#' 
#' @keywords internal
RFC_convert <- function(RFC, drop_time=FALSE){
  
  if(drop_time){
    d <-   as.Date(strptime(as.character(RFC), 
                            tz="UTC", 
                            "%Y-%m-%dT%H:%M:%OSZ"))
  } else {
    d <- strptime(as.character(RFC), 
                  tz="UTC", 
                  "%Y-%m-%dT%H:%M:%OSZ")
  }
  
  return(d)
}

#' Is this a try error?
#' 
#' Utility to test errors
#' 
#' @param test_me an object created with try()
#' 
#' @return Boolean
#' 
#' @keywords internal
is.error <- function(test_me){
  inherits(test_me, "try-error")
}

#' Get the error message
#'
#' @param test_me an object that has failed is.error
#'
#' @return The error message
#'
#' @keywords internal
error.message <- function(test_me){
  if(is.error(test_me)) attr(test_me, "condition")$message
}


#' A helper function that tests whether an object is either NULL _or_
#' a list of NULLs
#'
#' @keywords internal
is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))

#' Recursively step down into list, removing all such objects
#'
#' @keywords internal
rmNullObs <- function(x) {
  x <- Filter(Negate(is.NullOb), x)
  lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
}

#' Camel case to dot.case
#'
#' @param character vector
#'
#' @return All camelCase becomes lowercase with dots e.g. camel.case
camelToDot <- function(camelCase){
  s <- gsub("([a-z])([A-Z])", "\\1.\\L\\2", camelCase, perl = TRUE)
  sub("^(.[a-z])", "\\L\\1", s, perl = TRUE) # make 1st char lower case
}

#' camelCase to Title Case
#'
#' @param character vector
#'
#' @return All camelCase becomes Title with spaces e.g. Camel Case
camelToTitle <- function(camelCase){
  s <- camelToDot(camelCase)
  s <- gsub("."," ",s, fixed=TRUE)

  stringr::str_to_title(s)
}

#' Pretty display names
#'
#' @param choice vecotr to get different names
#' @param displayNames names to replace choice
#'
#' @return choice named with pretty names
#'
getDisplayNames <- function(choice, displayNames){
  if(!is.null(displayNames)){
    overwrite <- displayNames[choice]
    overwrite[is.na(overwrite)] <- choice[is.na(overwrite)]
    names(choice) <- overwrite
  }

  choice

}

## modify the defaults if ... has been used to specify
default_overwrite <- function(..., overwrite){

  default_args <- list(...)

  if(inherits(overwrite, "list")){
    out <- modifyList(default_args, overwrite, keep.null = TRUE)
  } else {
    out <- default_args
  }

  out

}

pad_digits <- function(n, pad=2){
  gsub(" ","0", sprintf(paste0("%",pad,"d"), n))
}

diff_pages <- function(data_frame, column_name, suffix){

  last <- data_frame[,paste0(column_name,suffix[1])]
  before <- data_frame[,paste0(column_name,suffix[2])]

  last[is.na(last)] <- 0
  before[is.na(before)] <- 0

  out <- last - before

  out

}

diff_dataframe <- function(data_frame, col_names, suffix = c(".lastmonth",".monthbefore"), outsuffix=".diffmonth"){

  out <- as.data.frame(lapply(col_names, function(x) diff_pages(data_frame, x, suffix)))
  names(out) <- paste0(col_names, outsuffix)

  out
}



#' Add name of list entry of dataframe to dataframe colum
#'
listNameToDFCol <- function(named_list, colName = "listName"){
  lapply(names(named_list),
         function(x) {named_list[[x]][colName] <- x
         named_list[[x]]
         })
}

#' Make start and end month date range
#'
#' @param the_date A date to get previous month end and start
#'
#' @return list of $start and $end
#'
#' @import lubridate
#' @export
month_start_end <- function(the_date = Sys.Date()){

  if(is.null(the_date)) the_date <-  Sys.Date()

  the_data <- as.Date(the_date)

  start <- floor_date(the_date %m-% months(1), unit = "month")
  end <- ceiling_date(the_date %m-% months(1), unit = "month")

  ## a bug if start of the month?
  if(start == end){
    end <- ceiling_date((the_date + 1) %m-% months(1), unit = "month")
  }

  end <- end - 1

  list(start = start, end = end)

}
