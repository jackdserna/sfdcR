strQuery<- function(fields, object, filter = NULL) {
        if(!is.null(filter)) {
                return(paste("SELECT", fields, "FROM", object, filter))
        } else return(paste("SELECT", fields, "FROM", object))
}
