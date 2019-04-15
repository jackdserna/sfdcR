# Function for exploratory querying
sfdc_query<- function(session, object = NULL, query = NULL) {
      # If session missing, stop
      if(missing(session)) stop("Login to Salesforce with sfdc_login()")
      # If object and query are NULL, explore objects and fields
      if(is.null(object) & is.null(query)) {
            objects = rforcecom.getObjectList(session)[,c("label", "name")]
            objects[] = lapply(objects, as.character)
            object = objects[objects$label %in%
                                   dlg_list(objects$label,
                                            Sys.info()["user"])$res,
                             "name"]
            fields = rforcecom.getObjectDescription(session, object)
            fields[] = lapply(fields, as.character)
            query = fields[(fields$label %in%
                                   dlg_list(arrange(fields, label)$label,
                                            multiple = TRUE,
                                            title = "Select the field(s) to pull",
                                            Sys.info()["user"])$res),
                            "name"]
            query = paste("SELECT", paste(fields, collapse = ", "), "FROM", object)
      }
      if(!is.null(object) & !is.null(query)){
            if_else(object == objects[objects$name %in% object, "name"], {
                  # If true, object exists. Run. No need to check query.
                  next
            }, {
                  # If false, provide object
                  stop(paste("Pass a Salesforce object API name,",
                             "or use sfdc_query() with no object and query argument."))
            })
      }
      # 3 Filter, if desired?
      filter<- dlg_message("yesno",
                           message = "Do you want to filter?",
                           Sys.info()["user"])$res
      if(filter == "yes") {
            filter.signs<- c('=', '!=', '<', '<=', '>', '>=', 'LIKE',
                             "IN", 'NOT IN', 'INCLUDES', 'EXCLUDES')
            dateLiterals
            dateInput
            dateRange
            dateCustom
            filterList<- "WHERE"
            filterTitle = paste("Filter Logic:", filterList)
            recordTypeIds<- sfdc_basicQuery(session,
                                            object = object,
                                            query = query)
      }
      while(filter != "no") {
            filterBy<- fields[(fields$label %in% dlg_list(arrange(fields, label)$label,
                                                          title = 'Filter Logic: Pick a field you want to filter by. IF ...',
                                                          Sys.info()["user"])$res), "name"]
            filter.sign<- dlg_list(filter.signs, title = paste(filterList, filterBy, " ..."),
                                   Sys.info()["user"])$res
            # if field type is picklist or lookupID, then present value to select
            # Check to see if reference types need this as well
            # Otherwise, free text input
            filter.value<- ""
            filterList<- paste(filterList, filterBy, filter.sign)
            # Make the filter list work as a while loop, which ends when the user says they're done making their filter
      }
      # Run query
      data = sfdc_basicQuery(session, object, query)
      return(data)
}
