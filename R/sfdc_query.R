# Function for exploratory querying
sfdc_query<- function(session, object = NULL, query = NULL) {
        # If session missing, stop
        if(missing(session)) stop("Login to Salesforce with sfdc_login()")
        # If object and query are NULL, explore objects and fields
        if(is.null(object) & is.null(query)) {
                objects = RForcecom::rforcecom.getObjectList(session)[, c("label", "name")]
                objects[] = lapply(objects, as.character)
                object = objects[
                        objects$label %in% svDialogs::dlg_list(
                                dplyr::arrange(objects, label)$label, 
                                Sys.info()["user"])$res,
                        "name"]
                fields = RForcecom::rforcecom.getObjectDescription(session, object)
                fields[] = lapply(fields, as.character)
                query = fields[
                        fields$label %in% svDialogs::dlg_list(
                                dplyr::arrange(fields, label)$label, 
                                multiple = TRUE,
                                title = "Select the field(s) to pull",
                                Sys.info()["user"])$res,
                        "name"]
                # Get references index to initiate smart query
                ref <- subset(fields[fields$name %in% query, 
                                     c("type", "name", "referenceTo", "relationshipName")], 
                              type == "reference")
                # If any types are reference class, use smart query
                if(length(ref) > 0) {
                        prompt = svDialogs::dlg_message(type = "yesno",
                                               message = paste(
                                                       "There are reference-indexed fields in your query.",
                                                       "They will return an ID.", 
                                                       "Do you want to return the indexed value, instead?"),
                                               Sys.info()["user"])$res
                        if(prompt == "yes") {
                                i = 0
                                new_query = c()
                                for(i in nrow(ref)) {
                                        ind = ref[i, 3]
                                        assign(ind, RForcecom::rforcecom.getObjectDescription(session, objectName = ind))
                                        ind = get(ind)[, c("name", "label")]
                                        ind[] = lapply(ind, as.character)
                                        ind_q = ind[
                                                ind$label %in% svDialogs::dlg_list(
                                                        dplyr::arrange(ind, label)$label, 
                                                        title = "Select the indexed value to return",
                                                        Sys.info()["user"])$res,
                                                "name"]
                                        new_query = append(new_query, paste0(ref[i,4], ".", ind_q))
                                }
                                query = query[!query %in% ref$name]; query = append(query, new_query)
                        } 
                }
                query = strQuery(fields = paste(query, collapse = ", "), object)
        }
        if(!is.null(object) & !is.null(query)){
                if_else(object == objects[objects$name %in% object, "name"], {
                        # If true, object exists. Run. No need to check query.
                        next
                }, {    # If false, provide object
                        stop(paste(
                                "Pass a Salesforce object API name,",
                                "or use sfdc_query() with no object and query argument."))
                })
        }
        # 3 Filter, if desired?
        filter<- svDialogs::dlg_message(
                type = "yesno",
                message = "Do you want to filter?",
                Sys.info()["user"])$res
        # If yes, define basics
        if(filter == "yes") {
                filterList<- "WHERE"
                filterTitle = paste("Filter Logic:", filterList)
        }
        while(filter != "no") { # while filtering, loop filter entry form
                filterBy<- fields[fields$label %in%
                                          svDialogs::dlg_list(dplyr::arrange(fields, label)$label,
                                                   title = filterTitle,
                                                   Sys.info()["user"])$res, 
                                  "name"]
                type <- fields[fields$name == filterBy, "type"]
                switch(type,
                       "picklist" = picklistValue(fields, filterBy, type, filterTitle),
                       "multipicklist"  = picklistValue(fields, filterBy, type, filterTitle),
                       "date" = dateValue(),
                       "datetime" = dateValue(),
                       "boolean" = boolean(),
                       "string" = string(),
                       stop(paste("Filtering on", type, "field types are not supported")))
                # Depending on field type, use specific filter signs and values
                filter.sign<- dlg_list(
                        filter.signs,
                        title = paste(filterTitle, filterBy, "..."),
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
        data
}
