# Function for exploratory querying
sfdc_query<- function(session, object = NULL, query = NULL) {
   # If session missing, stop
   if(missing(session)) stop("Login to Salesforce with sfdc_login()")
   # If object and query are NULL, explore objects and fields
   if(is.null(object) & is.null(query)) {
      objects = rforcecom.getObjectList(session)[, c("label", "name")]
      objects[] = lapply(objects, as.character)
      object = objects[
         objects$label %in% dlg_list(objects$label, Sys.info()["user"])$res,
         "name"]
      fields = rforcecom.getObjectDescription(session, object)
      fields[] = lapply(fields, as.character)
      query = fields[
         fields$label %in% dlg_list(
            fields$label, multiple = TRUE,
            title = "Select the field(s) to pull",
            Sys.info()["user"])$res,
         "name"]
      # Get field types to allow for smart querying
      type <- fields[fields$name %in% query, "type"]
      # get references index to initiate smart query
      ref<- grep("ref", type)
      # If any types are reference class, use smart query
      if(length(ref)>0) {# Do something
      for(i in type) {

   }
   # Loop through all reference class fields
   # Get object prefix
   # Get field names from relationship object
   rforcecom.getObjectDescription(session, object)
   # Select field name to query from relationship

   # save new name for query
   }
   query = paste("SELECT", paste(query, collapse = ", "), "FROM", object)
   }
   if(!is.null(object) & !is.null(query)){
      if_else(object == objects[objects$name %in% object, "name"], {
      # If true, object exists. Run. No need to check query.
         next
      }, {
      # If false, provide object
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
      # Necessary? Or get relationship?
      recordTypeIds<- sfdc_basicQuery(
         session,
         object = "RecordType",
         query = "SELECT Id, Name FROM RecordType")
   }
   while(filter != "no") { # while filtering, loop filter entry form
      filterBy<- fields[fields$label %in%
      dlg_list(arrange(fields, label)$label,
      title = filterTitle,
      Sys.info()["user"])$res, "name"]
      type <- fields[fields$name == filterBy, "type"]
      if(filterBy == "RecordTypeId")
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
   return(data)
}
