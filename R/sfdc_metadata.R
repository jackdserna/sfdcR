# Extract metadata from all Salesforce Objects
# Specific field types whill have all relevant information included in the data set. 
# Otherwise, all field types will include only general information.
sfdc_meta<- function(session, objects_metadata = NULL, field_type = "all") {
        if(missing(session)) stop("Login to Salesforce with sfdc_login()")
        if(missing(objects_metadata)) assign("objects", RForcecom::rforcecom.getObjectList(session))
        SFDC_arch<- data.frame()
        cols = c("parent_object",
                 "referenceTo",
                 "label",
                 "name",
                 "custom",
                 "type",
                 "calculated",
                 "permissionable",
                 "nillable",
                 "restrictedPicklist") # Standard metadata
        collect<- function(data, index, fields, ...){
                data[, fields[1]]<- index # Create column "parent_object" and value is equal to object name
                data<- data[, fields]
                data[]<- lapply(data, as.character)
                dplyr::bind_rows(data, SFDC_arch)
        }
        arch<- function(data, index, field_type){
                # field type may be set to formula for calculated fields and the calculations only
                # field type may be set equal to "reference" for only relational or common fields between objects,
                # field type may be set equal to "picklist" for only picklist fields and picklist values
                # field type defaults to all types, and will provide only 1 referenceTo object
                if(!is.null(colnames(data)) & any(grepl("referenceTo", colnames(data))==TRUE)){
                        switch(field_type,
                               "picklist" = assign("fields",
                                                   append(cols,
                                                          colnames(data)[grep("picklistValues.value",
                                                                              colnames(data))])),
                               "formula" = assign("fields",
                                                  append(cols,
                                                         colnames(data)[grep("calculatedFormula",
                                                                             colnames(data))])),
                               "reference" = assign("fields",
                                                    append(cols,
                                                           colnames(data)[grep("referenceTo",
                                                                               colnames(data))])),
                               "all" = assign("fields", cols))
                               # Filter data for specific type if applicable
                               switch(field_type,
                                      "picklist" = assign("data", data[, "type" == field_type]),
                                      "formula" = assign("data", data[, "calculated" == "true"]),
                                      "reference" = assign("data", data[, "type" == field_type]))
                               if(nrow(data)>0) collect(data, index, fields)
                }
        }
        SFDC_arch<- lapply(objects$name, FUN = function(x) {
                lapply(x, FUN = function(x){arch(data = RForcecom::rforcecom.getObjectDescription(session, x),
                                                 index = x,
                                                 field_type = field_type)})
        })
        data<- data.table::rbindlist(lapply(SFDC_arch, data.table::rbindlist), fill = TRUE)
        if(ncol(data) > 10) {
                data <- data.table::melt(data, id.vars = cols) %>%
                        dplyr::drop_na(value)
        }
        data
}
