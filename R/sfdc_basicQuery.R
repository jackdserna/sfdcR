# Function for basic querying
sfdc_basicQuery<- function(session, object, query) {
        # If missing args, stop
        if(missing(session)) stop("Login to Salesforce with sfdc_login()")
        if(missing(object) | missing(query)) stop("Provide an object and query")
        # Run
        job<- RForcecom::rforcecom.createBulkJob(
                session,
                operation = 'query',
                object = object)
        info<- RForcecom::rforcecom.submitBulkQuery(
                session,
                jobId = job$id,
                query = query)
        repeat{
                status<- suppressWarnings(RForcecom::rforcecom.checkBatchStatus(
                        session,
                        jobId = info$jobId,
                        batchId = info$id))
                if (status[["state"]] == "Completed") {
                        break
                }
                if (status[["state"]] == "Failed") {
                        errorMessage = status$stateMessage
                        break
                }
        }
        details<- suppressWarnings(RForcecom::rforcecom.getBatchDetails(
                session,
                jobId = info$jobId,
                batchId = info$id))
        result<- suppressWarnings(RForcecom::rforcecom.getBulkQueryResult(
                session,
                jobId = info$jobId,
                info$id,
                resultId = details$result))
        closeJob<- RForcecom::rforcecom.closeBulkJob(
                session,
                jobId = job$id)
        if(exists("errorMessage")) stop(errorMessage)
        result
}
