# Function for basic querying
sfdc_basicQuery<- function(session, object, query) {
      # If missing args, stop
      if(missing(session)) stop("Login to Salesforce with sfdc_login()")
      if(missing(object) | missing(query)) stop("Provide an object and query")
      # Run
      job<- rforcecom.createBulkJob(session, operation = 'query', object = object)
      info<- rforcecom.submitBulkQuery(session, jobId = job$id, query = query)
      repeat{
            status<- suppressWarnings(rforcecom.checkBatchStatus(session,
                                                                 jobId = info$jobId,
                                                                 batchId = info$id))
            if (status[["state"]]=="Completed") {
                  break
            }
            if (status[["state"]]=="Failed") {
                  errorMessage = status$stateMessage
                  break
            }
      }
      Sys.sleep(0.1)
      details<- suppressWarnings(rforcecom.getBatchDetails(session,
                                          jobId = info$jobId,
                                          batchId = info$id))
      result<- suppressWarnings(rforcecom.getBulkQueryResult(session,
                                            jobId = info$jobId,
                                            info$id,
                                            resultId = details$result))
      closeJob<- rforcecom.closeBulkJob(session, jobId = job$id)
      if(exists("errorMessage")) stop(errorMessage)
      return(result)
}
