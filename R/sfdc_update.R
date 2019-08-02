# Function for updating
sfdc_update<- function(session, object, data, Mode = 'Parallel', batchSize = 250) {
        # If missing args, stop
        if(missing(session)) stop("Login to Salesforce with sfdc_login()")
        if(missing(object) | missing(data)) stop("Provide an object API Name and data")
        #  Depends on Progress Bar function
        progressValue<- function(x) {
                x = x$numberRecordsProcessed
                sum(as.numeric(x))
        }
        rows<- function(x) {
                as.numeric(nrow(x))
        }
        progressBar<- function(x, min = 0, max, value) {
                pb<- txtProgressBar(min, max, style = 3)
                for(i in 1 : max){
                        setTxtProgressBar(pb, value)
                        if(value / max == 1){
                                close(pb)
                        }
                }
        }
        #
        job<- RForcecom::rforcecom.createBulkJob(
                session,
                operation = 'update',
                object = object,
                concurrencyMode = Mode)
        info<- RForcecom::rforcecom.createBulkBatch(
                session,
                jobId = job$id,
                data = data,
                multiBatch = TRUE,
                batchSize = batchSize)
        status<- data.table::rbindlist(lapply(info, FUN = function(x) {
                RForcecom::rforcecom.checkBatchStatus(
                        session,
                        jobId = x$jobId,
                        batchId = x$id)
                }),
                fill = TRUE)
        # Continuously run all batches
        v = 0
        while(all(status$state == "Completed") != TRUE) {
                status<- data.table::rbindlist(lapply(info, FUN = function(x) {
                        RForcecom::rforcecom.checkBatchStatus(
                                session,
                                jobId = x$jobId,
                                batchId = x$id)
                }),
                fill = TRUE);
                progressBar(
                        status,
                        max = rows(data),
                        value = v + progressValue(status));
                if(any(status$state == "Failed") == TRUE) {
                        # Collect error message
                        errorMessage = status$stateMessage
                        break
                }
        }
        details<- suppressWarnings(data.table::rbindlist(lapply(split(
                status, status$id),
                FUN = function(x){
                        suppressWarnings(RForcecom::rforcecom.getBatchDetails(
                                session,
                                jobId = x$jobId,
                                batchId = x$id))
                })))
        closeJob<- RForcecom::rforcecom.closeBulkJob(
                session,
                jobId = job$id)
        # If the error was due to lock failure, then update remaining
        #return(details)
        Failed <- data[!(data$Id %in% details$Id), ]
        if (rows(Failed) > 0) {
                # Consider Serial Mode
                stop(paste("Consider using Mode = 'Serial'.", errorMessage))
        }
}
