# Function for updating
sfdc_update<- function(session, object, data) {
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
   job<- rforcecom.createBulkJob(
      session,
      operation = 'update',
      object = object,
      concurrencyMode = 'Parallel')
   info<- rforcecom.createBulkBatch(
      session,
      jobId = job$id,
      data = data,
      multiBatch = TRUE,
      batchSize = 250)
   status<- rbindlist(lapply(info, FUN = function(x) {
      rforcecom.checkBatchStatus(
         session,
         jobId = x$jobId,
         batchId = x$id)
      }),
      fill = TRUE)
   # Continuously run all batches
   v = 0
   while(all(status$state == "Completed") != TRUE) {
      status<- rbindlist(lapply(info, FUN = function(x) {
         rforcecom.checkBatchStatus(session,
            jobId = x$jobId,
            batchId = x$id)
         }),
         fill = TRUE)
      progressBar(
         status,
         max = rows(data),
         value = v+progressValue(status))
      if(any(status$state == "Failed") == TRUE) {
         # Try running in Serial?
         errorMessage = status$stateMessage
         break
      }
   }
   Sys.sleep(0.01)
   details<- rbindlist(lapply(split(
      status, status$id),
      FUN = function(x){
         rforcecom.getBatchDetails(
            session,
            jobId = x$jobId,
            batchId = x$id)
         }))
   closeJob<- rforcecom.closeBulkJob(
      session,
      jobId = job$id)
   # If the error was due to lock failure, then update remaining
   #return(details)
   Failed <- data[!(data$Id %in% details$Id), ]
   if (rows(Failed) > 0) {
   # if there are batch update errors, run in serial
   # mode
   }
}
