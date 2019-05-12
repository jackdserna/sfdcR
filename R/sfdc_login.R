sfdc_login<- function(reset = NULL){
  if(suppressWarnings(nrow(key_list("sfdc"))) == 0 || isTRUE(reset)) {
    if(isTRUE(reset)){
      # Do stuff for new password set up
      suppressWarnings(key_delete("sfdc", username = key_list("sfdc")[1,2]))
    }
    ### Set up
    dlg_message(message = "SFDC set-up: R login via Salesforce API")
    username = dlg_input("Enter a Salesforce username", Sys.info()["user"])$res
    # Validate username
    isValidEmail <- function(x) {
      grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
    }
    if(isFALSE(isValidEmail(username))) stop("Enter a valid username")
    # Prompt for password information
    dlg_message(paste("You will be asked for your Salesforce password.",
                      "This information will be saved securely saved via the keyring package."))
    pw = dlg_input(message = "Enter a password associated with the sfdc username",
                     Sys.info()["user"])$res
    dlg_message(paste("You will be asked for your Salesforce API token.",
                      "Salesforce automatically sends a random generated token via email.",
                      "To find it, login to Salesforce in web browser.",
                      "Go to 'My Settings'.",
                      "Go to 'Personal', then 'Reset My Security Token' and reset.",
                      "Copy the API token, and be ready to paste.", sep = " \n"))
    token = dlg_input(message = "Enter the API token",
                      Sys.info()["user"])$res
    token = trimws(token)
    if(nchar(token) < 24 || nchar(token) > 25) stop("Be sure to copy and paste all of the token")
    password = paste0(pw, token)
    # Enter Instance URL
    dlg_message("You will be asked to provide the Salesfoce domain instance URL")
    instance<- dlg_input("Copy the home page URL (e.g.: https://na85.salesforce.com/ )",
                    Sys.info()["user"])$res
    if(isFALSE(RCurl::url.exists(instance))) stop("Check your Instance in the Company Profile")
    # Save
    suppressWarnings(key_set_with_value(service = "sfdc",
                       username = username,
                       password = password,
                       keyring = instance))
    suppressWarnings(key_set_with_value("instance",
                                        username = instance,
                                        password = ""))
  }
  # Login to Salesforce API
  session<- rforcecom.login(username = suppressWarnings(key_list("sfdc")[1,2]),
                            password = suppressWarnings(key_get("sfdc",
                                               username = key_list("sfdc")[1,2])),
                            loginURL = suppressWarnings(key_list("instance")[1,2]))
  return(session)
}
