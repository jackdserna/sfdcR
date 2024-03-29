sfdc_login<- function(reset = NULL){
  if(nrow(keyring::key_list(keyring = "sfdc")) == 0 || isTRUE(reset)) {
    if(isTRUE(reset)){
      # Delete old key for new password set up
      keyring::key_delete(
        keyring = "sfdc",
        username = keyring::key_list(keyring = "sfdc")[1,2],
        service = keyring::key_list(keyring = "sfdc")[1,1])
      keyring::keyring_delete(keyring = "sfdc")
    }
    ### Set up
    svDialogs::dlg_message(
      message = "SFDC set-up: R login via Salesforce API")
    username = svDialogs::dlg_input(
      "Enter a Salesforce username",
      Sys.info()["user"])$res
    # Validate username
    isValidEmail <- function(x) {
      grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>",
            as.character(x), ignore.case = TRUE)
    }
    if(isFALSE(isValidEmail(username))) stop("Enter a valid username")
    # Prompt for password information
    svDialogs::dlg_message(paste(
      "You will be asked for your Salesforce password.",
      "This information will be saved securely",
      "via the keyring package."))
    pw = svDialogs::dlg_input(
      message = "Enter a password associated with the sfdc username",
      Sys.info()["user"])$res
    svDialogs::dlg_message(paste(
      "You will be asked for your Salesforce API token.",
      "Salesforce automatically sends a random generated token via email.",
      "To find it, login to Salesforce in web browser.",
      "Go to 'My Settings'.",
      "Go to 'Personal', then 'Reset My Security Token' and reset.",
      "Copy the API token, and be ready to paste.", sep = " "))
    token = svDialogs::dlg_input(
      message = "Enter the API token",
      Sys.info()["user"])$res
    token = trimws(token)
    # API tokens are 24-25 characters in length
    if(nchar(token) < 24 || nchar(token) > 25) {
      stop("Be sure to copy and paste all of the token")
    }
    password = paste0(pw, token)
    # Enter Instance URL
    svDialogs::dlg_message(
      "You will be asked to provide the Salesfoce domain instance URL")
    instance<- svDialogs::dlg_input(
      "Copy the home page URL (e.g.: https://na85.salesforce.com/ )",
      Sys.info()["user"])$res
    if(!grepl("https://", instance)) {
      stop("Include https:// in your Company Profile Instance")
    }
    # Save
    svDialogs::dlg_message(paste(
      "A keyring will be created to encrypt your user-password.",
      "Please enter a supplementary password for the keyring."))
    keyring::keyring_create(keyring = "sfdc")
    keyring::key_set_with_value(
      service = instance,
      username = username,
      password = password,
      keyring = "sfdc")
    rm(list = ls())
  }
  # Login to Salesforce API
  session<- RForcecom::rforcecom.login(
    username = keyring::key_list(keyring = "sfdc")[1,2],
    password = keyring::key_get(
      keyring = "sfdc", username = keyring::key_list(
        keyring = "sfdc")[1,2], service = keyring::key_list(
          keyring = "sfdc")[1,1]),
    loginURL = keyring::key_list(keyring = "sfdc")[1,1])
  session
}
