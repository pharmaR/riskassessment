#####################################################################################################################
# file_upload_error_handling.r - Handles errors while uploading the CSV file
# 
# Author:
# Created:
#####################################################################################################################

file_upload_error_handling <- function(UploadFile) {
  if (file_ext(UploadFile)[1] != "csv") {
    showModal(modalDialog(h3("Please Upload only CSV File")))
    return("error")
  } else{
    file_to_read <- UploadFile
    if (is.null(file_to_read))
      return()
    if(UploadFile[1,"size"]==0){
      showModal(modalDialog(
        h3(
          "This appears to be an empty file. Please upload with valid file."
        )
      ))
      return("error")
    }
    pkgs <-
      read.csv(file_to_read$datapath,
               sep = ",",
               stringsAsFactors = FALSE)
    names(pkgs) <- tolower(names(pkgs))
    
    upload_format_csv<-read.csv("./Data/upload_format.csv")
    
    if (dim(pkgs)[1] == 0) {
      showModal(modalDialog(
        h3(
          "This appears to be an empty data Please upload with file with valid data."
        )
      ))
      return("error")
    } else if (colnames(pkgs)[1] != colnames(upload_format_csv)[1] &&
               colnames(pkgs)[2] != colnames(upload_format_csv)[2]) {
      showModal(modalDialog(
        h3(
          "Improper file structure. Please refer to the sample template and upload again."
        )
      ))
      return("error")
    }
  }
  return("no_error")
}
