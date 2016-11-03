# ---------------------------------------------------------------------------- #
# SCRIPT FOR GENERATING FLC REPORT 
# Author: Steven Martell
# ---------------------------------------------------------------------------- #

# FLC_Script.R
library(lubridate)
library(rmarkdown)
library(mailR)

setwd("~/Dropbox/SSI/FLC/reports/dmr")

input_file  <- "FLC_report1.Rmd"
output_file <- paste0("FLC_Week_",week(today()),"_",year(today()),".pdf")
rmarkdown::render(input = input_file, output_file = output_file)


# Email out the report.
sender <- "steve@seastateinc.com"  # Replace with a valid address

# Replace with one or more valid addresses
recipients <- c("steve@seastateinc.com",
                "karl@seastateinc.com",
                "katherine@seastateinc.com",
                scan("FLC_emailList.txt",what=character(),comment.char="#")
                ) 

recipients <- c("steve@seastateinc.com")



email <- send.mail(from = sender,
                   to = recipients,
                   subject= paste("Sea State Reporting Service: FLC Report",output_file),
                   body = paste0("This report was generated on ",now(),".\n"),
                   smtp = list(host.name = "smtp.gmail.com", port = 465, 
                      user.name = "martell.steve@gmail.com",            
                      passwd = "H@libut2014", ssl = TRUE),
                   authenticate = TRUE,
                   attach.files = output_file,
                   send = FALSE)
## Not run: email$send() # execute to send email

email$send()