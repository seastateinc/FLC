# ---------------------------------------------------------------------------- #
# SCRIPT FOR GENERATING FLC REPORT 
# Author: Steven Martell
# ---------------------------------------------------------------------------- #

# FLC_Script.R
library(lubridate)
library(rmarkdown)
library(mailR)
library(rJava)

setwd("~/DropBox/SSI/FLC/reports/dmr")

input_file  <- "FLC_report1.Rmd"
output_file <- paste0("FLC_Week_",week(today()),"_",year(today()),".pdf")

if(!file.exists(output_file))
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

bodymsg <- "\n Please note changes to the Blue King Crab numbers to reflect the calendar year for crab fisheries.\n\nSteve\n\n"

email <- send.mail(from = sender,
                   to = recipients,
                   subject= paste("Sea State Reporting Service: FLC Report",output_file),
                   body = paste0("This report was generated on ",now(),".\n",bodymsg),
                   smtp = list(host.name = "smtp.gmail.com", port = 465, 
                      user.name = "martell.steve@gmail.com",            
                      passwd = "H@libut2014", ssl = TRUE),
                   authenticate = TRUE,
                   attach.files = output_file,
                   send = FALSE)
## Not run: email$send() # execute to send email

email$send()