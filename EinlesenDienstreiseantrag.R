# Skript zum automatisierten Einlesen von Dienstreiseanträgen ++++++++++++++
# Author: Kai Budde
# Created: 2021/02/17
# Last changed: 2021/04/21
# Version: 0.1.1

# User-defined parameters --------------------------------------------------
input_dir <- "Dienstreiseantraege"

# Define name of output directory (going to be a subdirectory of input_dir)
output_text <- "Textausgaben"
output_image <- "Bildschnipsel"
output_analysis <- "Analyseergebnisse"

# Load packages ------------------------------------------------------------
list.of.packages <- c("tesseract", "pdftools", "magick")

new.packages <- list.of.packages[
  !(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)){
  install.packages(new.packages)
}
require("tesseract")
require("pdftools")
require("magick")

if(is.na(match("deu", tesseract_info()$available))){
  tesseract_download("deu")
}
  
deu <- tesseract("deu")

# Read pdfs and extract information ----------------------------------------

# Get pdf file names
pdf_files <- list.files(path = paste(input_dir, "/", sep = ""))
pdf_files <- pdf_files[grepl(pattern = "\\.pdf", x = pdf_files)]

old_wd <- getwd()
setwd(input_dir)

number_of_pdfs <- length(pdf_files)

# Initialize data frame
df_results <- data.frame(
  "DienstreiseID" = rep(NA, number_of_pdfs),
  "Reisestart" = rep(NA, number_of_pdfs),
  "Reiseziel" = rep(NA, number_of_pdfs),
  "Rueckkehrdatum" = rep(NA, number_of_pdfs),
  "Bahn" = rep(NA, number_of_pdfs),
  "Bus" = rep(NA, number_of_pdfs),
  "Schiff" = rep(NA, number_of_pdfs),
  "DienstKFZ" = rep(NA, number_of_pdfs),
  "Bahncard" = rep(NA, number_of_pdfs),
  "Flugzeug" = rep(NA, number_of_pdfs),
  "PrivatKFZ_persönlicheG" = rep(NA, number_of_pdfs),
  "MitfahrtBei" = rep(NA, number_of_pdfs),
  "PrivatKFZ_triftigeG" = rep(NA, number_of_pdfs),
  "Sonstige" = rep(NA, number_of_pdfs)
  )

# Go through every file and try to parse it
for(i in 1:number_of_pdfs){
  
  # Convert pdf (first page) to png
  current_pdf <- pdf_files[i]
  current_pdf_as_png <- pdftools::pdf_convert(pdf = current_pdf,
                                              format = "png",
                                              pages = c(1),
                                              dpi = 300)
  
  text_from_png <- tesseract::ocr(image = current_pdf_as_png, engine = deu)
  
  # Crop image and only save information about destination and means of
  # transport of business trip
  full_png <- magick::image_read(path = current_pdf_as_png)
  full_png_height <- as.numeric(magick::image_info(full_png)["height"])
  full_png_width <- as.numeric(magick::image_info(full_png)["width"])
  
  start_pos_destination <- as.integer(0.23*full_png_height)
  height_destination <- as.integer(full_png_height/20)
  
  start_pos_means_of_transport <- as.integer(0.37*full_png_height)
  height_means_of_transport <- as.integer(full_png_height/12)
  
  destination_png <- image_crop(image = full_png,
                                geometry = paste(full_png_width,"x",
                                                 height_destination, "+0+",
                                                 start_pos_destination,
                                                 sep=""))
  means_of_transport_png <- image_crop(
    image = full_png, geometry = paste(full_png_width,"x",
                                       height_means_of_transport, "+0+",
                                       start_pos_means_of_transport, sep=""))
  
  
  # Delete png file
  if(file.remove(current_pdf_as_png)){
    print(paste("File ", current_pdf_as_png, " successfully removed.", sep=""))
  }
    
  # Remove personal information (everything until "Reiseziel")
  text_from_png <- gsub(pattern = ".+(Reiseziel.+)",
                        replacement = "\\1", x = text_from_png,
                        ignore.case = TRUE)
  
  if(grepl(pattern = "Name, Vorname", x = text_from_png,
           ignore.case = TRUE)){
    print("Der obere Teil des pdf wurde nicht gelöscht.")
  }
  
  # Keep only those lines that are relevant
  text_from_png_short <- unlist(strsplit(x = text_from_png, split = "\n"))
  start_line <- grep(pattern = "Reiseziel",
                     x = text_from_png_short, ignore.case = TRUE)
  keep_lines <- start_line:(start_line+2)
  start_line <- grep(pattern = "Beförderungsmittel",
                     x = text_from_png_short, ignore.case = TRUE)[1]
  keep_lines <- c(keep_lines, start_line:(start_line+4))
  text_from_png_short <- text_from_png_short[keep_lines]
  
  
  # Fill data frame
  reisestart <- paste(unlist(strsplit(
    x = text_from_png_short[2], split = " "))[-1], collapse = " ")
  
  reiseziel <- gsub(pattern = ".*nach: (.+) vom:.*",
                    replacement = "\\1", x = text_from_png_short[3],
                    ignore.case = TRUE)
  
  rueckkehrdatum <- gsub(pattern = ".+ (.+$)",
                         replacement = "\\1", x = text_from_png_short[3],
                         ignore.case = TRUE)
  
  bahn <- gsub(pattern = ".+ (.+) Bahn.*",
               replacement = "\\1", x = text_from_png_short[4],
               ignore.case = TRUE)
  
  bus <- gsub(pattern = ".+ (.+) Bus.*",
               replacement = "\\1", x = text_from_png_short[4],
               ignore.case = TRUE)
  
  schiff <- gsub(pattern = ".+Bus (.*) .*",
              replacement = "\\1", x = text_from_png_short[4],
              ignore.case = TRUE)
  
  dienstkfz <- gsub(pattern = ".+Bus .* (.*$)",
                 replacement = "\\1", x = text_from_png_short[4],
                 ignore.case = TRUE)
  
  bahncard <- gsub(pattern = "(^.*) Bahncard.*",
                    replacement = "\\1", x = text_from_png_short[5],
                    ignore.case = TRUE)
  
  flugzeug <- gsub(pattern = "^.* (.*) Flugzeug.*",
                   replacement = "\\1", x = text_from_png_short[5],
                   ignore.case = TRUE)
  
  privatkfzpers <- gsub(pattern = "(^.*) Privat.*",
                        replacement = "\\1", x = text_from_png_short[6],
                        ignore.case = TRUE)
  
  mitfahrt <- gsub(pattern = ".+ (.*) Mitfahrt.+",
                        replacement = "\\1", x = text_from_png_short[6],
                        ignore.case = TRUE)
  
  privatkfztrift <- gsub(pattern = "(^.*) Privat.*",
                         replacement = "\\1", x = text_from_png_short[7],
                         ignore.case = TRUE)
  
  sonstige <- gsub(pattern = "(^.*) Sonstige.*",
                    replacement = "\\1", x = text_from_png_short[8],
                    ignore.case = TRUE)
  
  df_results$DienstreiseID[i] <- i
  df_results$Reisestart[i] <- reisestart
  df_results$Reiseziel[i] <- reiseziel
  df_results$Rueckkehrdatum[i] <- rueckkehrdatum
  df_results$Bahn[i] <- bahn
  df_results$Bus[i] <- bus
  df_results$Schiff[i] <- schiff
  df_results$DienstKFZ[i] <- dienstkfz
  df_results$Bahncard[i] <- bahncard
  df_results$Flugzeug[i] <- flugzeug
  df_results$PrivatKFZ_persönlicheG[i] <- privatkfzpers
  df_results$MitfahrtBei[i] <- mitfahrt
  df_results$PrivatKFZ_triftigeG[i] <- privatkfztrift
  df_results$Sonstige[i] <- sonstige
  
  # Save texts in file
  dir.create(output_text, showWarnings = FALSE)
  
  utils::write.table(x = text_from_png_short,
                     file = paste(output_text, "/Dienstreise_", i,
                                  "_Textauszug_kurz.txt", sep = ""),
                     sep = "", row.names = FALSE,
                     col.names = FALSE, quote = FALSE)
  
  utils::write.table(x = text_from_png,
                     file = paste(output_text, "/Dienstreise_", i,
                                  "_Textauszug.txt", sep = ""),
                     sep = "", row.names = FALSE,
                     col.names = FALSE, quote = FALSE)
  
  # Save png files for validation
  dir.create(output_image, showWarnings = FALSE)
  
  magick::image_write(image = destination_png,
                      path = paste(output_image, "/Dienstreise_", i,
                                   "_Reiseziel.png", sep = ""))
  
  magick::image_write(image = means_of_transport_png,
                      path = paste(output_image, "/Dienstreise_", i,
                                   "_Transportmittel.png", sep = ""))
  
}

# Save data frame in file
dir.create(output_analysis, showWarnings = FALSE)
write.csv(x = df_results, file = paste(
  output_analysis, "/textanalysis_en.csv", sep = ""), row.names = FALSE)
write.csv2(x = df_results, file = paste(
  output_analysis, "/textanalysis_de.csv", sep = ""), row.names = FALSE)

# Set default working directory and clean environment
setwd(old_wd)
rm(list = ls())