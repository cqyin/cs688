rm(list=ls()); cat('\014') # clear workspace


pdftotext.cmd <- Sys.which('pdftotext')
pdf.dir <- file.path(getwd(), 'PDF Files')
pdf.files <- normalizePath(list.files(pdf.dir, pattern='pdf', full.names=TRUE))

system(paste(pdftotext.cmd, paste0('"', pdf.files[1], '"')), wait=FALSE)

library(pdftools)

pdftext_doc1 <- pdf_text(pdf.files[1])

write.table(pdftext_doc1, file=paste0(pdf.dir, '/text.txt'), quote=FALSE, row.names=FALSE, col.names=FALSE, eol=' ')

convert.pdf <- function(files) {
  for (i in 1:length(files)) {
    pdf.file <- files[i]
    pdf.text <- pdf_text(pdf.file)
    file.name <- sub('.pdf', '.txt', pdf.file)
    print(file.name)
    write.table(pdf.text, file=basename(file.name), quote=FALSE, row.names=FALSE, col.names=FALSE, eol=' ')
  }
}

convert.pdf(pdf.files)
