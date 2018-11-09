# write _bookdown.yml
bookdown.yml <- "_bookdown.yml"
if(bookdown.yml %in% list.files(".")) {
  file.remove(bookdown.yml)
}
file.create(bookdown.yml)
chapters <- paste(file.path("Chapters", list.files("Chapters/")),
                  collapse = '", "')
text_to_write <- paste0('rmd_files: ["index.Rmd", "', chapters, '"]')
fileConn<-file(bookdown.yml)
writeLines(text_to_write, fileConn)
close(fileConn)

# now render book
bookdown::render_book("index.Rmd", output_format = bookdown::gitbook(split_by="none"))
