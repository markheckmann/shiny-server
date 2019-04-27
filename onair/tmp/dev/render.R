# render .Rmd document and open in browser

# generate tempdir and add current grid file

yaml.header <- '---\ntitle: "Untitled" \noutput: html_document\n---\n'
chunk2 <- '```{r, echo=FALSE}\nplot(cars)\n```'
chunk1 <- '# Header\n\nDeine mudda\n\n'
chunks <- list(yaml.header, chunk1, chunk2)

folder <- tempdir()
f <- tempfile("org-", fileext = ".Rmd")
for (chunk in chunks) 
  write(chunk, file=f,append=TRUE)

rmarkdown::render(f)
f.html <- gsub(".[Rr]md", ".html", f)
browseURL(f.html)
