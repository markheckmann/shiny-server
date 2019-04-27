## extra functions

#### GENERATE UI INFO BOXES ####

# produce collapsible info boxes at top of every page
#
# An anchor and a div (with the content) is generated with the ids "toggle-id-info" and "id-info".
# The js function toggleAndChangeText is passed the div's id and will do the collapsing and 
# change of the anchor text.
#
inject_info_on_top_of_ui_pages <- function(id, infofile="") {
  if (!file.exists(infofile)) {
    cat("infofile does not exist")
    return("")
  }   
  part.1 <- 
    '<div>
  <p class="alignright"> <a id="toggle-__id__-info" class="toggle-info" href="javascript:toggleAndChangeText(\'__id__-info\');"> Info &#9660</a> </p>
  </div>
  <div style="clear: both;"></div>
  <div id="__id__-info" class="mycoll">'
  part.1 <- str_replace_all(part.1, "__id__", id)
  part.2 <- paste0(readLines(infofile), collapse="\n")
  part.3 <- '\n</div>'
  paste(part.1, part.2, part.3, collapse="\n")
}


#### REPORT GENERATION ####

# include external css, js and html code inline to make document portable
#
inject_inline_code <- function(file) {
  ext <- tolower(file_ext(file))
  if (ext == "js") {
    in.tag <- paste0("<script>")
    out.tag <- paste0("</script>")
  } else if (ext == "css") {
    in.tag <- paste0("<style>")
    out.tag <- paste0("</style>")
  } else if (ext == "html") {
    in.tag <- ""
    out.tag <- ""
  } else 
    stop("included file is no .js, .css or .html file!")
  l <- readLines(file)
  cat("<!---------------- code made inline from file", file, "------------------> \n")
  cat(in.tag, "\n")
  cat(paste0(l, "\n"))
  cat(out.tag, "\n\n")
}





# function to inject the HTML code for the header and the info box
#
# id:      id of section
# header:   section header shown
# infofile: path to HTML infofile to dislay (optional)
#
inject_info <- function(id, header, infofile="") {
  extra.info <- infofile != ""
  cat("<div>\n")
  cat (paste0('\t<a id=\"', id, '\"></a>\n') )     # anchor for reference
  cat( paste0('\t<p class="alignleft">', header, '</p>\n') )    # name of header
  if (extra.info)
    cat( '\t<p class="alignright">',
         paste0('<a id="toggle-', id, '-info"'),
         paste0('href="javascript:toggleAndChangeText(\'', id, '-info\');\">'),
         'Info &#9660</a> </p>\n')
  cat('</div>\n')
  cat('<div style="clear: both;"></div>\n')
  if (extra.info) {
    cat(paste0('<div id="', id, '-info" class="mycoll">\n'))
    inject_inline_code(infofile)
    cat('</div>\n')    
  }
}
