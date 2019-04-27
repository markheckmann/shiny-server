# docu <- list()
# docu$biplot2d <- c("<h5>The Biplot</h5>",
#                    "The biplot is the way to visualize  
#                    elements and constructs in a single plot. Depending on the parameters chosen it 
#                    contains information on the distances between elements and 
#                    constructs. Also the relative values the elements have on a
#                    construct can be read off by projetion the element onto the
#                    construct vector. A lot of parameters can be changed rendering
#                    different types of biplots. Also, the look can be modified
#                    (colors, text size).",
#                     "<h5>Parameters</h5>",
#                    "<p><b>g</b></p>",  
#                    "Power of the singular value matrix assigned to the left singular 
#                    vectors, i.e. the constructs.")
# 
# get_html_docu <- function(f)
# {
#   HTML(docu[[f]])
# }
# 
# #get_html_docu("biplot2d")