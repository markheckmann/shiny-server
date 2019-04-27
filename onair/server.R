# sudo scp -r /Users/mark/Documents/_mh/R/pkg_dev/shiny/dev_apps/onair mark@134.102.100.220:~/Dokumente/public_apps
# sudo cp -a ~/Dokumente/public_apps/ /var/shiny-server/www/
options(rgl.useNULL=TRUE) 
options(shiny.sanitize.errors = FALSE)

library(shiny)
library(rgl)
library(knitr)
library(markdown)
library(stringr)
library(OpenRepGrid, quietly=TRUE)
library(rglwidget)
library(rgl)

source("R/utils.R") 
source("R/webGLParser.R")             # for 3D canvas
source("R/samplegrid_infotext.R")     # info texts for sample grids

html_resource_path <- file.path(getwd(), "reports/html_resources")

eval_text <- function(x) {
  eval(parse(text=x))
}
options(width=150)
values <- reactiveValues()
values$current_grid <- NULL
values$e.names <- NA
values$c.names <- NA
values$constructs_clusterboot <- NULL
values$elements_clusterboot <- NULL

# directory to store the reports in depends on whether working locally or not
#
work.local <- Sys.info()["nodename"] != "gridhub"
if (work.local) {     # check if we work on server or local machine
  values$server.ip <- "localhost:8100"
  values$app.dir <- getwd()
  values$reports.dir <- "tmp_reports"   
} else {
  values$server.ip <- "134.102.100.220"
  values$app.dir <- "~/ShinyApps/onair/"
  values$reports.dir <- "/var/www/reports"   
}

shinyServer(function(input, output, session) {
  
  get_file <- reactive({
    values$current_grid
  })
  
  
  #### OBSERVERS #### 
  
  # extract grid passed via get variable
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$grid)) {
      str <- str_replace_all(query$grid, "\\\\n", "\n")   #replace "\\n" by "\n"
      f <- paste0(tempfile(), ".txt")
      writeLines(str, f)
      values$current_grid <- importTxt(f)
    }
  })
  
  
  observe({
    infile <- input$gridfile
    if (!is.null(infile)) {
      values$current_grid <- importTxt(infile$datapath)   
      # TODO: the updating currently has a bug so choices may not be NULL: https://github.com/rstudio/shiny/issues/176 
#       choices <- c("Select a grid"="none", 
#                    "Boeker"="boeker", 
#                    "Fransella et al. 2003, p.29"="fbb2003")
      updateSelectInput(session, inputId="samplegrid", choices=NULL) 
    }
  })
  
  observe({
    samplegrid <- input$samplegrid
    if (samplegrid != "none") 
      values$current_grid <- eval(parse(text=samplegrid))
  })
  
  # current grid gets changed
  observe({
    g <- values$current_grid
    if (!is.null(g)) {
      values$e.names <- getElementNames(g)
      values$c.names <- getConstructNames2(g) 
      values$constructs_clusterboot <- NULL    # reset clusterboot results
    }
    # update all element and construct selectors
    updateCheckboxGroupInput(session, "biplot_element_selector_12", 
                             label="",  choices=values$e.names, 
                             selected=values$e.names)
    updateCheckboxGroupInput(session, "biplot_construct_selector_12", 
                             label="",  choices=values$c.names, 
                             selected=values$c.names) 
    updateSelectInput(session, "indexes_implicative_dilemma_ideal", 
                      label="Ideal element",  choices=values$e.names, 
                      selected=values$e.names[1]) 
    updateSelectInput(session, "indexes_implicative_dilemma_self", 
                      label="Self element",  choices=values$e.names, 
                      selected=values$e.names[2]) 
  })
  
  # update element selection
  observe({
    input$biplot_12_toggle_elements   # make reactive
    e.names <- isolate(values$e.names)
    if (is.null(isolate(input$biplot_element_selector_12)))
      select <- e.names
    else
      select <- NULL
    updateCheckboxGroupInput(session, "biplot_element_selector_12", label="", 
                             choices=e.names, selected=select)
  }, suspended=FALSE)
  
  observe({
    input$biplot_12_toggle_constructs   # make reactive
    c.names <- isolate(values$c.names)
    if (is.null(isolate(input$biplot_construct_selector_12)))
      select <- c.names
    else
      select <- NULL
    updateCheckboxGroupInput(session, "biplot_construct_selector_12", label="", 
                             choices=c.names, selected=select)
  }, suspended=FALSE)
  
  
  #### RENDERERS ####
  
  #### |-- Load grids ####
  
  output$load_grid <- renderUI({
    list(h3("Welcome to OpenRepGrid on Air"),
         HTML("<p><i>OpenRepGrid on Air</i> is the working title for this 
              online grid analysis software. It allows to carry out several kinds of
              analysis for repertory grid data. The software is based on the 
              <a href='http://www.openrepgrid.uni-bremen.de/' target='_blank'>OpenRepGrid</a> 
              R package which runs in the background to produce the output.</p>"),
         HTML("<p>In the top panel you will find the available (analysis) features. 
              Once you select a feature you will see the available settings for 
              the feature in the panel on the left.</p>"),
         HTML("To get started you can either select one of the already available sample grids
              from the literature or upload your own grid file. Your grid file has to be in 
              <a href='http://docu.openrepgrid.org/loading.html#txt-files' 
              target='_blank'>this format</a> to be read in."),
         HTML("<p><font color='red'><b>Caveat:</b></font> Not all browser support all application features properly. 
              The Google Chrome browser seems to work in most cases.
              </p>")
         )
  })  
  
  #### |-- Bertin ####
  
  output$bertin_info <- renderUI({
    HTML( inject_info_on_top_of_ui_pages("bertin", "www/info/bertin.html") )  
  })
  
  output$bertin <- renderPlot({
    cex <- 1.1
    x <- get_file()
    if (!is.null(x))
      bertin(x, 
             cex.elements=input$bertin_standard_cex_all, 
             cex.text=input$bertin_standard_cex_all,
             cex.constructs=input$bertin_standard_cex_all,
             colors=c(input$bertin_standard_color_left, input$bertin_standard_color_right),
             showvalues=input$bertin_standard_showvalues,
             xlim=c(input$bertin_standard_xlim_1, 1 - input$bertin_standard_xlim_2),
             ylim = c(0, input$bertin_standard_ylim))
  })
  
  #### |-- Biplot ####
  
  #### |---- Biplot Dim 1-2 ####
  
  output$biplot_info <- renderUI({
      HTML( inject_info_on_top_of_ui_pages("biplot", "www/info/biplot.html"))  
  })
  
  output$biplot2d_12 <- renderPlot({
    input$biplot_12_update_button_elements      # trigger rendering by button
    input$biplot_12_update_button_constructs    # trigger rendering by button
    x <- get_file() 
    e.sel <- isolate(input$biplot_element_selector_12)
    e.names <-isolate(values$e.names)    
    e.i <- which(e.names %in% e.sel)
    
    c.sel <- isolate(input$biplot_construct_selector_12)
    c.names <-isolate(values$c.names)    
    c.i <- which(c.names %in% c.sel)
    
    dim <- c(input$biplot_12_dim_1, input$biplot_12_dim_2)
    flipaxes <- c(input$biplot_12_flipaxes_1, input$biplot_12_flipaxes_2)
    if (dim[1] == dim[2]) {
      plot.new()
      text(.5, .5, "dimensions can not be identical", cex=1.2)
    } else if (!is.null(x))
      biplot2d(x, g=input$biplot_12_g, 
               dim=dim, center=input$biplot_12_center, 
               normalize=input$biplot_12_normalize,
               flipaxes=flipaxes, 
               e.points.show=e.i, e.labels.show=e.i,
               c.labels.show=c.i, c.points.show =c.i,
               e.label.cex=input$biplot_12_e_label_cex, 
               c.label.cex=input$biplot_12_c_label_cex,
               e.point.cex=input$biplot_12_e_point_cex,
               c.point.cex=input$biplot_12_c_point_cex,
               e.point.col=input$biplot_12_e_point_col,
               e.label.col=input$biplot_12_e_label_col,
               c.point.col=input$biplot_12_c_point_col,
               c.label.col=input$biplot_12_c_label_col,
               mai=c(input$biplot_12_mai_bottom,
                     input$biplot_12_mai_left,
                     input$biplot_12_mai_top,
                     input$biplot_12_mai_right) / 72,
               var.cex=1)
  }, width=function() input$biplot_12_plotsize,
     height=function() input$biplot_12_plotsize)
  
  #### |---- Biplot Dim 2-3 ####
  
  output$biplot2d_23 <- renderPlot({
    x <- get_file()
    dim <- c(input$biplot_23_dim_1, input$biplot_23_dim_2)
    flipaxes <- c(input$biplot_23_flipaxes_1, input$biplot_23_flipaxes_2)
    if (dim[1] == dim[2]) {
      plot.new()
      text(.5, .5, "dimensions must not be identical")
    } else if (!is.null(x))
      biplot2d(x, 
               dim=dim, flipaxes=flipaxes,
               e.label.cex=1, c.label.cex=1)
  }, width=function() input$biplot_23_plotsize, 
                                   height=function() input$biplot_23_plotsize)
  
  #### |---- Biplot 3D ####
  
  output$webGL <- reactive({
    x <- get_file()
    if (!is.null(x)) {
      biplot3d(x)
      view3d(theta = 0, phi = 0, zoom = .8)
      extractWebGL(input$biplot3d_size)       
    }
  })
  
  
  output$webGL2 <- rgl::renderRglwidget({
    input$biplot3d_size
    x <- get_file()
    if (!is.null(x)) 
    {
      try(rgl.close())
      biplot3d(x)
      view3d(theta = 0, phi = 0, zoom = .8)
      rglwidget()
      # extractWebGL(input$biplot3d_size)       
    }
  }, outputArgs = list(width = "auto", height = "300px"))
  
  #### |-- Constructs ####
  
  #### |---- Cluster ####
  
  output$construct_cluster <- renderPlot({
    x <- get_file()
    if (!is.null(x))
      cluster(x, 1, 
              dmethod=input$constructs_cluster_dmethod,
              cmethod=input$constructs_cluster_cmethod,
              type = input$constructs_cluster_type, 
              align=input$constructs_cluster_align,
              lab.cex=1.2, mar=c(4, 2, 3, 20), cex.main=1.2)
  })
  
  #### |---- Cluster (bootstrapped) ####
  
  # wird einmal automatisch aufgerufen, bei initialisierung
  observe({
    i <- input$constructs_clusterboot_update_button
    #cat("constructs_clusterboot_update_button:", i)
    x <- isolate(get_file())
    s <- NULL
    if (!is.null(x) & i > 0)
      s <- clusterBoot(x, along=1,
                       align=input$constructs_clusterboot_align,
                       nboot=isolate(input$constructs_clusterboot_nboot),
                       dmethod=isolate(input$constructs_clusterboot_dmethod),
                       cmethod=isolate(input$constructs_clusterboot_cmethod)) 
    values$constructs_clusterboot <- s
  })
  
  output$construct_clusterboot <- renderPlot({
    s <- values$constructs_clusterboot
    if (!is.null(s)) {
      plot(s)
      if (input$constructs_clusterboot_drawrects){
        pvclust::pvrect(s, max.only=input$constructs_clusterboot_maxonly,
                        alpha=input$constructs_clusterboot_alpha)
      }       
    } else {
      plot.new()
      text(.5, .5, "Analysis not yet prompted", cex=1.3)
    }
  })
  
  
  #### |---- Correlation ####
  
  output$construct_correlation <- renderPrint({
    x <- get_file()
    if (!is.null(x)) {
      constructCor(x, 
                   method=input$constructs_correlation_method, 
                   trim=input$constructs_correlation_trim)    
    }    
  })
  
  output$construct_correlation_rms <- renderPrint({
    x <- get_file()
    if (!is.null(x) & input$constructs_correlation_rms) {
      constructRmsCor(x,  
                      method=input$constructs_correlation_method, 
                      trim=input$constructs_correlation_trim)      
    }    
  })
  
  #### |---- Distance ####
  
  output$construct_distance <- renderPrint({
    x <- get_file()
    if (!is.null(x)) {
      d <- distance(x, along=1,
                    dmethod=input$constructs_distance_dmethod, 
                    trim=input$constructs_distance_trim)    
      print(d, digits=input$constructs_distance_digits)
    }     
  })
  
  #### |---- PCA ####
  
  output$construct_pca <- renderPrint({
    x <- get_file()
    if (!is.null(x)) {
      pca <- constructPca(x, nfactors = input$constructs_pca_nfactors, 
                          rotate = input$constructs_pca_rotate,
                          method = input$constructs_pca_correlation,
                          trim = input$constructs_pca_trim)  
      print(pca, digits=input$constructs_pca_digits,
            cutoff=input$constructs_pca_cutoff)
    }
  })
  
  #### |---- Somers' d ####
  
  output$construct_somers <- renderPrint({
    x <- get_file()
    if (!is.null(x)) {
      d <- constructD(x, dependent=input$constructs_somers_dependent, 
                      trim=input$constructs_somers_trim)    
      print(d, digits=input$constructs_somers_digits)
    }     
  })
  
  
  #### |-- Elements ####
  
  #### |---- Correlation ####
  
  output$elements_correlation <- renderPrint({
    x <- get_file()
    if (!is.null(x)) {
      elementCor(x, rc=input$elements_correlation_rc, 
                   method=input$elements_correlation_method, 
                   trim=input$elements_correlation_trim)    
    }    
  })
  
  output$elements_correlation_rms <- renderPrint({
    x <- get_file()
    if (!is.null(x) & input$elements_correlation_rms) {
      elementRmsCor(x,
                    rc=input$elements_correlation_rc,
                    method=input$elements_correlation_method, 
                    trim=input$elements_correlation_trim)      
    }    
  })

  #### |---- Distance ####
  
  output$elements_distance <- renderPrint({
    x <- get_file()
    if (!is.null(x)) {
      d <- distance(x, along=2,
                    dmethod=input$elements_distance_dmethod, 
                    trim=input$elements_distance_trim)    
      print(d, digits=input$elements_distance_digits)
    }     
  })
  
  #### |---- Cluster ####
  
    output$elements_cluster <- renderPlot({
      x <- get_file()
      if (!is.null(x))
        cluster(x, along=2, 
                dmethod=input$elements_cluster_dmethod,
                cmethod=input$elements_cluster_cmethod,
                type = input$elements_cluster_type, 
                lab.cex=1.2, mar=c(4, 2, 3, 20), cex.main=1.2)
    })
  
  
  #### |---- Cluster (bootstrapped) ####
  
  # wird einmal automatisch aufgerufen, bei initialisierung
  observe({
    input$elements_clusterboot_update_button
    x <- isolate(get_file())
    s <- NULL
    if (!is.null(x))
      s <- clusterBoot(x, along=2,
                       nboot=isolate(input$elements_clusterboot_nboot),
                       dmethod=isolate(input$elements_clusterboot_dmethod),
                       cmethod=isolate(input$elements_clusterboot_cmethod)) 
    values$elements_clusterboot <- s
  })
  
  output$elements_clusterboot <- renderPlot({
    s <- values$elements_clusterboot
    if (!is.null(s)) {
      plot(s)
      if (input$elements_clusterboot_drawrects){
        pvclust::pvrect(s, 
                        max.only=input$elements_clusterboot_maxonly,
                        alpha=input$elements_clusterboot_alpha)
      }       
    } else {
      plot.new()
      text(.5, .5, "Analysis not yet prompted", cex=1.3)
    }
  })
  
  
  
  
  
  #### |-- Indexes ####
  
  #### |---- PVAFF ####
  
  output$indexes_pvaff <- renderPrint({
    x <- get_file()
    if (!is.null(x))
      indexPvaff(x)
  })
  
  #### |---- Implicative Dilemma ####
  
  output$indexes_implicative_dilemma <- renderPrint({
    x <- get_file()
    self <- which(values$e.names %in% input$indexes_implicative_dilemma_self)
    ideal <- which(values$e.names %in% input$indexes_implicative_dilemma_ideal)
    if (!is.null(x))
      indexDilemma(x, self=self, ideal=ideal, 
                   r.min=input$indexes_implicative_dilemmas_rmin)
  })
  
  output$indexes_implicative_dilemma_plot <- renderPlot({
    x <- get_file()
    self <- which(values$e.names %in% input$indexes_implicative_dilemma_self)
    ideal <- which(values$e.names %in% input$indexes_implicative_dilemma_ideal)
    if (!is.null(x) & input$indexes_implicative_dilemmas_show)
      indexDilemma(x, self=self, ideal=ideal, 
                   show=TRUE, output=0)
  })
  
  #### |---- Intensity ####
  
  output$indexes_intensity <- renderPrint({
    x <- get_file()
    if (!is.null(x))
      indexIntensity(x)
  })
  

  #### |-- Generate Report ####
  
  # create environment with parameters and pass it to knitr
  get_report_settings <- function()
  {
    env <- new.env()
    
    isolate({
      env$html_resource_path <- html_resource_path
      env$x <- get_file()   # get current grid
      
      # which parts to report
      env$generate_report_do_bertin <- input$generate_report_do_bertin
      env$generate_report_do_biplots_12 <- input$generate_report_do_biplots_12
      
      #### |---- Bertin ####
      
      # parameters
      env$bertin.showvalues <- input$bertin_standard_showvalues
      env$bertin.cex.all <- input$bertin_standard_cex_all  
      env$bertin.colors <- c(input$bertin_standard_color_left,
                             input$bertin_standard_color_right)
      env$bertin.ylim <- c(0, input$bertin_standard_ylim)

      
      #### |---- Biplots ####
      
      # parameters
      biplot_12_plotsize <- input$biplot_12_plotsize
      
      e.sel <- input$biplot_element_selector_12
      e.names <-values$e.names
      e.i <- which(e.names %in% e.sel)
      
      c.sel <- input$biplot_construct_selector_12
      c.names <-values$c.names
      c.i <- which(c.names %in% c.sel)
      
      env$bipots.12.dim <- c(input$biplot_12_dim_1, input$biplot_12_dim_2)
      env$bipots.12.g <- input$biplot_12_g
      env$bipots.12.center <- input$biplot_12_center
      env$bipots.12.normalize <- input$biplot_12_normalize
      env$bipots.12.flipaxes <- c(input$biplot_12_flipaxes_1, input$biplot_12_flipaxes_2)  
      env$bipots.12.e.points.show <- e.i 
      env$bipots.12.e.labels.show <- e.i
      env$bipots.12.c.labels.show <- c.i
      env$bipots.12.c.points.show <- c.i
      env$bipots.12.e.label.cex <- input$biplot_12_e_label_cex
      env$bipots.12.c.label.cex <- input$biplot_12_c_label_cex
      env$bipots.12.e.point.cex <- input$biplot_12_e_point_cex
      env$bipots.12.c.point.cex <- input$biplot_12_c_point_cex
      env$bipots.12.e.point.col <- input$biplot_12_e_point_col
      env$bipots.12.e.label.col <- input$biplot_12_e_label_col
      env$bipots.12.c.point.col <- input$biplot_12_c_point_col
      env$bipots.12.c.label.col <- input$biplot_12_c_label_col
      env$biplot.12.mai.bottom <- input$biplot_12_mai_bottom
      env$biplot.12.mai.left<- input$biplot_12_mai_left
      env$biplot.12.mai.top <- input$biplot_12_mai_top
      env$biplot.12.mai.right <- input$biplot_12_mai_right
      env$bipots.12.var.cex <- 1
    })
    env
  }
  
#   observe({
#     if (!is.null(isolate(get_file())) & input$generate_report_button) {
#       env <- get_report_settings()
#       tmpdir <- tempdir()      
#       file.copy("reports/report_template.Rmd", tmpdir, overwrite = TRUE)
#       olddir <- setwd(tmpdir)
#       knit("report_template.Rmd", output="report.md", envir=env)
#       markdownToHTML("report.md", "report.html") 
#       setwd(olddir)
#       browseURL(file.path(tmpdir, "report.html"))      
#     }
#   })
  
 
  output$generate_report <- renderUI({
    if (!is.null(isolate(get_file())) & input$generate_report_button) {
      env <- get_report_settings()
      
      server.ip <- isolate(values$server.ip)
      app.dir <- isolate(values$app.dir)
      reports.dir <- isolate(values$reports.dir)
  
      #cat("current dir:", getwd(), "\n") 
      #cat("set application dir\n")
      setwd(app.dir)  
      
      #cat("current dir:", getwd(), "\n") 
      tmp.dir <- tempfile("grid_", reports.dir)
      dir.created <- dir.create(tmp.dir)
      #cat("dir created: ", dir.created, "\n")
      
      #cat("files: ", dir(), "\n")
      
      copied <- file.copy("reports/report_template.Rhtml", 
                          tmp.dir, overwrite = TRUE)
      #cat("copied:", copied, "\n")
      
      # include custom css to make it look nicer
      #source("prelude.R")
      stylesheet.path <- file.path(getwd(), "css/style.css")
      
      old.dir <- setwd(tmp.dir)

      #knit("report_template.Rmd", output="report.md", envir=env, quiet=TRUE)
      #markdownToHTML("report.md", "report.html", stylesheet=stylesheet.path) 
      knit2html("report_template.Rhtml", output="report.html", envir=env, 
                options=c("base64_images"))
      setwd(old.dir)
      if (work.local)      
        report.url <- paste0("file://localhost", getwd(), "/tmp_reports/", basename(tmp.dir), "/report.html")
      else
        report.url <- paste0("http://", server.ip, "/reports/", basename(tmp.dir), "/report.html")  
      HTML("<p>Copy URL into new browser tab to see the results:</p>",
           "<p>",
           paste0("<a href='", report.url, "' target='_blank'>", report.url, "</a>"),
           "</p>", "<hr>")            
      #cat("Copy URL into new browser tab to see the results: \n\n", report.url, "\n")
    } else {
      #cat("Report generation not yet prompted")
      HTML("<p>Report generation not yet prompted</p>", "<hr>")
    }
  })
  
  # DEV reports
  # idea: create code chunks
#   report_chunk_biplot <- observe(
#     isolate(
#       showvalues <- input$bertin_standard_showvalues
#       cex.all <- input$bertin_standard_cex_all  
#       colors <- c(input$bertin_standard_color_left,
#                   input$bertin_standard_color_right)
#       ylim <- c(0, input$bertin_standard_ylim)
#       )
#       paste("bertin=", "x", ",",
#             "showvalues=", showvalues, ",",
#             "colors=", colors, collapse="")
#   )

# parameters

  

   

  
  #### Conditional Panels ####
  
  output$samplegrid_info <- renderUI({ 
    HTML(samplegrid_info[input$samplegrid])
  })
  
  output$upload_dialog <- renderUI({ 
    #input$samplegrid
    fileInput("gridfile", "",
              accept=c("text", "text/plain"))
  })
  })
