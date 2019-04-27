library(shiny)

#### FUNCTIONS AND INIT ####

source("docu.R")
colors <- colors()         # all available color names in R
colors.all <- c("black", "darkred", "darkblue", "darkgreen", colors)

values <- reactiveValues()
values$e.names <- "No grid selected"
values$c.names <- "No grid selected"

reactiveWebGL <- function (outputId) 
{
  HTML(paste("<div id=\"", outputId, "\" class=\"shiny-webgl-output\"></div>", sep=""))
}


#### CONDITIONAL PANELS ####


#### |-- Settings ####

cond.panel.settings <- 
  conditionalPanel(condition="input.level1=='load_grid'",
                   
                   h3("Select a grid"),
                   HTML("<hr>"),
                   h4("Upload a grid file"),
                   #                    fileInput("gridfile", "",
                   #                              accept=c("text", "text/plain")),
                   uiOutput("upload_dialog"),
                   HTML("<hr>"),
                   h4("Select a sample grid"),
                   selectInput("samplegrid", "", 
                               choices= c("Select a grid"="none", 
                                 sort(c("Boeker (1996)"="boeker", 
                                        "Fransella et al. (2003)"="fbb2003",
                                        "Mackay (1992)"="mackay1992"))),
                               selectize = FALSE),
                   #"Bell & McGorry (1992)"="bellmcgorry1992")),
                   uiOutput("samplegrid_info")
  )


#### |-- Bertin ####

cond.panel.bertin <- 
  conditionalPanel(condition="input.level1=='bertin_standard'",
                   h3("Bertin Settings", id="bertin_settings"),
                   tags$div(checkboxInput("bertin_standard_showvalues", "Show values", value = TRUE),
                            id="bertin_standard_showvalues_wrapper_div", style="display:inline-block;"),
                   numericInput("bertin_standard_cex_all", "Textsize", 1.1, min = .1, max = 2,
                                step = .05),
                   span(numericInput("bertin_standard_xlim_1", "Space constructs left", .2, min = 0, max = 1,
                                     step = .02),
                        numericInput("bertin_standard_xlim_2", "Space constructs right", .2, min = 0, max = 1,
                                     step = .02)),
                   numericInput("bertin_standard_ylim", "Size element region", .6, min = 0, max = 1,
                                step = .05),
                   span(selectInput("bertin_standard_color_left", "Color left pole", 
                                    choices=colors.all, selected="white", selectize=FALSE),
                        selectInput("bertin_standard_color_right", "Color right pole", 
                                    choices=colors.all, selected="darkblue", selectize=FALSE)
                   )        
  )



#### |-- Biplots ####

choices.center <- c("No centering"=0,
                    "Row mean centering (construct)"=1,
                    "Column mean centering (elements)"=2,
                    "Double-centering (construct and element means)"=3,
                    "Midpoint centering of rows (constructs)"=4)
choices.normalize <- c(none=0, rows=1, columns=2)

biplot.element.selector.12 <- checkboxGroupInput("biplot_element_selector_12", 
                                                 "", isolate(values$e.names),
                                                 selected = isolate(values$e.names))
biplot.construct.selector.12 <- checkboxGroupInput("biplot_construct_selector_12", 
                                                   "", isolate(values$c.names),
                                                   selected = isolate(values$c.names))
#### |---- Biplots dim 1-2 ####

cond.panel.biplots.dim.12 <-
  conditionalPanel(condition="input.level1=='level1_biplot_standard' && input.level2_biplots=='level2_biplots_dim_12'",
                   h3("Biplots Settings", id="biplot_settings"),
                   tabsetPanel(
                     #tabPanel("Info", get_html_docu("biplot2d")),
                     tabPanel("Elements", 
                              actionButton("biplot_12_toggle_elements", "Toggle On/off"),
                              actionButton("biplot_12_update_button_elements", "Update Biplot"),
                              HTML("<hr>"),
                              biplot.element.selector.12
                     ),
                     tabPanel("Constructs", 
                              actionButton("biplot_12_toggle_constructs", "Toggle On/off"),
                              actionButton("biplot_12_update_button_constructs", "Update Biplot"),
                              HTML("<hr>"),
                              biplot.construct.selector.12
                     ),
                     tabPanel("Transforms",
                              h5("Transformations"),
                              selectInput("biplot_12_center", "Centering", 
                                          choices=choices.center, selected=choices.center[2],
                                          selectize=FALSE),
                              selectInput("biplot_12_normalize", "Normalize", 
                                          choices=choices.normalize, selected=choices.normalize[1],
                                          selectize=FALSE),
                              numericInput("biplot_12_g", "g", 0, 0, 1, .1)
                     ),
                     tabPanel("Axes",
                              HTML("<hr>"), 
                              h5("Axes"),      
                              numericInput("biplot_12_dim_1", "PC on y-axis", 1, min = 1, max = 100,
                                           step = 1),
                              numericInput("biplot_12_dim_2", "PC on x-axis", 2, min = 1, max = 100,
                                           step = 1),
                              p("Flip axis"),
                              div(
                                checkboxInput("biplot_12_flipaxes_1", "x-axis", FALSE),
                                checkboxInput("biplot_12_flipaxes_2", "y-axis", FALSE))
                     ),
                     tabPanel("Size",
                              h5("Label size"),
                              numericInput("biplot_12_c_label_cex", "Constructs", 1.2, 0, 2, .1),
                              numericInput("biplot_12_e_label_cex", "Elements", 1.2, 0, 2, .1),
                              h5("Symbol size"),       
                              numericInput("biplot_12_c_point_cex", "Constructs", 1.2, 0, 2, .1),      
                              numericInput("biplot_12_e_point_cex", "Elements", 1.2, 0, 2, .1),
                              h5("Plot size in pixel"),       
                              sliderInput("biplot_12_plotsize", "", 600, min=300, max=1200, step=100)            
                     ),
                     tabPanel("Margins",          # in pixel, need to be converted to inches
                              h5("Margins for construct labels"),
                              sliderInput("biplot_12_mai_bottom", "Bottom", 100, min=0, max=300, step=25) ,
                              sliderInput("biplot_12_mai_left", "Left", 100, min=0, max=300, step=25),            
                              sliderInput("biplot_12_mai_top", "Top", 100, min=0, max=300, step=25),            
                              sliderInput("biplot_12_mai_right", "Right", 100, min=0, max=300, step=25)            
                     ),
                     tabPanel("Colors",
                              h5("Element colors"),
                              selectInput("biplot_12_e_point_col", "Symbol", colors.all, "black", selectize=FALSE),
                              selectInput("biplot_12_e_label_col", "Label", colors.all, "black", selectize=FALSE),
                              h5("Constructs colors"),
                              selectInput("biplot_12_c_point_col", "Symbol", colors.all, "black", selectize=FALSE),
                              selectInput("biplot_12_c_label_col", "Label", colors.all, "black", selectize=FALSE) 
                     ),
                     id="todo")               
  ) 



#### |---- Biplots dim 2-3 ####

cond.panel.biplots.dim.23 <-
  conditionalPanel(condition="input.level1=='level1_biplot_standard' && input.level2_biplots=='level2_biplots_dim_23'",
                   h3("Biplots Settings"),
                   numericInput("biplot_23_dim_1", "PC on y-axis", 3, min = 1, max = 100,
                                step = 1),
                   numericInput("biplot_23_dim_2", "PC on x-axis", 2, min = 1, max = 100,
                                step = 1),
                   checkboxInput("biplot_23_flipaxes_1", label="x-axis", FALSE),
                   checkboxInput("biplot_23_flipaxes_2", label="y-axis", FALSE),
                   sliderInput("biplot_23_plotsize", "", 600, min=200, max=1200, step=100)
  )  



#### |---- Biplots dim 3D ####

cond.panel.biplots.dim.3d <- 
  conditionalPanel(condition="input.level1=='level1_biplot_standard' && input.level2_biplots=='level2_biplots_3d'",
                   h3("Biplots Settings"),
                   sliderInput("biplot3d_size", "Size of plot:", 650, min=400, max=1000, step=25)
  )

#### |-- Constructs ####

#### |---- Correlations ####

cond.panel.constructs.correlation <- 
  conditionalPanel(condition="input.level1=='level1_constructs' && input.level2_constructs=='level2_construct_correlation'",
                   h3("Correlation Settings"),
                   selectInput("constructs_correlation_method", label="Correlation Method",
                               c("pearson", "kendall", "spearman"),
                               selectize=FALSE),
                   numericInput("constructs_correlation_trim", label="Trim construct names to x characters", 
                                20, 0, 100, 5),
                   checkboxInput("constructs_correlation_rms", "Calculate Root-means-square correlations (RMS)", TRUE)
  )

#### |---- Cluster ####

cond.panel.constructs.cluster <- 
  conditionalPanel(condition="input.level1=='level1_constructs' && input.level2_constructs=='level2_construct_cluster'",
                   h3("Cluster Settings"),
                   selectInput("constructs_cluster_dmethod", label="Distance Method",
                               c("euclidean", "maximum", "manhattan", "canberra"),
                               selectize=FALSE),
                   selectInput("constructs_cluster_cmethod", label="Correlation Method",
                               c("ward", "single", "complete", "average", "mcquitty"),
                               selectize=FALSE),
                   checkboxInput("constructs_cluster_align", label="Reverse constructs if necessary", TRUE),
                   selectInput("constructs_cluster_type", label="Dendrogram style",
                               c("rectangle", "triangle"),
                               selectize=FALSE)
  )


#### |---- Cluster (bootstrapped) ####

cond.panel.constructs.clusterboot <- 
  conditionalPanel(condition="input.level1=='level1_constructs' && input.level2_constructs=='level2_construct_clusterboot'",
                   h3("Cluster (bootstrapped) Settings"),
                   tags$button("Run analysis", id="constructs_clusterboot_update_button", type="button", class="btn action-button btn-success"),
                   #actionButton("constructs_clusterboot_update_button", "Run analysis"),
                   HTML("<hr>"),
                   HTML("<p>Bootstrapped cluster anaylysis will yield the same",
                        "dendrogram as conventional clustering. Additionally, it",
                        "will report a p-values as a measure of the stability for each cluster partition.</p>",
                        "<p><font color='red'><b>Depending on the number of bootstrap replicates the analysis may take some time to complete.</b></font>",
                        "Meanwhile you will not see anything on the screen.",
                        "Running the analysis with the default settings will need about ",
                        "10 seconds of calculation time.</p>"),
                   HTML("<hr>"),
                   selectInput("constructs_clusterboot_dmethod", label="Distance Method",
                               c("euclidean", "maximum", "manhattan", "canberra"),
                               selectize=FALSE),
                   selectInput("constructs_clusterboot_cmethod", label="Correlation Method",
                               c("ward", "single", "complete", "average", "mcquitty"),
                               selectize=FALSE),
                   checkboxInput("constructs_clusterboot_align", label="Reverse constructs if necessary", TRUE),
                   numericInput("constructs_clusterboot_nboot", "Number of bootstrap replicates", 500, 100, 10000, 100),
                   HTML("<hr>"),                
                   numericInput("constructs_clusterboot_alpha", "Threshold for p-values", .95, 0, 1, .01),
                   checkboxInput("constructs_clusterboot_drawrects", label="Draw rectangles around stable structures", TRUE),
                   checkboxInput("constructs_clusterboot_maxonly", label="Top level stable structures only", FALSE)
  )


#### |---- Distance ####

cond.panel.constructs.distance <- 
  conditionalPanel(condition="input.level1=='level1_constructs' && input.level2_constructs=='level2_construct_distance'",
                   h3("Distance Settings"),
                   selectInput("constructs_distance_dmethod", label="Distance Method",
                               c("euclidean", "maximum", "manhattan", "canberra"),
                               selectize=FALSE),
                   numericInput("constructs_distance_trim", label="Trim construct names to x characters", 
                                50, 0, 100, 5),
                   numericInput("constructs_distance_digits", label="Number of digits to display", 
                                1, 0, 10, 1)
  )


#### |---- PCA ####

cond.panel.constructs.pca <- 
  conditionalPanel(condition="input.level1=='level1_constructs' && input.level2_constructs=='level2_construct_pca'",
                   h3("PCA Settings"),
                   numericInput("constructs_pca_nfactors", label="Number of Pricipal Components (PCs)", 
                                4, 1, 20, 1) ,
                   selectInput("constructs_pca_rotate", label="Rotation type",
                               c("none", "varimax", "promax"), "varimax",
                               selectize=FALSE),
                   selectInput("constructs_pca_correlation", label="Correlation Method",
                               c("pearson", "kendall", "spearman"),
                               selectize=FALSE),
                   numericInput("constructs_pca_trim", label="Trim construct names to x characters", 
                                60, 0, 100, 5),
                   numericInput("constructs_pca_digits", label="Number of digits to display", 
                                2, 0, 10, 1),
                   numericInput("constructs_pca_cutoff", label="Minimum loading to print", 
                                .3, 0, 1, .01) 
  )

#### |---- Somers' d ####


cond.panel.constructs.somers <- 
  conditionalPanel(condition="input.level1=='level1_constructs' && input.level2_constructs=='level2_construct_somers'",
                   h3("Somers' d"),
                   selectInput("constructs_somers_dependent", label="Dependent side",
                               c("columns", "rows", "symmetric"),
                               selectize=FALSE),
                   numericInput("constructs_somers_trim", label="Trim construct names to x characters", 
                                20, 0, 100, 5),
                   numericInput("constructs_somers_digits", label="Number of digits to display", 
                                2, 0, 10, 1)
  )


#### |-- Elements ####

#### |---- Correlations ####


cond.panel.elements.correlation <- 
  conditionalPanel(condition="input.level1=='level1_elements' && input.level2_elements=='level2_elements_correlation'",
                   h3("Correlation Settings"),
                   selectInput("elements_correlation_method", label="Correlation Method",
                               c("pearson", "kendall", "spearman"),
                               selectize=FALSE),
                   checkboxInput("elements_correlation_rc", "Use Cohen's rc (invariant to construct reflection)", TRUE),
                   numericInput("elements_correlation_trim", label="Trim element names to x characters", 
                                15, 0, 100, 5),
                   checkboxInput("elements_correlation_rms", "Calculate Root-means-square correlations (RMS)", TRUE)
  )



#### |---- Cluster ####

cond.panel.elements.cluster <- 
  conditionalPanel(condition="input.level1=='level1_elements' && input.level2_elements=='level2_elements_cluster'",
                   h3("Cluster Settings"),
                   selectInput("elements_cluster_dmethod", label="Distance Method",
                               c("euclidean", "maximum", "manhattan", "canberra"),
                               selectize=FALSE),
                   selectInput("elements_cluster_cmethod", label="Correlation Method",
                               c("ward", "single", "complete", "average", "mcquitty"),
                               selectize=FALSE),
                   selectInput("elements_cluster_type", label="Dendrogram style",
                               c("rectangle", "triangle"),
                               selectize=FALSE)
  )


#### |---- Cluster (bootstrapped) ####

cond.panel.elements.clusterboot <- 
  conditionalPanel(condition="input.level1=='level1_elements' && input.level2_elements=='level2_elements_clusterboot'",
                   h3("Cluster (bootstrapped) Settings"),
                   actionButton("elements_clusterboot_update_button", "Run analysis"),
                   HTML("<hr>"),
                   HTML("<p>Bootstrapped cluster anaylysis will yield the same",
                        "dendrogram as conventional clustering. Additionally, it",
                        "will report a p-values as a measure of the stability for each cluster partition.</p>",
                        "<p><font color='red'><b>Depending on the number of bootstrap replicates the analysis may take some time to complete.</b></font>",
                        "Meanwhile you will not see anything on the screen.",
                        "Running the analysis with the default settings will need about ",
                        "10 seconds of calculation time.</p>"),
                   HTML("<hr>"),
                   selectInput("elements_clusterboot_dmethod", label="Distance Method",
                               c("euclidean", "maximum", "manhattan", "canberra"),
                               selectize=FALSE),
                   selectInput("elements_clusterboot_cmethod", label="Correlation Method",
                               c("ward", "single", "complete", "average", "mcquitty"),
                               selectize=FALSE),
                   numericInput("elements_clusterboot_nboot", "Number of bootstrap replicates", 500, 100, 10000, 100),
                   HTML("<hr>"),                
                   numericInput("elements_clusterboot_alpha", "Threshold for p-values", .95, 0, 1, .01),
                   checkboxInput("elements_clusterboot_drawrects", label="Draw rectangles around stable structures", TRUE),
                   checkboxInput("elements_clusterboot_maxonly", label="Top level stable structures only", FALSE)
  )


#### |---- Distance ####

cond.panel.elements.distance <- 
  conditionalPanel(condition="input.level1=='level1_elements' && input.level2_elements=='level2_elements_distance'",
                   h3("Distance Settings"),
                   selectInput("elements_distance_dmethod", label="Distance Method",
                               c("euclidean", "maximum", "manhattan", "canberra"),
                               selectize=FALSE),
                   numericInput("elements_distance_trim", label="Trim element names to x characters", 
                                50, 0, 100, 5),
                   numericInput("elements_distance_digits", label="Number of digits to display", 
                                1, 0, 10, 1)
  )



#### |-- Indexes ####

#### |---- PVAFF ####

cond.panel.indexes.pvaff <- 
  conditionalPanel(condition="input.level1=='level1_indexes' && input.level2_indexes=='level2_indexes_pvaff'",
                   h3("PVAFF"),
                   HTML("<p>Percentage of Variance Accounted for by First Factor</p>")
  )


#### |---- Implicative Dilemmas ####

cond.panel.indexes.implicative.dilemma <- 
  conditionalPanel(condition="input.level1=='level1_indexes' && input.level2_indexes=='level2_indexes_implicative_dilemma'",
                   h3("Implicative Dilemmas"),
                   selectInput("indexes_implicative_dilemma_self", 
                               "Self element", isolate(values$e.names),
                               selectize=FALSE),
                   selectInput("indexes_implicative_dilemma_ideal", 
                               "Ideal element", isolate(values$e.names), 
                               isolate(values$e.names)[2],
                               selectize=FALSE),
                   numericInput("indexes_implicative_dilemmas_rmin", label="Minimal correlation to assume implications between constructs", 
                                .35, 0, 1, .01),
                   checkboxInput("indexes_implicative_dilemmas_show", label="Show correlation distribution", TRUE)
  )


#### |---- Intensity ####

cond.panel.indexes.intensity <- 
  conditionalPanel(condition="input.level1=='level1_indexes' && input.level2_indexes=='level2_indexes_intensity'",
                   h3("Intensity Index"),
                   HTML("The Intensity index has been suggested by Bannister (1960) 
                         as a measure of the amount of construct linkage.")
  )



#### |-- Generate Report ####

cond.panel.generate.report <- 
  conditionalPanel(condition="input.level1=='level1_generate_report'",
                   h3("Generate Report"),
                   #actionButton("generate_report_button", "Go!")
                   tags$button("Go!", id="generate_report_button", type="button", class="btn action-button btn-success")
  )


#### MAIN PANELS ####

#### |-- Bertin ####

level1.panel.bertin <- 
  tabPanel("Bertin", 
           htmlOutput("bertin_info"),
           plotOutput("bertin", width="600px", height="600px"), 
           value="bertin_standard")


#### |-- Biplots ####

level1.panel.biplots <- 
  tabPanel("Biplots", 
           tabsetPanel(
                   tabPanel("2D", 
                            htmlOutput("biplot_info"),
                            plotOutput("biplot2d_12", width="auto", height="auto"), 
                            value="level2_biplots_dim_12"),
                   #tabPanel("dim 2 vs 3", plotOutput("biplot2d_23", width="auto", height="auto"), value="level2_biplots_dim_23"),
                   tabPanel("3D", 
                            reactiveWebGL(outputId = "webGL"), 
                            value="level2_biplots_3d"),
                   tabPanel("3D2", 
                            rgl::rglwidgetOutput("rglPlot"),
                            value="level2_biplots_3d2"),
                      id="level2_biplots"),
            value="level1_biplot_standard")


#### |-- Constructs ####

level1.panel.constructs <- 
  tabPanel("Constructs", 
           tabsetPanel(
             tabPanel("Correlations", verbatimTextOutput("construct_correlation"), 
                      verbatimTextOutput("construct_correlation_rms"), value="level2_construct_correlation"),
             tabPanel("Distances", verbatimTextOutput("construct_distance"), value="level2_construct_distance"), 
             tabPanel("Cluster", plotOutput("construct_cluster", width="600px", height="600px"), value="level2_construct_cluster"), 
             tabPanel("Cluster (bootstrapped)", plotOutput("construct_clusterboot", width="600px", height="600px"), value="level2_construct_clusterboot"),             
             tabPanel("PCA", verbatimTextOutput("construct_pca"), value="level2_construct_pca"),
             tabPanel("Somers' d", verbatimTextOutput("construct_somers"), value="level2_construct_somers"), 
             id="level2_constructs"),
           value="level1_constructs")


#### |-- Elements ####

level1.panel.elements <- 
  tabPanel("Elements", 
           tabsetPanel(
             tabPanel("Correlations", 
                      verbatimTextOutput("elements_correlation"), 
                      verbatimTextOutput("elements_correlation_rms"), 
                      value="level2_elements_correlation"),
             tabPanel("Distances", verbatimTextOutput("elements_distance"), value="level2_elements_distance"),
             tabPanel("Cluster", plotOutput("elements_cluster", width="600px", height="600px"), value="level2_elements_cluster"), 
             tabPanel("Cluster (bootstrapped)", plotOutput("elements_clusterboot", width="600px", height="600px"), value="level2_elements_clusterboot"),             
             id="level2_elements"),
           value="level1_elements")


#### |-- Indexes ####

level1.panel.indexes <- 
  tabPanel("Indexes", 
           tabsetPanel(
             tabPanel("PVAFF", verbatimTextOutput("indexes_pvaff"), value="level2_indexes_pvaff"),
             tabPanel("Implicative Dilemma", 
                      verbatimTextOutput("indexes_implicative_dilemma"), 
                      plotOutput("indexes_implicative_dilemma_plot", width="600px", height="600px"),
                      value="level2_indexes_implicative_dilemma"),
             tabPanel("Intensity", verbatimTextOutput("indexes_intensity"), value="level2_indexes_intensity"),
             id="level2_indexes"),
           value="level1_indexes")


#### |-- Generate Report ####

level1.panel.generate.report <- 
  tabPanel("Generate Report", 
           #verbatimTextOutput("generate_report"),
           uiOutput("generate_report"),
           div(
             checkboxInput("generate_report_do_bertin", "Bertin", TRUE),
             HTML("<hr>"),
             checkboxInput("generate_report_do_biplots_12", "Biplots", TRUE),
             HTML("<hr>")       
           ),
           value="level1_generate_report")


#### APP ####

shinyUI(pageWithSidebar(
  headerPanel("OpenRepGrid on Air"),
  
  #### |-- Sidebar panel ####
  
  sidebarPanel( 
    
    ## Settings ##
    cond.panel.settings,
    
    ## Bertin ##
    cond.panel.bertin,
    
    ## Biplots ##
    cond.panel.biplots.dim.12,
    cond.panel.biplots.dim.23,
    cond.panel.biplots.dim.3d,
    
    ## constructs ##
    cond.panel.constructs.correlation,
    cond.panel.constructs.distance,
    cond.panel.constructs.cluster,
    cond.panel.constructs.clusterboot,
    cond.panel.constructs.pca,
    cond.panel.constructs.somers,
    
    ## elements ##
    cond.panel.elements.correlation,
    cond.panel.elements.cluster,
    cond.panel.elements.clusterboot,
    cond.panel.elements.distance,
    
    ## indexes ##
    cond.panel.indexes.pvaff,
    cond.panel.indexes.implicative.dilemma,
    cond.panel.indexes.intensity,
    
    ## report ##
    cond.panel.generate.report,
    
    ## tooltips ##
    HTML("<hr>"),
    tags$div(checkboxInput("chk_toggle_tooltips", "Info for controls"), id="chk_toggle_tooltip_wrapper_div", 
             style="float: left;"),
    HTML("<br>")
  ),
  
  #### |-- Main panel ####
  
  mainPanel(
    
    # additional js scripts in folder /www (that's where shiny looks for static files)
    tags$head(
      tags$script(src="js.js", type="text/javascript"),            # Custom JS script for collapsing
      #tags$script(src="//ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js", type="text/javascript"),            # load jquery
      tags$link(href="style.css", rel="stylesheet"),           
      tags$script(src="rgl.js", type="text/javascript"),           # Add our custom JS script which binds the custom WebGL type (just HTML)  
      tags$script(src="CanvasMatrix.js", type="text/javascript"),  # Add CanvasMatrix (needed for the WebGL generated by rgl)     
      tags$script(src="js/tooltips_defs.js", type="text/javascript")
    ),
    h3(textOutput("caption")),
    tabsetPanel(
      tabPanel("Load grid", uiOutput("load_grid"), value="load_grid"),
     # tabPanel("Settings", value="settings"),
      level1.panel.bertin,
      level1.panel.biplots,
      level1.panel.constructs,
      level1.panel.elements,
      level1.panel.indexes,
     # level1.panel.generate.report,
      id="level1")
  )
))
