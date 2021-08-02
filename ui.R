#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(plotly)
library(r2d3)

library(waiter)
library(stringr)
library(igraph)
library(intergraph)
library(numbers)

source("R/utils.R")
source("R/config_data.R")

dataset_dict <- get_datasets()

shinyUI(
    fluidPage(
        #shinythemes::themeSelector(),
        theme = shinythemes::shinytheme("lumen"),
        use_waiter(),
        use_waitress(),
        shinyjs::useShinyjs(),
        
        tags$head(
            tags$style(
            HTML("
                .hashButtons .btn {
                    border-radius: 30%;
                    background-color: #4CAFAF9C;
                    padding: 8px;
                    margin: 5px;
                }
                .simbutton .btn {
                    font-size: 14px;
                    border: 2px solid red;
                    height: 100%;
                    padding: 8px;
                }
                .vec_len_txt {
                    border-radius: 10%;
                    background-color: gray;
                    padding: 8px;
                    margin: 5px;
                }
                
                div.formula-output {
                    word-wrap:normal;
                    border: 2px solid #777;
                }
                .toggleCol {
                    color: rgb(230, 128, 142);
                }
            ")),
            tags$script(
                HTML("
                        // change color inside element
                        Shiny.addCustomMessageHandler ('changeTxtColour', function(e) {
                            var element = $('#'+e.id);
                            element.css({ 'color': e.colour });
                        });
                        
                        Shiny.addCustomMessageHandler ('hideTab', function(t) {
                            var element = $('li > a[data-value=\"' + t.name + '\"]');
                            element.hide();
                        });
                        
                        Shiny.addCustomMessageHandler ('showTab', function(t) {
                            var element = $('li > a[data-value=\"' + t.name + '\"]');
                            element.show();
                        });
                        
                        Shiny.addCustomMessageHandler('toggleBergmOptions', function(t) {
                            var element = $('.showBergmOptions');
                            element.click(); 
                            console.log('bergm options clicked');
                        });
                        
                        Shiny.addCustomMessageHandler('showOption', function(t) {
                            var opt1 = '.'+t.optName.toLowerCase()+'Options';

                            $(opt1).show();
                        });
                        
                        Shiny.addCustomMessageHandler('hideOption', function(t) {
                            var opt1 = '.'+t.optName.toLowerCase()+'Options';

                            $(opt1).hide();
                        });
                        
                        Shiny.addCustomMessageHandler('updateVectorLen', function(t) {
                            var element = $('#'+t.labName)
                            element.text(t.value)
                        });
                        
                        var outputFormulaBinding = new Shiny.OutputBinding();
                        
                        $.extend(outputFormulaBinding, {
                            find: function(scope) {
                                //console.log('find');
                                return $(scope).find('.formula-output');
                            },
                            
                            getId: function(el) {
                                //console.log('getId');
                                return $(el).attr('id');
                            },
                            
                            renderValue: function(el, data) {
                                //console.log(el);
                                //console.log(data);
                                
                                //$('pre#formulaText > span').off('click');
                                Shiny.setInputValue('formulaText',[]);
                                
                                let terms = data.val;
                                let formulaTags = Array.from(terms, function(d, i) {
                                    return `<span id=fterm${i}>${d}</span>`
                                });
                                formulaTags = formulaTags.join('+');
                                //el.innerHTML = formulaTags;
                                $(el).html(formulaTags.length>0?formulaTags:'Formula...');
                                
                                $('pre#formulaText > span').click( function() {
                                    $(this).toggleClass('toggleCol');
                                });
                            }
                            
                        });
                    
                        Shiny.outputBindings.register(outputFormulaBinding);
                        
                        
                        var inputFormulaBinding = new Shiny.InputBinding();
                        
                        $.extend(inputFormulaBinding, {
                            find: function(scope) {
                                //console.log('find');
                                return $(scope).find('.formula-output');
                            },
                            getValue: function(el) {
                              let res = $('pre#formulaText > span.toggleCol').map( (i, d) => +d.id.slice(5));
                              let resArr = Object.keys(res).filter(d => isFinite(d)).map( i => res[i]);
                              //console.log(resArr.length>0?resArr:[-1]);
                              return resArr.length>0?resArr:[-1];
                            },
                            subscribe: function(el, callback) {
                                $(el).click( function() {
                                    callback();
                                });
                            }
                        
                        });
                        
                        Shiny.inputBindings.register(inputFormulaBinding);
                     ")
            ),
        ),
        
        
        tags$br(),
        fluidRow(
            tags$div(class="simbutton1", 
                column(1, actionButton("goButton", "Estimate", class="btn btn-primary")),
                column(1, actionButton("gofButton", "GOF", class="btn btn-danger btn-block")),
                column(8,
                       tags$pre(id="formulaText", "Formula...", class="formula-output")
                ),
                column(2, actionButton("compareButton","Compare models", class="btn btn-secondary")),
                align="left", height="100%")
        ),
        tags$br(),
        sidebarLayout(
            sidebarPanel(
                wellPanel(class="loaddata", style="position: relative;",
                    selectInput("selData", "Dataset", choices=names(dataset_dict)),
                    fileInput("loadFile", "Add network"),
                    tags$a(class="showdata", "[-]", style="position: absolute; top: 2px; right: 7px;"),
                    tags$span(style="display: none;", "Select data...")
                ),
                #tags$hr(),
                selectInput("selConfig", "Add configuration", choices=names(ergm_configurations)),
                textInput("txtConfig", ""),
                tags$div(id = "cgfPlaceholder"),
                actionButton("addButton", "Add to Formula"),
                checkboxGroupInput("cbGroup",label=""),
                actionButton("delButton", "Remove from Formula")
                
            ),
            mainPanel(
                tabsetPanel(id="tabs",
                    # PLOTS ---------------------------------------------------
                    tabPanel("Network plot",
                             fluidRow(column(12, tableOutput("netCharacteristic"), align="center")),
                             fluidRow(
                                 column(2, tags$hr(), tags$div(class="hashButtons", tags$div(id = "ctgs"))),
                                 #column(10, plotlyOutput("netPlot"))
                                 column(10, d3Output("netPlotD3"))
                             )),
                    # OPTIONS -------------------------------------------------
                    tabPanel("Options",
                             fluidPage(
                                tags$br(),
                                selectInput("simSelect", label="Simulate using:", choices=c("bergm", "evidence"), selected="evidence"),
                                tags$br(),
                                # BERGM OPTIONS -------------------------------
                                wellPanel(class="bergmOptions",
                                    tags$h4("bergm options", 
                                            tags$span("(click to expand)", id="bergmOptionsPane"),
                                            id="bergmOptionsTitle"),
                                    tags$div(id="bergmOptionsPane", style="display: none;",
                                          tags$hr(),
                                          # gamma
                                          fluidRow(
                                              column(4, textInput("gammaOpt", label="gamma", value=0.5)),
                                              column(8, sliderInput("gammaSliderOpt", min=0, max=2, value=0.5, step=0.01, label=""))
                                          ),
                                          # V.proposal
                                          fluidRow(
                                              column(4, textInput("vproposalOpt", label="V.proposal", value=0.0025)),
                                              column(8, sliderInput("vproposalSliderOpt", min=0.001, max=0.007, value=0.0025, step=0.0005, label=""))
                                          ),
                                          # burn.in
                                          fluidRow(
                                              column(4, textInput("burninOpt", label="burn.in", value=100)),
                                              column(8, sliderInput("burninSliderOpt", min=0, max=5000, value=100, step=100, label=""))
                                          ),
                                          # main.iters
                                          fluidRow(
                                              column(4, textInput("mainitersOpt", label="main.iters", value=1000)),
                                              column(8, sliderInput("mainitersSliderOpt", min=100, max=10000, value=1000, step=100, label=""))
                                          ),
                                          # aux.iters
                                          fluidRow(
                                              column(4, textInput("auxitersOpt", label="aux.iters", value=1000)),
                                              column(8, sliderInput("auxitersSliderOpt", min=100, max=10000, value=1000, step=100, label=""))
                                          ),
                                          # # prior.mean
                                          # #id="inputandverbatim",
                                          # fluidRow(
                                          #          column(6, textInput("priormeanOpt", label="prior.mean", value="")),
                                          #          column(6, verbatimTextOutput("priormeanOptTxt", placeholder=TRUE), style="top:20px;")
                                          # ),
                                          # # prior.sigma
                                          # fluidRow(
                                          #   column(6, textInput("priorvarOpt", 
                                          #                       label=HTML("prior.sigma <b>diagonal</b>"), value="")),
                                          #   column(6, verbatimTextOutput("priorvarOptTxt", placeholder=TRUE), style="top: 20px;")
                                          # )
                                          # prior.mean
                                          fluidRow(
                                              column(12, textInput("priormeanOpt", value="", 
                                                                   label=HTML("prior.mean <span id='priormeanLen'></span>")
                                              ))
                                          ),
                                          fluidRow(
                                              column(12, textInput("priorvarOpt", value="",
                                                                   label=HTML("prior.sigma <b>diagonal</b> <span id='priorvarLen'></span>")))
                                          )
                                    )
                                          
                                ),
                                # EVIDENCE OPTIONS ----------------------------
                                wellPanel(class="evidenceOptions", #style="position: relative;", 
                                          # tags$a(class="showEvidenceOptions", "[-]", style="position: absolute; top: 2px; right: 7px;"),
                                          # tags$span(style="display: none;", "evidence options..."),
                                    tags$h4("evidence options", 
                                              tags$span("(click to expand)", id="evidenceOptionsPane"),
                                              id="evidenceOptionsTitle"),
                                    tags$div(id="evidenceOptionsPane", style="display: none;",
                                             tags$hr(),
                                             # aux.iters
                                             # estimate
                                             fluidRow(
                                                 column(4, selectInput("methodEvSelsectOpt", label="evidence.method", choices=c("CJ", "PP"), 
                                                                       selected="CJ"), style="top: 5px;")
                                             ),
                                             fluidRow(
                                                 column(4, textInput("auxitersEvOpt", label="aux.iters", value=1000)),
                                                 column(8, sliderInput("auxitersEvSliderOpt", min=100, max=10000, value=1000, step=100, label=""))
                                             ),
                                             # n.aux.draws
                                             fluidRow(
                                                 column(4, textInput("nauxdrawsEvOpt", label="n.aux.draws", value=5)),
                                                 column(8, sliderInput("nauxdrawsEvSliderOpt", min=1, max=100, value=5, step=5, label=""))
                                             ),
                                             # aux.thin
                                             fluidRow(
                                                 column(4, textInput("auxthinEvOpt", label="aux.thin", value=50)),
                                                 column(8, sliderInput("auxthinEvSliderOpt", min=5, max=300, value=50, step=10, label=""))
                                             ),
                                             # ladder
                                             fluidRow(
                                                 column(4, textInput("ladderEvOpt", label="ladder", value=30)),
                                                 column(8, sliderInput("ladderEvSliderOpt", min=3, max=300, value=30, step=10, label=""))
                                             ),
                                             # main.iters
                                             fluidRow(
                                                 column(4, textInput("mainitersEvOpt", label="main.iters", value=5000)),
                                                 column(8, sliderInput("mainitersEvSliderOpt", min=1000, max=1e5, value=5e3, step=1e3, label=""))
                                             ),
                                             # burn.in
                                             fluidRow(
                                                 column(4, textInput("burninEvOpt", label="burn.in", value=1000)),
                                                 column(8, sliderInput("burninEvSliderOpt", min=1000, max=5e4, value=1e3, step=100, label=""))
                                             ),
                                             # thin
                                             fluidRow(
                                                 column(4, textInput("thinEvOpt", label="thin", value=1)),
                                                 column(8, selectInput("thinEvSelectOpt", choices=divisors(5000), label=""), style="top: 5px;")
                                             ),
                                             # V.proposal
                                             fluidRow(
                                                 column(4, textInput("vproposalEvOpt", label="V.proposal", value=1.5)),
                                                 column(8, sliderInput("vproposalEvSliderOpt", min=1e-3, max=2, value=1.5, step=1e-2, label=""))
                                             ),
                                             # num.samples
                                             fluidRow(
                                                 column(4, textInput("numsamplesEvOpt", label="num.samples", value=25000)),
                                                 column(8, sliderInput("numsamplesEvSliderOpt", min=1e3, max=1e5, value=25000, step=1e3, label=""))
                                             ),
                                             # seed
                                             fluidRow(
                                                 column(4, textInput("seedEvOpt", label="seed", value=1))
                                             ),
                                             # estimate
                                             fluidRow(
                                                 column(4, selectInput("estimateEvSelsectOpt", label="estimate", choices=c("MLE", "CD"), 
                                                                       selected="CD"), style="top: 5px;")
                                             ),
                                             # prior.mean
                                             fluidRow(
                                                 column(12, textInput("priormeanEvOpt", 
                                                                      label=HTML("prior.mean <span id='priormeanEvLen'></span>"), value=""))
                                             ),
                                             fluidRow(
                                                 column(12, textInput("priorvarEvOpt", 
                                                                      label=HTML("prior.sigma <b>diagonal</b> <span id='priorvarEvLen'></span>"), value=""))
                                             )        
                                    )
                                ),
                                # GOF OPTIONS -------------------------------------
                                wellPanel(tags$h4("goodness of fit options",
                                                  tags$span("(click to expand)", id="gofOptionsPane"),
                                                  id="gofOptionsTitle"),
                                          tags$div(id="gofOptionsPane", style="display: none;",
                                                   tags$hr(),
                                                   fluidRow(
                                                       column(4, textInput("ssGofOpt", label="sample.size", value=100)),
                                                       column(8, sliderInput("ssGofSliderOpt", min=50, max=1000, value=100, step=50, label=""))
                                                   ),
                                                   fluidRow(
                                                       column(4, textInput("auxitersGofOpt", label="aux.iters", value=10000)),
                                                       column(8, sliderInput("auxitersGofSliderOpt", min=1000, max=50000, value=10000, step=1e3, label=""))
                                                   ),
                                                   fluidRow(
                                                       column(4, textInput("ndegGofOpt", label="n.deg", value=NULL))
                                                   ),
                                                   fluidRow(
                                                       column(4, textInput("ndistGofOpt", label="n.dist", value=NULL))
                                                   ),
                                                   fluidRow(
                                                       column(4, textInput("nespGofOpt", label="n.esp", value=NULL))
                                                   ),
                                                   fluidRow(
                                                       column(4, textInput("nidegGofOpt", label="n.ideg", value=NULL))
                                                   ),
                                                   fluidRow(
                                                       column(4, textInput("nodegGofOpt", label="n.odeg", value=NULL))
                                                   ),
                                                   # n.
                                          )
                                ),
                                # GENERAL OPTIONS -----------------------------
                                wellPanel(class="generalOptions",
                                          tags$h4("general options", 
                                                  tags$span("(click to expand)", id="generalOptionsPane"),
                                                  id="generalOptionsTitle"),
                                          tags$div(id="generalOptionsPane", style="display: none;",
                                                   tags$hr(),
                                                   checkboxInput("showSummaryBoxplot", label="Show Boxplots in the summary",
                                                                 value=TRUE)
                                                   )
                                         )
                            ),
                             
                        ),
                    # BERGM SUMMARY -------------------------------------------
                    tabPanel("Summary", 
                             tags$br(),
                             verbatimTextOutput("summaryAr"),
                             tags$div(align="center", tableOutput("summaryTable1")),
                             plotlyOutput("summaryBoxplot"),
                             verbatimTextOutput("statusBar")),
                    
                    # BERGM PLOTS ---------------------------------------------
                    tabPanel("Plots",
                             fluidRow(
                                 column(2, h3(HTML("&theta;:")), align="right"),
                                 column(10, selectInput("thetaSel", label="", choices=NULL))
                             ),
                             plotOutput("tracePlot"),
                             plotOutput("densAcPlot")),
                             #plotOutput("ergmPlot"))
                    
                    # BERGM GOF -----------------------------------------------
                    tabPanel("Goodness of fit",
                             tags$br(),
                             plotOutput("gofPlot")
                             ),
                    # COMPARE MODELS ------------------------------------------
                    tabPanel("Model comparison",
                             tags$br(),
                             dataTableOutput("compareTable")
                             )
                )
            )
        ),
        tags$hr(),
        tags$em("Created by Pawel Piela <pawel.piela@ucdconnect.ie> under supervision of Prof.Nial Friel <nial.friel@ucd.ie>, University College Dublin 2021."),
        tags$br(),
        tags$em("Bergm package created by: Alberto Caimo [aut, cre], Lampros Bouranis [aut], Robert Krause [aut] Nial Friel [ctb]")
    )
)
