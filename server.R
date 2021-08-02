#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(Bergm) # main 
library(ergm) # datasets

library(shiny) 
library(r2d3)
library(plotly) # plotting

library(igraph) # nodes layout
library(waiter) # waiting screen
library(rlist) # list.append 
library(coda) # low level mcmc object functions
library(numbers) # divisors of n
library(network)


library(purrr)

source("R/plot_net.R")
source("R/utils.R")

waiting_screen <- tagList(
    spin_wave(),
    h4("Please wait until bergm finishes the estimation...", style="color:white")
)

shinyServer(function(input, output, session) {
    
    #shinyjs::disable("gofButton")
    #shinyjs::disable("compareButton")
    
    shinyjs::runjs("
        $('.showdata').click(function() {
            $('.loaddata >').not($('.showdata')).toggle();
            if ($('.showdata').text() == '[+]') {
                $('.showdata').text('[-]');
            } else {
                $('.showdata').text('[+]');
            }
        });
        
        $('#bergmOptionsTitle').click(function() {
            $(\"[id='bergmOptionsPane']\").toggle();
        });
        
        $('#evidenceOptionsTitle').click(function() {
            $(\"[id='evidenceOptionsPane']\").toggle();
        });
        
        $('#gofOptionsTitle').click(function() {
            $(\"[id='gofOptionsPane']\").toggle();
        });
        
        $('#generalOptionsTitle').click(function() {
            $(\"[id='generalOptionsPane']\").toggle();
        });
    ")
    
    
    # shinyjs::runjs("
    #     $('.showBergmOptions').click(function() {
    #         toggleOptionFun('Bergm');
    #     });
    # ")
    # 
    # shinyjs::runjs("
    #     $('.showEvidenceOptions').click(function() {
    #         toggleOptionFun('Evidence');
    #     });
    # ")
    
    # JS MESSAGE HANDLERS -----------------------------------------------------
    
    changeTxtColour <- function(id, colour) {
        session$sendCustomMessage(type='changeTxtColour', message=list('id'=id, 'colour'=colour))
    }
    
    hideTabJS <- function(name) {
        session$sendCustomMessage(type='hideTab', message=list('name'=name))
    }
    
    showTabJS <- function(name) {
        session$sendCustomMessage(type='showTab', message=list('name'=name))
    }
    
    toggleBergmOpts <- function() {
        session$sendCustomMessage(type="toggleBergmOptions", message=list())
    }
    
    showOption <- function(name) {
        session$sendCustomMessage(type="showOption", message=list(optName=name))
    }
    
    hideOption <- function(name) {
        session$sendCustomMessage(type="hideOption", message=list(optName=name))
    }
    
    updateVectorLen <- function(labName, value) {
        message = list(labName=labName, value=paste0('(length=',value,')'))
        session$sendCustomMessage(type="updateVectorLen", message=message)
    }
    
    ###
    
    
    #hideTab(inputId="tabs", target="Summary")
    hideTabJS("Summary")
    #hideTab(inputId="tabs", target="Plots")
    hideTabJS("Plots")
    hideTabJS("Goodness of fit")
    hideTabJS("Model comparison")
    
    dataset_dict <- get_datasets()
    current_observers <- list()
    formula_observers <- list()
    current_cats <- NULL # categories of vertices in currently selected graph
    
    loadenvironment <- new.env() # new environment for loaded data
    
    formula_terms <- reactiveValues(terms=list(), selected=list())
    
    y <- reactiveValues(data=lazega, G=NA)
    new_formula_part <- reactiveValues(txt="")
    new_arg <- reactiveValues(val=list())
    simulation <- reactiveValues(result=NA)
    bergm_parameters <- reactiveValues(gamma=0.5, V.proposal=0.0025)
    evidence_parameters <- reactiveValues()
    gof_parameters <- reactiveValues()
    
    # data frame used to store compared models
    compareDf <- data.frame()
    
    # LOAD FILE
    
    observeEvent(input$loadFile, {
        infile <- input$loadFile
        tryCatch({
            load(infile$datapath, loadenvironment)
            showNotification("Network file loaded.", type="message", duration=10)
        }, error = function(cond) {
            showNotification("Incorrect file.", type="error", duration=NULL)
        })
        
        dataset_dict <<- get_datasets(loadenvironment)
        
        updateSelectInput(session, "selData", choices=dataset_dict)
    })
    
    
    # observe new terms in the formula and update the formula expression
    # If less than 2 dimensions selected, disable the "simulation" button
    ergm_formula_str <- reactive({
        
        formula_str <- paste('y$data', paste(formula_terms$terms, collapse=" + "), sep=" ~ ")
        
        formula_str
    })
    
    observeEvent(input$formulaText, {
        
        #print(input$formulaText)
        
        if (input$formulaText[1] == -1) {
            formula_terms$selected <<- c()
        } else {
            formula_terms$selected <<- input$formulaText   
        }
        
    })
    
    # add terms to the formula, activate the "remove" button
    observeEvent(input$addButton, {
        if (!is.null(input$txtConfig) & (input$txtConfig != "")) {
            formula_terms$terms[[length(formula_terms$terms) + 1]] <- input$txtConfig
            updateCheckboxGroupInput(session, "cbGroup", choices=formula_terms$terms)
            shinyjs::show("delButton")
            #shinyjs::disable("compareButton")
        }
        formula_terms$selected <<- c()
    })
    
    output$netCharacteristic <- renderTable({
        summary_data <- t(unlist(y$data$gal[c("n", "mnext", "directed", "hyper", "loops", "multiple", "bipartite")]))
        colnames(summary_data)[1:2] <- c("nvertices", "nedges")
        summary_data[1:2] <- c(length(y$data$val), network.edgecount(y$data))
        summary_data
    }, digits=0)
    
    # perform simulation
    # if unsuccessfull return the error message
    observeEvent(input$goButton, {
        er <- as.formula(ergm_formula_str()) # convert formula from character to formula object
        
        if(!is.atomic(er)) {
            waiter_show(html=waiting_screen)
            res <- tryCatch({
                if (input$simSelect == "bergm") {
                    # list of bergm parameters fetched from the options tab
                    bergm_params_call <- isolate(reactiveValuesToList(bergm_parameters))
                    bergm_params_call$formula <- er
                    #print(bergm_params_call)
                    do.call("bergm", args=bergm_params_call)
                } else if (input$simSelect == "evidence") {
                    # list of bergm parameters fetched from the options tab
                    ev_params_call <- isolate(reactiveValuesToList(evidence_parameters))
                    ev_params_call$formula <- er
                    print(ev_params_call)
                    do.call("evidence", args=ev_params_call)
                }

            }, error = function(cond) {
                waiter_hide() 
                showModal(modalDialog(
                    title = "Bergm Warning", cond$message, easyClose = TRUE
                ))
                return(NA)
            })
            #waiter_hide()   
        }
        
        if (!is.atomic(res)) {
            # update the list of available statistics in the plot tab
            idx_choices <- 1:nvar(res$Theta)
            names(idx_choices) <- names(res$ess)
            updateSelectInput(session, "thetaSel", choices=idx_choices)
            
            showTabJS("Summary")
            showTabJS("Plots")
            
            # update and go to the summary tab
            updateSummary(res)
            updateTabsetPanel(session, inputId="tabs", selected="Summary")
        }
        
        # update bergm results
        simulation$result <<- res
        shinyjs::enable("gofButton");
    })
    
    # GOF BUTTON ACTION -------------------------------------------------------
    observeEvent(input$gofButton, {
        if(!is.atomic(simulation$result)) {
            waiter_show(html=waiting_screen)
            res <- tryCatch({
                gof_params_call <- isolate(reactiveValuesToList(gof_parameters))
                gof_params_call$x <- simulation$result
                output$gofPlot <- renderPlot({
                    do.call("bgof", args=gof_params_call)  
                    waiter_hide()
                })
            }, error = function(cond) {
                waiter_hide() 
                showModal(modalDialog(
                    title = "Bergm Warning", cond$message, easyClose = TRUE
                ))
                return(NA)
            })
               
        }
        
        showTabJS("Goodness of fit")
        updateTabsetPanel(session, inputId="tabs", selected="Goodness of fit")
        
        # update bergm results
        simulation$gofResult <<- res
    })
    
    # COMPARE BUTTON ACTION ---------------------------------------------------
    observeEvent(input$compareButton, {
        if (length(formula_terms$selected)==0 | length(formula_terms$terms)<3) {
            message = "Select term(s) by clicking them on the formula...\nMake sure you added at least 3 terms to the formula"
            showModal(modalDialog(
                title = "Compare models", message, easyClose = TRUE
            ))
        } else {
            compare_waiting_screen <- Waitress$new(theme="overlay-percent")$start()
            
            er <- as.formula(ergm_formula_str()) # convert formula from character to formula object
            
            #fterms <- c('gwesp(decay=0.5, fixed=TRUE)', 'edges', 'nodecov(attr="Practice")')
            #selTer <- c(0, 1)
            #er <- as.formula('lazega ~ gwesp(decay=0.5, fixed=TRUE) + edges + nodecov(attr="Practice")')
            
            res0 <- tryCatch({
                ev_params_call <- isolate(reactiveValuesToList(evidence_parameters))
                ev_params_call$formula <- er
                do.call("evidence", args=ev_params_call)
            }, error = function(cond) {
                showModal(modalDialog(
                    title = "Model Comparison Warning", cond$message, easyClose = TRUE
                ))
                return(NA)
            })
            
            if (is.atomic(res0)) {
                compare_waiting_screen$close()
                showModal(modalDialog(
                    title = "Model Comparison Error",
                    "The simulation for this formula cannot converge...", 
                    easyClose = TRUE
                ))
                return()
            }
            
            compareDf[1,1] <- "none"
            compareDf[1,2] <- res0$log.evidence
            compareDf[1,3] <- res0$AR
            res0Vec <- apply(res0$Theta, 2, mean)
            compareDf[1,4:(4+length(res0Vec) -1 )] <- res0Vec
            colnames(compareDf) <-  c("omitted term", "log.evidence", "AR", res0$specs)
            
            compare_waiting_screen$inc(100/(length(formula_terms$selected)+1))
            
            for (i in formula_terms$selected) {
                partialTerms <- formula_terms$terms[-(i+1)]
                partialFormula <- paste('y$data', paste(partialTerms, collapse=" + "), sep=" ~ ")
                #print(formula_terms$terms[i+1])
                partialEr <- as.formula(partialFormula)
                
                res <- tryCatch({
                    ev_params_call <- isolate(reactiveValuesToList(evidence_parameters))
                    ev_params_call$formula <- partialEr
                    do.call("evidence", args=ev_params_call)
                }, error = function(cond) {
                    showModal(modalDialog(
                        title = "Model Comparison Warning", cond$message, easyClose = TRUE
                    ))
                    return(NA)
                })
                
                nextRowIdx <- nrow(compareDf) + 1
                compareDf[nextRowIdx, 1] <- formula_terms$terms[i+1]
                
                if (!is.atomic(res)) {
                    compareDf[nextRowIdx, 2] <- res$log.evidence
                    compareDf[nextRowIdx, 3] <- res$AR
                    
                    resVec <- apply(res$Theta, 2, mean)
                    compareDf[nextRowIdx, res$specs] <- resVec   
                }
                
                compare_waiting_screen$inc(100/(length(formula_terms$selected)+1))
            }
            
            compare_waiting_screen$close()
            output$compareTable <- renderDataTable(compareDf)
            showTabJS("Model comparison")
            updateTabsetPanel(session, inputId="tabs", selected="Model comparison")
            
        }
    })
    
    
    # plot coda plots for selected theta
    observeEvent(input$thetaSel, {
        idx <- as.integer(input$thetaSel)
        x <- simulation$result
        
        
        output$tracePlot <- renderPlot({
            par(mfrow=c(1, 1), oma = c(0, 0, 0, 0), mar = c(4, 3, 1.5, 1), cex=.5)
            coda::traceplot(x$Theta[,idx], type = "l", xlab = "Iterations", ylab = "")
            }, res=200)
        
        output$densAcPlot <- renderPlot({
            par(mfrow=c(1, 2), oma = c(0, 0, 3, 0), mar = c(4, 3, 1.5, 1), cex=.5)
            plot(density(x$Theta[, idx]), 
                 main = "", 
                 axes = TRUE, 
                 xlab = bquote(paste(theta[.(idx)], " (", .(x$specs[idx]), ")")),
                 ylab = "", lwd = 2)
            coda::autocorr.plot(x$Theta[, idx], lag.max=100, auto.layout = FALSE)
        }, res=200)
        
    })
    
    # update formula at the top of the screen
    # output$ergmFormula <- renderText({
    #     ergm_formula_str()$formula_html
    # })
    
    output$formulaText <- ({ function() { 
        if (length(formula_terms$terms) < 2) {
            shinyjs::disable("goButton")
            shinyjs::disable("compareButton")
        } else {
            shinyjs::enable("goButton")
            shinyjs::enable("compareButton")
        }
        
        list(val=formula_terms$terms) 
    }
    });

    
    updateSummary <- function(bergm_results) {
        x <- bergm_results
    
        output$summaryAr <- renderPrint( cat("Acceptance rate:", x$AR) )
        
        # Alberto Caimo
        Theta <- as.mcmc(x$Theta)
        quantiles <- c(0.025, 0.25, 0.5, 0.75, 0.975)
        
        statnames <- c("Mean", "SD", "Naive SE", "Time-series SE")
        varstats <- matrix(nrow = nvar(Theta), ncol = length(statnames), 
                           dimnames = list(varnames(Theta), statnames))
        
        Thetavar <- apply(Theta, 2, var)
        Thetatsvar <- apply(Theta, 2, function(x) coda::spectrum0.ar(x)$spec)
        varquant <- t(apply(Theta, 2, quantile, quantiles))
        
        varstats[, 1] <- apply(Theta, 2, mean)
        varstats[, 2] <- sqrt(Thetavar)
        varstats[, 3] <- sqrt(Thetavar / niter(Theta))
        varstats[, 4] <- sqrt(Thetatsvar / niter(Theta))
        ###
        
        rownames(varstats) <- sapply(1:nrow(varstats), function(i) glue::glue("<b>{names(x$ess)[i]}</b> (&#x1D703;<sub>{i}</sub>)"))
        output$summaryTable1 <- renderTable(drop(varstats), rownames=TRUE, digits=6, align="c", sanitize.text.function = function(x) x)
        
        # BOXPLOT
        output$summaryBoxplot <- renderPlotly({
            if (input$showSummaryBoxplot) {
                p <- plot_ly()
                p_mat <- as.mcmc(x$Theta)
                
                for (ic in 1:ncol(p_mat)) {
                    xdata <- p_mat[,ic]
                    p <- add_trace(p, x = xdata, name=glue::glue('&#x1D703;<sub>{ic}</sub>'), type="box")
                    p <- p %>% layout(yaxis = list(autorange = "reversed"))
                }
                waiter_hide()
                return(p)
            }
            waiter_hide()
            NULL
        })
    }
    
    # choose the data set
    observeEvent(input$selData, {
        
        shinyjs::disable("gofButton");
        hideTabJS("Summary")
        hideTabJS("Plots")
        hideTabJS("Goodness of fit")
        
        if (input$selData %in% ls(loadenvironment)) {
            y$data <<- get(input$selData, envir = loadenvironment)
        } else {
            suppressWarnings(data(list=c(dataset_dict[input$selData]), overwrite=FALSE))
            y$data <<- get(input$selData)
        }
        
        y$name <<- input$selData
        y$categories <<- list.append("NULL", extract_categories(y$data))

        ## update choices based on the ergm_configurations and y$data$gal
        allowed_configs <- c()
        
        ## determine applicable statistics for the selected dataset
        for (i in 1:length(ergm_configurations)) {
            
            conf_ <- ergm_configurations[[i]]
            elig <- conf_[[4]]
            
            relig <- c(ifelse(!is.null(y$data$gal$directed), y$data$gal$directed,FALSE),
                       ifelse(!is.null(y$data$gal$hyper), y$data$gal$hyper, FALSE),
                       ifelse(!is.null(y$data$gal$loops), y$data$gal$loops, FALSE),
                       ifelse(!is.null(y$data$gal$multiple), y$data$gal$multiple, FALSE),
                       ifelse(!is.null(y$data$gal$bipartite), y$data$gal$bipartite, FALSE))
                       
            relig <- as.numeric(relig)
            
            
            elig[which(is.na(elig))] <- relig[which(is.na(elig))]
            
            if (all(elig==relig)) {
                allowed_configs <- c(allowed_configs, names(ergm_configurations)[i])
            }   
            
        }
        
        updateSelectInput(session, "selConfig", choices=allowed_configs)

        
        formula_terms$terms <<- list()
        
        ## remove previous observers
        if (length(current_observers) > 0) {
            for (i in 1:length(current_observers)) {
                (current_observers[[i]])$destroy()
            }
            current_observers <- list()
        }
        
        # remove previous categories
        if(length(current_cats)>0) {
            for (cn in current_cats) {
                removeUI(
                    selector = paste0("#ctg",cn)
                )
            }
        }
        
        category_names <- extract_categories(y$data)
        y$G <<- ifelse(length(category_names)>0,category_names[1],NA)
        
        if(length(category_names)>0) {
            
            lapply(1:length(category_names), function(i) {
                cn <- category_names[i]
                
                insertUI(
                    selector = "#ctgs",
                    where = "afterEnd",
                    ui = actionButton(paste0("ctg",cn), cn)
                )
                
                current_observers[[i]] <<- observeEvent({input[[paste0("ctg",cn)]]}, {
                    y$G <- cn
                })
            })
        }
        current_cats <<- category_names
        
        remove_formula_terms()
        updateTabsetPanel(session, inputId="tabs", selected="Network plot")
        
    })
    
    remove_formula_terms <- function() {
        formula_terms$terms <<- list()
        
        removeUI(selector="#cbGroup >")
        
        if(length(formula_observers)>0) {
            lapply(1:length(formula_observers), function(i) { 
                formula_observers[[i]]$destroy() 
            })
        }
        
        shinyjs::hide("delButton")
    }
    
    output$netPlot <- renderPlotly({
         plot_net(y$data, y$name, current_cats, y$G)
    })
    
    output$netPlotD3 <- renderD3({
        net <- y$data
        
        edges <- network::as.edgelist(net)
        colnames(edges) <- c("source", "target")
        df_edges <- as.data.frame(edges)
        
        vid <- 1:net$gal$n
        
        cov_list <- list()
        categories <- current_cats
        
        if (length(categories) > 0 ){
            for (icat in 1:length(categories)) {
                cov_list[[icat]] <- unlist(lapply(net$val, function(x) x[categories[icat]]))
            }
        }

        df_cov <- as.data.frame(cov_list)
        colnames(df_cov) <- categories
        if (nrow(df_cov) > 0) {
            df_cov[,'id'] <- vid
        } else {
            df_cov <- as.data.frame(list(id=vid))
        }
        
        r2d3(
            data=list(edges=jsonlite::toJSON(df_edges, dataframe="rows"), 
                      vattr=jsonlite::toJSON(df_cov, dataframe="rows")),
            options = list(category=y$G,
                           directed=network::is.directed(net)),
            script = "script.js"
        )
    })
    
    # select new statistic
    observeEvent(input$selConfig, {
        config_data <- ergm_configurations[[input$selConfig]]
        selected_config <- prepare_config(config_data)
        new_formula_part$txt <<- selected_config$str_out
        
        updateTextInput(session, "txtConfig", value=new_formula_part$txt)
        removeUI(selector="div.form-group:has([id^=confsel])", multiple=TRUE)
        
        if(length(formula_observers)>0) {
            lapply(1:length(formula_observers), function(i) { 
                formula_observers[[i]]$destroy() 
            })
        }
        
        formula_observers$current <<- c()
        
        if (length(selected_config$controls) > 0) {
            lapply(1:length(names(selected_config$controls)), function(i) {
                new_arg$val <<- list()
                
                def_val <- config_data$default_vals[i]
                
                insertUI(
                    selector = "#cgfPlaceholder",
                    where = "beforeEnd",
                    ui = switch(selected_config$controls[i],
                                "{STRING}"=textInput(paste0("confselstr",i,input$selConfig),
                                                     paste0(names(selected_config$controls)[i],"="), value=def_val),
                                "{NUMBER}"=textInput(paste0("confselinp",i,input$selConfig), 
                                                     paste0(names(selected_config$controls)[i],"="), value=def_val),
                                "{BOOL}"=selectInput(paste0("confselbool",i,input$selConfig), 
                                                     names(selected_config$controls)[i], 
                                                     c("TRUE", "FALSE"), selected=def_val),
                                "{VERTEX}"=selectInput(paste0("confselver",i,input$selConfig), 
                                                       "Vertex attribute", y$categories),

                    )
                )
                
                input_label <- switch(selected_config$controls[i],
                                      "{NUMBER}"=paste0("confselinp",i,input$selConfig),
                                      "{BOOL}"=paste0("confselbool",i,input$selConfig),
                                      "{VERTEX}"=paste0("confselver",i,input$selConfig),
                                      "{STRING}"=paste0("confselstr",i,input$selConfig))
                                                           
                new_form_obs <- observeEvent(input[[input_label]], {
                    a_name <- names(selected_config$controls)[i]
                    a_val <- input[[input_label]]
                    
                    new_arg$val[a_name] <<- a_val
                    
                    new_formula_part$txt <<- prepare_config(config_data, replace_args=new_arg$val)$str_out
                    
                })
                
                formula_observers[[length(formula_observers) + 1]] <<- new_form_obs
                
            })
        }
        
    })
    
    observe({
        updateTextInput(session, "txtConfig", 
                        value=new_formula_part$txt)
    })
    
    observeEvent(input$delButton, {
        if(length(input$cbGroup)>0){
            for(t in input$cbGroup) {
                for(i in 1:length(formula_terms$terms)) {
                    if( t == formula_terms$terms[i]) {
                        formula_terms$terms <- formula_terms$terms[-i]
                        break
                    }
                }
            }
        }
        updateCheckboxGroupInput(session, "cbGroup", choices=formula_terms$terms)
        
        if(length(formula_terms$terms)==0) shinyjs::hide("delButton")
    })
    
    ## OPTIONS ----------------------------------------------------------------
    
    # Select method
    
    observeEvent(input$simSelect, {
        opt_methods_list <- c("Bergm", "Evidence")
        methods_list_aux <- c("bergm", "evidence")
        idx <- which( methods_list_aux == input$simSelect)
        showOption(input$simSelect)
        for (dont_show in opt_methods_list[-idx]) {
            hideOption(dont_show)
        }
    })
    
    # BERGM
    
    # gamma
    observeEvent(input$gammaSliderOpt, {
        updateTextInput(session, "gammaOpt", value=input$gammaSliderOpt)
        #bergm_parameters$gamma <<- input$gammaSliderOpt
    })
    
    observeEvent(input$gammaOpt, {
        bergm_parameters$gamma <<- as.numeric(input$gammaOpt)
    })
    
    # V.proposal
    observeEvent(input$vproposalSliderOpt, {
        updateTextInput(session, "vproposalOpt", value=input$vproposalSliderOpt)
    })
    
    observeEvent(input$vproposalOpt, {
        bergm_parameters$V.proposal <<- as.numeric(input$vproposalOpt)
    })
    
    # burn.in
    observeEvent(input$burninSliderOpt, {
        updateTextInput(session, "burninOpt", value=input$burninSliderOpt)
    })
    
    observeEvent(input$burninOpt, {
        bergm_parameters$burn.in <<- as.numeric(input$burninOpt)
    })
    
    # main.iters
    observeEvent(input$mainitersSliderOpt, {
        updateTextInput(session, "mainitersOpt", value=input$mainitersSliderOpt)
    })
    
    observeEvent(input$mainitersOpt, {
        bergm_parameters$main.iters <<- as.numeric(input$mainitersOpt)
    })
    
    # aux.iters
    observeEvent(input$auxitersSliderOpt, {
        updateTextInput(session, "auxitersOpt", value=input$auxitersSliderOpt)
    })
    
    observeEvent(input$auxitersOpt, {
        bergm_parameters$aux.iters <<- as.numeric(input$auxitersOpt)
    })
    
    
    # prior.mean
    observeEvent(input$priormeanOpt, {
        regex_express <- "[+-]?(?:[0-9]*[.])?[0-9]+" # parse floats
        
        if (stringr::str_detect(input$priormeanOpt, regex_express)) {
            changeTxtColour("priormeanOpt", "green")
            matched_floats <- stringr::str_match_all(input$priormeanOpt, regex_express)[[1]]
            bergm_parameters$prior.mean <- as.numeric(matched_floats)
            #output$priormeanOptTxt <- renderText({paste(matched_floats, collapse = ", ")})
            updateVectorLen("priormeanLen", length(matched_floats))
        } else {
            bergm_parameters$prior.mean <- NULL
            #output$priormeanOptTxt <- renderText({ "invalid vector" })
            updateVectorLen("priormeanLen", 0)
            changeTxtColour("priormeanOpt", "red")
        }
    })
    
    # prior.sigma
    observeEvent(input$priorvarOpt, {
        regex_express <- "[+-]?(?:[0-9]*[.])?[0-9]+" # parse floats
        
        if (stringr::str_detect(input$priorvarOpt, regex_express)) {
            changeTxtColour("priorvarOpt", "green")
            matched_floats <- stringr::str_match_all(input$priorvarOpt, regex_express)[[1]]
            bergm_parameters$prior.sigma <- diag(as.numeric(matched_floats))
            #output$priorvarOptTxt <- renderText({paste(matched_floats, collapse = ", ")})
            updateVectorLen("priorvarLen", length(matched_floats))
         } else {
            bergm_parameters$prior.sigma <- NULL
            #output$priorvarOptTxt <- renderText({ "invalid vector" })
            updateVectorLen("priorvarLen", 0)
            changeTxtColour("priorvarOpt", "red")
        }
    })
    
    # EVIDENCE OPTIONS
    
    # aux.iters
    observeEvent(input$auxitersEvSliderOpt, {
        updateTextInput(session, "auxitersEvOpt", value=input$auxitersEvSliderOpt)
    })
    
    observeEvent(input$auxitersEvOpt, {
        #evidence_parameters$aux.iters <<- as.numeric(input$auxitersEvSliderOpt)
        evidence_parameters$aux.iters <<- as.numeric(input$auxitersEvOpt)
    })
    
    # n.aux.draws
    observeEvent(input$nauxdrawsEvSliderOpt, {
        updateTextInput(session, "nauxdrawsEvOpt", value=input$nauxdrawsEvSliderOpt)
    })
    
    observeEvent(input$nauxdrawsEvOpt, {
        evidence_parameters$n.aux.draws <<- as.numeric(input$nauxdrawsEvOpt)
    })
    
    # aux.thin
    observeEvent(input$auxthinEvSliderOpt, {
        updateTextInput(session, "auxthinEvOpt", value=input$auxthinEvSliderOpt)
    })
    
    observeEvent(input$auxthinEvOpt, {
        evidence_parameters$aux.thin <<- as.numeric(input$auxthinEvOpt)
    })
    
    # ladder
    observeEvent(input$ladderEvSliderOpt, {
        updateTextInput(session, "ladderEvOpt", value=input$ladderEvSliderOpt)
    })
    
    observeEvent(input$ladderEvOpt, {
        evidence_parameters$ladder <<- as.numeric(input$ladderEvOpt)
    })
    
    # main.iters
    observeEvent(input$mainitersEvSliderOpt, {
        updateTextInput(session, "mainitersEvOpt", value=input$mainitersEvSliderOpt)
    })
    
    observeEvent(input$mainitersEvOpt, {
        evidence_parameters$main.iters <<- as.numeric(input$mainitersEvOpt)
        updateSelectInput(session, "thinEvSelectOpt", choices=divisors(as.numeric(input$mainitersEvOpt)))
        updateSliderInput(session, "numsamplesEvSliderOpt", max=as.numeric(input$mainitersEvOpt) - as.numeric(input$burninEvOpt))
    })
    
    # burn.in
    observeEvent(input$burninEvSliderOpt, {
        updateTextInput(session, "burninEvOpt", value=input$burninEvSliderOpt)
    })
    
    observeEvent(input$burninEvOpt, {
        evidence_parameters$burn.in <<- as.numeric(input$burninEvOpt)
        updateSliderInput(session, "numsamplesEvSliderOpt", max=as.numeric(input$mainitersEvOpt) - as.numeric(input$burninEvOpt))
    })
    
    # thin
    observeEvent(input$thinEvSelectOpt, {
        updateTextInput(session, "thinEvOpt", value=input$thinEvSelectOpt)
    })
    
    observeEvent(input$thinEvOpt, {
        evidence_parameters$thin <<- as.numeric(input$thinEvOpt)
    })
    
    # V.proposal
    observeEvent(input$vproposalEvSliderOpt, {
        updateTextInput(session, "vproposalEvOpt", value=input$vproposalEvSliderOpt)
    })
    
    observeEvent(input$vproposalEvOpt, {
        evidence_parameters$V.proposal <<- as.numeric(input$vproposalEvOpt)
    })
    
    # num.samples
    observeEvent(input$numsamplesEvSliderOpt, {
        updateTextInput(session, "numsamplesEvOpt", value=input$numsamplesEvSliderOpt)
    })
    
    observeEvent(input$numsamplesEvOpt, {
        evidence_parameters$num.samples <<- as.numeric(input$numsamplesEvOpt)
    })
    
    # seed
    observeEvent(input$seedEvOpt, {
        evidence_parameters$seed <<- as.numeric(input$seedEvOpt)
    })
    
    # estimate
    observeEvent(input$estimateEvSelsectOpt, {
        evidence_parameters$estimate <<- input$estimateEvSelsectOpt
    })
    
    # prior.mean
    observeEvent(input$priormeanEvOpt, {
        regex_express <- "[+-]?(?:[0-9]*[.])?[0-9]+" # parse floats
        
        if (stringr::str_detect(input$priormeanEvOpt, regex_express)) {
            changeTxtColour("priormeanEvOpt", "green")
            matched_floats <- stringr::str_match_all(input$priormeanEvOpt, regex_express)[[1]]
            evidence_parameters$prior.mean <- as.numeric(matched_floats)
            updateVectorLen("priormeanEvLen", length(matched_floats))
        } else {
            evidence_parameters$prior.mean <- NULL
            updateVectorLen("priormeanEvLen", 0)
            changeTxtColour("priormeanEvOpt", "red")
        }
    })
    
    # prior.sigma
    observeEvent(input$priorvarEvOpt, {
        regex_express <- "[+-]?(?:[0-9]*[.])?[0-9]+" # parse floats
        
        if (stringr::str_detect(input$priorvarEvOpt, regex_express)) {
            changeTxtColour("priorvarEvOpt", "green")
            matched_floats <- stringr::str_match_all(input$priorvarEvOpt, regex_express)[[1]]
            evidence_parameters$prior.sigma <- diag(as.numeric(matched_floats))
            updateVectorLen("priorvarEvLen", length(matched_floats))
        } else {
            evidence_parameters$prior.sigma <- NULL
            updateVectorLen("priorvarEvLen", 0)
            changeTxtColour("priorvarEvOpt", "red")
        }
    })
    
    # GOF OPTIONS -------------------------------------------------------------
    
    # sample.size
    observeEvent(input$ssGofSliderOpt, {
        updateTextInput(session, "ssGofOpt", value=input$ssGofSliderOpt)
    })
    
    observeEvent(input$ssGofOpt, {
        gof_parameters$sample.size <<- as.numeric(input$ssGofSliderOpt)
    })
    
    # aux.iters
    observeEvent(input$auxitersGofSliderOpt, {
        updateTextInput(session, "auxitersGofOpt", value=input$auxitersGofSliderOpt)
    })
    
    observeEvent(input$auxitersGofOpt, {
        gof_parameters$aux.iters <<- as.numeric(input$auxitersGofOpt)
        #updateSliderInput(session, "auxitersGofSliderOpt", value=input$auxitersGofOpt)
    })
    
    # n.deg
    observeEvent(input$ndegGofOpt, {
        paramVal <- as.numeric(input$ndegGofOpt)
        if (is.na(paramVal)) paramVal = NULL;
        gof_parameters$n.deg <<- paramVal
    })
    
    # n.dist
    observeEvent(input$ndistGofOpt, {
        paramVal <- as.numeric(input$ndistGofOpt)
        if (is.na(paramVal)) paramVal = NULL;
        gof_parameters$n.dist <<- paramVal
    })
    
    # n.esp
    observeEvent(input$nespGofOpt, {
        paramVal <- as.numeric(input$nespGofOpt)
        if (is.na(paramVal)) paramVal = NULL;
        gof_parameters$n.esp <<- paramVal
    })
    
    # n.ideg
    observeEvent(input$nidegGofOpt, {
        paramVal <- as.numeric(input$nidegGofOpt)
        if (is.na(paramVal)) paramVal = NULL;
        gof_parameters$n.ideg <<- paramVal
    })
    
    # n.odeg
    observeEvent(input$nodegGofOpt, {
        paramVal <- as.numeric(input$nodegGofOpt)
        if (is.na(paramVal)) paramVal = NULL;
        gof_parameters$n.odeg <<- paramVal
    })
    
    
})
