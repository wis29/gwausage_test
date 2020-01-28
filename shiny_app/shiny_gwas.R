library( shinydashboard )
library( shiny )
library( dplyr )
library( leaflet )
library( dashboardthemes )
library( data.table )
library( DECIPHER )
library( RColorBrewer )
library( ggplot2 )
library( plotly )
library( shinyWidgets )
library( shinycssloaders )
library( MASS )
library( qqman )
library( manhattanly )
library( haploR )




#### PATH #### 
for( i in 1 ){
  
  setwd( getwd() )
  f.gwas <- getwd( )
  f.p.gwas <- paste( f.gwas , "/program/" , sep = "" )
  f.p.plink <- paste( f.p.gwas , "/plink/" , sep = "" )
  f.p.gcta <- paste( f.p.gwas , "/gcta/bin/" , sep = "" )
  f.i.gwas <- paste( f.gwas , "/input/" , sep = "" )
  f.r.gwas <- paste( f.gwas , "/result/" , sep = "" )
  f.r.plots <- paste( f.r.gwas , "plot/" , sep = "" )
  f.temp <- paste( f.gwas , "/temp/" , sep = "" )
  
  
  
  input.pheno <- read.csv( "input/phenotypes.csv" , header = F )
  input.cov.d <- read.csv( "input/discrete_covariates.csv" , header = F )
  input.cov.c <- read.csv( "input/continuous_covariates.csv" , header = F )
  
  
}

## UI HEADER #### 
for( i in 1 ){
  header <- dashboardHeader(title = "GWASuage", 
                            tags$li(a(
                              img(src = 'GWAusageLogo.png',
                                  title = "GWASuage", height = "40px"),
                              style = "padding-top:10px; padding-bottom:10px;"),
                              class = "dropdown"))
}

#### UI SIDEBAR #### 
for( i in 1 ){
  sidebar <- dashboardSidebar( 
    sidebarMenu( id = 'sidebarmenu',
                 # textOutput("currentTime"),
                 menuItem( text = "GWAS Input Files" ,
                           tabName = "side1" , icon = icon('flask') ),
                 menuItem( text = "GWAS Analysis" ,
                           tabName = "side2" , icon = icon('atom') ),
                 menuItem( text = "GWAS Results" ,
                           tabName = "side3" , icon = icon('hand-peace') ), 
                 menuItem( text = "GWAS Mining" ,
                           tabName = "side4" , icon = icon('newspaper') ) ) )
  
}

#### UI BODY #### 
for( i in 1 ){
  body <- dashboardBody(
    
    shinyDashboardThemes( theme = "blue_gradient" ),

    ### FIRST Tab ###
    tabItems(
      ### Input tab ###                   
      tabItem( tabName = "side1",
               
               h3( em( "1: Load input files" ) ),
               hr(),
               
               tabsetPanel( type = "tabs",
                            
                   tabPanel( "Phenotype table",
                        
                        hr(),
                        em( tags$strong( "Save phenotype table into input/ folder as phenotype.txt" ) ),     
                        fluidRow(
                        hr(),
                 
                        column( width = 12,
                                 
                                 box( width = NULL,  solidHeader = TRUE,  status = "info", 
                                      
                                      
                                      actionButton( inputId = 'Load1' , label = "Load exple" , icon = icon( 'rocket' ) , style='padding-top:20px; padding-bottom:50px; font-size:140%' ),
                                      tags$hr(),
                                      # output$geneTab
                                      DT::dataTableOutput( "phenoTable" ) ) ) ) ),
               
                      tabPanel( "Discrete Covariate table",
                                
                         hr(),        
                         em( tags$strong( "Save discrete covariate table table into input/ folder as disc_cov.txt" ) ),
                         fluidRow(
                         hr(),
                 
                         column( width = 12,
                                 
                                 box( width = NULL,  solidHeader = TRUE,  status = "info", 
                                      
                                      
                                      actionButton( inputId = 'Load2' , label = "Load exple" , icon = icon( 'rocket' ) , style='padding-top:20px; padding-bottom:50px; font-size:140%' ),
                                      tags$hr(),
                                      # output$geneTab
                                      DT::dataTableOutput( "d.var.Table" ) ) ) ) ),
                   
                   tabPanel( "Continuous Covariate tables",
                             
                             hr(),
                             em( tags$strong( "Save continuous covariate table into input/ folder as cont_cov.txt" ) ),
                             fluidRow(
                               hr(),
                               
                               column( width = 12,
                                       
                                       box( width = NULL,  solidHeader = TRUE,  status = "info", 
                                            
                                            
                                            actionButton( inputId = 'Load3' , label = "Load exple" , icon = icon( 'rocket' ) , style='padding-top:20px; padding-bottom:50px; font-size:140%' ),
                                            tags$hr(),
                                            # output$geneTab
                                            DT::dataTableOutput( "c.var.Table" ) ) ) ) )
               ) ),
      
      ### Result tab ###                   
      tabItem( tabName = "side2",
               
               h3( em( "2: Start analysis" ) ),
               hr(),
    
                                      
                                      fluidRow(
                                        hr(),
                                        
                                        column( width = 6,
                                                em( tags$strong( "A: Format files - Filtering data - Regression analysis" ) ),
                                                br(),
                                                box( width = NULL,  solidHeader = TRUE,  status = "info", 
                                                     actionButton( inputId = 'Load4' , label = "Take-off" , icon = icon( 'rocket' ) , style='padding-top:20px; padding-bottom:50px; font-size:140%' ),
                                                     tags$hr() ),
                            
                                                conditionalPanel(
                                                  condition = "input.Load4 != 0",
                                                    
                                                            box( width = NULL,  solidHeader = TRUE, 
                                                                 h4( em( htmlOutput( "filtering_step_text" ) %>% withSpinner(color="#0dc5c1") ) )
                                                                 )
                                                            ) ),
                                        
                                        column( width = 6,
                                                em( tags$strong( "B: GWAS analysis" ) ),
                                                br(),
                                                box( width = NULL,  solidHeader = TRUE,  status = "info", 
                                                     actionButton( inputId = 'Load5' , label = "Take-off" , icon = icon( 'rocket' ) , style='padding-top:20px; padding-bottom:50px; font-size:140%' ),
                                                     tags$hr() ),
                                                 
                                        
                                                conditionalPanel(
                                                  condition = "input.Load5 != 0",
                                                  
                                                  box( width = NULL,  solidHeader = TRUE,
                                                       h4( em( htmlOutput( "gwas_text" ) %>% withSpinner(color="#0dc5c1") ) )
                                                ) ) )
                                        
                            ) ),
      
      ### Result tab ###                   
      tabItem( tabName = "side3",
               
               h3( em( "3: Plot Results" ) ),
               hr(),
               fluidRow(
                 hr(),
                 
                 column( width = 12,
                         
                         box( width = NULL,  solidHeader = TRUE,  status = "info", 
                              actionButton( inputId = 'Load6' , label = "Take-off" , icon = icon( 'rocket' ) , style='padding-top:20px; padding-bottom:50px; font-size:140%' ),
                              tags$hr() ),
                         
                         conditionalPanel(
                           condition = "input.Load6 != 0",
                           
                           box( width = NULL,  solidHeader = TRUE, 
                                plotlyOutput( "mymanhattan" ) %>% withSpinner(color="#0dc5c1"),
                                br(),
                                hr() ) ) ) 
                 ) ),
      
      ### Result tab ###                   
      tabItem( tabName = "side4",
               
               h3( em( "4: Extracting significant SNP and mining databases" ) ),
               hr(),
               fluidRow(
                 hr(),
                 
                 column( width = 12,
                         
                         box( width = NULL,  solidHeader = TRUE,  status = "info", 
                              actionButton( inputId = 'Load7' , label = "Take-off" , icon = icon( 'rocket' ) , style='padding-top:20px; padding-bottom:50px; font-size:140%' ),
                              tags$hr() ),
                         
                         conditionalPanel(
                           condition = "input.Load7 != 0",
                           
                           box( width = NULL,  solidHeader = TRUE, 
                                h4( em( htmlOutput( "dtmining_text" ) %>% withSpinner(color="#0dc5c1") ) ),
                                br(),
                                DT::dataTableOutput( "mining.Table" ), 
                                hr() ) ) ) ) )
      ) 
    )
  
  ############
  ui <- dashboardPage( header , sidebar , body )
  ############
  
}

#### SERVER #### 
server <- function( input, output , session ) {
  
  #### REACTIVE EXPRESSION #### 
  for( i in 1 ){
    
    get.plink <- reactive({
      
      setwd( f.p.plink )
      system( paste( "sh input_filtering.sh" , sep = "" ) , intern = T , ignore.stdout = T , ignore.stderr = T )
      setwd( f.gwas )

    })
    
    get.gcta1 <- reactive({
      
      setwd( f.p.gcta )
      system( paste( "sh GCTA_matrix.bash" , sep = "" ) , intern = T , ignore.stdout = T , ignore.stderr = F )
      setwd( f.gwas )
      
    })
    
    get.gcta2 <- reactive({
      
      setwd( f.p.gcta )
      system( paste( "cp  " , f.i.gwas , "phenotypes* " , f.temp , ".",  sep = "" ) , intern = T , ignore.stdout = T , ignore.stderr = T )
      system( paste( "bash GCTA_GWAS.bash" , sep = "" ) , intern = T , ignore.stdout = F , ignore.stderr = F )
      setwd( f.gwas )
      
    })
    
    get.regression <- reactive({
        
        names <- c( "FID" , "IID" , "Phen" ) 
        colnames( input.pheno ) <- names
        
        input.cov.d <- input.cov.d[ , colSums( is.na( input.cov.d ) ) == 0 ]
        names <- c()
        for(i in colnames(input.cov.d)[3:length(input.cov.d)]){names <- c(names, paste(i, "d", sep = "_"))}
        names <- c("FID", "IID", names)
        colnames(input.cov.d) <- names
        
        
        names <- c()
        for(i in colnames(input.cov.c)[3:length(input.cov.c)]){names <- c(names, paste(i, "c", sep = "_"))}
        names <- c("FID", "IID", names)
        colnames(input.cov.c) <- names
        df <- merge(input.pheno, input.cov.d, by = c("FID", "IID"))
        df <- merge(df, input.cov.c, by = c("FID", "IID"))
        df <- df[,3:length(colnames(df))]
        
        #Linear model and stepwise regression
        full.model <- lm(Phen ~., data = df)
        step.model <- stepAIC(full.model, direction = "both", trace = FALSE)
        
        #Remove insignificant covariats
        cov <- step.model$model
        keep <- c("FID", "IID")
        for(i in colnames(input.cov.d)){if(i %in% colnames(cov)){keep <- c(keep, i)}}
        input.cov.d <- input.cov.d[, keep]
        keep <- c("FID", "IID")
        for(i in colnames(input.cov.c)){if(i %in% colnames(cov)){keep <- c(keep, i)}}
        input.cov.c <- input.cov.c[, keep]
        
        #Write covariat table
        write.table(input.cov.d, file = paste( f.temp , "disc_cov.txt" , sep = "" ) , col.names = FALSE, row.names = FALSE, sep = "\t" , quote = F )
        write.table(input.cov.c, file = paste( f.temp , "cont_cov.txt", sep = "" ) , col.names = FALSE, row.names = FALSE, sep = "\t" , quote = F )
        
        
    })
    
    get.wait <- reactive({
      
      x <- as.data.frame( matrix( NA , 2 , 2 ) )
      print( x )
      return( x )
      
    })
    
    get.manhattan <- reactive({
      print( 'get.manhattan' )
      
      setwd( f.temp )
      #Read table
      results <- read.table( "genotype_data_sample.txt" , header = T )
      print( "FILE loaded" )
      colnames( results )[1] <- 'CHR'
      colnames( results )[3] <- 'BP'
      colnames( results )[9] <- 'P'
      
      setwd( f.gwas )
      
      
      return( results )
      
    })
      
    get.qq <- reactive({
      setwd( f.r.plots )
      #Create manhattan plot
      
      manplot <- manhattan( results.sample , chr = 'Chr' , bp = "bp", p = 'p', snp ="SNP" )
      pdf( "manhattan.pdf" )
      manplot
      dev.off()
      
      #Create Q-Q plot
      qqplot <- qq( results.sample$p )
      pdf( "qqplot.pdf" )
      qqplot
      dev.off()
      
      #Print plots
      
      setwd( f.gwas )
      
    })
    
    get.dtbase.SNP <- reactive({
      
      setwd( f.p.plink )
      system( paste( "sh Clump.bash" , sep = "" ) , intern = T , ignore.stdout = T , ignore.stderr = T )
      setwd( f.gwas )
      
    })
    
    get.query <- reactive({
      
      setwd( f.temp)
      index.snps <- read.table( "index_snps.txt" , header = T )
      list.table <- list()
      
      for( i in 1 : nrow( index.snps ) ){
        
        tryCatch({
          assign( paste( "haploreg" , i , sep = "" ) , 
                  queryHaploreg( query = paste( index.snps[ i , 1 ] , sep = "" ) ) )
          
          list.table[[ i ]] <-  get( paste( "haploreg" , i , sep = "" ) )
          
          write.table( get( paste( "haploreg" , i , sep = "" ) ) , 
                       paste( "haploreg" , i , ".txt" , sep = "" ),
                       sep = "\t" , row.names = F, quote = F )},
          error = function( e ){ cat( "Haploreg results unavailable for" ,
                                      paste( index.snps[ i , 1 ] ) , "\n" ) } )
      }
      
      final.t <- do.call( rbind , list.table )
      
      return( final.t )
      
    })
    
   
    
  }
  
  #### OUTPUT EXPRESSION #### 
  for( i in 1 ){
    
    
  }
  
  #### OUTPUT DATA TABLES #### 
  for( i in 1 ){
    
    
  }
  
  #### OUTPUT Plots #### 
  for( i in 1 ){}
  
  #### OBSERVE EVENTS #### 
  for( i in 1 ){
    
    observeEvent( input$Load1 , {
      output$phenoTable <- DT::renderDataTable({
        
        
        x <- read.csv( paste( f.i.gwas , "phenotypes.csv" , sep = "" ) , header = F   )
        
        
      },
      options = list( 
        
        lengthMenu = list( c(10, 50, 100 , -1), c('10', '50', '100' , 'All')),
        pageLength = -1,
        initComplete = JS("function(settings, json) {",
                          "$(this.api().table().header()).css({'background-color': '#185381', 'color': '#FFFFFF'});",
                          "}")
        
        
      ),
      callback = JS( 'table.page(3).draw(false);')
      
      )
    } )
    
    observeEvent( input$Load2 , {
      output$d.var.Table <- DT::renderDataTable({
        
        
        x <- read.csv( paste( f.i.gwas , "discrete_covariates.csv" , sep = "" ) , header = F   )
        
        
      },
      options = list( 
        
        lengthMenu = list( c(10, 50, 100 , -1), c('10', '50', '100' , 'All')),
        pageLength = -1,
        initComplete = JS("function(settings, json) {",
                          "$(this.api().table().header()).css({'background-color': '#185381', 'color': '#FFFFFF'});",
                          "}")
        
        
      ),
      callback = JS( 'table.page(3).draw(false);')
      
      )
    } )
    
    observeEvent( input$Load3 , {
      output$c.var.Table <- DT::renderDataTable({
        
        
        x <- read.csv( paste( f.i.gwas , "continuous_covariates.csv" , sep = "" ) , header = F   )
        
        
      },
      options = list( 
        
        lengthMenu = list( c(10, 50, 100 , -1), c('10', '50', '100' , 'All')),
        pageLength = -1,
        initComplete = JS("function(settings, json) {",
                          "$(this.api().table().header()).css({'background-color': '#185381', 'color': '#FFFFFF'});",
                          "}")
        
        
      ),
      callback = JS( 'table.page(3).draw(false);')
      
      )
    } )
    
    observeEvent( input$Load4 , {
      
      output$filtering_step_text <- renderUI({
        
        str1 <- paste(  "Converting Files ..." )
        str2 <- paste(  "Allele Frequency Filtering ..." )
        str3 <- paste(  "Done." )
        HTML( paste( str1 , str2 , str3 , sep = '<hr/><br/>') )
        
        
      })
      
      get.plink()
      get.gcta1()
      get.regression()
      
    } )
    
    observeEvent( input$Load5 , {
      
      output$gwas_text <- renderUI({
        
        str1 <- paste(  "GWAS analysis in progress ..." )
        str2 <- paste(  "Done." )
        HTML( paste( str1 , str2 , sep = '<hr/><br/>') )
        
        
      })
      
      get.gcta2()
      
      
    } )
    
    observeEvent( input$Load6 , {
      
      output$mymanhattan <- renderPlotly({
        
        manhattanly( subset( get.manhattan() , CHR %in% 21:22 ), snp = "SNP" , highlight = significantSNP )
        
      })
      
      
    } )
    
    observeEvent( input$Load7 , {
      
      output$dtmining_text <- renderUI({
        
        str1 <- paste(  "SNP extraction ..." )
        str2 <- paste(  "Done." )
        HTML( paste( str1 , str2 , sep = '<hr/><br/>') )
        
        
      })
      
      get.dtbase.SNP()
      
      output$mining.Table <- DT::renderDataTable({
        
        
        get.query()
        
        
      },
      options = list( 
        scrollX = TRUE ,
        lengthMenu = list( c(10, 50, 100 , -1), c('10', '50', '100' , 'All')),
        pageLength = -1,
        initComplete = JS("function(settings, json) {",
                          "$(this.api().table().header()).css({'background-color': '#185381', 'color': '#FFFFFF'});",
                          "}")
        
        
      ),
      callback = JS( 'table.page(3).draw(false);')
      
      )
      
      
    } )
    
    
  }
  
}

#### Launch Application ####
shinyApp( ui, server )


