# HCSS_ZOTERO project
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


#library(shinythemes)
library(shiny)
library(streamgraph)
shinyUI(navbarPage(theme = "bootstrap.min.css",
                  # theme = shinytheme("united"),
                   tags$title("ITMS-HCSS"),
                   navbarPage(
                     
                     title = div(
                       img(src="itmslogo.jpg", class="app-logo", height = 40),
                       div(class="app-title", tags$h5(strong("ITMS-HCSS")))
                     ),
                     position="fixed-top",
 
####### File Processing #######

    tabPanel(strong("File Processing"),
             fluidPage(
               navlistPanel(
                 widths = c(2, 10),  
                 
                 #########Zotero########
                 tabPanel(strong("ZOTERO"), 
                          fluidPage(
                            shiny::tags$head(shiny::tags$style(shiny::HTML(
                              "#text { font-size: 15px; height: 300px; overflow: auto; }"
                            ))),
                            fluidRow( 
                              column(5,
                                    # fileInput('file.rdf', 'Choose Corpus',multiple=FALSE, accept=c('txt','rdf')),
                                    radioButtons(
                                      'corpus', 
                                      'Corpus',
                                      c(zot = 'zot',
                                        #russian = 'russian',
                                      npcorpus = 'np-corpus'), 
                                      'np-corpus'
                                      ),
                                   # tags$h3("Corpus Stats"),
                                    uiOutput("print_length"),
                                    tags$hr(),
                                    uiOutput("zotero_slider"),
                                    uiOutput("choose_kwic_num"),
                                    tags$br(),
                                   
                                    uiOutput("print_content_rdf")#,
                                    #tableOutput("matrix")
                                    ### Test ####
                                   # uiOutput("matrix")
                                  #  
                                     
                              ),
                              column(6,
                                     tags$h6("Key Term Searches"),
                                     helpText("Enter ONE term or TWO terms"),
                                     #tags$h6("Two Key Terms Search"),
                                    # uiOutput("zotero_one_term"),
                                    # helpText("Type two query terms"),  # separated by AND  condition."),
                                    # tags$br(),
                                    uiOutput("zotero_term")#,
                                    #helpText("Select the number of words between two terms and the number of words before and after these terms."),
                                    # helpText("For example: ",tags$b("Влияние AND война")),
                                    
                                   # tags$br(),
                                      
                              )
                            ),
                            fluidRow(
                              column(10,
                                     tags$hr(),
                                     tags$h5("Hits per year: Expect ~1 minute to extract terms from large dataset"),
                                     plotOutput("distribution"),
                                     tags$h5("View Panel"),
                                     div(id = "text", uiOutput("print_zotero"))
                              )#,
                            )
                          )
                 ),
                 ######## Metadata ########
                 tabPanel("Metadata",
                          
                          p(
                            "Metadata consists of Ids, Year, Title, 
                            Author, Category, Location. 
                            If your rdf files contain metadata - it will be printed here."
                          ),
                          fluidPage(
                            fluidRow( 
                              column(10, 
                                     tags$hr(),
                                     DT::dataTableOutput("zotero_metadata_table")
                              )
                              # dataTableOutput("place_for_metadata_table")
                            )
                          )
                          )      
   ############Data Cleaning#######
                 # tabPanel("Stopwords",
                 #          
                 #          fluidPage(
                 #            fluidRow( 
                 #              column(4, tags$h4("Select Stopwords"),
                 #                     tags$hr(), 
                 #                     helpText("Stop words have been applied"),
                 #                     radioButtons('stops', 
                 #                                  'Select Default or Upload',
                 #                                  c(None="None",
                 #                                    english='english',
                 #                                    russian='russian',
                 #                                    Upload='Upload'),
                 #                                  'None'),
                 #                     helpText("Default is the list from tm package: stopwords(`SMART') and ('russian')"),
                 #                     tags$hr(),
                 #                     fileInput('stopwords.txt', 'Upload stopwords - txt format (one word per line)',multiple=FALSE,
                 #                               accept=c('txt')),
                 #                     tags$hr()#,
                 #                     # checkboxInput('show_stopwords','Show Stopwords',FALSE),
                 #                    # div(id = "text", verbatimTextOutput("print_stopwords"))
                 #              ),
                 #              column(4,tags$h4("Manual Removal"),tags$hr()#,
                 #                     # checkboxInput('remove_manual','Apply Manual',FALSE),
                 #                    # helpText("Select one or multiple words (hold shift key down)"),
                 #                    # uiOutput("choose_remove")#,
                 #                    # div(id = "text", uiOutput("printWords"))
                 #              ),
                 #              column(4,tags$h4("Viewer"),
                 #                     tags$hr(),
                 #                     radioButtons('stopwords', 'Apply Stopwords or None (no changes)',
                 #                                  c(apply='Apply Stopwords',
                 #                                    none='None'), 'None')#,
                 #                     #checkboxInput('stopwords','Apply Stopwords',FALSE),
                 #                   #  div(id = "text", uiOutput("print_apply_stops"))
                 #                     # selectizeInput("cutoff_lower", label = "Select or Type Lower Cutoff",choices = c(0,1,2,3,4),options = list(create = TRUE),selected = 0,multiple = FALSE),
                 #                     #  selectizeInput("cutoff_high", label = "Select or Type Upper Cutoff",choices = c(300,200,100),options = list(create = TRUE),selected = NULL,multiple = FALSE) 
                 #              )
                 #            ),
                 #            fluidRow(
                 #              column(12,
                 #                     p("List of stopwords: 1)", tags$a(href="http://www.ranks.nl/stopwords",
                 #                                                       "http://www.ranks.nl/stopwords"), "2) ",
                 #                       tags$a(href="http://www.lextek.com/manuals/onix/stopwords1.html","http://www.lextek.com/manuals/onix/stopwords1.html"),
                 #                       " 3) ",
                 #                       tags$a(href="http://www.webconfs.com/stop-words.php","http://www.webconfs.com/stop-words.php"),
                 #                       "3) Extensive stopword list with proper names from Matthew Jockers
                 #                       ",tags$a(href="http://www.matthewjockers.net/macroanalysisbook/
                 #                                expanded-stopwords-list/",
                 #                                "http://www.matthewjockers.net/macroanalysisbook/expanded-stopwords-list/"),".
                 #                       Words must be one per line."),
                 #                     br(),
                 #                     tags$hr()  
                 #              )
                 #              )
                 #              )
                 #          
                 #            ),
                 # tabPanel("Stemming", 
                 #          
                 #          fluidPage(
                 #            fluidRow(
                 #              column(4, 
                 #                     tags$h4("Stems - tm package"),
                 #                     tags$hr(), 
                 #                     
                 #                     # checkboxInput('choose_stem','Stem',FALSE),
                 #                     radioButtons('language', 'Choose Language',
                 #                                  c(none='none',
                 #                                    English='english',
                 #                                   # Spanish='spanish',
                 #                                   # Danish="danish",
                 #                                    Dutch="dutch",
                 #                                   # Finnish="finnish",
                 #                                   # French="french",
                 #                                   # German="german",   
                 #                                    #Hungarian= "hungarian",
                 #                                   # Italian="italian",
                 #                                   # Norwegian="norwegian",
                 #                                    Porter="porter",     
                 #                                   # Portuguese="portuguese",
                 #                                   # Romanian="romanian", 
                 #                                    Russian='russian',   
                 #                                   # Spanish='spanish' ,   
                 #                                   # Swedish="swedish"  ,  
                 #                                    Turkish="turkish"
                 #                                  ), 'none'),
                 #                     tags$hr()
                 #              ),
                 #              column(8, tags$h4("Stem Viewer"),
                 #                     tags$hr()#,
                 #                    # div(id = "text", uiOutput("print_stemmer"))
                 #              )
                 #            )
                 #          )
                 #          
                 # )#,
 
                 )
             )
    ),    
    
    #navbarMenu(title = "Data Visualization",
########## NGRAM #########
                tabPanel(strong("Ngrams"),
                #tabPanel("Word Frequency",
                         fluidPage(
                           navlistPanel(
                             widths = c(2, 10),
                             tabPanel("Unigram",
                                      fluidPage(
                                        fluidRow(
                                          column(8, 
                                                 tags$h4("Frequency Table"),
                                                 tags$hr(),
                                                 # checkboxInput('show_freq','Frequency',FALSE),
                                                 DT::dataTableOutput("freq_unigram")
                                          )#,
                                          #column(6, 
                                                # tags$h4("Frequency Table"),
                                                 #tags$hr(),
                                                 # checkboxInput('show_freq','Frequency',FALSE),
                                                # DT::dataTableOutput("ngramBuilder2")
                                          #)
                                        )
                                      )
                             ),
                             tabPanel("Bigram",
                                      fluidPage(
                                        fluidRow(
                                          column(8, 
                                                 tags$h4("Bigram Frequency Table"),
                                                 tags$hr(),
                                                 # checkboxInput('show_freq','Frequency',FALSE),
                                                 DT::dataTableOutput("freq_bigram")
                                          )
                                      )
                                      )
                             ),
                             tabPanel("Trigram",
                                      p("Ngram analysis",
                                        fluidPage(
                                          fluidRow(
                                            column(8,
                                                   tags$h4("Trigram Frequency Table"),
                                                   tags$hr(),
                                                   DT::dataTableOutput("freq_trigram"))
                                          )
                                          )
                                        )
                                      )

                             )
                           )
                ),
## Word Cloud ####
                            tabPanel(strong("Word Clouds"),
                                     
                                     fluidPage(
                                       fluidRow(
                                         column(4, 
                                                radioButtons('pal', 'Select Color Palette',
                                                             c(black='black',
                                                               green='green',
                                                               multi='multi'),
                                                             'multi'),
                                                tags$hr()
                                         ),
                                         column(4, 
                                                radioButtons('font', 'Select Font',
                                                             c('Sans Serif'='sans serif',
                                                               'Script'='script',
                                                               'Gothic'='gothic english'),
                                                             'sans serif'),
                                                tags$hr()
                                         ),
                                         column(4, 
                                                radioButtons('multicloud', 'Cloud Type',
                                                             c(#'Word Cloud'='Word Cloud',
                                                               'Commonality Cloud'='Commonality Cloud',
                                                               'Comparison of two or more docs'='Comparison Cloud'),
                                                             'Comparison Cloud'),
                                                tags$hr()
                                         ),
                                         fluidRow(
                                           column(5,
                                                  p("Set the minimum frequency for your cloud visualization"),
                                                  uiOutput("choose_min_frequency")
                                           ),
                                           column(5,p("Set the maximum words per plot"),
                                                  uiOutput("choose_max_words")
                                           )
                                         ),
                                         fluidRow(
                                           column(6,
                                                  wordcloud2::wordcloud2Output("print_cloud2", width = "70%", height = "500px")
                                           ),
                                           column(6,
                                                  plotOutput("print_cloud")
                                           )
                                         ),
                                         # plotOutput("word_count")),
                                       fluidRow(
                                         column(4, 
                                                # uiOutput("choose_cloud"),
                                                uiOutput("choose_top"),
                                                tags$hr()
                                         )
                                       ),
                                       fluidRow(
                                         column(8,
                                                #tags$h5("Frequency Bar Plot"),
                                                plotOutput("word_count")
                                         )
                                       )
                                     )
                                     
                            )
                            ),


########## Cluster Analysis #########
    tabPanel(strong("Cluster Analysis"),
             fluidPage(
               navlistPanel(
                 widths = c(2, 10),
                 tabPanel("Topic Labels for Cluster",
                          p("Labels are strings containing the most probable words for each topic."),
                          p("The algorithm is based on MalletLDA from mallet R package - it might take a while to calculate the estimates for topics, if your corpus is large."),
                          p("Current default - 10 topics, optimize hyperparameters every 20 iterations, after 50 burn-in iterations, training - 200, smoothed=T, normalized=T. Run through a few iterations where we pick the best topic for each token"),
                          p("Parameters optimization will be built in the next release."),
                          fluidPage(
                          fluidRow(column(12,
                        tableOutput("mallettop")
                 )
               )
             )      
             ),
             tabPanel("Cluster Visualization",
                      fluidPage(
                        fluidRow(column(12,
                                        plotOutput("cluster_plot")
                        )
                        )
                      )
             )
               )
             )
    ),
                          
 
########## Streamgraph #########
tabPanel(strong("Streamgraph"),
         fluidPage(
           navlistPanel(
             widths = c(2, 10),
             tabPanel("Topic 10 topic words",
               fluidPage(
                 fluidRow(
                 column(12,
                      tags$hr(),
                      DT::dataTableOutput("longfrequency")  
                 )
                 )
               )
             ),
         tabPanel("Stream Graph",
          fluidPage(
         fluidRow(
           column(12,  tags$h4("Stream Graph - Topics"),
                   streamgraph::streamgraphOutput("sg1")
                 # plotOutput("sg1")
           )
         )
)
         )
)
)
),
           
           
    tabPanel(strong("Topic Analysis"),
             fluidPage(
               navlistPanel(
                 widths = c(2, 10),
                 ########## Topic Model ##########
                 tabPanel("Model Creation",
                          
                          p("This analysis is based on lda, topicmodels  and stm packages. For lda package - collapsed.gibbs.sampler is used, 
                            where the number of topics, iteration, alpha and eta values can be adjusted. By default, num of topics = 3; iteration = 500, alpha = 0.02, eta = 0.02",
                            tags$a("http://search.r-project.org/library/lda/html/rtm.collapsed.gibbs.sampler.html","lda package")),
                          hr(),
                          
                          tags$fieldset(class="ITMS-border",
                                        tags$legend(class="ITMS-border", "Topic selection"),
                                        fluidPage(
                                          fluidRow(
                                            column(6,
                                                   #h5("Topic Selection:"),
                                                   #tags$hr(), 
                                                   helpText("Select number of topics - an integer representing the number of topics in the model. Default is 3."),
                                                   uiOutput("choose_topic_num"),
                                                   helpText("Select the top number of words associated with a given topic. Default is 3."),
                                                   uiOutput("choose_word_num")
                                            ),   
                                            column(6,
                                                   helpText("The number of sweeps of Gibbs sampling over the entire corpus to make. Default is 500"),
                                                   uiOutput("iter"),
                                                   helpText("Select alpha. Default is 0.02"),
                                                   uiOutput("alpha"),
                                                   helpText("Select eta. Default is 0.02"),
                                                   uiOutput("eta")
                                            )
                                          )
                                        )                          
                          ),
                          
                          fluidPage(
                            fluidRow(
                              
                              column(12,         
                                     p("To determine the best number of topics, run Best Topic Number analysis based on Likelihood Log. set.seed(2013)"),
                                     p("It may take a while to run this analysis depending 
                                       on the number of documents and their size. Meanwhile you make explore 
                                       your articles with frequency and cluster analyses."),#,tags$a("http://search.r-project.org/library/lda/html/rtm.collapsed.gibbs.sampler.html","lda package"),"set.seed(2013)"),
                                     tags$h5("Best Topic Number:"),
                                     tags$hr(), 
                                     helpText("Run this analysis to determine the 
                                              best number of topics to use."),
                                     uiOutput("best_topic_num"),
                                     tableOutput("best_k"),
                                     plotOutput("best_k_plot")
                                     )                                                                                                        
                              )
                            )
                          
                 ),
                 ######### LDA #######
                 tabPanel("LDA Visualization",
                          
                          fluidPage(                                     
                            fluidRow( #uiOutput("choose_lda"),
                              column(12, br(),
                                     radioButtons('lda', 'Run LDA Analysis',
                                                  c(none='None',
                                                    run='RUN'),
                                                  'None'),
                                     helpText("Selected Topics LDA (lda.collapsed.gibbs.sampler from lda package)"),
                                     tableOutput("topics")#,
                                     # helpText("Plotting Documents and Topics Relations"),
                                     # plotOutput("printCoordinates")
                              )#,
                             # column(5, br(),
                              #       uiOutput("docsNames"),
                               #      verbatimTextOutput("docs")
                             # )
                            )
                          )
                          
                 ),
                 ########## Stream Graph #######
                 #https://shiny.rstudio.com/reference/shiny/latest/reactivePlot.html
              #   tabPanel("Stream Graph1",
               #  fluidPage(
                  # radioButtons('stream', 'Select Interpolate Options',
                  #              c(cardinal='cardinal',
                   #               linear='linear',
                   #               step = 'step'),
                    #            'cardinal'),
                 #  fluidRow(
                  #   column(12,  tags$h4("Stream Graph - Topics"),
                           # streamgraph::streamgraphOutput("sg1")
                   #        plotOutput("gg")
                 #    )#,
                   # fluidRow(
                  #    column(6, verbatimTextOutput("chronology_top2"),
                  #   column(6,
                      #      tableOutput("print_data")
                    # )
                   #  )
                   #  )
                  # )
              #   )
              #   )
              #   ),
                 
                 tabPanel("STM Visualization",
  
                          fluidPage(
                            fluidRow( #p("STM is a structural topic modeling."),
                              column(8,  tags$h4("Structural Topic Modeling"),
                                     #  uiOutput("choose_stm"),
                                     radioButtons('stm', 'Run STM Analysis',
                                                  c(none='None',
                                                    run='RUN'),
                                                  'None'),
                                     tags$hr(),
                                     verbatimTextOutput("docs"),
                                     helpText("Structural Topics STM"),#,
                                     plotOutput("perspectives")
                              ),
                              column(12,
                                     tags$hr(), 
                                     tags$h5("Structural Topic Modeling"),
                                     helpText("package stm with init LDA"),
                                     verbatimTextOutput("topics_stm"),
                                     tags$h5("Proportion of Topics in Documents"),
                                     plotOutput("proportion"),
                                      tags$h5("Most Common Topic Terms"),
                                      plotOutput("cloud_stm"),
                                    # plotOutput("perspectives"),
                                     tags$h5("Correlation Plot"),
                                     plotOutput("corelation")
                              )  
                            )
                          )
                          
                 ),
                 tabPanel("Metadata Topic Visualization",

                          fluidPage( p("This analysis requires metadata - please load or extract metadata in Data Preparation Panel."),
                                     fluidRow(
                                       column(12,
                                              tags$h5("Topic Modeling with topicmodels package:"),
                                             # uiOutput("choose_chronology"),
                                              #radioButtons('chronology', 'Run LDA Analysis',
                                               #            c(none='None',
                                               #              run='RUN'),
                                               #            'None'),
                                              tableOutput("chronology_table"),
                                              verbatimTextOutput("chronology_top"),
                                              plotOutput("chronology_plot")
                                       )
                                     )
                          )

                 )
                 
                 )      
               )
               )    
)

    )
)
