
library(shiny)


##### data
############################################################

data_model_choices <- list(
  "Bernoulli" = "Bernoulli", 
  "Poisson" = "Poisson",
  "Normal (unknown variance)" = "Normal (unknown variance)",
  "Normal (known variance)" = "Normal (known variance)"
)


##### UI
############################################################

navbarPage("Graphical Elicitation",
  
  ##### main tabpanel
  ############################################################
  
  tabPanel("Main", icon = icon("home"),
    
    # column(11, offset = 0,
      
    navlistPanel(
      
      ##### select model
      ########################################
      tabPanel("Select Model", icon = icon("sign-in"),
        
        column(4,
        	column(10,
	        	style = "padding-top: 20px;",
          	
            selectInput("data_model", label = "Data Model", 
              choices = data_model_choices, selected = "Bernoulli"),
      			
        		strong("Prior Family"),
        		verbatimTextOutput("prior_family"),
          	tags$head(tags$style("#prior_family{
							font-family: helvetica; 
          		font-size: 14px;}"
          	))
        	),
        	
        	column(12,		
        		br(),
        		p("Based on your previous experiences, how many times have you 
        			seen the scenario of interest?"),
        		numericInput("n", label = "Number of Times", 20L, min = 5, 
        			max = 1e4, width = "60%"),

        		conditionalPanel(
        			condition = "input.data_model == 'Normal (known variance)'",
        			
        			br(),
        			p("What is the standard deviation of the population?"),
        			numericInput("pop_sd", label = "Population SD", 10L, 
        				width = "50%")
        		)
        	),
        		
        	conditionalPanel(
        		condition = "input.data_model == 'Bernoulli'",
        		
        		column(12,
        			actionButton("set_bern_inputs", label = "Set Inputs",
        				class = "btn btn-primary")
        		)
        	)
        ),
      	
      	column(4,
      		conditionalPanel(
      			condition = "input.data_model != 'Bernoulli'",
      			
      			br(),
      			p("What is the smallest value you believe your measurement could be
      				(approximately)?"),
      			numericInput("min_init", label = "Smallest Value", 0L, 
      				width = "50%"),
      			
      			br(),
      			p("What is the largest value you believe your measurement could be
      				(approximately)?"),
      			numericInput("max_init", label = "Largest Value", 100L, 
      				width = "50%"),
      			
      			actionButton("set_non_bern_inputs", label = "Set Inputs",
      				class = "btn btn-primary")
      		)
      	)  # end column
      ),  # end tabPanel("Select Model", ...)
      
      	
      ##### start training
    	########################################
      tabPanel("Start Training", icon = icon("power-off"),
        conditionalPanel(
          condition = "input.data_model == 'Bernoulli'",
          
          column(7,
            h5("Suppose you are presented with a hypothetical dataset of size 100
              for your scenario."),
            h5("How many successes would you expect for such a dataset?"),
            h5("Look for expected and unexpected characteristics in the plots to
              get a better feel for natural variability in data."),
            
            style = "padding: 10px;"
            ),
          
          column(4,
            column(12,
              numericInput("training_successes", label = "Expected Successes", 
                50L, width = "60%")
            ),
            
            column(7,
              actionButton("generate_bern_training_plots", label = "Generate Graphs", 
                class = "btn btn-primary"),
              
              align = "center"
            )
          )
        ),
        
        conditionalPanel(
          condition = "input.data_model == 'Poisson'",
          
          column(7,
            h5("Suppose you are presented with a hypothetical dataset of size 100
              for your scenario."),
            h5("What is a common value for your measurement?"),
            h5("Look for expected and unexpected characteristics in the plots to
              get a better feel for natural variability in data."),
            
            style = "padding: 10px;"
            ),
          
          column(4,
            column(12,
              numericInput("training_rate", label = "Common Value", 25L, 
                width = "60%")
            ),
            
            column(7,
              actionButton("generate_pois_training_plots", 
                label = "Generate Graphs", class = "btn btn-primary"
              ),
              
              align = "center"
            )
          )
        ),
        
        conditionalPanel(
          condition = "input.data_model == 'Normal (known variance)'",
          
          column(7,
            h5("Suppose you are presented with a hypothetical dataset of size 100
              for your scenario."
            ),
            h5("What is a reasonable average value of your measurement?"),
            h5("What is the standard deviation of the population?"),
            h5("Look for expected and unexpected characteristics in the plots to
              get a better feel for natural variability in data."
            ),
            
            style = "padding: 10px;"
          ),
          
          column(4,
            column(12,
              numericInput("training_mean1", label = "Average", 100L, 
                width = "60%")
            ),
            
            column(12,
              numericInput("training_sd1", label = "Population SD", 10L,
                width = "60%")
            ),
            
            column(7,
              actionButton("generate_norm_kv_training_plots", 
                label = "Generate Graphs", 
                class = "btn btn-primary"
              ),
              
              align = "center"
            )
          )
        ),
        
        conditionalPanel(
          condition = "input.data_model == 'Normal (unknown variance)'",
          
          column(7,
            h5("Suppose you are presented with a hypothetical dataset of size 100
              for your scenario."),
            h5("What is a reasonable average value of your measurement?"),
            h5("What is a reasonable standard deviation of your measurement?"),
            h5("Look for expected and unexpected characteristics in the plots to
              get a better feel for natural variability in data."),
            
            style = "padding: 10px;"
          ),
          
          column(4,
            column(12,
              numericInput("training_mean2", label = "Average", 100L, 
                width = "60%")
            ),
            
            column(12,
              numericInput("training_sd2", label = "Standard Deviation", 10L,
                width = "60%")
            ),
            
            column(7,
              actionButton("generate_norm_uv_training_plots", 
                label = "Generate Graphs", 
                class = "btn btn-primary"
              ),
              
              align = "center"
            )
          )
        ),
        
        column(10, plotOutput("training_plots", height = 375)),
        column(2,	      		
          actionButton("new_training_plots", label = "Generate New Graphs", 
            class = "btn btn-primary"),
          style = "padding-top: 50px;"
        ) #,
        
        # column(6,
        #   checkboxInput("change_N", label = "Change Size of Hypothetical Dataset", 
        #     value = FALSE),
        #   
        #   conditionalPanel(
        #     condition = "input.change_N",
        #     
        #     h5("Change the number of observations in the hypothetical dataset."),
        #     
        #     numericInput("new_N", label = "Number of Observations", value = 100, 
        #       width = "45%"),
        #     actionButton("hypothetical_N", "Update", class = "btn btn-primary")
        #   )
        # )
      ),


	    ##### make selections
	    ########################################
    	tabPanel("Select Graphs", icon = icon("random"),

        conditionalPanel(
          condition = "input.data_model == 'Bernoulli'",      		      		
    	
        	column(10,
	    			h4("Select the graph that is most reasonable for a hypothetical 
    				   dataset of size 100."),
  					align = "center",
	      		style = "padding-top: 30px;"
        	),
        	
      		column(2,
      			h4("Selections"),
      			verbatimTextOutput("selections_bern"),
      			tags$head(tags$style(type="text/css", "#selections_bern{max-width: 80px;}")),
      			align = "center"
      		),
        	
        	column(10,
        		column(2,
		      		actionButton("plot_1_bern", "Graph 1", class = "btn btn-primary", 
		      			width = "90px"),
        			style = "padding-left: 10.3%; padding-right: 12%"
        		),
		        column(2,
		        	actionButton("plot_2_bern", "Graph 2", class = "btn btn-primary", 
		        		width = "90px"),
		        	style = "padding-left: 6.5%; padding-right: 12%"
		        ),
        		column(2,
        			actionButton("plot_3_bern", "Graph 3", class = "btn btn-primary", 
		        		width = "90px"),
		        	style = "padding-left: 6.4%; padding-right: 12%"
        		),
		        column(2,
		        	actionButton("plot_4_bern", "Graph 4", class = "btn btn-primary", 
		        		width = "90px"),
		        	style = "padding-left: 6.5%; padding-right: 12%"
		        ),
        		column(2,
			        actionButton("plot_5_bern", "Graph 5", class = "btn btn-primary", 
			        	width = "90px"),
			        style = "padding-left: 6.4%; padding-right: 12%"
        		)
        	),
        	
        	column(10, plotOutput("select_plots_bern"))
        ),
    		
        conditionalPanel(
          condition = "input.data_model != 'Bernoulli'",      		      		
    	
    			column(10,
    				h4("Select the graph that is most reasonable for a hypothetical 
    				   dataset of size 100."),

						align = "center",
      			style = "padding: 30px;"
    			),
 
      		column(2,
      			h4("Selections"),
      			verbatimTextOutput("selections"),
      			tags$head(tags$style(type="text/css", "#selections{max-width: 80px;}")),
      			align = "center"
      		),
      			      
        	column(10, plotOutput("select_plots")),			
      		
        	column(2,
        		br(),
	      		actionButton("plot_1", label = "Graph 1", class = "btn btn-primary", 
	      			width = "90px"),
      			br(),
      			br(),
      			br(),
	    			actionButton("plot_2", label = "Graph 2", class = "btn btn-primary", 
	    				width = "90px"),
      			br(),
      			br(),
      			br(),
      			actionButton("plot_3", label = "Graph 3", class = "btn btn-primary", 
      				width = "90px"),
      			br(),
      			br(),
      			br(),
      			actionButton("plot_4", label = "Graph 4", class = "btn btn-primary", 
      				width = "90px"),
      			br(),
      			br(),
      			br(),
      			actionButton("plot_5", label = "Graph 5", class = "btn btn-primary", 
      				width = "90px"),
        		align = "center"
      		)
				)
	    ),


      ##### compute prior
      ########################################
      tabPanel("Compute Prior", icon = icon("flash"),

        conditionalPanel(
          condition = "input.data_model != 'Normal (unknown variance)'",

        	column(3,
          	strong("Prior Family"),
          	verbatimTextOutput("prior_family_output"),
        	  tags$head(tags$style("#prior_family_output{font-family: helvetica;" 
        	    #font-size: 13px;}"
        	  )),

            strong("Elicited Parameters"),
            verbatimTextOutput("prior_params"),

            strong("Summaries"),
            verbatimTextOutput("prior_summaries"),

          	strong("Number of Selections"),
          	verbatimTextOutput("all_selections")
        	),
        	
        	column(9, plotOutput("prior_plot", height = 460)),
        		
      		column(3,
          	checkboxInput("more_prior_options", label = "Find Prior Probability", 
          		value = FALSE
          	 ),

          	conditionalPanel(
          		condition = "input.more_prior_options",

          		column(12,
	          	 	h5("Find the probability between two values of the parameter 
                   (based on the elicited prior)."
	          	 	)
          		),
	          		
          		column(6, numericInput("lower_prob", "Lower Value", 0, width = "120%")),
		            
	          	column(6, numericInput("upper_prob", "Upper Value", 1, width = "120%")),
	          		
          		column(12,
	            	actionButton("interval_prob", "Find Probability", 
	            		class = "btn btn-primary"
	            	),
          			align = "center",
          			h3(""),
          			
          			column(10, offset = 1,
		          		verbatimTextOutput("prob_output"),
	          			align = "center"
          			)
          		)
          	)
      		),
        	
          column(3,
            checkboxInput("change_n", label = "Change Previous Experience", 
              value = FALSE
            ),
            
            conditionalPanel(
              condition = "input.change_n",
              
              column(12, 
                h5("Change the number of times you have previously seen the scenario
                   of interest."
                )
              ),
              
              numericInput("new_n", label = "New Number of Times", value = 20),
              actionButton("change_ess", "Update", class = "btn btn-primary")
            )
          )
        ),

        conditionalPanel(
          condition = "input.data_model == 'Normal (unknown variance)'",

        	column(3,
        		# joint prior
        		h3("Joint"),
        		strong("Prior Family"),
        		verbatimTextOutput("prior_family_output_joint"),
           	tags$head(tags$style("#prior_family_output_joint{font-size: 13px;}")),
        		
        		strong("Elicited Parameters"),
        		verbatimTextOutput("prior_params_joint"),
        		
         		# mean prior -- conditional on variance
          	h3("Mean (conditional on variance)"),
          	strong("Prior Family"),
          	verbatimTextOutput("prior_family_output_mean"),
          	tags$head(tags$style("#prior_family_output_mean{font-size: 13px;}")),

            strong("Elicited Parameters"),
          	verbatimTextOutput("prior_params_mean"),
        	  p("(Sigma^2)* = mode of Inverse-Gamma prior on the variance"),

						strong("Summaries"),
						verbatimTextOutput("prior_summaries_mean"),

          	strong("Number of Selections"),
          	verbatimTextOutput("all_mean_selections"),
 				
        		# variance prior -- marginal
          	h3("Variance (marginal)"),
          	strong("Prior Family"),
          	verbatimTextOutput("prior_family_output_var"),
          	tags$head(tags$style("#prior_family_output_var{font-size: 13px;}")),

            strong("Elicited Parameters"),
          	verbatimTextOutput("prior_params_var"),

						strong("Summaries"),
						verbatimTextOutput("prior_summaries_var"),

          	strong("Number of Selections"),
          	verbatimTextOutput("all_var_selections")
         	),
          	       	
        	column(9,
        		style = "padding-top: 275px; padding-bottom: 50px;",
        		
        		# prior on mean plot
            plotOutput("prior_plot_normal_mean", height = 460),
        		br(),
        		br(),
        	  
        	  column(4,
        	    checkboxInput("change_normal_n", label = "Change Previous Experience", 
        	      value = FALSE
        	    ),
        	    
        	    conditionalPanel(
        	      condition = "input.change_normal_n",
        	      
        	      column(12, 
        	        h5("Change the number of times you have previously seen the 
                     scenario of interest."
        	        )
        	      ),
        	      
        	      numericInput("new_normal_n", label = "New Number of Times", 
        	        value = 20
        	      ),
        	      actionButton("change_normal_ess", "Update", class = "btn btn-primary")
        	    )
        	  ),

        		# prior on variance plot
            plotOutput("prior_plot_normal_var", height = 460)
          )
      	)
			),

      	
      ##### view history plot
      ########################################
      tabPanel("View History Plot", icon = icon("line-chart"),
        conditionalPanel(
        	condition = "input.data_model != 'Normal (unknown variance)'",
        		
        	plotOutput("history_plot")
        ),

      	conditionalPanel(
      		condition = "input.data_model == 'Normal (unknown variance)'",

      		plotOutput("history_plot_normal_mean"),
      		br(),
      		plotOutput("history_plot_normal_var")
      	)
      ),
      	
      	
    	##### download report of pertinent info
    	#######################################	
    	tabPanel("Download Results", icon = icon("download"),
    		
    		column(4,
    			column(12,
      			h4("Download report of results"),
      			selectInput("format",
      				label = "File Type", 
      				choices = c("PDF", "Word", "HTML"),
      				selected = "PDF",
      				width = "50%"
      			)
    			),
    			
    			column(6,
    				downloadButton("download_report", "Download", class = "btn btn-primary"),
    				align = "center"
    			)
    		),
    		
    		column(4,	
					h4("Download selected values"),
    			h5("File type: CSV"),
  				downloadButton("download_selected", "Download", class = "btn btn-primary"),
    			align = "center"
      	)
    	),
  
  
      # other arguments to navlistPanel()
      widths = c(2, 9)
  
    ) # end navlistPanel()
    
  ), # end tabPanel("Main", ...)
  
  
  ##### help tabpanel
  ############################################################
  
  tabPanel("Help", icon = icon("question-circle"),
    
    column(4,
      
      # select model
      h3("Select Model"),
      
      p("1. To start, you must input the following:"),
      p("(1) a data model"),
      p("(2) the number of times you have seen the scenario of interest, based 
        on your previous experiences"),
      p("(3) approximate minimum and maximum values for the measurement (for all
        data models other than Bernoulli). This number does not need to be 
        exact; its purpose is to initialize the algorithm."),
      p("(4) the population standard deviation (only for the normal data model
        with known variance)"),
      p("Notes: the app defaults the prior family based on the chosen data 
        model.  For the gamma prior, the parameterization used has mean 
        alpha/beta."),
      p("2. Once you have made these choices, click the 'Set Inputs' button. 
        You now have the option of moving to either the 'Start Training' tab or 
        the 'Select Graphs' tab (if you wish to skip the training)."),
      hr(),
      
      # start training
      h3("Start Training"),
      p("Nine datasets, each of 50 hypothetical observations, are plotted based 
        on the chosen data model and either an expected number of successes
        (for a Bernoulli data model) or a common measurement value (for a 
        Poisson data model). These plots are meant to help you visualize the 
        natural variability that is often present in real datasets. Look for 
        expected and unexpected characteristics in the plots. This training 
        process is known as the Rorschach procedure."),
      p("Note: there are no selections to be made at this time."),
      p("Additional Options:"),
      p("(1) To see new sets of graphs, click the 'Generate New Graphs' button."),
      p("(2) To change the size of the hypothetical datasets for the training
        and selection graphs, click the checkbox at the bottom of the screen
        and update the number of observations."),
      hr()
      ),
    
    column(4,
      # select graphs
      h3("Select Graphs"),
      p("Five datasets, each with 100 observations, are plotted; these are based 
        on the inputs you specified in the 'Select Model' tab. You must select 
        the graph that is most reasonable as a hypothetical future dataset. Once 
        you select a graph, five new ones will be displayed. The number of 
        selections you must make is specified in the top-right corner of the
        screen."),
      p("Note: you may not see any graphs that truly represent your beliefs,
        particularly at the beginning of the process. Still choose the best out
        of the ones presented to you. The algorithm may need a few selections to
        reach the point where you see graphs that fit your beliefs."),
      hr(),
      
      # compute prior
      h3("Compute Prior"),
      p("After making the specified number of selections, you can move to the 
        'Compute Prior' tab. Here you will see (1) the prior family, (2) the 
        elicited parameters for this family, (3) summaries of the elicited 
        prior, (4) the total number of selections made, the number kept 
        (defaulted to 5), and the number discarded (referred to as the 
        'warm-up'), and (5) a density plot of the elicited prior."),
      p("Additional Options:"),
      p("(1) Find Prior Probability: allows you to find the probability between 
        two values of the parameter (based on the elicited prior)."),
      p("(2) Change Past Sample Size: allows you to change the number of times 
        you have seen the scenario of interest. You previously set this value on 
        the 'Select Model' tab."),
      p("(3) Change Warm-up: allows you to change the warm-up value (the number
        of selections discarded before fitting the prior)."),
      p("(4) Change Plot: allows you to change the x-axis values for the density 
        plot."),
      hr()
      ),
    
    column(4,
      # view history plot
      h3("View History Plot"),
      p("After making the specified number of selections, you can view a plot of 
        the parameter values at each stage (proportions for a Bernoulli data 
        model, rates for a Poisson data model, means for a Normal model with
        known variance, and means and variances for a Normal data model with 
        unknown variance). Red circles indicate the values of the parameters 
        that correspond to the selected graphs at each stage, and black circles 
        indicate the values that correspond to the four unselected graphs."),
      hr(),
      
      # download results
      h3("Download Results"),
      p("If you want to save your results, you can download two files:"),
      p("(1) a report that contains all of your inputs as well as all output 
        from the 'Compute Prior' and 'View History Plot' tabs. Three file types
        (PDF, Word, and HTML) are available for this document, which is created
        using R Markdown, and"),
      p("(2) a CSV file that contains all of the values you selected.")
    )
  ),
  
  
  ##### about tabpanel
  ############################################################
  
  tabPanel("About", icon = icon("info-circle"),
    
    column(4,
      
      # information about the developers
      h4("Developers"),
      p("This tool was developed by Chris Casement (Ph.D. candidate) and David 
        Kahle (Associate Professor), both of the Department of Statistical 
        Science at Baylor University."),
      br(),
      
      # information about the app
      h4("About the App"),
      p("This tool elicits prior distributions for Bayesian analyses by drawing
        on recent developments in the graphical inference literature on visual 
        hypothesis testing."),
      br(),
      
      # contact info
      h4("Contact"),
      p("Email: casementc@gmail.com"),
      br(),
      br(),
      
      # copyright statement
      p("Copyright (c) 2016 - 2017 Christopher J. Casement & David J. Kahle."),
      p("The license statement can be found", 
        a("here.", href = "https://opensource.org/licenses/MIT", target = "_blank")
      )
    )
  )
) # end navbarPage()

