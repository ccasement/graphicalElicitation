
library(shiny)
library(ggplot2); theme_set(theme_bw(16))
library(scales)
library(tidyr)
library(plyr)
library(dplyr)
library(rmarkdown)

shinyServer(function(input, output){
	
	##### define basic quantities and global functions
	############################################################
	
	n_frames <- 5       # number of graphs shown at each step

	smallest_lambda <- function(x, q = 0.95, L = 0.001, U = 1e6){
		f <- function(la) ppois(x, la, lower.tail = FALSE) - q
		uniroot(f, lower = L, upper = U)$root
	}
	
	biggest_lambda <- function(x, q = 0.95, L = 0.001, U = 1e6){
		f <- function(la) ppois(x, la) - q
		uniroot(f, lower = L, upper = U)$root
	}
	
	
	# functions for switching between probability and logistic scales
	logistic <- function(p) log(p/(1-p))
	expit <- function(x) 1/(1 + exp(-x))
	

	##### construct and initialize reactive stuff
	############################################################
	bag <- reactiveValues()
	
	# number of hypothetical samples
	bag$N <- 50
	
	# selection counter
	bag$counter <- 0
	
	# store all parameters at each step
	bag$store_mesh <- data.frame(intervals = 0, step = 0)
	bag$store_all_selected <- data.frame(selected_param = 0, step = 0)

	
	##### define general sampling functions
	############################################################
	generate_permutation <- function() bag$permutation <- sample(n_frames)
	
	# generate datasets
	generate_datasets <- function(){
		
		if(input$data_model == "Bernoulli"){
			list_of_datasets <- lapply(bag$param_mesh, function(th){
				data.frame(x = rbinom(bag$N, 1, th), param = th, stringsAsFactors = FALSE)
			})
		}
		
		if(input$data_model == "Poisson"){
			list_of_datasets <- lapply(bag$param_mesh, function(th){
				data.frame(x = rpois(bag$N, th), param = th, stringsAsFactors = FALSE)
			})
		}

		# combine datasets and store in bag
		bag$datasets <<- do.call(rbind, list_of_datasets)
		
		# permute
		bag$datasets$perm <<- rep(bag$permutation, each = bag$N)
	}

	
	##### define general updating and storage functions
	############################################################
	store_values <- function(){
		
		bag$counter <<- bag$counter + 1

		bag$store_mesh[((n_frames*(bag$counter - 1) + 1) : (n_frames*bag$counter)), 1] <<- bag$param_mesh
		bag$store_mesh[((n_frames*(bag$counter - 1) + 1) : (n_frames*bag$counter)), 2] <<- bag$counter
		bag$store_all_selected[bag$counter, 1] <<- bag$param_selected
		bag$store_all_selected[bag$counter, 2] <<- bag$counter
	}
	
	
	##### generate output plots
	############################################################
	make_plots <- function(my_df){
		
		# bernoulli case
		if(input$data_model == "Bernoulli"){
			g <- ggplot(my_df, aes(x = factor(x, levels = 0:1))) +
				geom_bar() +
				facet_grid(. ~ perm) + 
				scale_x_discrete("", breaks = 0:1, labels = c("Failure", "Success"))
		}
		
		# poisson case
		if(input$data_model == "Poisson"){
			g <- ggplot(my_df, aes(x = x)) + 
				geom_bar() +
				facet_grid(perm ~ ., scales = "free_y") +
				scale_x_continuous("Number of Occurrences")
		}

		# return plot
		g + ylab("Count") + 
			theme(
				axis.title.x = element_text(margin = margin(15, 0, 0, 0)), 
				axis.title.y = element_text(margin = margin(0, 10, 0, 0))
			)
	}
	
	
	##### generate training plots
	###########################################################
	make_training_plots <- function(){
		
		# number of training plots
		samples <- 1:9

		# change number of hypothetical samples if user selects option
		observeEvent(input$hypothetical_N, {
		  bag$N <- input$new_N
		  
		  # randomly generate datasets
		  list_of_datasets <- lapply(bag$param_mesh, function(th){
		    data.frame(x = bag$init_rng(bag$N, th), param = th)
		  })
		  
		  # combine datasets and store in bag
		  bag$datasets <- do.call(rbind, list_of_datasets)
		  
		  # permute
		  generate_permutation()
		  bag$datasets$perm <- rep(bag$permutation, each = bag$N)
		  
		  # make initial selection plots
		  output$select_plots_bern <- renderPlot( make_plots(bag$datasets) )
		})
		
		
		# bernoulli case
		if(input$data_model == "Bernoulli"){
			training_datasets <- lapply(seq_along(samples), function(samples){
				data.frame(x = rbinom(bag$N, 1, input$training_successes / bag$N), 
				  samp = samples, stringsAsFactors = FALSE)
			})
			training_df <- do.call(rbind, training_datasets)
			
			g <- ggplot(training_df, aes(x = factor(x, levels = 0:1))) +
				geom_bar() +
				facet_wrap(~ samp) + 
				scale_x_discrete("", breaks = 0:1, labels = c("Failure", "Success"))
		}
		
		# poisson case
		if(input$data_model == "Poisson"){
			training_datasets <- lapply(seq_along(samples), function(samples){
				data.frame(x = rpois(bag$N, input$training_rate), samp = samples, 
					stringsAsFactors = FALSE)
			})
			training_df <- do.call(rbind, training_datasets)
			
			g <- ggplot(training_df, aes(x = x)) + 
				geom_bar() +
				facet_wrap(~ samp, scales = "free_y") +
				scale_x_continuous("Number of Occurrences")
		}

		# return plot
		g + ylab("Count") + 
			theme(
				axis.title.x = element_text(margin = margin(15, 0, 0, 0)), 
				axis.title.y = element_text(margin = margin(0, 10, 0, 0))
			)
	}
	

	# generate new parameter mesh
	make_interval <- function(interval, selected){
		
		# bernoulli case
		if(input$data_model == "Bernoulli"){

			logit_l <- logistic(selected) - bag$w * bag$multiplier^bag$counter / 2
			logit_u <- logistic(selected) + bag$w * bag$multiplier^bag$counter / 2
			param_l <- expit(logit_l)
			param_u <- expit(logit_u)
			bag$param_mesh <<- seq(param_l, param_u, length.out = n_frames)
		}
		
		# poisson case
		if(input$data_model == "Poisson"){

		  log_l <- log(selected) - (log(selected) - log(min(interval))) * bag$multiplier
		  log_u <- log(selected) + (log(max(interval)) - log(selected)) * bag$multiplier
			param_l <- exp(log_l)
			param_u <- exp(log_u)
			bag$param_mesh <<- seq(param_l, param_u, length.out = n_frames)
		}
	}
	
	##### specify what to do when a selection is made
	############################################################
	update_all <- function(){
		store_values()
		
		make_interval(bag$param_mesh, bag$param_selected)
		generate_permutation()
		generate_datasets()

		if(input$data_model == "Bernoulli"){
			output$select_plots_bern <- renderPlot(make_plots(bag$datasets))
		} else{
			output$select_plots <- renderPlot(make_plots(bag$datasets))
		}
	}
	
	# set prior family
	observeEvent(input$data_model, {
		
		bag$prior_fam <- switch(input$data_model,
			Bernoulli = "Beta",
			Poisson = "Gamma"
		)

		output$prior_family <- renderText(bag$prior_fam)
		output$prior_family_output <- renderText(bag$prior_fam)
	})
	
	
	observeEvent(input$set_bern_inputs, {
		
		# set initial parameter mesh
	  low_init_bern <- 0.05
	  high_init_bern <- 0.95
	  bag$param_mesh <- seq(low_init_bern, high_init_bern, length.out = n_frames)
		bag$w <- logistic(high_init_bern) - logistic(low_init_bern)  # initial logistic width
		
		# set multiplier for shrinking interval width
		bag$multiplier <- 0.85
		
		# set total number of selections and warmup
		bag$total_bern_selections <- 25
		bag$warmup <- bag$total_bern_selections - 5
		
		# set previous sample size to be used in prior computation
		bag$n <- input$n

		# set prior distribution
		bag$prior_density <- dbeta
		
		# define random number generator functions
		bag$init_rng <- function(N, th) rbinom(N, 1, th)
		
		# randomly generate datasets
		list_of_datasets <- lapply(bag$param_mesh, function(th){
			data.frame(x = bag$init_rng(bag$N, th), param = th)
		})
		
		# combine datasets and store in bag
		bag$datasets <- do.call(rbind, list_of_datasets)
		
		# permute
		generate_permutation()
		bag$datasets$perm <- rep(bag$permutation, each = bag$N)

		# make initial selection plots
		output$select_plots_bern <- renderPlot( make_plots(bag$datasets) )
	})
	
	
	observeEvent(input$set_pois_inputs, {
	  
	  # set initial parameter mesh
    low_init_pois <- smallest_lambda(input$min_pois)
    high_init_pois <- biggest_lambda(input$max_pois)
    bag$param_mesh <- seq(low_init_pois, high_init_pois, length.out = n_frames)
    bag$w <- log(high_init_pois) - log(low_init_pois)       # initial log width
    
    # multiplier for shrinking interval width
    bag$multiplier <- 0.9
    
    # set total number of selections and warmup
    tol <- 5        # tolerance for when graphics are similar
    total_pois_selections <- log(tol/bag$param_mesh[3]) / log(bag$multiplier)
    bag$total_pois_selections <- round_any(total_pois_selections, 5, f = ceiling)
    bag$warmup <- bag$total_pois_selections - 5
    
    # set previous sample size to be used in prior computation
    bag$n <- input$n

	  # set prior distribution
	  bag$prior_density <- dgamma

	  # define random number generator functions
	  bag$init_rng <- function(N, th) rpois(N, th)
	  
	  # randomly generate datasets
	  list_of_datasets <- lapply(bag$param_mesh, function(th){
	    data.frame(x = bag$init_rng(bag$N, th), param = th)
	  })
	  
	  # combine datasets and store in bag
	  bag$datasets <- do.call(rbind, list_of_datasets)
	  
	  # permute
	  generate_permutation()
	  bag$datasets$perm <- rep(bag$permutation, each = bag$N)
    
	  # make initial selection plots
	  output$select_plots_pois <- renderPlot( make_plots(bag$datasets) )
	})	
	
	
	# create training plots
	observeEvent(input$generate_bern_training_plots, {
	  output$training_plots <- renderPlot( make_training_plots() )
	})
	
	observeEvent(input$generate_pois_training_plots, {
	  output$training_plots <- renderPlot( make_training_plots() )
	})
	
	
	# make new training plots
	observeEvent(input$new_training_plots, {
		output$training_plots <- renderPlot( make_training_plots() )
	})
	
	
	##### react to graph selections -- bernoulli
	############################################################
	observeEvent(input$plot_1_bern, {
		bag$param_selected <- unique(bag$datasets[bag$datasets$perm == 1, ]$param)
		update_all()
	})
	
	observeEvent(input$plot_2_bern, {
		bag$param_selected <- unique(bag$datasets[bag$datasets$perm == 2, ]$param)
		update_all()
	})
	
	observeEvent(input$plot_3_bern, {
		bag$param_selected <- unique(bag$datasets[bag$datasets$perm == 3, ]$param)
		update_all()
	})
	
	observeEvent(input$plot_4_bern, {
		bag$param_selected <- unique(bag$datasets[bag$datasets$perm == 4, ]$param)
		update_all()
	})
	
	observeEvent(input$plot_5_bern, {
		bag$param_selected <- unique(bag$datasets[bag$datasets$perm == 5, ]$param) 
		update_all()
	})
	
	
	##### react to graph selections -- poisson
	############################################################
	observeEvent(input$plot_1_pois, {
		bag$param_selected <- unique(bag$datasets[bag$datasets$perm == 1, ]$param)
		update_all()
	})
	
	observeEvent(input$plot_2_pois, {
		bag$param_selected <- unique(bag$datasets[bag$datasets$perm == 2, ]$param)
		update_all()
	})
	
	observeEvent(input$plot_3_pois, {
		bag$param_selected <- unique(bag$datasets[bag$datasets$perm == 3, ]$param)
		update_all()
	})
	
	observeEvent(input$plot_4_pois, {
		bag$param_selected <- unique(bag$datasets[bag$datasets$perm == 4, ]$param)
		update_all()
	})
	
	observeEvent(input$plot_5_pois, {
		bag$param_selected <- unique(bag$datasets[bag$datasets$perm == 5, ]$param) 
		update_all()
	})
	
	
	# print selection number
	output$selections_pois <- renderText({
	  paste(bag$counter, "/", bag$total_pois_selections)
	})
	
	output$selections_bern <- renderText({
	  paste(bag$counter, "/", bag$total_bern_selections)
	})
	

	# change warm-up if user selects option
	observeEvent(input$warm_up, bag$warmup <- input$new_warmup)
	
	
	# change sample size if user selects option
	observeEvent(input$previous_n, bag$n <- input$new_n)
	
	
	# calculate hyperparameters using ESS
	compute_hypers <- function(){
		
		if(input$data_model == "Bernoulli"){
			if(bag$warmup == 0){
				bag$selected_keep <- bag$store_all_selected
			} else {
					bag$selected_keep <- bag$store_all_selected[-(1:bag$warmup), ]    # remove warm-up
			}
			
			bag$th_hat1 <- mean(bag$selected_keep$selected_param) * (bag$n - 2) + 1  # alpha
			bag$th_hat2 <- bag$n - bag$th_hat1                                       # beta
		}
		
		if(input$data_model == "Poisson"){
			if(bag$warmup == 0){
				bag$selected_keep <- bag$store_all_selected			
			} else {
					bag$selected_keep <- bag$store_all_selected[-(1:bag$warmup), ]
			}
			
			bag$th_hat1 <- bag$n * mean(bag$selected_keep$selected_param) + 1  # alpha
			bag$th_hat2 <- bag$n                                               # beta
		}
	}
	
	# fit prior
	output$prior_params <- renderPrint({
		compute_hypers()

		bag$hyper_params <<- data.frame(
			"." = round(c(bag$th_hat1, bag$th_hat2), 3),
			row.names = c("Alpha  ", "Beta")
		)

		cat(capture.output(bag$hyper_params)[-1], sep = "\n")
		invisible(bag$hyper_params)
	})
	
	
	# calculate prior summaries
	compute_summaries <- function(){
		
		if(input$data_model == "Bernoulli"){
			# beta prior
			bag$prior_mean   <- bag$th_hat1 / (bag$th_hat1 + bag$th_hat2)
			bag$prior_median <- qbeta(0.5, bag$th_hat1, bag$th_hat2)
			bag$prior_mode   <- (bag$th_hat1 - 1) / (bag$th_hat1 + bag$th_hat2 - 2)
			bag$prior_sd     <- sqrt(bag$th_hat1*bag$th_hat2 / ((bag$th_hat1 + bag$th_hat2)^2 * 
					(bag$th_hat1 + bag$th_hat2 + 1)))
			bag$prior_2_5    <- qbeta(0.025, bag$th_hat1, bag$th_hat2)
			bag$prior_97_5   <- qbeta(0.975, bag$th_hat1, bag$th_hat2)
			bag$ess          <- bag$th_hat1 + bag$th_hat2
		}
		
		if(input$data_model == "Poisson"){
			# gamma prior
			bag$prior_mean   <- bag$th_hat1 / bag$th_hat2
			bag$prior_median <- qgamma(0.5, bag$th_hat1, bag$th_hat2)
			bag$prior_mode   <- (bag$th_hat1 - 1) / bag$th_hat2
			bag$prior_sd     <- sqrt(bag$th_hat1/(bag$th_hat2)^2)
			bag$prior_2_5    <- qgamma(0.025, bag$th_hat1, bag$th_hat2)
			bag$prior_97_5   <- qgamma(0.975, bag$th_hat1, bag$th_hat2)
			bag$ess          <- bag$th_hat2
		}
		
		
		bag$summaries <<- data.frame(
			"." = round(
				c(bag$prior_mean, bag$prior_median, bag$prior_mode, bag$prior_sd, 
					bag$prior_2_5, bag$prior_97_5, bag$ess), 
				4), 
			row.names = c("Mean", "Median", "Mode", "Std. Dev.  ", "2.5%", 
				"97.5%", "ESS")
		)
	}
	
	# print prior summaries
	output$prior_summaries <- renderPrint({
		compute_summaries()
		cat(capture.output(bag$summaries)[-1], sep = "\n")
		invisible(bag$summaries)
	})
	
	
	# prints number of selections -- total, warm-up, and kept
	output$all_selections <- renderPrint({
		bag$n_selections <<- data.frame(
			"." = c(bag$counter, bag$warmup, bag$counter - bag$warmup),
			row.names = c("Total", "Warm-up  ", "Kept")
		)
		
		cat(capture.output(bag$n_selections)[-1], sep = "\n")
		invisible(bag$n_selections)
	})
	
	
	# plot elicited prior
	output$prior_plot <- renderPlot({

		# make plot
	  beta_density <- stat_function(fun = dbeta, geom = "area", n = 1e5,
	    args = list(shape1 = bag$th_hat1, shape2 = bag$th_hat2),
	    fill = "blue", alpha = 0.5, color = NA
	  )
	  
	  gamma_density <- stat_function(fun = dgamma, geom = "area", n = 1e5,
	    args = list(shape = bag$th_hat1, rate = bag$th_hat2),
	    fill = "blue", alpha = 0.5, color = NA
	  )

		p_plot <- ggplot(bag$selected_keep, aes(x = selected_param, geom = "blank")) +
		  labs(y = "Density", title = "Elicited Prior Density") +
			theme(
				plot.title = element_text(size = 17),
				axis.title.x = element_text(margin = margin(15, 0, 0, 0)), 
				axis.title.y = element_text(margin = margin(0, 10, 0, 0))
			)
		
		if(input$data_model == "Bernoulli"){
			p_plot <- p_plot + beta_density + xlab("p") + xlim(c(0, 1))
		} else {
				p_plot <- p_plot + gamma_density + xlab(expression(lambda))
		}
		
		# change x-axis bounds based on user inputs
		if(input$change_x_axis){
			p_plot <- p_plot + xlim(c(input$lower_x, input$upper_x))
		}

		(bag$p_plot <- p_plot)
	})
	
	
	# allow user to find probability for specific interval
	findProb <- eventReactive(input$interval_prob, {
		
		if(input$data_model == "Bernoulli"){
			prior_prob <- round(pbeta(input$upper_prob, bag$th_hat1, bag$th_hat2) 
				- pbeta(input$lower_prob, bag$th_hat1, bag$th_hat2), 4)
		} else {
				prior_prob <- round(pgamma(input$upper_prob, bag$th_hat1, bag$th_hat2)
					- pgamma(input$lower_prob, bag$th_hat1, bag$th_hat2), 4)
		}

		prior_prob_df <- data.frame("." = prior_prob, row.names = "Prob. =")
		cat(capture.output(prior_prob_df)[-1], sep = "\n")
		invisible(prior_prob_df)
	})
	
	# print user's selected prior interval
	output$prob_output <- renderPrint( findProb() )
	
	
	# make history plot of parameters at each step
	output$history_plot <- renderPlot({
		h_plot <- ggplot() +
			geom_point(data = bag$store_mesh, aes(x = step, y = intervals, size = 0.7)) + 
			geom_point(data = bag$store_all_selected, aes(x = step, y = selected_param, 
				color = "red", size = 0.7)) +
			geom_line(data = bag$store_all_selected, aes(x = step, y = selected_param, 
				color = "red", size = 0.6)) +
			scale_x_continuous(limits = c(0, bag$counter), breaks = seq(0, bag$counter, 
			  by = 5)) +
			xlab("Selection Number") +
			theme(plot.title = element_text(size = 16, hjust = 0), 
				axis.title.x = element_text(margin = margin(15, 0, 0, 0)), 
				axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
				legend.position = "none")
		
		if(input$data_model == "Bernoulli"){
			h_plot <- h_plot + ylab("p") + 
				ggtitle("Red:    Selected proportion\nBlack:  Unselected proportion")
		} else {
				h_plot <- h_plot + ylab(expression(lambda)) + 
					ggtitle("Red:    Selected rate\nBlack:  Unselected rate")
		}

		(bag$h_plot <- h_plot)
	})
	
	
	# create report of inputs and results
	output$download_report <- downloadHandler(
		filename = function(){
			paste("elicitation-results", sep = ".", switch(
				input$format, PDF = "pdf", HTML = "html", Word = "docx"
			))
		},
		
		content = function(file){
			markdown_file <- "report.Rmd"
			
			src <- normalizePath(markdown_file)
			
			# temporarily switch to the temp dir, in case you do not have write
			# permission to the current working directory
			owd <- setwd(tempdir())
			on.exit(setwd(owd))
			file.copy(src, markdown_file, overwrite = TRUE)
			
			out <- render(markdown_file, switch(
				input$format,
				PDF = pdf_document(), HTML = html_document(), Word = word_document()
			))
			
			file.rename(out, file)
		}
	)
	
	
	# create csv file of selected parameters
	output$download_selected <- downloadHandler(
		filename = function(){
			paste("elicitation-selected-values", ".csv", sep = "")
		},
		
		content = function(file){
			all_selected <- data.frame("Selected" = bag$store_all_selected$selected_param)
			
			write.csv(all_selected, file, row.names = FALSE)
		}
	)
	
})



