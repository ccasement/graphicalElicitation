
library(shiny)
library(ggplot2); theme_set(theme_bw(18))
library(plyr)
library(invgamma)
library(rmarkdown)


shinyServer(function(input, output, session) {
  
  ##### define basic quantities and global functions
  ############################################################
	
  n_frames <- 5       # number of graphs shown at each step
  
  smallest_lambda <- function(x, q = 0.99, L = 0.001, U = 1e6) {
    f <- function(la) ppois(x, la, lower.tail = FALSE) - q
    uniroot(f, lower = L, upper = U)$root
  }

  biggest_lambda <- function(x, q = 0.99, L = 0.001, U = 1e6) {
    f <- function(la) ppois(x, la) - q
    uniroot(f, lower = L, upper = U)$root
  }

  # functions for switching between probability and logistic scales
  logistic <- function(p) log(p/(1-p))
  expit <- function(x) 1/(1 + exp(-x))
  
  
  ##### construct and initialize reactive stuff
  ############################################################
  bag <- reactiveValues()
  
  # hypothetical future sample size
  bag$N <- 100
  
  # counters
  bag$counter <- 0
  bag$mean_counter <- 0
  bag$var_counter <- 0
    
  # reactives for all cases except normal (unknown var)
  bag$store_mesh <- data.frame(intervals = 0, step = 0)
  bag$store_all_selected <- data.frame(selected_param = 0, step = 0)
  
  # reactives for normal (unknown var) case
  bag$store_normal_mean_mesh <- data.frame(intervals = 0, step = 0)
  bag$store_normal_var_mesh <- data.frame(intervals = 0, step = 0)
  bag$store_normal_mean_selected <- data.frame(selected_param = 0, step = 0)
  bag$store_normal_var_selected <- data.frame(selected_param = 0, step = 0)
  
  
  ##### define general sampling functions
  ############################################################
  generate_permutation <- function() bag$permutation <- sample(n_frames)
  
  # generate datasets -- one unknown parameter cases
  generate_datasets <- function() {
  	
  	if (input$data_model == "Bernoulli") {
  	  list_of_datasets <- lapply(bag$param_mesh, function(th) {
      	data.frame(x = rbinom(bag$N, 1, th), param = th, stringsAsFactors = FALSE)
    	})
  	} else if (input$data_model == "Poisson") {
  	  list_of_datasets <- lapply(bag$param_mesh, function(th) {
      	data.frame(x = rpois(bag$N, th), param = th, stringsAsFactors = FALSE)
    	})
  	} else if (input$data_model == "Normal (known variance)") {
  	  list_of_datasets <- lapply(bag$param_mesh, function(th) {
    	  data.frame(x = rnorm(bag$N, th, input$pop_sd), param = th, 
    		  stringsAsFactors = FALSE)
    	})
  	}

    # combine datasets and store in bag
    bag$datasets <<- do.call(rbind, list_of_datasets)
    
    # permute
    bag$datasets$perm <<- rep(bag$permutation, each = bag$N)
  }
  
  # generate datasets -- unknown mean and var case
	generate_normal_mean_datasets <- function() {
		list_of_datasets <- lapply(bag$normal_mean_mesh, function(th){
    	data.frame(x = rnorm(bag$N, th, sqrt(bag$var_selected)), param = th, 
    		stringsAsFactors = FALSE)
		})
		
    bag$datasets <<- do.call(rbind, list_of_datasets)
    bag$datasets$perm <<- rep(bag$permutation, each = bag$N)
	}
	
	generate_normal_var_datasets <- function() {
		list_of_datasets <- lapply(bag$normal_var_mesh, function(th) {
    	data.frame(x = rnorm(bag$N, bag$mean_selected, sqrt(th)), param = th, 
    		stringsAsFactors = FALSE)
		})
		
    bag$datasets <<- do.call(rbind, list_of_datasets)
    bag$datasets$perm <<- rep(bag$permutation, each = bag$N)
	}
  
	
  ##### define general updating and storing functions
  ############################################################
  store_values <- function() {
  	
  	bag$counter <<- bag$counter + 1
  	
  	if (input$data_model == "Normal (unknown variance)") {
  		if (bag$counter %% 2 == 0) {
  			bag$var_counter <<- bag$var_counter + 1
  			bag$var_selected <<- bag$param_selected
  			
	    	bag$store_normal_var_mesh[((n_frames*(bag$var_counter - 1) + 1) : (n_frames*bag$var_counter)), 
	    		1] <<- bag$normal_var_mesh
	    	bag$store_normal_var_mesh[((n_frames*(bag$var_counter - 1) + 1) : (n_frames*bag$var_counter)), 
	    		2] <<- bag$var_counter
	    	bag$store_normal_var_selected[bag$var_counter, 1] <<- bag$var_selected
	    	bag$store_normal_var_selected[bag$var_counter, 2] <<- bag$var_counter
  		} else {
  			bag$mean_counter <<- bag$mean_counter + 1
  			bag$mean_selected <<- bag$param_selected
  			
	    	bag$store_normal_mean_mesh[((n_frames*(bag$mean_counter - 1) + 1) : (n_frames*bag$mean_counter)), 
	    		1] <<- bag$normal_mean_mesh
	    	bag$store_normal_mean_mesh[((n_frames*(bag$mean_counter - 1) + 1) : (n_frames*bag$mean_counter)), 
	    		2] <<- bag$mean_counter
	    	bag$store_normal_mean_selected[bag$mean_counter, 1] <<- bag$mean_selected
	    	bag$store_normal_mean_selected[bag$mean_counter, 2] <<- bag$mean_counter
  		}
  	} else {
	    bag$store_mesh[((n_frames*(bag$counter - 1) + 1) : (n_frames*bag$counter)), 1] <<- bag$param_mesh
	    bag$store_mesh[((n_frames*(bag$counter - 1) + 1) : (n_frames*bag$counter)), 2] <<- bag$counter
	    bag$store_all_selected[bag$counter, 1] <<- bag$param_selected
	    bag$store_all_selected[bag$counter, 2] <<- bag$counter
  	}
  }

  
  ##### generate output plots
  ############################################################
  make_plots <- function(my_df) {
  	
  	if (input$data_model == "Bernoulli") {
  		g <- ggplot(my_df, aes(x = factor(x, levels = 0:1))) +
  			geom_bar() +
  			facet_grid(. ~ perm) + 
  			scale_x_discrete("", breaks = 0:1, labels = c("Failure", "Success"))
  	} else if (input$data_model == "Poisson") {
    	g <- ggplot(my_df, aes(x = x)) + 
    	  geom_histogram(bins = nclass.scott(my_df$x) + 5) +
    		facet_grid(perm ~ .) +
      	scale_x_continuous("Number of Occurrences")
  	} else {
    	g <- ggplot(my_df, aes(x = x)) + 
    	  geom_histogram(bins = nclass.scott(my_df$x) + 5) +
    	  geom_rug() +
    		facet_grid(perm ~ .) +
      	scale_x_continuous(NULL)
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
  make_training_plots <- function() {
  	
  	# number of training plots
		samples <- 1:9
		
		# change number of hypothetical samples if user selects option
		observeEvent(input$hypothetical_N, {
		  bag$N <- input$new_N
		  
		  # randomly generate datasets
		  list_of_datasets <- lapply(bag$param_mesh, function(th) {
		    data.frame(x = bag$init_rng(bag$N, th), param = th)
		  })
		  
		  # combine datasets and store in bag
		  bag$datasets <- do.call(rbind, list_of_datasets)
		  
		  # permute
		  generate_permutation()
		  bag$datasets$perm <- rep(bag$permutation, each = bag$N)
		  
		  # remake initial selection plots
		  if (input$data_model == "Bernoulli") {
		    output$select_plots_bern <- renderPlot( make_plots(bag$datasets) )
		  } else {
		    output$select_plots <- renderPlot( make_plots(bag$datasets) )
		  }
		})
		
		if (input$data_model == "Bernoulli") {
  	  training_datasets <- lapply(seq_along(samples), function(samples) {
  	    data.frame(x = rbinom(bag$N, 1, input$training_successes / bag$N), 
  	      samp = samples, stringsAsFactors = FALSE
  	    )
  	  })
  	  training_df <- do.call(rbind, training_datasets)
  	  
  		g <- ggplot(training_df, aes(x = factor(x, levels = 0:1))) +
  			geom_bar() +
  			facet_wrap(~ samp) + 
  			scale_x_discrete("", breaks = 0:1, labels = c("Failure", "Success"))
  	} else if (input$data_model == "Poisson") {
  	  training_datasets <- lapply(seq_along(samples), function(samples) {
  	    data.frame(x = rpois(bag$N, input$training_rate), samp = samples, 
  	      stringsAsFactors = FALSE
  	    )
  	  })
  	  training_df <- do.call(rbind, training_datasets)
  		
    	g <- ggplot(training_df, aes(x = x)) + 
    	  geom_histogram(bins = nclass.scott(training_df$x)) +
    		facet_wrap(~ samp) +
      	scale_x_continuous("Number of Occurrences")
  	} else if (input$data_model == "Normal (known variance)") {
  	  training_datasets <- lapply(seq_along(samples), function(samples) {
      	data.frame(x = rnorm(bag$N, input$training_mean1, input$training_sd1), 
      	  samp = samples, stringsAsFactors = FALSE
      	)
    	})
  	  training_df <- do.call(rbind, training_datasets)
  	  
    	g <- ggplot(training_df, aes(x = x)) + 
    	  geom_histogram(bins = nclass.scott(training_df$x)) +
    		facet_wrap(~ samp) +
      	scale_x_continuous(NULL)
  	} else {
			training_datasets <- lapply(seq_along(samples), function(samples) {
				data.frame(x = rnorm(bag$N, input$training_mean2, input$training_sd2),
				  samp = samples, stringsAsFactors = FALSE
				)
			})
			training_df <- do.call(rbind, training_datasets)

    	g <- ggplot(training_df, aes(x = x)) +
    	  geom_histogram(bins = nclass.scott(training_df$x)) +
    		facet_wrap(~ samp) +
      	scale_x_continuous(NULL)
  	}

  	# return plot
  	g + ylab("Count") + 
  	  theme(
  	    axis.title.x = element_text(margin = margin(15, 0, 0, 0)), 
    		axis.title.y = element_text(margin = margin(0, 10, 0, 0))
  	  )
  }

	
	# generate new parameter mesh
	make_interval <- function(interval, selected) {
	  
	  # bernoulli case
	  if (input$data_model == "Bernoulli") {
	    logit_l <- logistic(selected) - bag$w_link * bag$multiplier^bag$counter / 2
	    logit_u <- logistic(selected) + bag$w_link * bag$multiplier^bag$counter / 2
	    param_l <- expit(logit_l)
	    param_u <- expit(logit_u)
	    bag$param_mesh <<- seq(param_l, param_u, length.out = n_frames)
	  } else if (input$data_model == "Poisson") {
	    log_l <- log(selected) - (log(selected) - log(min(interval))) * bag$multiplier
	    log_u <- log(selected) + (log(max(interval)) - log(selected)) * bag$multiplier
	    param_l <- exp(log_l)
	    param_u <- exp(log_u)
	    bag$param_mesh <<- seq(param_l, param_u, length.out = n_frames)
	  } else if (input$data_model == "Normal (known variance)") {
	    param_l <- selected - bag$w_link * bag$multiplier^bag$counter / 2
	    param_u <- selected + bag$w_link * bag$multiplier^bag$counter / 2
	    bag$param_mesh <<- seq(param_l, param_u, length.out = n_frames)
	  } else {
	    # mean if counter is even, variance if counter is odd
	    if (bag$counter %% 2 == 0) {
	      param_l <- selected - bag$w_link_mean * bag$multiplier^bag$mean_counter / 2
	      param_u <- selected + bag$w_link_mean * bag$multiplier^bag$mean_counter / 2
	      bag$param_mesh <<- seq(param_l, param_u, length.out = n_frames)
	      bag$normal_mean_mesh <<- bag$param_mesh
	    } else {
	      log_l <- log(selected) - (log(selected) - log(min(interval))) * bag$multiplier
	      log_u <- log(selected) + (log(max(interval)) - log(selected)) * bag$multiplier
	      param_l <- exp(log_l)
	      param_u <- exp(log_u)
	      bag$param_mesh <<- seq(param_l, param_u, length.out = n_frames)
	      bag$normal_var_mesh <<- bag$param_mesh
	    }
	  }
	}
	
	
  ##### specify what to do when a selection is made
  ############################################################
  update_all <- function() {
    store_values()

    if (input$data_model != "Normal (unknown variance)") {
      make_interval(bag$param_mesh, bag$param_selected)
      generate_permutation()
      generate_datasets()
    } else {
  		if (bag$counter %% 2 == 0) {
  			make_interval(bag$normal_mean_mesh, bag$mean_selected)
  			generate_permutation()
  			generate_normal_mean_datasets()
  		} else {
  		  if (bag$counter > 1) make_interval(bag$normal_var_mesh, bag$var_selected)

  			generate_permutation()
  			generate_normal_var_datasets()
  		}
    }
  
    if (input$data_model == "Bernoulli") {
      output$select_plots_bern <- renderPlot( make_plots(bag$datasets) )
    } else{
      output$select_plots <- renderPlot( make_plots(bag$datasets) )
    }
  }


	# set prior family
	observeEvent(input$data_model, {
	  if (input$data_model == "Bernoulli") bag$prior_fam <- "Beta"
	  else if (input$data_model == "Poisson") bag$prior_fam <- "Gamma"
	  else if (input$data_model == "Normal (known variance)") bag$prior_fam <- "Normal"
	  else bag$prior_fam <- "Normal-Inverse-Gamma"
	  
	  output$prior_family <- renderText(bag$prior_fam)
	  output$prior_family_output <- renderText(bag$prior_fam)
	  output$prior_family_output_mean <- renderText("Normal")
	  output$prior_family_output_var <- renderText("Inverse-Gamma")
	  output$prior_family_output_joint <- renderText("Normal-Inverse-Gamma")
	})


	# initial process for bernoulli case
	observeEvent(input$set_bern_inputs, {
	  
	  # set initial parameter mesh
	  low_init <- 0.05
	  high_init <- 0.95
	  bag$param_mesh <- seq(low_init, high_init, length.out = n_frames)
	  bag$w_param <- high_init - low_init           # initial width on param scale
	  bag$w_link <- logistic(high_init) - logistic(low_init)  # initial width on logistic scale
	  
	  # set multiplier for shrinking interval width
	  bag$multiplier <- 0.85
	  
	  # set total number of selections and warmup
	  tol <- 0.02        # tolerance for when graphics are similar
	  total_selections <- log(tol/bag$w_param) / log(bag$multiplier)
	  bag$total_bern_selections <- round_any(total_selections, 5, f = ceiling)
	  bag$warmup <- bag$total_bern_selections - 5
	  
	  # set previous sample size to be used in prior computation
	  bag$n <- input$n
	  updateNumericInput(session, "new_n", value = bag$n)
	  
	  # set prior distribution
	  bag$prior_density <- dbeta
	  
	  # define random number generator functions
	  bag$init_rng <- function(N, th) rbinom(N, 1, th)
	  
	  # randomly generate datasets
	  list_of_datasets <- lapply(bag$param_mesh, function(th) {
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
	
	
	# initial process for all cases except bernoulli
	observeEvent(input$set_non_bern_inputs, {
	  
	  if (input$data_model == "Poisson") {
  	  # set initial parameter mesh
  	  low_init <- smallest_lambda(input$min_init)
  	  high_init <- biggest_lambda(input$max_init)
  	  bag$param_mesh <- seq(low_init, high_init, length.out = n_frames)
  	  bag$w_param <- high_init - low_init           # initial width on param scale
  	  bag$w_link <- log(high_init) - log(low_init)  # initial width on log scale
  	  
  	  # multiplier for shrinking interval width
  	  bag$multiplier <- 0.9
  	  
  	  # set total number of selections and warmup
  	  tol <- 5        # tolerance for when graphics are similar
  	  total_selections <- log(tol/bag$w_param) / log(bag$multiplier)
  	  bag$total_selections <- round_any(total_selections, 5, f = ceiling)
  	  bag$warmup <- bag$total_selections - 5
  	  
  	  # set previous sample size to be used in prior computation
  	  bag$n <- input$n
  	  updateNumericInput(session, "new_n", value = bag$n)
  	  
  	  # set prior distribution
  	  bag$prior_density <- dgamma
  	  
  	  # define random number generator functions
  	  bag$init_rng <- function(N, th) rpois(N, th)
  	  
  	  # randomly generate datasets
  	  list_of_datasets <- lapply(bag$param_mesh, function(th) {
  	    data.frame(x = bag$init_rng(bag$N, th), param = th)
  	  })
	  } else if (input$data_model == "Normal (known variance)") {
	    # set initial parameter mesh
	    low_init <- input$min_init
	    high_init <- input$max_init
	    bag$param_mesh <- seq(low_init, high_init, length.out = n_frames)
	    bag$w_param <- high_init - low_init        # initial width on param scale
	    bag$w_link <- bag$w_param   # same as param scale since link = identity
	    
	    # multiplier for shrinking interval width
	    bag$multiplier <- 0.85
	    
	    # set total number of selections and warmup
	    tol <- 5        # tolerance for when graphics are similar
	    total_selections <- log(tol/bag$w_param) / log(bag$multiplier)
	    bag$total_selections <- round_any(total_selections, 5, f = ceiling)
	    bag$warmup <- bag$total_selections - 5
	    
	    # set previous sample size to be used in prior computation
	    bag$n <- input$n
	    updateNumericInput(session, "new_n", value = bag$n)
	    
	    # set prior distribution
	    bag$prior_density <- dnorm
	    
	    # define random number generator functions
	    bag$init_rng <- function(N, th) rnorm(N, th, input$pop_sd)
	    
	    # randomly generate datasets
	    list_of_datasets <- lapply(bag$param_mesh, function(th) {
	      data.frame(x = bag$init_rng(bag$N, th), param = th)
	    })
	  } else if (input$data_model == "Normal (unknown variance)") {
	    # set initial parameter mesh for mean
	    low_init_mean <- input$min_init
	    high_init_mean <- input$max_init
	    bag$mean_param_mesh <- seq(low_init_mean, high_init_mean, length.out = n_frames)
	    bag$normal_mean_mesh <- bag$mean_param_mesh
	    bag$w_param_mean <- high_init_mean - low_init_mean  # initial width on param scale
	    bag$w_link_mean <- bag$w_param_mean          # initial width on link scale (identity)

	    # set initial parameter mesh for variance
	    bag$est_sd <- (input$max_init - input$min_init) / 6   # based on empirical rule
	    low_init_var <- 1/4 * bag$est_sd^2
	    high_init_var <- 4 * bag$est_sd^2
	    bag$normal_var_mesh <- seq(low_init_var, high_init_var, length.out = n_frames)
	    bag$w_param_var <- high_init_var - low_init_var
	    
	    # multiplier for shrinking interval width
	    bag$multiplier <- 0.85
	    
	    # set total number of selections and warmup
	    tol_mean <- 5        # tolerance for when graphics are similar
	    tol_var <- 25
	    total_mean_selections <- log(tol_mean/bag$w_param_mean) / log(bag$multiplier)
	    total_var_selections <- log(tol_var/bag$w_param_var) / log(bag$multiplier)
	    
	    # total number of selections
	    n_selections <- max(total_mean_selections, total_var_selections)
	    bag$total_selections <- 2 * round_any(n_selections, 5, f = ceiling)
	    bag$warmup <- bag$total_selections / 2 - 5
	    
	    # set previous sample size to be used in prior computation
	    bag$n <- input$n
	    updateNumericInput(session, "new_normal_n", value = bag$n)
	    
	    # set prior distribution
	    bag$prior_density_normal_mean <- dnorm
	    bag$prior_density_normal_var <- dinvgamma
	    
	    # define random number generator functions
	    bag$init_rng <- function(N, th) rnorm(N, th, bag$est_sd)
	    
	    # randomly generate datasets
	    list_of_datasets <- lapply(bag$mean_param_mesh, function(th) {
	      data.frame(x = bag$init_rng(bag$N, th), param = th)
	    })
	  }
	 
	  # set previous sample size to be used in prior computation
	  bag$n <- input$n
	  updateNumericInput(session, "new_n", value = bag$n)
	  
	  # combine datasets and store in bag
	  bag$datasets <- do.call(rbind, list_of_datasets)
	  
	  # permute
	  generate_permutation()
	  bag$datasets$perm <- rep(bag$permutation, each = bag$N)
	  
	  # make initial selection plots
	  output$select_plots <- renderPlot( make_plots(bag$datasets) )
	})

	
	# create training plots
	observeEvent(input$generate_bern_training_plots, {
	  output$training_plots <- renderPlot( make_training_plots() )
	})
	observeEvent(input$generate_pois_training_plots, {
	  output$training_plots <- renderPlot( make_training_plots() )
	})
	observeEvent(input$generate_norm_kv_training_plots, {
	  output$training_plots <- renderPlot( make_training_plots() )
	})
	observeEvent(input$generate_norm_uv_training_plots, {
	  output$training_plots <- renderPlot( make_training_plots() )
	})
	
	# make new training plots
	observeEvent(input$new_training_plots, {
	  output$training_plots <- renderPlot( make_training_plots() )
	})


  ##### react to graph selections -- bernoulli case
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
  
  
  ##### react to graph selections -- all cases except bernoulli
  ############################################################
  observeEvent(input$plot_1, {
    bag$param_selected <- unique(bag$datasets[bag$datasets$perm == 1, ]$param)
    update_all()
  })
  observeEvent(input$plot_2, {
    bag$param_selected <- unique(bag$datasets[bag$datasets$perm == 2, ]$param)
    update_all()
  })
  observeEvent(input$plot_3, {
    bag$param_selected <- unique(bag$datasets[bag$datasets$perm == 3, ]$param)
    update_all()
  })
  observeEvent(input$plot_4, {
    bag$param_selected <- unique(bag$datasets[bag$datasets$perm == 4, ]$param)
    update_all()
  })
  observeEvent(input$plot_5, {
    bag$param_selected <- unique(bag$datasets[bag$datasets$perm == 5, ]$param) 
    update_all()
  })
  

  # print selection number
  output$selections_bern <- renderText({
    paste(bag$counter, "/", bag$total_bern_selections)
  })
  
  output$selections <- renderText({
    paste(bag$counter, "/", bag$total_selections)
  })
  

  # change warm-up if user selects option
  observeEvent(input$warm_up, bag$warmup <- input$new_warmup)
  
  
  # calculate hyperparameters using ESS
  compute_hypers <- function() {
    if (input$data_model != "Normal (unknown variance)") {
      if (bag$warmup == 0) {
        bag$selected_keep <- bag$store_all_selected
      } else {
        # remove warm-up
        bag$selected_keep <- bag$store_all_selected[-(1:bag$warmup), ]
      }
      
      if (input$data_model == "Bernoulli") {
        bag$th_hat1 <- mean(bag$selected_keep$selected_param) * (bag$n - 2) + 1  # alpha
        bag$th_hat2 <- bag$n - bag$th_hat1                                       # beta
      } else if (input$data_model == "Poisson") {
        bag$th_hat1 <- bag$n * mean(bag$selected_keep$selected_param) + 1  # alpha
        bag$th_hat2 <- bag$n                                               # beta
      } else if (input$data_model == "Normal (known variance)") {
      
        bag$th_hat1 <- mean(bag$selected_keep$selected_param)  # mu
        bag$th_hat2 <- (input$pop_sd)^2 / bag$n                # sigma^2
      }
    } else {
      if (bag$warmup == 0) {
        bag$selected_keep_normal_var <<- bag$store_normal_var_selected
        bag$selected_keep_normal_mean <<- bag$store_normal_mean_selected
      } else {
        bag$selected_keep_normal_var <<- bag$store_normal_var_selected[-(1:bag$warmup), ]
        bag$selected_keep_normal_mean <<- bag$store_normal_mean_selected[-(1:bag$warmup), ]
      }
      
      # alpha
      bag$th_hat1_var <- bag$n / 2
      # beta
      bag$th_hat2_var <- 0.5 * mean(bag$selected_keep_normal_var$selected_param) * 
                           (bag$n + 3)
      # mu_0
      bag$th_hat1_mean <- mean(bag$selected_keep_normal_mean$selected_param)
      # (sigma^2)* = (mode of IG prior on variance) / lambda
      bag$th_hat2_mean <- bag$th_hat2_var / ((bag$th_hat1_var + 1) * bag$n)
    }
  }

  
  # print hyperparameters for all cases except normal (unknown var)
  print_hypers <- function() {
    if (input$data_model == "Bernoulli" || input$data_model == "Poisson") {
      bag$hyper_params <<- data.frame(
        "." = round(c(bag$th_hat1, bag$th_hat2), 3),
        row.names = c("Alpha  ", "Beta")
      )
    } else if (input$data_model == "Normal (known variance)") {
      bag$hyper_params <<- data.frame(
        "." = round(c(bag$th_hat1, bag$th_hat2), 3),
        row.names = c("Mu0", "Sigma0^2  ")
      )
    }
    
    cat(capture.output(bag$hyper_params)[-1], sep = "\n")
    invisible(bag$hyper_params)
  }
  
  
  output$prior_params <- renderPrint({
    compute_hypers()
    print_hypers()
	})
  
  
  # change ESS if user selects option
  observeEvent(input$change_ess, {
    bag$n <- input$new_n
    compute_hypers()
    output$prior_params <- renderPrint( print_hypers() )
  })


 	# print hyperparameters for normal (unknown var) case
 	print_normal_hypers_joint <- function() {  
  	if (input$data_model == "Normal (unknown variance)") {
			bag$hyper_params_joint <<- data.frame(
				"." = round(c(bag$th_hat1_mean, bag$n, bag$th_hat1_var, bag$th_hat2_var), 3),
				row.names = c("Mu0", "Lambda  ", "Alpha", "Beta")
			)
  	}
			
  	cat(capture.output(bag$hyper_params_joint)[-1], sep = "\n")
	  invisible(bag$hyper_params_joint)
 	}
 	
 	print_normal_hypers_mean <- function() {  
 	  if (input$data_model == "Normal (unknown variance)") {
 	    bag$hyper_params_mean <<- data.frame(
 	      "." = round(c(bag$th_hat1_mean, bag$th_hat2_mean), 3),
 	      row.names = c("Mu0", "(Sigma^2)*/Lambda  ")
 	    )
 	  }
 	  
 	  cat(capture.output(bag$hyper_params_mean)[-1], sep = "\n")
 	  invisible(bag$hyper_params_joint)
 	}
 	
 	print_normal_hypers_var <- function() {  
 	  if (input$data_model == "Normal (unknown variance)") {
 	    bag$hyper_params_var <<- data.frame(
 	      "." = round(c(bag$th_hat1_var, bag$th_hat2_var), 3),
 	      row.names = c("Alpha  ", "Beta")
 	    )
 	  }
 	  
 	  cat(capture.output(bag$hyper_params_var)[-1], sep = "\n")
 	  invisible(bag$hyper_params_joint)
 	}


	# fit prior for the normal (unknown var) mean
  output$prior_params_joint <- renderPrint({
    compute_hypers()
    print_normal_hypers_joint()
  })
  
  output$prior_params_mean <- renderPrint( print_normal_hypers_mean() )
  output$prior_params_var <- renderPrint( print_normal_hypers_var() )
  
  # change normal (unknown var) ESS if user selects option
  observeEvent(input$change_normal_ess, {
    bag$n <- input$new_normal_n
    compute_hypers()
    
    output$prior_params_normal <- renderPrint({
      print_normal_hypers_joint()
      print_normal_hypers_mean()
      print_normal_hypers_var()
    })
  })
  
  
  # calculate prior summaries
  compute_summaries <- function() {
    if (input$data_model != "Normal (unknown variance)") {
    	if (input$data_model == "Bernoulli") {
    		# beta prior
    		bag$prior_mean   <- bag$th_hat1 / (bag$th_hat1 + bag$th_hat2)
    		bag$prior_median <- qbeta(0.5, bag$th_hat1, bag$th_hat2)
    		bag$prior_mode   <- (bag$th_hat1 - 1) / (bag$th_hat1 + bag$th_hat2 - 2)
    		bag$prior_sd     <- sqrt(bag$th_hat1*bag$th_hat2 / ((bag$th_hat1 + bag$th_hat2)^2 * 
    													(bag$th_hat1 + bag$th_hat2 + 1)))
    		bag$prior_2_5    <- qbeta(0.025, bag$th_hat1, bag$th_hat2)
    		bag$prior_97_5   <- qbeta(0.975, bag$th_hat1, bag$th_hat2)
    		bag$ess          <- bag$th_hat1 + bag$th_hat2
    	} else if (input$data_model == "Poisson") {
    		# gamma prior
    		bag$prior_mean   <- bag$th_hat1 / bag$th_hat2
    		bag$prior_median <- qgamma(0.5, bag$th_hat1, bag$th_hat2)
    		bag$prior_mode   <- (bag$th_hat1 - 1) / bag$th_hat2
    		bag$prior_sd     <- sqrt(bag$th_hat1/(bag$th_hat2)^2)
    		bag$prior_2_5    <- qgamma(0.025, bag$th_hat1, bag$th_hat2)
    		bag$prior_97_5   <- qgamma(0.975, bag$th_hat1, bag$th_hat2)
    		bag$ess          <- bag$th_hat2
    	} else if (input$data_model == "Normal (known variance)") {
    		# normal prior
    		bag$prior_mean   <- bag$th_hat1
    		bag$prior_median <- bag$th_hat1
    		bag$prior_mode   <- bag$th_hat1
    		bag$prior_sd     <- sqrt(bag$th_hat2)
    		bag$prior_2_5    <- qnorm(0.025, bag$prior_mean, bag$prior_sd)
    		bag$prior_97_5   <- qnorm(0.975, bag$prior_mean, bag$prior_sd)
    		bag$ess          <- bag$n
    	}
      
      bag$summaries <<- data.frame("." = round(c(bag$prior_mean, bag$prior_median, 
        bag$prior_mode, bag$prior_sd, bag$prior_2_5, bag$prior_97_5, bag$ess), 4
        ), 
        row.names = c("Mean", "Median", "Mode", "Std. Dev.  ", "2.5%", "97.5%", "ESS")
      )
  	} else {
  		# mean summaries -- normal prior
	  	bag$mean_prior_mean   <- bag$th_hat1_mean
			bag$mean_prior_median <- bag$th_hat1_mean
			bag$mean_prior_mode   <- bag$th_hat1_mean
			bag$mean_prior_sd     <- sqrt(bag$th_hat2_mean)
			bag$mean_prior_2_5    <- qnorm(0.025, bag$mean_prior_mean, bag$mean_prior_sd)
			bag$mean_prior_97_5   <- qnorm(0.975, bag$mean_prior_mean, bag$mean_prior_sd)
			bag$mean_ess          <- bag$n
			
			# variance summaries -- inverse-gamma prior
			bag$var_prior_mean   <- bag$th_hat2_var / (bag$th_hat1_var - 1)
			bag$var_prior_median <- qinvgamma(0.5, bag$th_hat1_var, bag$th_hat2_var)
			bag$var_prior_mode   <- bag$th_hat2_var / (bag$th_hat1_var + 1)
			bag$var_prior_sd     <- sqrt(bag$th_hat2_var^2 / ((bag$th_hat1_var - 1)^2 * 
													 			(bag$th_hat1_var - 2)))
			bag$var_prior_2_5    <- qinvgamma(0.025, bag$th_hat1_var, bag$th_hat2_var)
			bag$var_prior_97_5   <- qinvgamma(0.975, bag$th_hat1_var, bag$th_hat2_var)
			bag$var_ess          <- bag$n
			
			bag$summaries_mean <<- data.frame(
			  "." = round(c(bag$mean_prior_mean, bag$mean_prior_median, bag$mean_prior_mode, 
			    bag$mean_prior_sd, bag$mean_prior_2_5, bag$mean_prior_97_5, bag$mean_ess), 4), 
			  row.names = c("Mean", "Median", "Mode", "Std. Dev.  ", "2.5%", "97.5%", "ESS")
			)
			
			bag$summaries_var <<- data.frame(
			  "." = round(c(bag$var_prior_mean, bag$var_prior_median, bag$var_prior_mode, 
			    bag$var_prior_sd, bag$var_prior_2_5, bag$var_prior_97_5, bag$var_ess), 4), 
			  row.names = c("Mean", "Median", "Mode", "Std. Dev.  ", "2.5%", "97.5%", "ESS")
			)
  	}
  }
  
  
  # print prior summaries for all cases except normal (unknown var)
  output$prior_summaries <- renderPrint({
  	compute_summaries()
  	cat(capture.output(bag$summaries)[-1], sep = "\n")
  	invisible(bag$summaries)
  })
  
  # print prior summaries for normal (unknown var) mean
  output$prior_summaries_mean <- renderPrint({
  	compute_summaries()
  	cat(capture.output(bag$summaries_mean)[-1], sep = "\n")
  	invisible(bag$summaries_mean)
  })
  
  # print prior summaries for normal (unknown var) variance
  output$prior_summaries_var <- renderPrint({
  	cat(capture.output(bag$summaries_var)[-1], sep = "\n")
  	invisible(bag$summaries_var)
  })
  
  
  # print number of selections
  output$all_selections <- renderPrint({
  	bag$n_selections <<- data.frame(
			"." = c(bag$counter, bag$counter - bag$warmup),
			row.names = c("Total", "Kept")
		)
		cat(capture.output(bag$n_selections)[-1], sep = "\n")
		invisible(bag$n_selections)
  })
 
  # print number of selections for normal (unknown var) mean
  output$all_mean_selections <- renderPrint({
  	bag$n_mean_selections <<- data.frame(
			"." = c(bag$mean_counter, bag$mean_counter - bag$warmup),
			row.names = c("Total", "Kept")
		)
		cat(capture.output(bag$n_mean_selections)[-1], sep = "\n")
		invisible(bag$n_mean_selections)
  })

  # print number of selections for normal (unknown var) var
  output$all_var_selections <- renderPrint({
  	bag$n_var_selections <<- data.frame(
			"." = c(bag$var_counter, bag$var_counter - bag$warmup),
			row.names = c("Total", "Kept")
		)
		cat(capture.output(bag$n_var_selections)[-1], sep = "\n")
		invisible(bag$n_var_selections)
  })

  
  # plot elicited prior -- all cases except normal (unknown var)
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
    
    normal_density <- stat_function(fun = dnorm, geom = "area", n = 1e5,
      args = list(mean = bag$th_hat1, sd = sqrt(bag$th_hat2)),
      fill = "blue", alpha = 0.5, color = NA
    )
    
    p_plot <- ggplot(bag$selected_keep, aes(x = selected_param, geom = "blank")) +
      labs(y = "Density", title = "Elicited Prior Density") +
      theme(
        plot.title = element_text(size = 17),
        axis.title.x = element_text(margin = margin(15, 0, 0, 0)), 
        axis.title.y = element_text(margin = margin(0, 10, 0, 0))
      )
    
    if (input$data_model == "Bernoulli") {
      p_plot <- p_plot + beta_density + xlab("p") + xlim(c(0, 1))
    } else if (input$data_model == "Poisson") {
      # set x-axis limits based on widest possible prior (n = 10)
      alpha_plot <- 10 * mean(bag$selected_keep$selected_param) + 1
      beta_plot <- 10
      
      p_plot <- p_plot + gamma_density + xlab(expression(lambda)) +
        xlim(c(qgamma(1e-5, alpha_plot, beta_plot), 
            qgamma(1e-5, alpha_plot, beta_plot, lower.tail = FALSE)
        ))
    } else if (input$data_model == "Normal (known variance)") {
      # set x-axis limits based on widest possible prior (n = 10)
      mean_plot <- mean(bag$selected_keep$selected_param)
      sd_plot <- input$pop_sd / sqrt(10)
      
      p_plot <- p_plot + normal_density + xlab(expression(mu)) +
        xlim(c(qnorm(1e-5, mean_plot, sd_plot), 
            qnorm(1e-5, mean_plot, sd_plot, lower.tail = FALSE)
        ))
    }
    
    # # change x-axis bounds based on user inputs
    # if (input$change_x_axis) {
    #    p_plot <- p_plot + xlim(c(input$lower_x, input$upper_x))
    # }

    (bag$p_plot <- p_plot)
  })
  

  # plot elicited prior -- conditional of mean for normal (unknown var) case
  output$prior_plot_normal_mean <- renderPlot({
    ##### set x-axis limits based on widest possible prior (n = 10)
    # for variance plot
    bag$alpha_plot <- 10 / 2
    bag$beta_plot <- 0.5 * mean(bag$selected_keep_normal_var$selected_param) * (10 + 3)
    
    # for mean plot
    bag$mean_plot <- mean(bag$selected_keep_normal_mean$selected_param)
    bag$var_plot <- bag$beta_plot / ((bag$alpha_plot + 1) * 10)

    p_plot_mean <- ggplot(bag$selected_keep_normal_mean, aes(x = selected_param)) +
		  stat_function(fun = dnorm, geom = "area", n = 1e5,
		    args = list(mean = bag$th_hat1_mean, sd = sqrt(bag$th_hat2_mean)),
		    fill = "blue", alpha = 0.5, color = NA
		  ) +
      xlim(c(qnorm(1e-5, bag$mean_plot, sqrt(bag$var_plot)), 
          qnorm(1e-5, bag$mean_plot, sqrt(bag$var_plot), lower.tail = FALSE)
      )) +
    	labs(x = expression(mu), y = "Density", title = "Elicited Prior Density") +
    	theme(plot.title = element_text(size = 17),
    		axis.title.x = element_text(margin = margin(15, 0, 0, 0)),
    		axis.title.y = element_text(margin = margin(0, 10, 0, 0))
    	)

    (bag$p_plot_mean <- p_plot_mean)
  })


  # plot elicited prior -- variance for normal (unknown var) case
  output$prior_plot_normal_var <- renderPlot({
    p_plot_var <- ggplot(data = bag$selected_keep_normal_var, aes(x = selected_param)) +
      stat_function(fun = dinvgamma, geom = "area", n = 1e5,
        args = list(shape = bag$th_hat1_var, rate = bag$th_hat2_var),
        fill = "blue", alpha = 0.5, color = NA
      ) +
      xlim(c(qinvgamma(1e-2, bag$alpha_plot, bag$beta_plot), 
          qinvgamma(1e-2, bag$alpha_plot, bag$beta_plot, lower.tail = FALSE)
      )) +
      labs(x = expression(sigma^2), y = "Density", title = "Elicited Prior Density") +
      theme(plot.title = element_text(size = 17),
        axis.title.x = element_text(margin = margin(15, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0))
      )

    (bag$p_plot_var <- p_plot_var)
  })

 
  # allow user to find probability for specific interval
  findProb <- eventReactive(input$interval_prob, {
  	if (input$data_model == "Bernoulli") {
  		prior_prob <- round(pbeta(input$upper_prob, bag$th_hat1, bag$th_hat2) 
        - pbeta(input$lower_prob, bag$th_hat1, bag$th_hat2), 4
  		)
  	} else if (input$data_model == "Poisson") {
  		prior_prob <- round(pgamma(input$upper_prob, bag$th_hat1, bag$th_hat2)
        - pgamma(input$lower_prob, bag$th_hat1, bag$th_hat2), 4
  		)
  	} else if (input$data_model == "Normal (known variance)") {
  		prior_prob <- round(pnorm(input$upper_prob, bag$th_hat1, sqrt(bag$th_hat2))
				- pnorm(input$lower_prob, bag$th_hat1, sqrt(bag$th_hat2)), 4
  		)
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
			geom_point(data = bag$store_all_selected, 
			  aes(x = step, y = selected_param, color = "red", size = 0.7)
			) +
	  	geom_line(data = bag$store_all_selected, 
	  	  aes(x = step, y = selected_param, color = "red", size = 0.6)
	    ) +
			scale_x_continuous(limits = c(0, bag$counter), 
			  breaks = seq(0, bag$counter, by = 10)
			) +
    	xlab("Selection Number") +
			theme(plot.title = element_text(size = 16, hjust = 0), 
				axis.title.x = element_text(size = 16, margin = margin(15, 0, 0, 0)), 
    		axis.title.y = element_text(size = 16, margin = margin(0, 10, 0, 0)),
				legend.position = "none"
			)
  	
  	if (input$data_model == "Bernoulli") {
  		h_plot <- h_plot + ylab("p") + 
  			ggtitle("Red:    Selected proportion\nBlack:  Unselected proportion")
  	} else if (input$data_model == "Poisson") {
  		h_plot <- h_plot + ylab(expression(lambda)) + 
  			ggtitle("Red:    Selected rate\nBlack:  Unselected rate")
  	} else if (input$data_model == "Normal (known variance)") {
  	  h_plot <- h_plot + ylab(expression(mu)) + 
  	  	ggtitle("Red:    Selected mean\nBlack:  Unselected mean")
  	}
  	
    (bag$h_plot <- h_plot)
  })
  
  output$history_plot_normal_mean <- renderPlot({
  	h_plot_mean <- ggplot() +
			geom_point(data = bag$store_normal_mean_mesh, 
			  aes(x = step, y = intervals, size = 0.7)
			) + 
			geom_point(data = bag$store_normal_mean_selected, 
			  aes(x = step, y = selected_param, color = "red", size = 0.7)
			) +
	  	geom_line(data = bag$store_normal_mean_selected, 
	  	  aes(x = step, y = selected_param, color = "red", size = 0.6)
	  	) +
			scale_x_continuous(
			  limits = c(0, bag$mean_counter), 
			  breaks = seq(0, bag$mean_counter, by = 10)
			) +
			labs(x = "Selection Number", y = expression(mu), 
				title = "Red:    Selected mean\nBlack:  Unselected mean"
			) +
			theme(plot.title = element_text(size = 16, hjust = 0), 
				axis.title.x = element_text(margin = margin(15, 0, 0, 0)), 
    		axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
				legend.position = "none"
			)
  	
  	(bag$h_plot_mean <- h_plot_mean)
  })
  
  output$history_plot_normal_var <- renderPlot({
  	h_plot_var <- ggplot() +
			geom_point(data = bag$store_normal_var_mesh, 
			  aes(x = step, y = intervals, size = 0.7)
			) + 
			geom_point(data = bag$store_normal_var_selected, 
			  aes(x = step, y = selected_param, color = "red", size = 0.7)
      ) +
	  	geom_line(data = bag$store_normal_var_selected, 
	  	  aes(x = step, y = selected_param, color = "red", size = 0.6)
	  	) +
			scale_x_continuous(limits = c(0, bag$var_counter), 
			  breaks = seq(0, bag$var_counter, by = 10)
			) +
			labs(x = "Selection Number", y = expression(sigma^2), 
				title = "Red:    Selected variance\nBlack:  Unselected variance"
			) +
			theme(plot.title = element_text(size = 16, hjust = 0), 
				axis.title.x = element_text(margin = margin(15, 0, 0, 0)), 
    		axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
				legend.position = "none"
			)
  	
  	(bag$h_plot_var <- h_plot_var)
  })
  
  
  # create report of inputs and results
  output$download_report <- downloadHandler(
  	filename = function() {
  		paste("elicitation-results", sep = ".", switch(
  			input$format, PDF = "pdf", HTML = "html", Word = "docx"
  		))
  	},
  	
  	content = function(file) {
  		if (input$data_model != "Normal (unknown variance)") markdown_file <- "report.Rmd"
  		else markdown_file <- "report2.Rmd"
  		
  		src <- normalizePath(markdown_file)

  		# temporarily switch to the temp dir, in case user doesn't have write
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
  	filename = function() {
  		paste("elicitation-selected-values", ".csv", sep = "")
  	},
  	
  	content = function(file) {
  		if (input$data_model != "Normal (unknown variance)") {
  			all_selected <- data.frame("Selected" = bag$store_all_selected$selected_param)
  		} else {
  		  if (length(bag$store_normal_mean_selected$selected_param) == 
  					length(bag$store_normal_var_selected$selected_param)
  		     ) {
  				
  				all_selected <- data.frame(
  					"Mean" = bag$store_normal_mean_selected$selected_param,
  					"Variance" = bag$store_normal_var_selected$selected_param
  				)
  			} else {
  				all_selected <- data.frame(
  				"Mean" = bag$store_normal_mean_selected$selected_param,
  				"Variance" = c(bag$store_normal_var_selected$selected_param, NA)
  				)
  			}
  		}
  	
  		write.csv(all_selected, file, row.names = FALSE)
  	}
  )

})

