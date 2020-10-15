# Prior predictive simulation 
prior_sim<- function (model, parameter){
  prior<- prior_samples(model)
  if (length(parameter)>=4){
    par(mfrow=c(2,2))
  } else{
    par(mfrow=c(3,2))
  }
  ## intercept population prior 
  plot(density(prior$Intercept,adjust=0.1),main= "Intercept logit scale")
  plot(density(inv_logit_scaled(prior$Intercept),adjust=0.1),main= "Intercept probability scale")
  ## SD prior
  plot(density(prior[, colnames(prior)==parameter[1]],adjust=0.1),main= paste(parameter[1],"logit scale"))
  ## Correlation prior
  plot(density(prior[, colnames(prior)==parameter[2]],adjust=0.1),main=  paste(parameter[2],"logit scale"))
  ## Beta  prior

  for (t in 3:length(parameter)) {
    plot(density(prior[, colnames(prior)==parameter[t]],adjust=0.1),main=  paste(parameter[t],"logit scale"))
    plot(density(inv_logit_scaled(prior$Intercept + prior[, colnames(prior)==parameter[t]])-inv_logit_scaled(prior$Intercept) ,adjust=0.01),
         main= "Condition probability difference")
  }
}
#-----------------------------------------------------------------------------------------------------------------------------------------
# Extract total sample for each parameter and Rhat range
bayes_samples<- function (model){
  n_iter <- (model$fit@sim$iter - model$fit@sim$warmup) * model$fit@sim$chains
  model_sample<-neff_ratio(model)%>% 
    data.frame ()%>%
    rownames_to_column()%>%
    set_names("parameter", "neff_ratio") %>%
    mutate (eff_sample=(neff_ratio*n_iter) %>% round (digit=0))
  max_rhat<- round(max(rhat(model), na.rm=T),digit=3); min_rhat<- round(min (rhat(model), na.rm=T), digit=3)
  max_sample<-max(model_sample$eff_sample, na.rm=T);min_sample<-min(model_sample$eff_sample, na.rm=T)
  print (paste("Rhat range: [",min_rhat,",", max_rhat, "]", sep=""))
  print (paste("Effective sample range: [",min_sample,",", max_sample, "]", sep=''))
  print (paste('MCMC sample size:', n_iter))
  return (model_sample)
} 
#-----------------------------------------------------------------------------------------------------------------------------------------
# Function to calculate BF for order restricted model that is used to compare
restricted_BF<- function(ordered_model, total_samples, n_conditions){
  count <- sum(ordered_model)
  prior<-dim(gtools::permutations(n_conditions, n_conditions))[1]
  ordervsunorder<- (count/total_samples)/ (1/prior)
  print (paste("Total count: ", count,"/", total_samples, sep=""))
  print (paste("Ordered restricted vs unrestricted BF:", ordervsunorder))
  return (ordervsunorder)
}
#-----------------------------------------------------------------------------------------------------------------------------------------
# Do latent SD extraction
latent_SD<- function (posterior){
  log_samples<-posterior %>%
        dplyr::select(tidyselect::vars_select(names(posterior), tidyr::starts_with('b_disc', ignore.case = TRUE)))
  latent_samples<- 1/exp(log_samples)
  print(brms::posterior_summary(latent_samples))
  return (latent_samples)
}
#-----------------------------------------------------------------------------------------------------------------------------------------
# Function to create dataset for violin plot and plot the gg object for violin, also display posterior summary for each posterior
violin_bayes<- function (plot.df, HDI_range, rope){
  # I typically only use 2 HDI 95% or 89% below is the 89% HDI used for plotting
  coefficient <- 0.7
  if (missing(HDI_range)){
  # Default plot 95% HDI not 89% 
    HDI_range <- c(0.025, 0.975)
    coefficient <- 1
  }
  display_plot<-ggplot(plot.df, aes(x=Condition, y=Posterior, fill= Condition)) +
                      geom_violin(trim = FALSE, alpha = 0.6, aes(fill = Condition, color = Condition), size = 1) +
                      geom_boxplot(width = 0.1, fill = "white", outlier.size = 1, aes(colour = Condition)) +
                      theme_classic() +
                      geom_hline(yintercept=0, linetype="dashed", color = "red", size=0.5) +
                      stat_summary(fun = mean, geom = "point", shape = 19, size = 2, aes(colour = Condition)) +
                      stat_boxplot(geom = "errorbar", width = 0.05, coef = coefficient, aes(colour = Condition)) +
                      theme(legend.position = "none")
  if (missing(rope)){
  } else {
    display_plot <- display_plot+geom_hline(yintercept=rope, linetype="dashed", color = "blue", size=0.5)
  }
  conditions<-length(unique(plot.df$Condition))
  posteriors<- data.frame()
  for (x in 1:conditions){
    posterior_sum<- posterior_summary(plot.df$Posterior[plot.df$Condition==unique(plot.df$Condition)[x]], probs=HDI_range)
    row.names(posterior_sum)<- unique(plot.df$Condition)[x]
    posteriors<- rbind(posteriors, posterior_sum)
  }
  return (list(display_plot,posteriors))
}
#-----------------------------------------------------------------------------------------------------------------------------------------
# Checking different priors 
prior_check <- function (intercept, slope, iter, type){
  int<- rnorm (iter, intercept[1], intercept[2])
  b<- rnorm (iter, slope[1], slope[2])
  if (type== "logit"){
    reference_prob<- brms::inv_logit_scaled(int)
    cond_prob <- brms::inv_logit_scaled(int+b)
    diff_prob <- cond_prob - reference_prob
    par(mfrow=c(3,1))
    plot(density(reference_prob, adjust =0.01), main = "intercept probability")
    plot (density(cond_prob, adjust =0.01), main = "slope probability assuming intercept = 0")
    plot(density(diff_prob, adjust =0.01), main = "probability difference")
    print ("Difference range: ")
    brms::posterior_summary(diff_prob)
  } else if (type == "normal"){
    par(mfrow=c(2,1))
    plot(density(int, adjust =0.01), main = "intercept")
    plot (density(b, adjust =0.01), main = "slope")
  }
}
#-----------------------------------------------------------------------------------------------------------------------------------------
# Function used to calculate the difference on the probability scale given ROPE
ROPE<- function (value, int_parameter){
  intercept<- rnorm(1e4, int_parameter[1], int_parameter[2])
  rope<- intercept+value
  prob_diff<- brms::inv_logit_scaled(rope) - brms::inv_logit_scaled(intercept)
  print (paste("Probability difference range:", round(min (prob_diff),digits = 3), '-', round(max(prob_diff), digit=3)))
  output<- cbind.data.frame(intercept, rope, prob_diff)
  return (output)
}