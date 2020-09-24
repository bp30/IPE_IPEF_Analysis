# Extract participant means
means <- function(var,grp) {
  return(tapply(var,grp,mean,na.rm=T)[grp])
}

# groupe centering 
group_center <- function(var,grp) {
  return(var-tapply(var,grp,mean,na.rm=T)[grp])
}

#Generate 4 plots to look at the random slope effect of linear mixed model when participant as the random effect 
#plot display option 1= random effect plot ref_group vs dummy1, 2=random slope plot ref_group vs dummy1, 3= individual random slope plot ref_group vs dummy1, 
#4= individual random slope plot ref_group vs dummy2
Plot_LMM<- function (LMM_model, ref_group,dummy1, dummy2, plot_display, DV){
  #extracts varaince parameters from LMM including intercept and random slope values for each participant
  u0_u1 <-coef(LMM_model)[[1]]
  #Obtain the intercept values for participants 
  intercept<- u0_u1[1][,1,]
  #add random slope parameter to each intercept value for each participant, control should be the reference group
  random_slope<- data.frame ('Participant'=rownames(u0_u1) , ref_group= intercept, dummy1= intercept+u0_u1[[2]], dummy2= intercept+u0_u1[[3]])
  names(random_slope)[names(random_slope) == "ref_group"] <- ref_group
  names(random_slope)[names(random_slope) == "dummy1"] <- dummy1
  names(random_slope)[names(random_slope) == "dummy2"] <- dummy2
  #Create individual slope plots
  random_slope2 <- reshape::melt(random_slope, id= 'Participant')
  colnames(random_slope2) <- c('Participants','Condition', 'Mean')
  plot_0v1.df<- random_slope2[random_slope2$Condition!=dummy2,]
  plot_0v2.df <-random_slope2[random_slope2$Condition!=dummy1,]
  #Generate random effect and individual random slope plots for each condition of interest relative to the reference group
  if (plot_display==1){
    plot(random_slope$Participant,u0_u1[[2]],xlab='Participants', ylab=paste(dummy1,'condition random slopes'));abline(0,0, col='red');abline(LMM_model@beta[2],0, col='blue')
  } else if (plot_display==2){
    plot(random_slope$Participant,u0_u1[[3]],xlab='Participants', ylab=paste(dummy2,'condition random slopes'));abline(0,0, col='red');abline(LMM_model@beta[3],0, col='blue')
  } else if (plot_display==3){
    lattice::xyplot (Mean~Condition, data = plot_0v1.df,ylab=DV, groups= Participants, type = c("p","l"),auto.key=list(space="left", columns=4,title="Participants", cex.title=1),ylim=c(1,7))
  } else if (plot_display==4){
    lattice::xyplot (Mean~Condition, data = plot_0v2.df,ylab=DV, groups= Participants, type = c("p","l"),auto.key=list(space="left", columns=4,title="Participants", cex.title=1), ylim=c(1,7))
  }
}

# refit LMM with a bettert start position
refit_LMM<- function (model){
  re_model<- update(model, .~., start= getME(model, 'theta'))
  return (re_model)
}

# Bar graph function
bar_plot<- function (means){
  plot<- ggplot(means, aes(x=Conditions, y=emmean ) ) +
                geom_bar(stat='identity', width=0.5, fill="steelblue") +
                theme_minimal() +
                ylab("Willingness to help") +
                xlab ("Conditions") +
                geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=.2,position=position_dodge(.9)) +
                coord_cartesian(ylim = c(1, 7)) + 
                expand_limits(y = c(1,7)) + 
                scale_y_discrete(limits=c(1,2,3,4,5,6,7)) +
                theme_classic()
  return (plot)
}
