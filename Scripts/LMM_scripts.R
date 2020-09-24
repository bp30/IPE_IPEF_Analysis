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
