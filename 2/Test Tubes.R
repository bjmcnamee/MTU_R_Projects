# 4) Consider a machine that inserts a needle into test tubes on a conveyer for sampling in a factory process. 
# - This machine may become misaligned in the 2 dimensions of the plane of conveyer travel (x and y axes) independently. 
# - The machine is realigned to centre at the start of each day and it then samples 200 test tubes throughout the day. 
# - The machine fails to sample correctly if it is misaligned in any direction by 2cm or more, as it misses the test tube (possibly colliding with the glass).
# - The x misalignment is 0.1mm on average in the direction of conveyor travel (positive x-direction) for each test, but this can vary somewhat with a standard deviation of 0.1mm. 
# - Similarly, the y misalignment is biased in the negative y-direction, and is much smaller on average; the engineers believe that the average misalignment in the negative y direction is 0.05mm per test, with a standard deviation of 0.05mm.


########################################################################
# 4) a. Simulate the distribution of misalignments at the end of the day
 
library(ggplot2)
library(gridExtra)
set.seed(2021-01-02)

generate_trial_run_histograms <- function(trials){
  zmc <- rep(NA,trials)
  for(j in 1:trials){
      x <- 0; y <- 0; z <- 0
      for(i in 1:200){
          x <- x + rnorm(1,0.1,0.1)
          y <- y + rnorm(1,0.05,0.05)
          z <- round(sqrt(x**2 + y**2),4)
        }
      zmc[j] <- z
    }
  
  df <- data.frame(zmc)
  p <- ggplot(df,aes(zmc)) + geom_histogram(binwidth=0.3) + ggtitle(paste(trials,"trials : Outcomes Distribution")) + theme(plot.title = element_text(size = 10, face = "bold")) + xlab("Cumulative Misalignment")
  return(p)
}

p <- lapply(c("10","100","1000","10000"), generate_trial_run_histograms)
do.call(grid.arrange, c(p))


##############################################################
# 4) b. Estimate the likelihood of failure throughout the day?

generate_trial_sample_data <- function(trials){
  zmc <- rep(NA,200); sample <- rep(NA,200); dfmany <- data.frame()
  for(j in 1:trials){
    x <- 0; y <- 0; z <- 0;
    for(i in 1:200){
          x <- x + rnorm(1,0.1,0.1)
          y <- y + rnorm(1,0.05,0.05)
          z <- round(sqrt(x**2 + y**2),4)
          zmc[i] <- z
          sample[i] <- i
          }
      df <- data.frame(sample,zmc,prob=zmc/20)
      dfmany <- rbind(dfmany,df)
      }
return(dfmany)
}

df <- generate_trial_sample_data(1000)

ggplot(df,aes(sample,zmc)) + geom_line(aes(color = ifelse(zmc<20,'green','red'))) + scale_colour_manual(labels = c("< 20mm", "> 20mm"), values=c('green','red')) + labs(color = "Threshold") + labs(x="Samples", y="Misalignment") + ggtitle("Samples v Misalignment") + geom_hline(yintercept=20,linetype="dashed") + annotate("text", x=40, y=21, label="Max misalignment, 2cm (20mm)", color = "blue")

# sum of frequencies < threshold value for large number trial
success_rate <- round(sum(df$zmc<20)/sum(df$zmc)*100,4)

print(paste("Sum of relative frequencies for large number trial method : cumulative misalignments > 2cm threshold value at any point during day (not end of day) is",success_rate,"%"))

print("Equivalent to the average chance of failing with each sample - not very meaningful")

success_rate <- round(length(df[df$sample == 200 & df$zmc < 20,"sample"]) / length(df[df$sample == 200 & df$zmc > 20,"sample"]) * 100,4)

print(paste("Sum of relative frequencies for large number trial method : cumulative misalignments > 2cm threshold value at end of day is",success_rate,"%"))

#######################################################################################################################
# 4) c. Visualise the simulated alignments of the machine at the end of the day on a scatterplot, showing the 2cm limit

generate_trial_run_data <- function(trials){
  zmc <- vector(); run <- vector()
  for(j in 1:trials){
    x <- 0; y <- 0; z <- 0;
    for(i in 1:200){
      x <- x + rnorm(1,0.1,0.1)
      y <- y + rnorm(1,0.05,0.05)
      z <- round(sqrt(x**2 + y**2),4)
    }
    zmc[j] <- z
    run[j] <- j
    df <- data.frame(run,zmc)
    }
  return(df)
}

df <- generate_trial_run_data(10000)

ggplot(df,aes(run,zmc)) + geom_point(aes(color = ifelse(zmc<20,'green','red'))) + scale_colour_manual(labels = c("< 20mm", "> 20mm"), values=c('green','red')) + labs(color = "Threshold") + labs(x="Runs", y="Misalignment") + ggtitle("Runs v Misalignment") + geom_hline(yintercept=20,linetype="dashed")  + annotate("text", x=2500, y=19.7, label="Max misalignment, 2cm", color = "blue")


# 5) It costs 50,000 when the machine goes offline due to excessive misalignment and no further batches can be tested for the reminder of the day. Each batch passed through the machine results in gross profit of 400. If a batch is ready for testing but the machine is offline, there is a 500 cost for storage and alternate testing of each untested batch under the target number of tests per day. Given these, use Monte Carlo simulations to find the best strategy - i.e. what is the optimal target number of runs per day before realignment should be done.

# Costs : 50000 offline daily fixed charge + 500 storage/testing per batch charge
# Benefits : 400 profit per 200 sample batch

# out of order sign - machine needs to be replaced or repaired
# current 3% batch success rate is so low and the as to not be worthwhile doing even one batch and risking the offline daily fixed charge for a relatively low return
# if however the machine was replaced or repaired then the new max misalignment to ensure breakeven costs would be calculated as 
min_nr <- 50000/400
new_success_rate <- 100 - 100/min_nr
new_success_rate
improvement <- new_success_rate/success_rate
improvement

df <- generate_trial_sample_data(1000)
threshold1 <- 0.99 * 20
threshold2 <- 0.95 * 20
maxsample1 <- min(subset(df, df$zmc >= threshold1,select=sample))
maxsample1
maxsample2 <- min(subset(df, df$zmc >= threshold2,select=sample))
maxsample2

ggplot(df,aes(sample,zmc)) + geom_line(aes(color = ifelse(zmc<20,'green','red'))) + scale_colour_manual(labels = c("< 20mm", "> 20mm"), values=c('green','red')) + labs(color = "Threshold") + labs(x="Samples", y="Misalignment") + ggtitle("Samples v Misalignment") + geom_hline(yintercept=20,linetype="dashed") + geom_vline(xintercept=maxsample1,linetype="dashed") + annotate("text", x=40, y=21, label="Max misalignment, 2cm", color = "blue") + geom_vline(xintercept=maxsample2,linetype="dashed") + annotate("text", x=115, y=15, label=paste(maxsample2,"samples - 95%"), color = "blue")  + annotate("text", x=175, y=17, label=paste(maxsample1,"samples - 99%"), color = "blue") 








