rm(list = ls()) ;  gc(); cat("\014")

# load results
load(file = paste0(getwd(), "/type2-error/simulation_type2.Rdata"))


# coverage
ave <- apply(type2.list$coverage.arr, c(2,3), mean)
long <- c(ave[,1], ave[,2])
df.temp <- data.frame(value = as.numeric(long),
                      num.outliers = c(0:20, 0:20),
                      mediation.analysis = c(rep("classical", 21), rep("robust", 21)),
                      measure = "Coverage")

df <- df.temp

# rejection rate
ave <- apply(type2.list$power.arr, c(2,3), mean)
long <- c(ave[,1], ave[,2])
df.temp <- data.frame(value = as.numeric(long),
                      num.outliers = c(0:20, 0:20),
                      mediation.analysis = c(rep("classical", 21), rep("robust", 21)),
                      measure = "Rejection rate")

df <- rbind(df, df.temp)
df$mediation.analysis <- factor(df$mediation.analysis)

# make plot
library(ggplot2)

plt <- ggplot(df, aes(x = num.outliers, y = value, color = mediation.analysis)) +
  geom_line() + 
  theme(legend.position = "bottom") +
  facet_grid(cols = vars(measure), 
             scales = "free") +
  xlab("Number of Outliers") +
  ylab("Estimate") 

# save plot
ggsave(filename = paste0(getwd(), "/type2-error/plot.pdf"), 
       plot = plt, 
       device = cairo_pdf, 
       width = 7, 
       height = 7, 
       units = "in")