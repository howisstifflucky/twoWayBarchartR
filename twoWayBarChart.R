rm(list=ls())

# Set your wd. Most of my mistakes come from messing this
#up...

# Home
#setwd('...')
# Work
#setwd('...')
# Laptop
#setwd('...')

# Check for idiocy
getwd()

library(plyr) 
library(dplyr)
library(ggplot2)

# Allows for the use of the Times font
windowsFonts(Times=windowsFont('TT Times New Roman'))

#############################################################
# An APA Theme. Saved in an object so it can be used in     #
# multiple plots.                                           #
#############################################################

apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Times', size= 12))

#############################################################
# Function to calculate the mean, the standard deviation    #
# the standard error for each group                         #
#############################################################

# data: a data frame
# varname: the name of a column containing the variable
#to be summarised
# groupnames: vector of column names to be used as
#grouping variables

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE),
      se = sd(x[[col]], na.rm=TRUE)/sqrt(length(na.omit(x[[col]]))))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- plyr::rename(data_sum, c('mean' = varname))
  return(data_sum)
}

###########################

df <- read.csv(file='FILENAME.csv')
df$logSacOnset <- as.numeric(as.character(df$logSacOnset))

###########################

df2 <- data_summary(df, varname='DV', 
                    groupnames=c('IV1', 'IV2'))
                    
# Convert probeloc to a factor variable
# This was only required as I had NA values
df2$probeloc=as.factor(df2$IV1)
head(df2)

df2 <- na.omit(df2)

###########################

# Standard deviation of the mean as error bar
p <- ggplot(df2, aes(x=IV1, y=DV, fill=IV2)) + ylim(0,5) +
  geom_bar(stat='identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=logSacOnset-se, ymax=logSacOnset+se), width=.2,
                position=position_dodge(.9)) +
  labs(x='IV1',y='DV') # Renames axis

# Create theme, adjust axis and rename legend
p + scale_fill_brewer(palette=1,name='IV2') + apatheme + 
  coord_cartesian(ylim=c(2.75,2.85))

