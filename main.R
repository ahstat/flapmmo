rm(list = ls())
setwd("E:/gitperso/flapmmo/")
#setwd("E:/to/your/directory/")
source("helpers/transform_data.R")
#install.packages("data.table")
#install.packages("dplyr")
#install.packages("dgof")
library(data.table)
library(dplyr)
library(dgof) # to test geometric hypothesis

width = 560
height = 560
dir.create("outputs", showWarnings = FALSE)

#################
# Preprocessing #
#################
# From log data (in folder "data/1csv") to the data frame containing
#   the last jump for each attempt of each user (in folder "data/2jumps").
transform_data("20140213")
transform_data("20140302")

#################################
# Loading data frame to analyze #
#################################
#logdate = "20140213"
logdate = "20140302"

filepath = paste("data/2jumps/", logdate, "_jumps.RDS", sep = "")
df_jumps = readRDS(filepath)

#########################
# 1: Number of attempts #
#########################
# Analysis of a global behavior: the mean over all individuals

##
# Number of attempts for each user
##
num_attempts = df_jumps %>% group_by(id) %>% summarise(length = n())
num_attempts = num_attempts$length

table_attempt = table(num_attempts)/length(num_attempts)
percent_attempt = rep(0, max(num_attempts))
idx = as.numeric(names(table_attempt))
percent_attempt[idx] = table_attempt

## Percent of players for each attempt
outfile = paste("outputs/01_number_attempts_",logdate,".png",sep="")
png(outfile, width, height)
plot(1:length(percent_attempt), 100*percent_attempt, 
     xlab="Number of attempts", ylab="Percent of players",
     xlim=c(0,110)) #, ylim=c(0,16)
dev.off()

## Log percent of players for each log attempt
outfile = paste("outputs/02_number_attempts_",logdate,"_log.png",sep="")
png(outfile, width, height)
plot(log(1:length(percent_attempt)) , log(100*percent_attempt),
     xlab="log(Number of attempts)", ylab="log(Percent of players)")
dev.off()

## Quantile of number of attempts for each player
quantile(num_attempts, probs=c(0,0.05,0.25,0.5,0.75,0.95,1))
#
# Same shape for the two sets: 
# 20140213:
#  0%   5%  25%  50%  75%  95% 100% 
#   1    1    3   10   25   88 1076 
#
# 20140302: 
#  0%   5%  25%  50%  75%  95% 100% 
#   1    1    4   10   25   81  460 

## Boxplot of number of attempts for each player
boxplot(num_attempts, ylab="Number of attempts")

##
# Probability to make a new attempts knowing n attempts have been done
##
cum_perc_attempts = cumsum(percent_attempt) #P(X <= i)
len = length(cum_perc_attempts)

# P(X > 1)
conditional_init= 1-cum_perc_attempts[1]

# P(X > 2 | X >= 2) = P(X > 2) / P(X >= 2), etc.
conditional_after=(1-cum_perc_attempts[2:len])/(1-cum_perc_attempts[1:(len-1)])

# P(X > n | X >= n)
conditional_proba = c(conditional_init, conditional_after)

outfile = paste("outputs/03_number_attempts_conditional_",logdate,".png",sep="")
png(outfile, width, height)
plot(conditional_proba, xlim=c(0,110), ylim=c(0.9,1), 
     xlab="Attempt number n", 
     ylab="Probability to make a new attempt knowing that n attempts have been done")
dev.off()

##########################################################
# 2: Transformation from instant of jump to pipe reached #
##########################################################
# We do not know explictly the reached pipe, we only have the relative date.
# We transform it with a manual regression.
# After this step, we know the reached pipe for each attempts for each player.
# Here, reached pipe means the pipe the user banged.
date_jump = df_jumps$last_jump # relative date of the last jump

# On the following graph, we show that each peak corresponds to a pipe
# The first around 185, the next one around 330, etc.
hist(unlist(date_jump), breaks=20000, xlim=c(0,2000), xlab="Last flap instant")

# The pipe are linearly separated, then a regression should work.
# The regression should verify:
relative_date=c(185, 330, 480, 635, 780, 930)
corresponding_pipe=1:6
reg=lm(relative_date~corresponding_pipe)
summary(reg)

# We tune manually a and b:
b_reg = 39.83
a_reg = 147

# We get a time interval for each pipe
pipe=2:(10+max(unlist(date_jump))/a_reg)
bounds=c(0, 130, a_reg*pipe-b_reg)

# We define pipe_reached = 0 if relative_date <= bounds[2]=130,
#           pipe_reached = 1 if relative_date <= bounds[3]=254.17,
#           etc.
jump_to_pipe = function(x) {
  return(sum(x>bounds)-1)
}

pipe_reached=sapply(date_jump, jump_to_pipe)

## Graphs
hist_relative_date = function(date_jump, bounds, pos_text=1600, ...) {
  hist(unlist(date_jump), breaks=20000, xlab="Last flap instant", ...)
  text(bounds+diff(bounds)[1]/2, rep(pos_text,length(bounds)), 
       0:(length(bounds)-1), col = "gray")
  abline(v=bounds ,col="blue")
}

hist_relative_date(date_jump, bounds, xlim=c(0,2000))
hist_relative_date(date_jump, bounds, pos_text = 22.5, xlim=c(1500,3500), ylim=c(0,25))

# For this one do not work perfectly, not so important however
hist_relative_date(date_jump, bounds, pos_text = 2.5, xlim=c(3000,5000), ylim=c(0,3))

#Removing the 0
df_jumps$last_pipe = pipe_reached
df_jumps = df_jumps %>% filter(pipe_reached > 0)

#########################
# 3: Reached pipe plots #
#########################
# Analysis of a global behavior: mean over all individuals
# Regression for conditional passage through the pipe
last_pipe = df_jumps$last_pipe

rle_last_pipe = df_jumps %>% 
  group_by(id) %>% 
  summarize(touched_pipe = max(last_pipe))

rle_last_pipe = rle(sort(rle_last_pipe$touched_pipe))

reached_pipe=rle_last_pipe[[2]] 
related_percent=rle_last_pipe[[1]]/sum(rle_last_pipe[[1]])

## Maximum pipe reached for players 
outfile = paste("outputs/04_maximum_reached_",logdate,".png",sep="")
png(outfile, width, height)
plot(reached_pipe , 100*related_percent, xlim=c(0,30),
     xlab="Maximum banged pipe", 
     ylab="Percent of players")
dev.off()

quantile(sapply(last_pipe, max))
# 20140213:
#  0%  25%  50%  75% 100% 
#   1    1    1    2   51

# 20140302:
#  0%  25%  50%  75% 100% 
#   1    1    2    3  135

##################################
# 4: Markovian model of attempts #
##################################
# Analysis of a global behavior
# We don't see progression of inidividuals, and assume the model is homogeneous
#
# With this analysis, we can show that:
# * If you reached pipe 1, you next try will be probably 1 too,
# * If you reached pipe >5, you are more likely to reach >5 for the next try,
# * When your score increases, you are more likely to quit the game then.

##
# Update of the matrix of transition
##
# The matrix of transition is updated for each new entry (i,j)
#
# i is the pipe reached for any attempt A,
# j is the pipe reached for the attempt A+1
# There are two special state, the first is ">" (to reach a score higher than
# score_limit) and "cem" (to quit the game instead of attempting).
#
# Note: Computationaly, updating the matrix is not very efficient, but our data
# it's since data are not large.
markov_matrix_update = function(i, j, M, score_limit = 8) {
  if(is.na(i)) {
    if(is.na(j)) {
      M["cem","cem"] = M["cem","cem"] + 1
    } else {
      stop("Impossible to reach any score from cemetery.")
    }
  } else if(i > score_limit) {
    if(is.na(j)) {
      M[">","cem"] = M[">","cem"] + 1
    } else if(j > score_limit) {
      M[">",">"] = M[">",">"] + 1  
    } else {
      M[">",j] = M[">",j] + 1    
    }
  } else {
    if(is.na(j)) {
      M[i,"cem"] = M[i,"cem"] + 1
    } else if(j > score_limit) {
      M[i,">"] = M[i,">"] + 1  
    } else {
      M[i,j] = M[i,j] + 1    
    }
  }
  return(M)
}

##
# Add the score for the next attempt
##
df_jumps = df_jumps %>% group_by(id) %>% mutate(last_pipe_next=lead(last_pipe))

##
# Compute the unnormalized matrix of transition M
##
score_limit = 8
M=matrix(0, ncol=score_limit+2, nrow=score_limit+2, 
         dimnames=list(c(1:score_limit,">","cem"),
                       c(1:score_limit,">","cem")))

for(k in 1:nrow(df_jumps)) {
  if(k %% 10000 == 0) {
    print(k)
  }
  i = df_jumps$last_pipe[k]
  j = df_jumps$last_pipe_next[k]
  M = markov_matrix_update(i, j, M)
}
M["cem","cem"] = 1

##
# Deduce the matrix of transition
##
transition_matrix = M / apply(M,1,sum)
round(transition_matrix, 2)

##
# Output graph
##

## Probability to score 1, or to score >8 in the next try
outfile = paste("outputs/05_markov_transition_", logdate, ".png",sep="")
png(outfile, width, height)
par(mfrow=c(2,1))
plot(transition_matrix[1:9,1], 
     xlab="Score in the current try", 
     ylab="Probability to score 1 in the next try",  xaxt="n")
axis(1, at=1:9,labels=c(1:8,">"), col.axis="black", las=1)
plot(transition_matrix[1:9,9], 
     xlab="Score in the current try", 
     ylab="Probability to score 8 or more in the next try", xaxt="n")
axis(1, at=1:9,labels=c(1:8,">"), col.axis="black", las=1)
dev.off()
par(mfrow=c(1,1))

## Probability to quit the game in the next try
outfile = paste("outputs/06_markov_quit_", logdate, ".png",sep="")
png(outfile, width, height)
plot(transition_matrix[1:9,10], 
     xlab="Score in the current try", 
     ylab="Probability to stop playing", xaxt="n")
axis(1, at=1:9,labels=c(1:8,">"), col.axis="black", las=1)
dev.off()

#######################
# 5: Skill of players #
#######################
# Local analysis: Each player is analysed individually.
#
# Player's score is modeled with a geometric distribution on N*:
# For a player k, Geom(p_k) where p_k represents probability the player
# bangs at each pipe.
p_k = df_jumps %>% group_by(id) %>% summarize(p = 1/mean(last_pipe),
                                              nb_attempts = n())
nb_attempts = p_k$nb_attempts

# Estimated p_k for each player as a function of log number of attempts,
# The red line indicated number of attempt = 30.
nb_attempts_min = 30

plot(log(nb_attempts), p_k$p,
     xlab = "log(Number of attempts of the player)",
     ylab = "Estimated p parameter of the player")
abline(v = log(nb_attempts_min), col = "red")

# Histogram of estimated p
p_k_filtered = p_k %>% filter(nb_attempts > nb_attempts_min)
outfile = paste("outputs/07_hist_geom_p_", logdate, ".png",sep="")
png(outfile, width, height)
hist(p_k_filtered$p, breaks=50, freq = F, xlab="Estimation of p",
     main = "")
dev.off()

##
# Test of the geometric hypothesis
##
# List of players
id = unique(df_jumps$id)

## pvalue obtained with the Cramer-von Mises goodness-of-fit test.
# For each player, we compare his tries with the distribution Geom(p_hat),
# where p_hat is the estimated parameter for this player.
#
# A large p-value indicates the geometric distribution with the estimated
# parameter p_hat cannot be rejected.
pvalue = rep(NA, length(id))

for(k in 1:length(id)) {
  if(k %% 100 == 0) {
    print(paste(k, "/", length(id), sep = ""))
  }
  
  # attempts for this player
  x = df_jumps$last_pipe[which(df_jumps$id == id[k])] 
  
  # estimated param for this player
  p_hat = p_k$p[which(p_k$id == id[k])] 
  
  if(p_hat == 1) {
    pvalue[k] = NA
    # If p_hat = 1, all scores for this player are 1 and the Geometric
    # hypothesis cannot be rejected.
  } else {
    pvalue[k]=try(cvm.test(x, ecdf(rgeom(100000,p_hat)+1), type='W2')[[2]])
    # Note: when an error occurs here, this might indicates the Geometric 
    # hypothesis is not valid.
  }
}
pvalue=as.numeric(pvalue)
plot(nb_attempts, pvalue,
     xlab="Number of attempts", ylab="p-value")
abline(h=0.01, col="red")
# For almost all the players, the hypothesis of geometric distribution 
# cannot be rejected with this test (even for players who play many times).
# For player with a very high number of attempts (over 300) however, 
# the geometric hypothesis seems less valid.

## p-value sorted from the lowest one.
# The players below the red line are those for which the geometric
# hypothesis can be rejected (with alpha = 0.01).
plot(sort(pvalue)[1:100])
abline(h=0.01, col="red")

## Players with p-value lower than 0.01 and related number of attempts
idx_small_pvalue =which(pvalue<0.01)
nb_attempts[idx_small_pvalue]

###################################
# 6: Skill progression of players #
###################################
##
# Selected players
##
id_many_attempts = p_k$id[which(nb_attempts>300)]

##
# Convolution function
##
# Estimate locally the Geometric rate of the player with a convolution.
#
# attempts: Attempts for a selected player
# conv_len: Length of the convolution to the left and to the right
p_hat_convol_func = function(attempts, conv_len = NA) {
  if(is.na(conv_len)) {
    conv_len = floor(length(attempts)/2)
  }
  
  len=length(attempts)
  p_hat_convol=rep(NA,len)
  for(j in 1:len) {
    interval = max(1,j-conv_len):min(len,j+conv_len)
    p_hat_convol[j]= 1/(mean(attempts[interval]))
  }
  return(p_hat_convol)
}

##
# Plot of progression for selected players
##
# Players selected:
# Player 1338298679 (1725 before) : k = 4
# Player 1116623241 (1433 before) : k = 3
# Player 3230596199 (4147 before) : k = 9
for(k in 1:length(id_many_attempts)) {
  id = id_many_attempts[k]
  df_jumps_current = df_jumps[which(df_jumps == id),]
  attempts = df_jumps_current$last_pipe
  
  outfile = paste("outputs/08_p_convol", logdate, "_", id, ".png",sep="")
  png(outfile, width, height)
  par(mfrow=c(1,2), oma = c(0,0,2,0))
  plot(attempts, xlab="Attempts", ylab="Score reached")
  plot(p_hat_convol_func(attempts),
       xlab="Attempts (indexed by l)", ylab="p_{k,l}")
  mtext(paste("Scores and rates estimations for player k=",id, sep=""), line = -2, outer = TRUE,cex=1.2,font=2)
  dev.off()
}