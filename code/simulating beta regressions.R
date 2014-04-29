dataFP$TOTP_avg

# These are the relationships between mu,phi and a,b from the different formulations of a beta distribution / regression 
# mu = a / a+b 
# phi = a + b 
# a = mu*phi
# b = -mu*phi + phi 

# here is what it looks like to generate some random beta-distributed values 
n=1000
mu=0.5
phi=100
a = mu*phi
b = (-mu*phi) + phi
rbeta(n,a,b) 

#################################
# no slope 
# Just an intercept @ 0.5 
# very low percision 
#################################
n=length(dataFP$FPcover_max)
mu = 0.5 
phi = 0.6
a = mu*phi
b = (-mu*phi) + phi
temp <- cbind(dataFP$TOTP_avg, rbeta(n,a,b))
plot(temp[,1],temp[,2],log="x",ylim=c(0,1))

#################################
# no slope 
# Just an intercept @ 0.5 
# very hight percision 
#################################
n=length(dataFP$FPcover_max)
mu = 0.5 
phi = 100
a = mu*phi
b = (-mu*phi) + phi
temp <- cbind(dataFP$TOTP_avg, rbeta(n,a,b))
plot(temp[,1],temp[,2],log="x",ylim=c(0,1))

#################################
# no slope 
# Just an intercept @ 0.8 
# very hight percision 
#################################
n=length(dataFP$FPcover_max)
mu = 0.8 
phi = 100
a = mu*phi
b = (-mu*phi) + phi
temp <- cbind(dataFP$TOTP_avg, rbeta(n,a,b))
plot(temp[,1],temp[,2],log="x",ylim=c(0,1))

#################################
# with a slope 
# very high percision 
# this might not be the best slope
# it should probably hit a ceiling before TOTP ~ 0.5 
#################################
# this is the slope of the line 
m = (max(dataFP$FPcover_max,na.rm=T) - min(dataFP$FPcover_max,na.rm=T)) / 
  (max(dataFP$TOTP_avg,na.rm=T) - min(dataFP$TOTP_avg,na.rm=T))
intercept = min(dataFP$FPcover_max,na.rm=T) # this is the intercept of the line 

phi = 100
mu <- rep(0,length(dataFP$TOTP_avg)) # make a blank vector of mu

# fill it will approprate mu values based on the regression on TOTP 
for (i in 1:length(mu)) {
  mu[i] <- m*dataFP$TOTP_avg[i] + intercept 
}

# make a blank vector of a and b
a <- rep(0,length(dataFP$TOTP_avg))
b <- rep(0,length(dataFP$TOTP_avg))

# figure out the a and b for each mu 
for (i in 1:length(a)) {
  a[i] <- mu[i] * phi 
  b[i] <- (-mu[i]*phi) + phi
}

# generate the random betas with each mu valuae 
temp <- matrix(rep(0,2*length(dataFP$TOTP_avg)),ncol=2)
for (i in 1:length(a)) {
  temp[i,1] <- dataFP$TOTP_avg[i]
  temp[i,2] <- rbeta(1,a[i],b[i])
}

# plot it  
plot(temp[,1],temp[,2],log="x",ylim=c(0,1))

#################################
# with a slope 
# very high percision 
# slope hits a ceiling at TOTP ~ 0.5 
#################################
# this is the slope of the line 
m = (max(dataFP$FPcover_max,na.rm=T) - min(dataFP$FPcover_max,na.rm=T)) / 
  (0.5 - min(dataFP$TOTP_avg,na.rm=T))
intercept = min(dataFP$FPcover_max,na.rm=T) # this is the intercept of the line 

# make a blank vector of mu
mu <- rep(0,length(dataFP$TOTP_avg))
phi <- 100

# fill it will approprate mu values based on the regression on TOTP 
for (i in 1:length(mu)) {
  if (is.na(dataFP$TOTP_avg[i]) == T | dataFP$TOTP_avg[i] < 0.5) {
     mu[i] <- m*dataFP$TOTP_avg[i] + intercept
  }
  else (mu[i] <- max(dataFP$FPcover_max,na.rm=T))
}

# make a blank vector of a and b
a <- rep(0,length(dataFP$TOTP_avg))
b <- rep(0,length(dataFP$TOTP_avg))

# figure out the a and b for each mu 
for (i in 1:length(a)) {
  a[i] <- mu[i] * phi 
  b[i] <- (-mu[i]*phi) + phi
}

# generate the random betas with each mu valuae 
temp <- matrix(rep(0,2*length(dataFP$TOTP_avg)),ncol=2)
for (i in 1:length(a)) {
  temp[i,1] <- dataFP$TOTP_avg[i]
  temp[i,2] <- rbeta(1,a[i],b[i])
}

# plot it  
plot(temp[,1],temp[,2],log="x",ylim=c(0,1))

#################################
# with a slope 
# very high percision 
# slope hits a ceiling at TOTP ~ 0.1 
#################################
# this is the slope of the line 
m = (max(dataFP$FPcover_max,na.rm=T) - min(dataFP$FPcover_max,na.rm=T)) / 
  (0.1 - min(dataFP$TOTP_avg,na.rm=T))
intercept = min(dataFP$FPcover_max,na.rm=T) # this is the intercept of the line 

# make a blank vector of mu
mu <- rep(0,length(dataFP$TOTP_avg))
phi <- 100

# fill it will approprate mu values based on the regression on TOTP 
for (i in 1:length(mu)) {
  if (is.na(dataFP$TOTP_avg[i]) == T | dataFP$TOTP_avg[i] < 0.5) {
    mu[i] <- m*dataFP$TOTP_avg[i] + intercept
  }
  else (mu[i] <- max(dataFP$FPcover_max,na.rm=T))
}

# make a blank vector of a and b
a <- rep(0,length(dataFP$TOTP_avg))
b <- rep(0,length(dataFP$TOTP_avg))

# figure out the a and b for each mu 
for (i in 1:length(a)) {
  a[i] <- mu[i] * phi 
  b[i] <- (-mu[i]*phi) + phi
}

# generate the random betas with each mu valuae 
temp <- matrix(rep(0,2*length(dataFP$TOTP_avg)),ncol=2)
for (i in 1:length(a)) {
  temp[i,1] <- dataFP$TOTP_avg[i]
  temp[i,2] <- rbeta(1,a[i],b[i])
}

# plot it  
plot(temp[,1],temp[,2],log="x",ylim=c(0,1))

#################################
# with a slope 
# very moderate percision - 30
# slope hits a ceiling at TOTP ~ 0.1 
#################################
# this is the slope of the line 
m = (max(dataFP$FPcover_max,na.rm=T) - min(dataFP$FPcover_max,na.rm=T)) / 
  (0.1 - min(dataFP$TOTP_avg,na.rm=T))
intercept = min(dataFP$FPcover_max,na.rm=T) # this is the intercept of the line 

# make a blank vector of mu
mu <- rep(0,length(dataFP$TOTP_avg))
phi <- 30

# fill it will approprate mu values based on the regression on TOTP 
for (i in 1:length(mu)) {
  if (is.na(dataFP$TOTP_avg[i]) == T | dataFP$TOTP_avg[i] < 0.5) {
    mu[i] <- m*dataFP$TOTP_avg[i] + intercept
  }
  else (mu[i] <- max(dataFP$FPcover_max,na.rm=T))
}

# make a blank vector of a and b
a <- rep(0,length(dataFP$TOTP_avg))
b <- rep(0,length(dataFP$TOTP_avg))

# figure out the a and b for each mu 
for (i in 1:length(a)) {
  a[i] <- mu[i] * phi 
  b[i] <- (-mu[i]*phi) + phi
}

# generate the random betas with each mu valuae 
temp <- matrix(rep(0,2*length(dataFP$TOTP_avg)),ncol=2)
for (i in 1:length(a)) {
  temp[i,1] <- dataFP$TOTP_avg[i]
  temp[i,2] <- rbeta(1,a[i],b[i])
}

# plot it  
plot(temp[,1],temp[,2],log="x",ylim=c(0,1))

#################################
# with a slope 
# very low percision 
# slope hits a ceiling at TOTP ~ 0.5 
#################################
# this is the slope of the line 
m = (max(dataFP$FPcover_max,na.rm=T) - min(dataFP$FPcover_max,na.rm=T)) / (0.5 - min(dataFP$TOTP_avg,na.rm=T))
# this is the intercept of the line 
intercept = min(dataFP$FPcover_max,na.rm=T)

# make a blank vector of mu
mu <- rep(0,length(dataFP$TOTP_avg))
phi <- 1

# fill it will approprate mu values based on the regression on TOTP 
for (i in 1:length(mu)) {
  if (is.na(dataFP$TOTP_avg[i]) == T | dataFP$TOTP_avg[i] < 0.5) {
    mu[i] <- m*dataFP$TOTP_avg[i] + intercept
  }
  else (mu[i] <- max(dataFP$FPcover_max,na.rm=T))
}

# make a blank vector of a and b
a <- rep(0,length(dataFP$TOTP_avg))
b <- rep(0,length(dataFP$TOTP_avg))

# figure out the a and b for each mu 
for (i in 1:length(a)) {
  a[i] <- mu[i] * phi 
  b[i] <- (-mu[i]*phi) + phi
}

# generate the random betas with each mu valuae 
temp <- matrix(rep(0,2*length(dataFP$TOTP_avg)),ncol=2)
for (i in 1:length(a)) {
  temp[i,1] <- dataFP$TOTP_avg[i]
  temp[i,2] <- rbeta(1,a[i],b[i])
}

# plot it  
plot(temp[,1],temp[,2],log="x",ylim=c(0,1))

################################
# with a slope 
# very low percision 
# slope hits a ceiling at TOTP ~ 0.1 
#################################
# this is the slope of the line 
m = (max(dataFP$FPcover_max,na.rm=T) - min(dataFP$FPcover_max,na.rm=T)) / (0.1 - min(dataFP$TOTP_avg,na.rm=T))
# this is the intercept of the line 
intercept = min(dataFP$FPcover_max,na.rm=T)

# make a blank vector of mu
mu <- rep(0,length(dataFP$TOTP_avg))
phi <- 1

# fill it will approprate mu values based on the regression on TOTP 
for (i in 1:length(mu)) {
  if (is.na(dataFP$TOTP_avg[i]) == T | dataFP$TOTP_avg[i] < 0.5) {
    mu[i] <- m*dataFP$TOTP_avg[i] + intercept
  }
  else (mu[i] <- max(dataFP$FPcover_max,na.rm=T))
}

# make a blank vector of a and b
a <- rep(0,length(dataFP$TOTP_avg))
b <- rep(0,length(dataFP$TOTP_avg))

# figure out the a and b for each mu 
for (i in 1:length(a)) {
  a[i] <- mu[i] * phi 
  b[i] <- (-mu[i]*phi) + phi
}

# generate the random betas with each mu valuae 
temp <- matrix(rep(0,2*length(dataFP$TOTP_avg)),ncol=2)
for (i in 1:length(a)) {
  temp[i,1] <- dataFP$TOTP_avg[i]
  temp[i,2] <- rbeta(1,a[i],b[i])
}

# plot it  
plot(temp[,1],temp[,2],log="x",ylim=c(0,1))
