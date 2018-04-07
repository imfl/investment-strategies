########## install packages and read data ########################################

# install.packages('readxl') if not installed
library(readxl)  
gdp <- read_excel('gdp.xlsx')


# rename the columns of gdp
# y for year, q for quarter, g for gdp, r for growth rate, b for changes in inventories
colnames(gdp) <- c('y', 'q', 'g', 'r', 'v')

# install.packages('zoo') if not installed
library(zoo)
gdp$yq <- as.yearqtr(paste(gdp$y, gdp$q, sep = '-'))  # yq for year-quarter

# install.packages('depmixS4') if not stalled
library(depmixS4)



########## get posterior probabilities using Hidden Markov Models ###############

prob.get <- function(x) {
    hmm <- depmix(x ~ 1, family = gaussian(), nstates = 2, data = data.frame(x))
    hmmfit <- fit(hmm, verbose = FALSE)
    prob <- posterior(hmmfit)
    if (prob$S1[1] == 0) {
        prob$S1 = 1 - prob$S1
        prob$S2 = 1 - prob$S2
    }
    prob
}

r.prob <- prob.get(gdp$r)  # r for growth rate
v.prob <- prob.get(gdp$v)  # v for changes in inventories



########## plot and compare posterior probabilities ##############################

prob.plot <- function(x, prob, x.axis = gdp$yq, main.title = '', y.lab = '') {

    layout(1 : 2)  # if layout is not ideal, run this function again with window maximized

    if (main.title == '')
        prob.title <- 'Regime Posterior Probabilities'
    else
        prob.title <- paste('Regime Posterior Probabilities Based on', main.title, sep = ' ')

    plot(x.axis, x, type = 'l', main = main.title, xlab = 'Year', ylab = y.lab)

    matplot(x.axis, prob[ , -1], type = 'l', main = prob.title, xlab = 'Year', ylab = 'Probability')

    legend(x = 'right', c('Regime Boom','Regime Bust'), fill = 1 : 2, bty = 'n')   
}

m1 <- 'GDP Growth Rate'
m2 <- 'Changes in Inventories'
y1 <- 'Growth Rate (%)'
y2 <- 'HK$ M'

prob.plot(gdp$r, r.prob, main.title = m1, y.lab = y1)
prob.plot(gdp$v, v.prob, main.title = m2, y.lab = y2)

prob.comp <- function(prob1, prob2, x.axis = gdp$yq, main1 = '', main2 = '') {

    layout(1 : 2)  # if layout is not ideal, run this function again with window maximized

    if (main1 == '')
        main1 <- 'Regime Posterior Probabilities'
    else
        main1 <- paste('Regime Posterior Probabilities Based on', main1, sep = ' ')

    if (main2 == '')
        main2 <- 'Regime Posterior Probabilities'
    else
        main2 <- paste('Regime Posterior Probabilities Based on', main2, sep = ' ')

    matplot(x.axis, prob1[ , -1], type = 'l', main = main1, xlab = 'Year', ylab = 'Probability')
    legend(x = 'right', c('Regime Boom','Regime Bust'), fill = 1 : 2, bty = 'n')

    matplot(x.axis, prob2[ , -1], type = 'l', main = main2, xlab = 'Year', ylab = 'Probability')
    legend(x = 'right', c('Regime Boom','Regime Bust'), fill = 1 : 2, bty = 'n')
}

prob.comp(r.prob, v.prob, main1 = m1, main2 = m2)



########## moving windows: apply Hidden Markov Models dynamically ##########


start <- which(gdp$yq == as.yearqtr('2008-1'))
end <- which(gdp$yq == as.yearqtr('2016-3'))
n <- end - start + 1
r.moving.update <- matrix(nrow = n, ncol = 3)
v.moving.update <- matrix(nrow = n, ncol = 3)

for (i in 1 : n) {
    moving <- gdp[1 : (start + i - 1), ]
    r.moving <- moving$r
    v.moving <- moving$v
    r.moving.prob <- prob.get(r.moving)
    v.moving.prob <- prob.get(v.moving)
    r.moving.update[i, ] <- unlist(tail(r.moving.prob, n = 1))
    v.moving.update[i, ] <- unlist(tail(v.moving.prob, n = 1))
}

mm1 <- paste('Moving', m1, sep = ' ')
mm2 <- paste('Moving', m2, sep = ' ')

prob.plot(gdp$r[start:end], r.moving.update, x.axis = gdp$yq[start:end],  main.title = mm1, y.lab = y1)
prob.plot(gdp$v[start:end], v.moving.update, x.axis = gdp$yq[start:end],  main.title = mm2, y.lab = y2)
prob.comp(r.moving.update, v.moving.update, x.axis = gdp$yq[start:end], main1 = mm1, main2 = mm2)



########## save result into a csv file ########################################

hmm.mat <- cbind(gdp$y[(start + 1):(end + 1)], gdp$q[(start + 1):(end + 1)], r.moving.update, v.moving.update)  # lag 1 quarter to avoid hindsight bias

colnames(hmm.mat) <- c('Year', 'Quarter', 'r.State', 'r.Prob.1', 'r.Prob.2', 'v.State', 'v.Prob.1', 'v.Prob.2')

write.csv(hmm.mat, file = 'hmm.csv')