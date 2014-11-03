# this file holds the plotting functions

makeCeacPlot <- function(ceac.int, lambda.int, names.int, ...) {
  ## makes the CEAC plot
  plot(ceac.int$l.seq, ceac.int$p[, 1], type="l", ylim=c(0,1), ...)
  for (i in 2:ceac.int$d){
    lines(ceac.int$l.seq, ceac.int$p[, i], col = i, lty = i)
  }
  abline(v=lambda.int, lty=2)
  # too difficult to see
#   for (i in 1:ceac.int$d){  
#     points(lambda.int, ceac.int$p[which(ceac.int$l.seq == lambda.int), i], pch=20, col="black")
#     text(lambda.int, ceac.int$p[which(ceac.int$l.seq == lambda.int), i], ceac.int$p[which(ceac.int$l.seq == lambda.int), i], pos=1, offset=0.1, cex=0.7)
#   }
  legend("topright", names.int, col = 1:i, lty = 1:i)
}

makeCEPlanePlot <- function(costs.int, effects.int, lambda, ...) {
  ## makes the CE plane
  
  inc_costs <- costs.int[, 2] - costs.int[, 1]
  inc_effects <- effects.int[, 2] - effects.int[, 1]
  
  m.costs <- max(abs(inc_costs))
  m.effects <- max(abs(inc_effects))
  m2.effects <- m.costs / lambda
  m2.costs <- m.effects * lambda
  m3.costs <- max(m.costs, m2.costs)
  m3.effects <- max(m.effects, m2.effects)
  
  main <- paste("Standardised Cost-effectiveness Plane per Person\nlambda =", lambda)
  plot(inc_effects, inc_costs, pty="s", cex=0.4,
       ylim=c(-m3.costs, m3.costs), xlim=c(-m3.effects, m3.effects), col="lightblue", ...)
  abline(1, lambda, lty=2)
  abline(h=0)
  abline(v=0)
  points(mean(inc_effects), mean(inc_costs), pch=20, col="blue", cex=1)
  #text(mean(inc_effects), mean(inc_costs),"mean", pos=1, offset=0.1, cex=0.7)
}

makeEvpiPlot <- function(costs.int, effects.int, costscale = TRUE, lambda, ...) {
  ## makes the overall EVPI plot
  l.seq <- seq(0, lambda * 10, lambda / 20)
  p <- c()
  for (lambda.int in l.seq) {
    inb.int <- data.frame(as.matrix(effects.int) * lambda.int - as.matrix(costs.int))

    evpi <- mean(do.call(pmax, inb.int)) - max(colMeans(inb.int))
    if(!costscale) evpi <- evpi / lambda.int
    p <- c(p, evpi)
  }  
  plot(l.seq, p, type="l", xlim = c(0, 3*lambda), ...)
  abline(v=lambda, lty=2)
  points(lambda, p[which(l.seq == lambda)], pch=20, col="black")
  #text(lambda, p[which(l.seq == lambda)], round(p[which(l.seq == lambda)],2), 
  #     pos=1, offset=0.1)
}

makeEvpiPopPlot <- function(costs.int, effects.int, costscale = TRUE, lambda, prevalence, 
                            measure) {
  ## makes the overall EVPI plot
  l.seq <- seq(0, lambda * 10, lambda / 20)
  p <- c()
  for (lambda.int in l.seq) {
    inb.int <- data.frame(as.matrix(effects.int) * lambda.int - as.matrix(costs.int))
     
    evpi <- (mean(do.call(pmax, inb.int)) - max(colMeans(inb.int))) * prevalence
    if(!costscale) evpi <- evpi / lambda.int
    p <- c(p, evpi)
  }  
  plot(l.seq, p, type="l", main = paste("Overall EVPI per annual prevalence ", measure), xlab = "Threshold willingness to pay", ylab = paste("Annual population EVPI ", measure))
  abline(v=lambda, lty=2)
  points(lambda, p[which(l.seq == lambda)], pch=20, col="black")
  text(lambda, p[which(l.seq == lambda)], round(p[which(l.seq == lambda)],0), 
       pos=1, offset=0.1)
}

makeEvpiHorizonPlot <- function(costs.int, effects.int, costscale = TRUE, lambda, prevalence, 
                                horizon, measure) {
  ## makes the overall EVPI plot
  l.seq <- seq(0, lambda * 10, lambda / 20)
  p <- c()
  for (lambda.int in l.seq) {
    inb.int <- data.frame(as.matrix(effects.int) * lambda.int - as.matrix(costs.int))

    evpi <- (mean(do.call(pmax, inb.int)) - max(colMeans(inb.int))) * prevalence * horizon
    if(!costscale) evpi <- evpi / lambda.int
    p <- c(p, evpi)
  }  
  plot(l.seq, p, type="l", main = paste("Overall EVPI over decision relevance ", measure), xlab = "Threshold willingness to pay", ylab = paste("Annual population EVPI ", measure))
  abline(v=lambda, lty=2)
  points(lambda, p[which(l.seq == lambda)], pch=20, col="black")
  text(lambda, p[which(l.seq == lambda)], round(p[which(l.seq == lambda)],0), 
       pos=1, offset=0.1)
}

make4wayEvpiPlot <- function(costs.int, effects.int, lambda, prevalence, horizon, measure1, 
                             measure2) {
  ## makes a four way plot of CE plane, CEAC and EVPI
  opar <- par(mfrow = c(2,2))
  makeEvpiPopPlot(costs.int, effects.int, costscale = TRUE, lambda, prevalence, measure1)
  makeEvpiPopPlot(costs.int, effects.int, costscale = FALSE, lambda, prevalence, measure2)
  makeEvpiHorizonPlot(costs.int, effects.int, costscale = TRUE, lambda, prevalence, 
                      horizon, measure1)
  makeEvpiHorizonPlot(costs.int, effects.int, costscale = FALSE, lambda, prevalence, 
                      horizon, measure2)
  on.exit(par(opar))
}

makeInbOptBar <- function(costs.int, effects.int, lambda) { # NOT SURE ABOUT THIS - NEED TO DISCUSS
  nb <- createNb(costs.int, effects.int, lambda)
  c <- which.max(as.matrix(colMeans(nb)))
  inbOpt <- nb-nb[,c]
  means <- colMeans(inbOpt)
  sd <- apply(inbOpt, 2, sd)
  lCI <- apply(inbOpt, 2, quantile, 0.025)
  uCI <- apply(inbOpt, 2, quantile, 0.975)
  colnames(inbOpt) <- colnames(nb)
  mp <- barplot(means, 
          main = paste("Expected Incremental Net Benefit vs. Optimal Strategy\nOptimal Strategy is Strategy",c), 
          xlab = "Strategy", ylab = "INB vs. Optimal Strategy", ylim = c(min(lCI), max(uCI)),
          col=0, border=0, names.arg = 1:length(lCI)) 
  segments(mp - 0.2, means, mp + 0.2, means, lwd=2)
  segments(mp, lCI, mp, uCI, lwd=2)
  segments(mp - 0.1, lCI, mp + 0.1, lCI, lwd=2)
  segments(mp - 0.1, uCI, mp + 0.1, uCI, lwd=2)
  abline(h=0, lty=2)

}

makeNbDensity <- function (costs.int, effects.int, lambda) {
  nb <- createNb(costs.int, effects.int, lambda)
  d <- ncol(costs.int) + ifelse(FALSE, 1, 0)
  xmax<-max(nb) + 0.1 * (max(nb) - min(nb))
  xmin<-min(nb) - 0.1 * (max(nb) - min(nb))
  ymax<-c(1:d)
  for (i in 1:d){
    den<-density(nb[, i])
    ymax[i]<-max(den$y)
  }
  ymax<-max(ymax)
  plot(density(nb[, 1]), type = "l", col = 1, xlim = c(xmin, xmax), ylim = c(0, ymax), 
       xlab="Net Benefit",main="Net Benefit Densities")
  for (i in 2:d){
    lines(density(nb[, i]), col = i, lty = i)
  }
  # Need strategy names adding
  legend("topleft", colnames(nb), col=c(1:d), lty = 1:d, cex=0.7)
}

makeInbOptDens <- function (costs.int, effects.int, lambda) {
  nb <- createNb(costs.int, effects.int, lambda)
  c <- which.max(as.matrix(colMeans(nb)))
  inbOpt <- nb - nb[,c]
  inbOpt <- as.matrix(inbOpt[, -c])
  colnames(inbOpt) <- colnames(nb)[-c]
  d <- ncol(inbOpt) + ifelse(FALSE, 1, 0) # what does this ifelse do?
  xmax <- max(inbOpt) + 0.1 * (max(inbOpt) - min(inbOpt))
  xmin <- min(inbOpt) - 0.1 * (max(inbOpt) - min(inbOpt))
  ymax <- 1:d
  for (i in 1:d){
    den <- density(inbOpt[, i])
    ymax[i] <- max(den$y)
  }
  ymax <- max(ymax)
  plot(density(inbOpt[, 1]), type = "l", col = 1, xlim = c(xmin, xmax), 
       ylim = c(0, ymax), xlab="INB vs. Optimal Strategy",
       main="Incremental Net Benefit Density")
  if (d>1) {
    for (i in 2:d){
      lines(density(inbOpt[, i]), col = i, lty = i)
    }    
  }
  # Need strategy names adding
  legend("topleft", colnames(inbOpt), col=1:d, lty = 1:d, cex=0.7)
  abline(v=0, lty=2)
}

make2wayDensity <- function(costs.int, effects.int, lambda) {
  ## makes a two way plot of Net Benefit and incremental density
  opar <- par(mfrow = c(1,2))
  makeNbDensity(costs.int, effects.int, lambda)
  makeInbOptDens(costs.int, effects.int, lambda)
  on.exit(par(opar))
}


makeEvppiBar <- function(pEVPI.int, params) {
  EVPPI <- matrix(pEVPI.int[order(pEVPI.int)], ncol = length(pEVPI.int), nrow = 1)
  colnames(EVPPI) <- colnames(params[order(pEVPI.int)])
  barplot(EVPPI, horiz = TRUE, cex.names=0.7, las=1, main= "Single parameter Partial EVPI per person", xlab = "Partial EVPI per person")  
  
}
