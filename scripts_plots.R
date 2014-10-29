# this file holds the plotting functions


makeCeacPlot <- function(ceac.int, lambda.int, names, ...) {
  ## makes the CEAC plot
  plot(ceac.int$l.seq, ceac.int$p[, 1], type="l", ylim=c(0,1), ...)
  for (i in 2:ceac.int$d){
    lines(ceac.int$l.seq, ceac.int$p[, i], col = i)
  }
  abline(v=lambda.int, lty=2)
  for (i in 1:ceac.int$d){  
    points(lambda.int, ceac.int$p[which(ceac.int$l.seq == lambda.int), i], pch=20, col="black")
    text(lambda.int, ceac.int$p[which(ceac.int$l.seq == lambda.int), i], ceac.int$p[which(ceac.int$l.seq == lambda.int), i], pos=1, offset=0.1, cex=0.7)
  }
  legend("topright", names, col = c(1:i), lty = 1)
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
       ylim=c(-m3.costs, m3.costs), xlim=c(-m3.effects, m3.effects), col="orangered", ...)
  abline(1, lambda, lty=2)
  abline(h=0)
  abline(v=0)
  points(mean(inc_effects),mean(inc_costs), pch=20, col="black", cex=1.5)
  text(mean(inc_effects),mean(inc_costs),"mean", pos=1, offset=0.1, cex=0.7)
}

makeEvpiPlot <- function(costs.int, effects.int, 
                         incremental.int = FALSE, costscale = TRUE, lambda, ...) {
  ## makes the overall EVPI plot
  l.seq <- seq(0, 60000, 1000)
  p <- c()
  for (lambda.int in l.seq) {
    inb.int <- as.matrix(effects.int) * lambda.int - as.matrix(costs.int)
    if(incremental.int) {
      inb.int <- cbind(0, inb.int)
    } else {
      inb.int <- inb.int - inb.int[, 1]
    }
    #inb.int
    evpi <- mean(pmax(inb.int[, 2], inb.int[, 1])) - max(colMeans(inb.int))
    if(!costscale) evpi <- evpi / lambda.int
    p <- c(p, evpi)
  }  
  plot(l.seq, p, type="l", xlim = c(0, 3*lambda), ...)
  abline(v=lambda, lty=2)
  points(lambda, p[which(l.seq == lambda)], pch=20, col="black")
  text(lambda, p[which(l.seq == lambda)], round(p[which(l.seq == lambda)],2), 
       pos=1, offset=0.1)
}

makeEvpiPopPlot <- function(costs.int, effects.int, costscale = TRUE, lambda , prevalence, 
                            measure) {
  ## makes the overall EVPI plot
  l.seq <- seq(0, 60000, 1000)
  p <- c()
  for (lambda.int in l.seq) {
    inb.int <- as.matrix(effects.int) * lambda.int - as.matrix(costs.int)
    inb.int <- inb.int - inb.int[, 1]
    #inb.int
    evpi <- (mean(pmax(inb.int[, 2], inb.int[, 1])) - max(colMeans(inb.int)))*prevalence
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
  l.seq <- seq(0, 60000, 1000)
  p <- c()
  for (lambda.int in l.seq) {
    inb.int <- as.matrix(effects.int) * lambda.int - as.matrix(costs.int)
    inb.int <- inb.int - inb.int[, 1]
    #inb.int
    evpi <- (mean(pmax(inb.int[, 2], inb.int[, 1])) - max(colMeans(inb.int))) * prevalence * horizon
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


makeNbDensity <- function (costs.int, effects.int, lambda, ...) {
  nb <- createNb(costs.int, effects.int, lambda, FALSE)
  d <- ncol(costs.int) + ifelse(FALSE, 1, 0)
  xmax<-max(nb)
  xmin<-min(nb)
  ymax<-c(1:d)
  for (i in 1:d){
    den<-density(nb[, i])
    ymax[i]<-max(den$y)
  }
  ymax<-max(ymax)
  plot(density(nb[, 1]), type = "l", col = 1, xlim = c(xmin, xmax), ylim = c(0, ymax),xlab="Net Benefit",main="Net Benefit Densities")
  for (i in 2:d){
    lines(density(nb[, 2]), col = i)
  }
  # Need strategy names adding
  legend("topright",colnames(nb),col=c(1:d), lty = 1)
}

makeInbOptDens <- function (costs.int, effects.int, lambda) {
  nb <- createNb(costs.int, effects.int, lambda, FALSE)
  c <- which.max(as.matrix(colMeans(nb)))
  inbOpt <- nb-nb[,c]
  inbOpt <- as.matrix(inbOpt[,-c])
  colnames(inbOpt) <- colnames(nb)[-c]
  d <- ncol(inbOpt) + ifelse(FALSE, 1, 0)
  xmax<-max(inbOpt)
  xmin<-min(inbOpt)
  ymax<-c(1:d)
  for (i in 1:d){
    den<-density(nb[, i])
    ymax[i]<-max(den$y)
  }
  ymax<-max(ymax)
  plot(density(inbOpt[, 1]), type = "l", col = 1, xlim = c(xmin, xmax), 
       ylim = c(0, ymax),xlab="INB vs. Optimal Strategy",
       main="Incremental Net Benefit Density")
  if (d>1) {
    for (i in 2:d){
      lines(density(inbOpt[, i]), col = i)
    }    
  }
  # Need strategy names adding
  legend("topright",colnames(inbOpt),col=c(1:d), lty = 1)
  abline(v=0, lty=2)
}

make2wayDensity <- function(costs.int, effects.int, lambda) {
  ## makes a two way plot of Net Benefit and incremental density
  opar <- par(mfrow = c(1,2))
  makeNbDensity(costs.int, effects.int, lambda)
  makeInbOptDens(costs.int, effects.int, lambda)
  on.exit(par(opar))
}
