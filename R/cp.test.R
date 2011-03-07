cp.test <-
function (xx, yy, nrep = 1000, alt = 1){
    if (alt == 1) 
        alte <- "One-sided p-values for positive correlation"
    else if (alt == -1) 
        alte <- "One-sided p-values for negative correlation"
    else stop("alt must be 1 or -1")
    p0 <- 1
    p0w <- 0
    ntot0 <- 0
    stat0 <- 0
    npop <- length(xx[, 1])
    nxexpt <- length(xx[1, ])
    nyexpt <- length(yy[1, ])
    for (i in 1:nxexpt) {
        for (j in 1:nyexpt) {
            out <- spearnoties(xx[, i], yy[, j], alt)
            if (out[3] > 2) {
                ntot0 <- ntot0 + (out[3] - 1)
                p0 <- p0 * out[1]
                p0w <- p0w + log(out[1]) * (out[3] - 1)
                stat0 <- stat0 + out[2] * (out[3] - 1)
            }
        }
    }
    stat0 <- stat0/ntot0
    p0w <- p0w/ntot0
    ptest <- rep(1, nrep)
    ptestw <- rep(0, nrep)
    stat <- rep(0, nrep)
    ntot <- rep(0, nrep)
    for (irep in 1:nrep) {
        sam <- sample(1:npop)
        for (i in 1:nxexpt) {
            for (j in 1:nyexpt) {
                out <- spearnoties(xx[, i], yy[sam, j], alt)
                if (out[3] > 2) {
                  ntot[irep] <- ntot[irep] + out[3] - 1
                  ptest[irep] <- ptest[irep] * out[1]
                  ptestw[irep] <- ptestw[irep] + log(out[1]) * 
                    (out[3] - 1)
                  stat[irep] <- stat[irep] + out[2] * (out[3] - 
                    1)
                }
            }
        }
    }
    stat <- stat/ntot
    ptestw <- ptestw/ntot
    ret.val <- list(Alternative = alte, F.p.value = length(ptest[ptest <= 
        p0])/nrep, Fw.p.value = length(ptestw[ptestw <= p0w])/nrep, 
        S.p.value = length((alt * stat)[alt * stat >= alt * stat0])/nrep, 
        Correlation = stat0)
    names(ret.val$Alternative) <- " "
    names(ret.val$F.p.value) <- "Fisher, unweighted"
    names(ret.val$Fw.p.value) <- "Fisher, weighted"
    names(ret.val$S.p.value) <- "Spearman"
    names(ret.val$Correlation) <- "Weighted average Spearman correlation"
    return(ret.val)
}

