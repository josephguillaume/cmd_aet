## hydromad: Hydrological Modelling and Analysis of Data
##
## Derived from cmd.R
## Copyright (c) Felix Andrews <felix@nfrac.org>
##
## Modified by Joseph Guillaume to use actual ET

## IHACRES Catchment Moisture Deficit (CMD) model.
cmd_aet.sim <-
    function(DATA,
             e, d, shape = 0,
             M_0 = d/2,
             return_state = FALSE)
{
    stopifnot(c("P","aET") %in% colnames(DATA))
    ## check values
    stopifnot(e >= 0)
    stopifnot(d >= 0)
    stopifnot(shape >= 0)
    ## default initial state
    if (is.na(M_0)) M_0 <- d / 2

    inAttr <- attributes(DATA[,1])
    DATA <- as.ts(DATA)
    P <- DATA[,"P"]
    E <- DATA[,"aET"]
    ## skip over missing values (maintaining the state M)
    bad <- is.na(P) | is.na(E)
    P[bad] <- 0
    E[bad] <- 0
    COMPILED <- (hydromad.getOption("pure.R.code") == FALSE)
    if (COMPILED) {
        ans <- .C(sma_cmd_aet,
                as.double(P),
                as.double(E),
                as.integer(NROW(DATA)),
                as.double(e),
                as.double(d),
                as.double(shape),
                as.double(M_0),
                U = double(NROW(DATA)),
                M = double(NROW(DATA)),
                ET = double(NROW(DATA)),
                NAOK=FALSE, DUP=FALSE, PACKAGE="cmd.aet")
        U <- ans$U
        M <- ans$M
		ET <- ans$ET
    } else {
        ## implementation in R for cross-checking (slow)
        U <- M <- ET <- P
        M_prev <- M_0
        for (t in seq(1, length(P))) {
            ## default, for when P[t] == 0:
            Mf <- M_prev
            ## select form of dU/dP relationship
            if (P[t] > 0) {
                ## rainfall reduces CMD (Mf)
                if (shape < 1) {
                    ## linear form: dU/dP = 1 - (M/d) for M < d
                    if (M_prev < d) {
                        Mf <- M_prev * exp(-P[t] / d)
                    } else if (M_prev < d + P[t]) {
                        Mf <- d * exp((-P[t] + M_prev - d) / d)
                    } else {
                        Mf <- M_prev - P[t]
                    }
                }
                else if (shape == 1) {
                    ## trigonometric form: dU/dP = 1 - sin^2(pi M / 2d) for M < d
                    if (M_prev < d) {
                        Mf <- 1 / tan((M_prev / d) * (pi / 2))
                        Mf <- (2 * d / pi) * atan(1 / (pi * P[t] / (2 * d) + Mf))
                    } else if (M_prev < d + P[t]) {
                        Mf <- (2 * d / pi) * atan(2 * d / (pi * (d - M_prev + P[t])))
                    } else {
                        Mf <- M_prev - P[t]
                    }
                }
                else { ## shape > 1
                    ## power form: dU/dP = 1 - (M/d)^b for M < d
                    a <- 10 ^ (shape / 50)
                    if (M_prev < d) {
                        Mf <- M_prev * (1 - ((1-a) * P[t] / (d^a)) /
                                        (M_prev ^ (1-a))) ^ (1/(1-a))
                    } else if (M_prev < d + P[t]) {
                        Mf <- d * (1 - (1-a) * (P[t] - M_prev + d) / d) ^ (1/(1-a))
                    } else {
                        Mf <- M_prev - P[t]
                    }
                }
            }
            ## drainage (rainfall not accounted for in -dM)
            U[t] <- max(0, P[t] - M_prev + Mf)
            ## evapo-transpiration
            ET[t] <- e * E[t]
            ET[t] <- max(0, ET[t])
            ## mass balance
            M[t] <- M_prev - P[t] + U[t] + ET[t]
            M_prev <- M[t] <- max(0, M[t])
        }
    }
    attributes(U) <- inAttr
    ## re-insert missing values
    U[bad] <- NA
    ans <- U
    if (return_state) {
        attributes(M) <- attributes(ET) <- attributes(U)
        ans <- cbind(U=U, CMD=M, ET=ET)
    }
    return(ans)
}

cmd_aet.ranges <- function()
    list(e = c(0.9, 1.1),
         d = c(50, 550),
         shape = 0)
