######################################################################
## Copyright (C) 2016, Dave Straube, http://davestraube.com
##     
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA
######################################################################

library(segmented)

JP_DEBUG <- FALSE

# Define a function to perform joinpoint operation using the segmented package.
# See: http://surveillance.cancer.gov/joinpoint/
# See: https://www.researchgate.net/publication/234092680_Segmented_An_R_Package_to_Fit_Regression_Models_With_Broken-Line_Relationships

# This function returns a list which always has the following elements:
#       $x       - x values for best fit line points
#       $y       - y values for best fit line points
#       $fit     - either an lm object or segmented object which represents the best/first fit found
#       $allfits - a list of lists whose elements are $fit, $x, and $y of all fits found which can be
#                  referenced by [[i]] where i correlates to $df$ifit
#       $df      - a data frame summarizing all the fits in $allfits

joinpoint <- function(x, y,                        # vectors of x and y values
                      exit_rule = "first_fit",     # one of c("first_fit", "best_fit")
                      max_joinpoints = 10,         # maximum number of joinpoints to (attempt to) find
                      points_between = 10) {       # number of points to interpolate between values of x
    
    stopifnot(exit_rule %in% c("first_fit", "best_fit"))
    
    # Call to segmented::segmented() may succeed both with and without warnings, or fail with exceptions.
    # A typical exception is when too many inflection points are defined, such as:
    # <simpleError: at least one coef is NA: breakpoint(s) at the boundary? (possibly with many x-values replicated)>
    # A typical warning with success is:
    # # <simpleWarning in summary.lm(object): essentially perfect fit: summary may be unreliable>
    # Use tryCatch() construct to capture first success case wrapped as helper to isolate logic.
    
    my_segmented <- function(fit.lm, psi) {
        
        tryCatch({
            fit <- NULL
            fit <- segmented(fit.lm, seg.Z = ~ x, psi = psi)
        }, warning = function(warn) {
            return(fit)
        }, error = function(err) {
            NULL
        }, finally = {
            return(fit)
        })
    }
    
    # Call to segmented::segmented() can fail to produce a segmented fit when there are too few
    # points in the input. Fake it out by adding points on a straight line between existing points.
    # Relationship of gaps to number of points insures interpolated line has points at all the
    # original locations.
        
    if ( 0 == points_between ) {
        interpolated_xylist <- list(x = x, y = y)
    } else {
        npts <- length(x)
        ngaps <- npts - 1
        interpolated_xylist <- approx(x, y, method = "linear", n = (ngaps * points_between) + npts)
    }
    
    # We want to automatically find the maximum number of statistically significant inflection points so iteratively
    # try with successively shrinking sets of starter inflection points which segmented::segmented() requires.
    
    xmin <- min(x)
    xmax <- max(x) 
    make_psi <- function(num_joinpoints) {
        
        if ( 1 == num_joinpoints ) { 
            median(c(xmin, xmax))
        } else {
            delta <- (xmax - xmin) / (num_joinpoints + 1)
            seq(xmin + delta, xmax - delta, delta)
        }
    }
    
    # Retry with successively smaller number of inflection points.
    
    fit.lm <- lm(y ~ x, interpolated_xylist)
    allfits <- list()
    df <- NULL
    ifit <- 1
    for ( num_joinpoints in seq(max_joinpoints, 0, -1) ) {

        if ( 0 == num_joinpoints ) {
            thisfit <- fit.lm
            thisx <- x
            thisy <- predict(fit.lm, data.frame(x = x))
        } else {
            thisfit <- my_segmented(fit.lm, psi = make_psi(num_joinpoints))
            if ( !is.null(thisfit) ) {
                thisx <- c(xmin, thisfit$psi[, "Est."], xmax)
                thisy <- predict(thisfit, data.frame(x = thisx))
            }
        }
        if ( !is.null(thisfit) ) {
            allfits[[ifit]] <- list(fit = thisfit, x = thisx, y = thisy)
            df <- rbind(df,
                        data.frame(ifit = ifit,
                                   interpolation = points_between,
                                   numjp = num_joinpoints,
                                   BIC = BIC(thisfit)))
            ifit <- ifit + 1
            if ( "first_fit" == exit_rule ) {
                break
            }
        }    
    }
    
    ifit <- NULL
    if ( "best_fit" == exit_rule && dim(df)[1] > 1 ) {

        # Pick "best" fit. General dilemma is that fits with low joinpoint count may not show all relevant
        # segments while fits with high joinpoint count overfit the data. We use the simpler of the NIH
        # National Cancer Institute methods based on the Bayesian Information Criterion (BIC). See:
        #       http://surveillance.cancer.gov/joinpoint/faq/final_model.html
        #       http://surveillance.cancer.gov/joinpoint/Joinpoint_Help_4.2.0.0.pdf
        # The BIC generally decreases as the number of joinpoints increases and empirical testing has shown
        # that it takes a deep nosedive at the ideal fit when looking at fits in increasing
        # joinpoint order. (Though it may rise again later when into the overfitting range.)
        # So we calculate change in BIC and look for the first delta bigger than -50%.
        
        # Add BIC_delta to df where BIC_delta is percent change in BIC from prior fit with less joinpoints.
        df <- df[order(df$numjp),]
        num_rows <- dim(df)[1]
        new <- df$BIC[2:num_rows]
        old <- df$BIC[1:(num_rows-1)]
        df$BIC_delta <- c(0, ((new - old) / abs(old)))
        
        # Find ifit value at the dropoff.
        for ( i in df$ifit ) {
            this_delta <- df[df$ifit == i,][, "BIC_delta"]
            if ( -1 == sign(this_delta) && abs(this_delta) > 0.5 ) {
                ifit <- i
                if ( JP_DEBUG ) { message(paste("best ifit =", ifit)) }
                break
            }
        }
        if ( is.null(ifit) ) {
            # No best fit found using BIC_delta methodology. Assume caller passed in a reality-based max_joinpoints
            # value. In this case, last row in df will be closest to their expected joinpoint count.
            ifit <- df[dim(df)[1], "ifit"]
            if ( JP_DEBUG ) { message(paste("no best fit ifit =", ifit)) }
        }
    } else {
        # "first_fit" case or df had length 1.
        ifit <- df[1, "ifit"]
        if ( JP_DEBUG ) { message(paste("first fit or only fit ifit =", ifit)) }
    }
    if ( JP_DEBUG ) { print(df) }
    
    # Construct return value.
    list(fit = allfits[[ifit]]$fit,
         x = allfits[[ifit]]$x,
         y = allfits[[ifit]]$y,
         allfits = allfits,
         df = df)
}

joinpoint.unit.test <- function() {
    
    debug_save <- JP_DEBUG
    JP_DEBUG <<- TRUE
    
    dlist <- list()
    # Flat line with non-statistically-significant blip.
    dlist[[1]] <- data.frame(x = seq(100), y = c(seq(1, 50), 45, seq(52, 100)))
    # Single inflection point test case.
    dlist[[2]] <- data.frame(x = seq(30), y = c(rep(1, 15), seq(1, 15)))
    # Double inflection point test case.
    dlist[[3]] <- data.frame(x = seq(30), y = c(rep(1, 10), seq(1, 10), rep(10, 10)))
    # Triple inflection point test case.
    dlist[[4]] <- data.frame(x=seq(100), y = c(seq(25), seq(25, 1, -1), seq(25), seq(25, 1, -1)))
    
    # Set up plot panel.
    par(mfrow = c(5, 4))
    
    interp_cases <- c(0, 10, 20, 25)
    for ( points_between in interp_cases ) {

        jplist <- list()
        for ( i in seq(4) ) {
            
            # Generate lm and joinpoint fits.
            jplist[[i]] <- list()
            jplist[[i]]$lm <- lm(y ~ x, data = dlist[[i]])
            jplist[[i]]$jp <- joinpoint(dlist[[i]]$x, dlist[[i]]$y,
                                        exit_rule = "best_fit",
                                        points_between = points_between)
            
            # Print fitted lines in points_between == 10 case to prove that's the smart default.
            # I.e. Top row of BIC_delta plots (points_between == 0) should show that first BIC_delta knee
            # is at bad fit/numjp.
            if ( points_between == interp_cases[2] ) {
                # Plot original points in gray, lm line in black, joinpoint lines/points in red.
                plot(dlist[[i]]$x, dlist[[i]]$y, col = "gray",
                     main = paste0("Expect: ", i-1, "  Found: ", length(jplist[[i]]$jp$x)-2),
                     xlab = "x data", ylab = "y data")
                lines(dlist[[i]]$x, fitted(jplist[[i]]$lm), col = "black")
                points(jplist[[i]]$jp$x, jplist[[i]]$jp$y, col = "red", pch = 23)
                lines(jplist[[i]]$jp$x, jplist[[i]]$jp$y, col = "red", lwd = 3)
            }
        }
        
        for ( i in seq(4) ) {
            # Plot BIC_delta versus numjp to prove BIC_delta algorithm makes sense.
            plot(jplist[[i]]$jp$df$numjp, jplist[[i]]$jp$df$BIC_delta,
                 main = paste("interp =", points_between),
                 xlab = "numjp", ylab = "BIC_delta",
                 xlim = c(0, 10))
            lines(jplist[[i]]$jp$df$numjp, jplist[[i]]$jp$df$BIC_delta)
        }
    }
    
    JP_DEBUG <<- debug_save
}

#joinpoint.unit.test()
