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

# Define a function to perform joinpoint operation using the segmented package.
# Returns a vector of y values representing the joinpoint line.

# See: http://surveillance.cancer.gov/joinpoint/
# See: https://cran.r-project.org/web/packages/JPSurv/index.html
# See: https://www.researchgate.net/publication/234092680_Segmented_An_R_Package_to_Fit_Regression_Models_With_Broken-Line_Relationships
#
# Would like to have used JPSurv::joinpoint() but it is specific to population-based cancer survival analysis
# and expects input data to include columns for number of people at risk, number who died, etc. Instead use
# segmented::segmented() which seems to do the same job for simple y = f(x) data.

joinpoint <- function(x, y, msg_id, max.inflection.points = NULL, verbose = FALSE) {

    if ( verbose ) { cat(paste0("joinpoint(", msg_id, ")\n")) }
    
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
    
    thisx <- NULL
    interpolate <- function(x, y, points_between) {
        
        if ( 0 == points_between ) {
            thisx <<- x
            list(x = x, y = y)
        } else {
            npts <- length(x)
            ngaps <- npts - 1
            retval <- approx(x, y, method = "linear", n = (ngaps * points_between) + npts)
            if ( verbose ) { cat(paste0("\tinterpolate(nx = ", length(retval$x), " , ny = ", length(retval$y), ")\n")) }
            thisx <<- retval$x
            retval
        }
    }

    # We want to automatically find the maximum number of statistically significant inflection points so 
    # iteratively try with successively shrinking sets of starter inflection points which segmented::segmented()
    # requires. Set max/starting number of inflection points to 1/4 of total points otherwise resulting model
    # will just follow the original line almost point by point - as well as generate errors. N.B. - This works
    # OK for the expected initial application of Oregon Health Authority hospital reports where 2009 <= x <= 2014
    # but may be ill-advised for larger ranges of x.
    
    xmin <- min(x)
    xmax <- max(x) 
    make_psi <- function(num.inflection.points) {
        
        if ( 1 == num.inflection.points ) { return(median(c(xmin, xmax))) }
        delta = (xmax - xmin) / num.inflection.points
        retval <- seq(xmin + delta, xmax - delta, delta)
        if ( verbose ) { cat(paste0("\tpsi length = ", length(retval), "\n")) }
        retval
    }
    
    initial.inflection.points <- max.inflection.points
    if ( is.null(initial.inflection.points) ) {
        initial.inflection.points <- floor(length(x) / 4)
    }
    
    # Retry with successively smaller number of inflection points. And wrap that in
    # retries with successively more dense interpolation.
    
    for ( points_between in c(0, 4, 8, 12) ) {
        xylist <- interpolate(x, y, points_between)
        num.inflection.points <- initial.inflection.points
        fit.seg <- NULL
        while ( is.null(fit.seg) & num.inflection.points > 0 ) {
            fit.lm <- lm(y ~ x, xylist)
            fit.seg <- my_segmented(fit.lm, make_psi(num.inflection.points))
            num.inflection.points <- num.inflection.points - 1
        }
        if ( !is.null(fit.seg) ) {
            break
        }
    }
    
    if ( is.null(fit.seg) ) {
        message(paste0("joinpoint(", msg_id, ") - no segmented fit possible, returning lm line."))
        retval <- fitted(lm(y ~ x, data.frame(x = x, y = y)))
    } else {
        # Verify that interpolated line includes the exact same points as original x argument.
        stopifnot(length(x) == sum(x %in% thisx))
        retval <- fitted(fit.seg)[match(x, thisx)]
    }
    retval
}

joinpoint.unit.test <- function() {
    # Set up plot panel.
    par(mfrow = c(2, 2))
    
    # Flat line with non-statistically-significant blip.
    d0 <- data.frame(x = seq(100), y = c(seq(1, 50), 45, seq(52, 100)))
    plot(d0$x, d0$y)
    d0.lm <- lm(y ~ x, data = d0)
    lines(d0$x, fitted(d0.lm))
    lines(d0$x, joinpoint(d0$x, d0$y, "1 segment", verbose = TRUE), col = "red", lwd = 3)
    
    # Single inflection point test case.
    d1 <- data.frame(x = seq(30), y = c(rep(1, 15), seq(1, 15)))
    plot(d1$x, d1$y)
    d1.lm <- lm(y ~ x, data = d1)
    lines(d1$x, fitted(d1.lm))
    lines(d1$x, joinpoint(d1$x, d1$y, "2 segment", verbose = TRUE), col = "red", lwd = 3)
    
    # Double inflection point test case.
    d2 <- data.frame(x = seq(30), y = c(rep(1, 10), seq(1, 10), rep(10, 10)))
    plot(d2$x, d2$y)
    d2.lm <- lm(y ~ x, data = d2)
    lines(d2$x, fitted(d2.lm))
    lines(d2$x, joinpoint(d2$x, d2$y, "3 segment", verbose = TRUE), col = "red", lwd = 3)
    
    # Triple inflection point test case.
    d3 <- data.frame(x=seq(100), y = c(seq(25), seq(25, 1, -1), seq(25), seq(25, 1, -1)))
    plot(d3$x, d3$y)
    d3.lm = lm(y ~ x, data = d3)
    lines(d3$x, fitted(d3.lm))
    lines(d3$x, joinpoint(d3$x, d3$y, "4 segment", verbose = TRUE), col = "red", lwd = 3)
}

#joinpoint.unit.test()


