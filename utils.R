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

# Helper functions which seem to cross project boundaries.

library(compiler)

# Pretty print routines - generic, money, and percentage versions. Numbers in, strings out.

scalar_pp <- function(val, digits = 2) {
    format(ifelse ( digits > 0, round(val, digits = digits), as.integer(val) ),
           nsmall = ifelse ( digits > 0, digits, 0 ),
           scientific = FALSE,
           big.mark = ',')
}
scalar_money <- function(val, digits = 2) { paste('$', pp(val, digits), sep = '') }
scalar_moneyM <- function(val) { paste('$', pp(val, digits = 0), 'M', sep = '') }
scalar_pct <- function(val, digits = 2) { paste(pp(val, digits), '%', sep = '') }

pp <- cmpfun(Vectorize(scalar_pp, USE.NAMES = FALSE))
pp0 <- cmpfun(function(val) { pp(val, digits = 0) })
pp1 <- cmpfun(function(val) { pp(val, digits = 1) })
money <- cmpfun(Vectorize(scalar_money, USE.NAMES = FALSE))
moneyM <- cmpfun(Vectorize(scalar_moneyM, USE.NAMES = FALSE))
pct <- cmpfun(Vectorize(scalar_pct, USE.NAMES = FALSE))
pct0 <- cmpfun(function(val) { pct(val, digits = 0) })
pct1 <- cmpfun(function(val) { pct(val, digits = 1) })

# Function to check expected versus offered value accuracy.

scalar_close.enough <- function(expected, offered, permissible.pct.error) {
    lopct <- (100 - permissible.pct.error) / 100
    hipct <- (100 + permissible.pct.error) / 100
    if ( sign(expected) == sign(offered) ) {
        expected <- abs(expected)
        offered <- abs(offered)
        if ( (offered >= (lopct * expected)) && (offered <= (hipct * expected)) ) {
            return(TRUE)
        }
    }
    FALSE
}
close.enough <- cmpfun(Vectorize(scalar_close.enough, USE.NAMES = FALSE))

# Function to round N to nearest integer divisible by M.

scalar_mround <- function(x, base) { 
    base * round(x / base) 
}
mround <- cmpfun(Vectorize(scalar_mround, USE.NAMES = FALSE))
