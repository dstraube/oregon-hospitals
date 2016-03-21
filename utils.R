# Pretty print routines - generic, money, and percentage versions.

pp <- function(val, digits = 2) {
    format(ifelse ( digits > 0, round(val, digits = digits), as.integer(val) ),
           nsmall = ifelse ( digits > 0, digits, 0 ),
           scientific = FALSE,
           big.mark = ',')
}
money <- function(val, digits = 2) { paste('$', pp(val, digits), sep = '') }
pct <- function(val, digits = 2) { paste(pp(val, digits), '%', sep = '') }