
## HAS_TESTS
## returns matrix with dimensions 'n_sex', 'n_region'
unpack_births <- function(x) {
    n_region <- nrow(x)
    offset_dth_emig <- 2L
    start <- n_region + offset_dth_emig + 1L
    end <- ncol(x)
    cols <- seq.int(from = start, to = end)
    ans <- x[ , cols]
    ans <- t(ans) ## sex dimension comes before region dimension
    ans
}

## HAS_TESTS
## returns vector with length 'n_region'
unpack_deaths <- function(x) {
    n_region <- nrow(x)
    x[ , n_region + 1L]
}

## HAS_TESTS
## returns vector with length 'n_region'
unpack_emigration <- function(x) {
    n_region <- nrow(x)
    x[ , n_region + 2L]
}

## HAS_TESTS
## returns matrix with dimensions 'n_region', 'n_region'
unpack_internal <- function(x) {
    n_region <- nrow(x)
    s <- seq_len(n_region)
    x[s, s]
}




        
    
