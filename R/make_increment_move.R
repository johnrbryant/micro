
## HAS_TESTS
## returns vector with length 'n_region'
make_increment_move <- function(x, has_internal) {
    if (has_internal) {
        n_region <- nrow(x)
        internal <- x[ , seq_len(n_region)]
        internal_in <- colSums(internal)
        internal_out <- rowSums(internal)
        deaths <- x[ , n_region + 1L]
        emigration <- x[ , n_region + 2L]
        internal_in - internal_out - deaths - emigration
    }
    else {
        deaths <- x[ , 1L]
        emigration <- x[ , 2L]
        -deaths - emigration
    }
}
