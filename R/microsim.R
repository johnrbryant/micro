
## Future extensions
## - do *not* drop age, sex, or region dimensions - though allow sex or region dimension to have length 1
## - do not change triangle dimension at all
## - allow rates/means to be NULL
## - allow immigration to be rates (where exposure = population at start of period)


microsim <- function(initial_popn,
                     fertility_rates,  # zeros outside repr ages
                     mortality_rates,
                     internal_rates,
                     immigration_means,
                     emigration_rates,
                     step_length,
                     dominant_sex = "Female") {
    ## check names of dimensions
    stopifnot(identical(names(dimnames(initial_popn)),
                        c("age", "sex", "region")))
    stopifnot(identical(names(dimnames(fertility_rates)),
                        c("age", "triangle", "sex", "region", "time")))
    stopifnot(identical(names(dimnames(internal_rates)),
                        c("age", "triangle", "sex", "region_orig", "region_dest", "time")))
    stopifnot(identical(names(dimnames(immigration_means)),
                        c("age", "triangle", "sex", "region", "time")))
    stopifnot(identical(names(dimnames(emigration_rates)),
                        c("age", "triangle", "sex", "region", "time")))
    ## check triangles are "Lower", "Upper"
    stopifnot(identical(dimnames(fertility_rates)$triangle,
                        c("Lower", "Upper")))
    ## extract lengths of dimensions
    n_age <- dim(initial_popn)[[1L]]
    n_triangle <- 2L
    n_sex <- dim(initial_popn)[[2L]]
    n_region <- dim(initial_popn)[[3L]]
    n_time <- dim(fertility_rates)[[5L]]
    ## check lengths of dimensions
    stopifnot(identical(dim(fertility_rates),
                        c(n_age, n_triangle, n_sex, n_region, n_time)))
    stopifnot(identical(dim(mortality_rates),
                        c(n_age, n_triangle, n_sex, n_region, n_time)))
    stopifnot(identical(dim(internal_rates),
                        c(n_age, n_triangle, n_sex, n_region, n_region, n_time)))
    stopifnot(identical(dim(immigration_means),
                        c(n_age, n_triangle, n_sex, n_region, n_time)))
    stopifnot(identical(dim(emigration_rates),
                        c(n_age, n_triangle, n_sex, n_region, n_time)))
    ## create arrays for output
    period_first <- dimnames(fertility_rates)$time[[1L]]
    time_first <- as.integer(sub("^([0-9]+)-.*", "\\1", period_first)) - 1L
    times <- seq.int(from = time_first,
                     by = step_length,
                     length.out = n_time + 1L)
    popn <- array(0L,
                  dim = c(n_age, n_sex, n_region, n_time + 1L),
                  dimnames = c(dimnames(initial_popn), list(time = times)))
    births <- array(0L,
                    dim = dim(fertility_rates),
                    dimnames = dimnames(fertility_rates))
    deaths <- array(0L,
                    dim = dim(mortality_rates),
                    dimnames = dimnames(mortality_rates))
    internal <- array(0L,
                      dim = dim(internal_rates),
                      dimnames = dimnames(internal_rates))
    immigration <- array(0L,
                         dim = dim(immigration_means),
                         dimnames = dimnames(immigration_means))
    emigration <- array(0L,
                        dim = dim(emigration_rates),
                        dimnames = dimnames(emigration_rates))
    ## create vectors, arrays for intermediate quantities
    offset_dth_emig <- 2L
    move_rates_indiv <- matrix(nrow = n_region,
                               ncol = n_region + offset_dth_emig)
    accession <- array(0L, dim = c(n_age, n_sex, n_region)) # ages n, 2n, ...., A
    events_total <- matrix(0L,
                           nrow = n_region,
                           ncol = n_region + offset_dth_emig + n_sex)
    popn_start <- integer(length = n_region)
    immigrants <- integer(length = n_region)
    ## make 'i_dominant_sex'
    sexes <- dimnames(initial_popn)$sex
    i_dominant_sex <- match(dominant_sex, sexes)
    ## put 'initial_popn' into 'popn' array
    popn[ , , , 1L] <- initial_popn
    ## simulate period by period
    for (i_time in seq_len(n_time)) {
        ## Process upper triangles before lower,
        ## following cohorts upwards.
        ## Also required so that migration and
        ## mortality of new-born cohort
        ## is simulated last.
        for (i_triangle in 2:1) {
            is_lower <- i_triangle == 1L
            ## go from oldest age group to youngest,
            ## so that migration and mortality of
            ## new-born cohort simulated last
            for (i_age in seq.int(from = n_age, to = 1L)) {
                is_first_age <- i_age == 1L
                is_final_age <- i_age == n_age
                ## Construct a matrix of fertility rates for this combination
                ## of age, triangle, and time. The matrix is transposed so that
                ## it aligns with the 'move_rates_indiv' matrix.
                ## The 'simulate_one_triangle' function only uses the matrix
                ## when 'is_dominant_sex' is TRUE. Note that 'sex' in 
                ## 'fert_rates_indiv' refers to the sex of the
                ## child, not the parent.
                fert_rates_indiv <- t(fertility_rates[i_age, i_triangle, , , i_time])
                ## do dominant sex before other sex(es) so that births generated in first iteration
                s_sex <- order(seq_len(n_sex) != i_dominant_sex) # sequence with 'i_dominant_sex' first
                for (i_sex in s_sex) {
                    is_dominant_sex <- i_sex == i_dominant_sex
                    ## Construct a matrix of internal migration, death and emigration rates
                    ## for this combination of age, triangle, sex, and time.
                    move_rates_indiv[ , (1 : n_region)] <- internal_rates[i_age, i_triangle, i_sex, , , i_time]
                    move_rates_indiv[ , n_region + 1L] <- mortality_rates[i_age, i_triangle, i_sex, , i_time]
                    move_rates_indiv[ , n_region + 2L] <- emigration_rates[i_age, i_triangle, i_sex, , i_time]
                    events_total[] <- 0L
                    for (i_region in seq_len(n_region)) {
                        ## construct initial population for this age-triangle-sex-region-time cell
                        if (is_lower) {
                            if (is_first_age)
                                n_popn_start <- sum(births[ , , i_sex, i_region, i_time]) ## sum over age, triangle of parent
                            else
                                n_popn_start <- accession[i_age - 1L, i_sex, i_region]
                        }
                        else
                            n_popn_start <- popn[i_age, i_sex, i_region, i_time]
                        ## save value to use later in demographic accounting
                        popn_start[[i_region]] <- n_popn_start
                        ## generate immigrants for this age-triangle-sex-region-time cell
                        lambda_immigrants <- immigration_means[i_age, i_triangle, i_sex, i_region, i_time]
                        n_immigrants <- rpois(n = 1L,
                                              lambda = lambda_immigrants)
                        ## save value to use later in demographic accounting
                        immigrants[[i_region]] <- n_immigrants
                        ## simulate triangles for each individual that appears in this
                        ## age-triangle-sex-region-time cell, collecting the results
                        ## in 'events_total'
                        n_indiv <- n_popn_start + n_immigrants
                        for (i_indiv in seq_len(n_indiv)) {
                            is_immigrant <- i_indiv > n_popn_start
                            time_remaining <- make_time_remaining(step_length = step_length,
                                                                  is_immigrant = is_immigrant,
                                                                  is_lower = is_lower,
                                                                  is_final_age = is_final_age)
                            events_indiv <- simulate_one_triangle(move_rates_indiv = move_rates_indiv,
                                                                  fert_rates_indiv = fert_rates_indiv,
                                                                  i_region = i_region,
                                                                  time_remaining = time_remaining,
                                                                  generate_births = is_dominant_sex)
                            events_total <- events_total + events_indiv
                        }
                    } # region
                    ## record the accumulated events
                    if (is_dominant_sex) { ## only have births if this is the dominant sex
                        births[i_age, i_triangle, , , i_time] <- (births[i_age, i_triangle, , , i_time]
                            + unpack_births(events_total))
                    }
                    deaths[i_age, i_triangle, i_sex, , i_time] <- (deaths[i_age, i_triangle, i_sex, , i_time]
                        + unpack_deaths(events_total))
                    internal[i_age, i_triangle, i_sex, , , i_time] <- (internal[i_age, i_triangle, i_sex, , , i_time]
                        + unpack_internal(events_total))
                    immigration[i_age, i_triangle, i_sex, , i_time] <- immigrants
                    emigration[i_age, i_triangle, i_sex, , i_time] <- (emigration[i_age, i_triangle, i_sex, , i_time]
                        + unpack_emigration(events_total))
                    ## do demographic accounting
                    increment_move <- make_increment_move(events_total)
                    if (is_lower) {
                        if (is_final_age) {
                            existing_cohort <- accession[n_age, i_sex, ]
                            new_cohort <- popn_start + immigrants + increment_move
                            popn[n_age, i_sex, , i_time + 1L] <- existing_cohort + new_cohort
                        }
                        else 
                            popn[i_age, i_sex, , i_time + 1L] <- popn_start + immigrants + increment_move
                    }
                    else {
                        accession[i_age, i_sex, ] <- popn_start + immigrants + increment_move
                    }
                } # sex
            } # age
        } # triangle
    } # time
    list(popn = popn,
         births = births,
         deaths = deaths,
         internal = internal,
         immigration = immigration,
         emigration = emigration)
}


       
