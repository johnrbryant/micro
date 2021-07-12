#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double make_time_remaining_bd(int step_length,
                              bool is_lower,
                              bool is_final_age) {
  if (!is_lower && is_final_age)
    return 1.0 * step_length;
  return R::runif(0, step_length);
}


// // [[Rcpp::export]]
// IntegerVector sim_one_tri_bd(NumericVector fert_rate_tri, 
//                              double mortality_rate_tri,
//                              double time_remaining,
//                              bool generate_births) {
//   int n_sex = fert_rate_tri.size();
//   IntegerVector events(n_sex + 1);
//   double scale = 1 / mortality_rate_tri; // rexp uses scale, not rate
//   double time_die = R::rexp(scale);
//   bool died = (time_die < time_remaining);
//   if (died)
//     events[0] = 1;
//   if (generate_births) {
//     double exposure = died ? time_die : time_remaining;
//     for (int i = 0; i < n_sex; i++) {
//       double expected_births = fert_rate_tri[i] * exposure;
//       events[i + 1] = R::rpois(expected_births);
//     }
//   }
//   return events;
// }


// [[Rcpp::export]]
List micro_bd(IntegerVector initial_popn, // n_age x n_sex x n_iter
              NumericVector fertility_rates, // n_age x n_tri x n_sex x n_period
              NumericVector mortality_rates, // n_age x n_tri x n_sex x n_period
              int n_age,
              int n_sex,
              int n_period,
              int n_iter,
              int step_length,
              int i_dom_sex) { // 0-bsed
  int n_tri = 2;
  IntegerVector population(n_age * n_sex * (n_period + 1) * n_iter);
  IntegerVector births(n_age * n_tri * n_sex * n_period * n_iter);
  IntegerVector deaths(n_age * n_tri * n_sex * n_period * n_iter);
  NumericVector fert_rate_tri(n_sex);
  IntegerVector events_total(n_sex + 1);
  IntegerVector accession(n_age * n_sex);
  for (int i_iter = 0; i_iter < n_iter; i_iter++) {
    // initial population for this iteration
    for (int i_agesex = 0; i_agesex < n_age * n_sex; i_agesex++) {
      int i_popn = i_agesex +  i_iter * n_age * n_sex * (n_period + 1);
      int i_init = i_agesex + i_iter * n_age * n_sex;
      population[i_popn] = initial_popn[i_init];
    }
    for (int i_period = 0; i_period < n_period; i_period++) {
      for (int i_sex = 0; i_sex < n_sex; i_sex++) {
        bool generate_births = i_sex == i_dom_sex;
        for (int i_tri = n_tri - 1; i_tri >= 0; i_tri--) {
          bool is_lower = i_tri == 0;
          for (int i_age = n_age - 1; i_age >= 0; i_age--) {
            bool is_first_age = i_age == 0;
            bool is_final_age = i_age == n_age - 1;
            // make 'mortality_rate_tri' - scalar
            int i_mort = (i_age + i_tri * n_age + i_sex * n_age * n_tri +
                          i_period * n_age * n_tri * n_sex);
            double mort_rate_tri = mortality_rates[i_mort];
            // make 'fert_rate_tri' - vector of length 'n_sex'
            for (int i_sex_fert = 0; i_sex_fert < n_sex; i_sex_fert++) {
              int i_fert = (i_age + i_tri * n_age + i_sex_fert * n_age * n_tri + 
                            i_period * n_age * n_tri * n_sex);
              fert_rate_tri[i_sex_fert] = fertility_rates[i_fert];
            }
            // calcuate 'n_popn_start'
            int n_popn_start = 0;
            if (is_lower) {
              if (is_first_age) { // starting population is births
                for (int i_age_popn = 0; i_age_popn < n_age; i_age_popn++) {
                  for (int i_tri_popn = 0; i_tri_popn < n_tri; i_tri_popn++) {
                    int i_births_popn = (i_age_popn + i_tri_popn * n_age +
                                         i_sex * n_age * n_tri + 
                                         i_period * n_age * n_tri * n_sex +
                                         i_iter * n_age * n_tri * n_sex * n_period);
                    n_popn_start += births[i_births_popn];
                  }
                }
              }
              else { // starting population is accession
                int i_acc_start = i_age - 1 + i_sex * n_age;
                n_popn_start = accession[i_acc_start];
              }
            }
            else { // starting population is popn
              int i_popn = (i_age + i_sex * n_age + i_period * n_age * n_sex 
                              + i_iter * n_age * n_sex * (n_period + 1));
              n_popn_start = population[i_popn];
            }
            // clear 'events_total' vector
            for (int i = 0; i < n_sex + 1; i++)
              events_total[i] = 0;
            // generate events
            for (int i_indiv = 0; i_indiv < n_popn_start; i_indiv++) {
              double time_remaining = make_time_remaining_bd(step_length,
                                                             is_lower,
                                                             is_final_age);
              double time_die = R::rexp(1 / mort_rate_tri); // rexp uses scale, not rate
              bool died = (time_die < time_remaining);
              if (died)
                events_total[0]++;
              if (generate_births) {
                double exposure = died ? time_die : time_remaining;
                for (int i = 0; i < n_sex; i++) {
                  double expected_births = fert_rate_tri[i] * exposure;
                  events_total[i + 1] += R::rpois(expected_births);
                }
              }
            }
            // record deaths
            int i_deaths = (i_age + i_tri * n_age + i_sex * n_age * n_tri 
                              + i_period * n_age * n_tri * n_sex
                              + i_iter * n_age * n_tri * n_sex * n_period);
            int deaths_total = events_total[0];
            deaths[i_deaths] = deaths_total;
            // record births
            if (generate_births) {
              for (int i_sex_births = 0; i_sex_births < n_sex; i_sex_births++) {
                int i_births = (i_age + i_tri * n_age + i_sex_births * n_age * n_tri 
                                  + i_period * n_age * n_tri * n_sex
                                  + i_iter * n_age * n_tri * n_sex * n_period);
                births[i_births] = events_total[i_sex_births + 1];
              }
            }
            // do demographic accounting
            int i_popn_end = (i_age + i_sex * n_age + (i_period + 1) * n_age * n_sex +
                              i_iter * n_age * n_sex * (n_period + 1));
            int i_acc_end = i_age + i_sex * n_age;
            int n_popn_end = n_popn_start - deaths_total;
            if (is_lower) {
              if (is_final_age) {
                int existing_cohort = accession[i_acc_end];
                population[i_popn_end] = existing_cohort + n_popn_end;
              }
              else
                population[i_popn_end] = n_popn_end;
            }
            else
              accession[i_acc_end] = n_popn_end;
          } // age
        } // triangle
      } // sex
    } //time
  } //iteration
  IntegerVector dim_popn {n_age, n_sex, n_period + 1, n_iter};
  IntegerVector dim_bd {n_age, n_tri, n_sex, n_period, n_iter};
  population.attr("dim") = dim_popn;
  births.attr("dim") = dim_bd;
  deaths.attr("dim") = dim_bd;
  return List::create(Named("population") = population,
                      Named("births") = births,
                      Named("deaths") = deaths);
}



/*** R
make_time_remaining_bd(5, TRUE, TRUE)
make_time_remaining_bd(5, FALSE, TRUE)
make_time_remaining_bd(5, TRUE, FALSE)
make_time_remaining_bd(5, FALSE, FALSE)
*/