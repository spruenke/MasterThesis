#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
//using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//


// [[Rcpp::export]]
Rcpp::NumericVector f_psi_cpp(Rcpp::NumericVector x, Rcpp::List data, Rcpp::NumericVector psi){
    Rcpp::NumericVector m(data.length());
    Rcpp::NumericVector res(x.length());
    for(int b = 0; b < x.length(); b++){
        for(int j = 0; j < data.length(); j++){
            Rcpp::NumericVector v1 = data[j];
            Rcpp::LogicalVector a1 = v1 < x[b];
            Rcpp::LogicalVector a2 = v1 == x[b];
            m[j] = (sum(a1) + 0.5 * sum(a2)) / v1.length();
          }
        res[b] = sum(psi * m);
      }
    return res;
}



double f_psi_cpp2(double x, Rcpp::List data, Rcpp::NumericVector psi){
  Rcpp::NumericVector m(data.length());
  for(int j = 0; j < m.length(); j++){
    Rcpp::NumericVector v1 = data[j];
    Rcpp::LogicalVector a1 = v1 < x;
    Rcpp::LogicalVector a2 = v1 == x;
    m[j] = (sum(a1) + 0.5 * sum(a2)) / v1.length();
  }
  return sum(m * psi);
}


// [[Rcpp::export]]

Rcpp::NumericVector f_theta_cpp(Rcpp::NumericVector x, Rcpp::List data, Rcpp::NumericVector theta, Rcpp::List psi){
  Rcpp::NumericVector res(x.length());
  for(int j = 0; j < x.length(); j++){
    double y = x[j];
    Rcpp::NumericVector ab(data.length());
    for(int i = 0; i < ab.length(); i++){
      Rcpp::List v2 = data[i];
      Rcpp::NumericVector psi1 = psi[i];
      ab[i] = f_psi_cpp2(y, v2, psi1);
    }
    res[j] = sum(theta * ab);
  }
  return res;
}

// [[Rcpp::export]]
Rcpp::NumericVector rel_eff_cpp(Rcpp::List data, Rcpp::NumericVector theta, Rcpp::List psi){
  Rcpp::NumericVector p(data.length());

  for(int i = 0; i < data.length(); i++){
      Rcpp::List vi = data[i];
      Rcpp::NumericVector mm(vi.length());
      for(int j = 0; j < vi.length(); j++){
            Rcpp::NumericVector vij = vi[j];
            mm[j] = Rcpp::mean(f_theta_cpp(vij, data, theta, psi));
        }
      Rcpp::NumericVector psi_i = psi[i];
      p[i] = Rcpp::sum(psi_i * mm);
    }
  return p;
}

// [[Rcpp::export]]
double kappa(arma::vec psi, int j){
    double res = 1 - 2 * psi[j] + arma::dot(psi, psi);
    return res;
}

// [[Rcpp::export]]
arma::mat sigma_est_cpp(Rcpp::List n, Rcpp::List data, Rcpp::NumericVector theta, Rcpp::List psi){
  int d = data.length();
  Rcpp::IntegerVector ind = Rcpp::seq(0, (d-1));
  Rcpp::List A(d);
  for(int i = 0; i < d; i++){
        int nii = n[i];
        Rcpp::List sublist(nii);
        Rcpp::List subdat = data[i];
        for(int j = 0; j < nii; j++){
            Rcpp::NumericVector subvec(d);
            Rcpp::NumericVector subdat_j = subdat[j];
            for(int s = 0; s < d; s++){
                if(s == i){
                    Rcpp::IntegerVector s_new = s;
                    Rcpp::IntegerVector cc = Rcpp::match(s_new, ind) - 1;
                    int cc_new = cc[0];
                    Rcpp::IntegerVector ind_new = ind[-cc_new];
                    for(int hh = 0; hh < ind_new.length(); hh++){
                        int h = ind_new[hh];
                        subvec[s] += theta[h] * mean(f_psi_cpp(subdat_j, data[h], psi[i]));
                    }
                } else {
                    subvec[s] = (-1) * theta[i] * mean(f_psi_cpp(subdat_j, data[s], psi[i]));
                }
            }
            sublist(j) = subvec;
        }
        A(i) = sublist;
  }

  Rcpp::List A_bar(d);
  for(int i = 0; i < d; i++){
      Rcpp::NumericVector A_vec = A[i];
      Rcpp::NumericVector tmp(d);
      Rcpp::NumericVector psi_i = psi[i];
      for(int s = 0; s < d; s++){
          for(int j = 0; j < A_vec.length(); j++){
              Rcpp::NumericVector A_subvec = A_vec[j];
              tmp[s] += A_vec[s] * psi_i[j];
          }
      }
  }


  Rcpp::List sigma_list(d);
  for(int i = 0; i < d; i++){
        Rcpp::List A_list = A[i];
        Rcpp::List sublist(A_list.length());
        for(int j = 0; j < A_list.length(); j++){
            arma::vec y_lhs =  Rcpp::as<arma::vec>(A_list[j]) - Rcpp::as<arma::vec>(A_bar[i]) ;

            sublist(j) = y_lhs * y_lhs.t();
        }
  }

  arma::mat sigma(d, d, arma::fill::zeros);
  for(int i = 0; i < d; i++){
      Rcpp::List sigma_sublist = sigma_list[i];
      arma::vec psi_i = Rcpp::as<arma::vec>(psi[i]);
      for(int j = 0; j < A.length(); j++){
            sigma += (Rcpp::as<arma::mat>(sigma_sublist[j]) * (pow(psi_i[j], 2)) / kappa(psi_i, j));
      }
  }
  return sigma;
}

// [[Rcpp::export]]
Rcpp::List sigma_est_cpp2(Rcpp::NumericVector n, Rcpp::List data, Rcpp::NumericVector theta, Rcpp::List psi){
  int d = n.length();
  arma::vec ind = arma::regspace(0, (d-1));
  Rcpp::List A(d);
  for(int i = 0; i < d; i++){
    int nii = n[i];
    Rcpp::List sublist(nii);
    Rcpp::List subdat = data[i];
    Rcpp::NumericVector psi_i = psi[i];
    for(int j = 0; j < nii; j++){
      Rcpp::NumericVector subvec(d);
      Rcpp::NumericVector subdat_j = subdat[j];
      for(int s = 0; s < d; s++){
          if(s == i){
            arma::uvec ind_new = arma::find(ind != s);
           for(unsigned int hh = 0; hh < ind_new.n_elem; hh++){
             unsigned int h = ind_new(hh);
             Rcpp::List dat_h = data[h];
             subvec[s] += theta[h] * mean(f_psi_cpp(subdat_j, dat_h, psi_i));
           }
         } else {
           subvec[s] = (-1) * theta[i] * mean(f_psi_cpp(subdat_j, data[s], psi_i));
         }
       }
      sublist(j) = subvec;
    }
    A(i) = sublist;
  }
  return A;
}


