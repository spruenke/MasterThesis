#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
//using namespace Rcpp;

// [[Rcpp::export]]
arma::vec f_psi_cpp3(arma::vec x, Rcpp::List data, arma::vec psi){
  unsigned int d = data.length();
  arma::vec m = arma::zeros(d);
  arma::vec res = arma::zeros(x.n_elem);
  for(unsigned int b = 0; b < x.n_elem; b++){
    for(unsigned int j = 0; j < d; j++){
      arma::vec v1 = data[j];
      arma::uvec a1 = arma::find(v1 < x(b));
      arma::uvec a2 = arma::find(v1 == x(b));
      m(j) = (a1.n_elem + 0.5 * a2.n_elem) / v1.n_elem;
    }
    res(b) = arma::sum(psi % m);
  }
  return res;
}

double f_psi_cpp4(double x, Rcpp::List data, arma::vec psi){
  unsigned int d = data.length();
  arma::vec m = arma::zeros(d);
  double res;
    for(unsigned int j = 0; j < d; j++){
      arma::vec v1 = data[j];
      arma::uvec a1 = arma::find(v1 < x);
      arma::uvec a2 = arma::find(v1 == x);
      m(j) = (a1.n_elem + 0.5 * a2.n_elem) / v1.n_elem;
    }
    res = arma::sum(psi % m);
  return res;
}

// [[Rcpp::export]]
arma::vec f_theta_cpp2(arma::vec x, Rcpp::List data, arma::vec theta, Rcpp::List psi){
  arma::vec res = arma::zeros(x.n_elem);
  for(unsigned int j = 0; j < x.n_elem; j++){
    //double y = x(j);
    arma::vec ab = arma::zeros(data.length());
    for(unsigned int i = 0; i < ab.n_elem; i++){
      Rcpp::List v2 = data[i];
      arma::vec psi_i = psi[i];
      ab(i) = f_psi_cpp4(x(j), v2, psi_i);
    }
    res[j] = arma::sum(theta % ab);
  }
  return res;
}

//[[Rcpp::export]]
arma::vec rel_eff_cpp2(Rcpp::List data, arma::vec theta, Rcpp::List psi){
  unsigned int d = data.length();
  arma::vec p = arma::zeros(d);

  for(unsigned int i = 0; i < d; i++){
    Rcpp::List vi = data[i];
    unsigned int vi_n = vi.length();
    arma::vec mm = arma::zeros(vi_n);
    for(unsigned int j = 0; j < vi_n; j++){
      arma::vec vij = vi[j];
      mm(j) = arma::sum(f_theta_cpp2(vij, data, theta, psi)) / vij.n_elem;
    }
    arma::vec psi_i = psi[i];
    p(i) = arma::sum(psi_i % mm);
  }
  return p;
}

//// [[Rcpp::export]]
double kappa(arma::vec psi, int j){
  double res = 1 - 2 * psi[j] + arma::dot(psi, psi);
  return res;
}

// [[Rcpp::export]]
arma::mat sigma_est_cpp3(arma::vec n, Rcpp::List data, arma::vec theta, Rcpp::List psi){
  int d = n.n_elem;
  arma::vec ind = arma::regspace(0, (d-1));
  Rcpp::List A(d);
  Rcpp::List A_bar(d);
  Rcpp::List sigma_list(d);
  for(int i = 0; i < d; i++){
    int nii = n(i);
    Rcpp::List sublist(nii);
    Rcpp::List subdat = data[i];
    arma::vec psi_i = psi(i);
    for(int j = 0; j < nii; j++){
      arma::vec subvec = arma::zeros(d);
      arma::vec subdat_j = subdat[j];
      for(int s = 0; s < d; s++){
        if(s == i){
          arma::uvec ind_new = arma::find(ind != s);
          for(unsigned int hh = 0; hh < ind_new.n_elem; hh++){
            unsigned int h = ind_new(hh);
            Rcpp::List dat_h = data[h];
            subvec(s) += theta(h) * arma::sum(f_psi_cpp3(subdat_j, dat_h, psi_i)) / subdat_j.n_elem;
          }
        } else {
          subvec(s) = (-1) * theta(s) * arma::sum(f_psi_cpp3(subdat_j, data[s], psi_i)) / subdat_j.n_elem;
        }
      }
      sublist(j) = subvec;
    }
    A(i) = sublist;
    arma::vec tmp = arma::zeros(d);
    for(int t = 0; t < d; t++){
       for(int j = 0; j < sublist.length(); j++){
          arma::vec A_vec = sublist[j];
          tmp(t) += A_vec(t) * psi_i(j);
       }
    }
    A_bar(i) = tmp;
    Rcpp::List sublist2(d);
    for(int j = 0; j < sublist2.length(); j++){
      arma::vec A_ij = sublist[j];
      arma::vec y_lhs = A_ij - (tmp) ;
      
      sublist2(j) = y_lhs * y_lhs.t(); // sigma_list[[i]][[j]]
    }
    sigma_list(i) = sublist2;
  }
  
  // Create sigma matrix
  arma::mat sigma(d, d, arma::fill::zeros);
  for(int i = 0; i < d; i++){
    Rcpp::List siglist = sigma_list[i];
    arma::vec psi_i = psi[i];
    for(int j = 0; j < sigma_list.length(); j++){
        arma::mat subsig = siglist[j];
        sigma += subsig * (pow(psi_i[j], 2)) / kappa(psi_i, j);
      }
    }
  
  return sigma;
}

// -- Stats --
// [[Rcpp::export]]
Rcpp::List q_wald_arma(arma::vec n, Rcpp::List data, arma::vec theta, Rcpp::List psi, arma::mat cmat){
  arma::mat res;
  arma::vec p = rel_eff_cpp2(data, theta, psi);
  arma::mat sigma = sigma_est_cpp3(n, data, theta, psi);
  res = p.t() * cmat.t() * arma::pinv(cmat * sigma * cmat.t()) * cmat * p;
  unsigned int df_1 = arma::rank(cmat * sigma);
  return Rcpp::List::create(res, df_1);
}

// [[Rcpp::export]]
Rcpp::List q_anova_arma(arma::vec n, Rcpp::List data, arma::vec theta, Rcpp::List psi, arma::mat cmat){
  arma::mat res;
  arma::vec p = rel_eff_cpp2(data, theta, psi);
  arma::mat sigma = sigma_est_cpp3(n, data, theta, psi);
  arma::mat M = cmat.t() * arma::pinv(cmat * cmat.t()) * cmat;
  double nen = arma::trace(M * sigma);
  res = p.t() * M * p / nen;
  double df_1 = pow(arma::sum(arma::diagvec(M * sigma)), 2) / arma::sum(arma::diagvec(M * sigma * M * sigma));
  return Rcpp::List::create(res, df_1,nen);
}