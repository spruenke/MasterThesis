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


double kappa_cpp(arma::vec psi, int j){
  double res = 1 - 2 * psi[j] + arma::dot(psi, psi);
  return res;
}

double g(arma::vec n){
  return arma::sum(n);
}


double Y_abc(arma::vec x_ab, Rcpp::List data, int c){
  int m_ab = x_ab.n_elem;
  int d = data.length();
  arma::vec res_1 = arma::zeros(m_ab);
  for(int k = 0; k < m_ab; k++){
      //for(int i = 0; i < d; i++){
          Rcpp::List sub_data = data[c];
          int n_i = sub_data.length();
          arma::vec m_ij(n_i);
          arma::vec vnn(n_i);
          for(int j = 0; j < n_i; j++){
              arma::vec v1 = sub_data[j];
              arma::uvec a1 = arma::find(v1 < x_ab(k));
              arma::uvec a2 = arma::find(v1 == x_ab(k));
              m_ij(j) = (a1.n_elem + 0.5 * a2.n_elem); // v1.n_elem;
              vnn(j)  = v1.n_elem;
          }
          res_1(k) = arma::sum(m_ij) / arma::sum(vnn);
      //}
    }
    return arma::mean(res_1);
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
        sigma += subsig * (pow(psi_i[j], 2)) / kappa_cpp(psi_i, j);
      }
    }

  return g(n) * sigma;
}


// [[Rcpp::export]]
arma::mat sigma_est_cpp4(arma::vec n, Rcpp::List data, arma::vec theta, Rcpp::List psi){
  int d = n.n_elem;
  arma::vec ind = arma::regspace(0, (d-1));
  Rcpp::List A(d);
  Rcpp::List A_bar(d);
  Rcpp::List sigma_list(d);
  std::vector<std::vector<arma::mat>>sigma_list_2;
  for(int i = 0; i < d; i++){
    int nii = n(i);
    Rcpp::List sublist(nii);
    Rcpp::List subdat = data(i);
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
            subvec(s) += theta(h) * Y_abc(subdat_j, data, h);
          }
        } else {
          subvec(s) = (-1) * theta(s) * Y_abc(subdat_j, data, s);
        }
      }
      sublist(j) = subvec;
    }
    A(i) = sublist;
  //}
    arma::vec tmp = arma::zeros(d);
    for(int t = 0; t < d; t++){
      for(int j = 0; j < sublist.length(); j++){
        arma::vec A_vec = sublist[j];
        tmp(t) += A_vec(t) * psi_i(j);
      }
    }
    A_bar(i) = tmp;
    Rcpp::List sublist2(nii);
    //std::vector<arma::mat> sublist22;






    for(int j = 0; j < sublist.length(); j++){
      arma::vec A_ij = sublist[j];
      arma::vec y_lhs = A_ij - tmp;
      //Rcpp::Rcout << A_ij;
      sublist2(j) = y_lhs * y_lhs.t(); // sigma_list[[i]][[j]]
      //sublist22.push_back(y_lhs * y_lhs.t());
    }
    sigma_list(i) = sublist2;
    // sigma_list_2.push_back(sublist22);

  }

  //Create sigma matrix
  arma::mat sigma(d, d, arma::fill::zeros);
  for(int i = 0; i < d; i++){
    Rcpp::List siglist = sigma_list[i];
    //std::vector<arma::mat> siglist = sigma_list_2.at(i);
    arma::vec psi_i = psi[i];

    for(int j = 0; j < siglist.length(); j++){
      arma::mat subsig = siglist[j];
      //Rcpp::Rcout << siglist.at(j) << "\n";
      sigma += subsig * (pow(psi_i[j], 2)) * kappa_cpp(psi_i, j);
    }
  }

  return g(n) * sigma;
  //return A_bar;
}




// -- Stats --
// [[Rcpp::export]]
Rcpp::List q_wald_arma(arma::vec n, Rcpp::List data, arma::vec theta, Rcpp::List psi, arma::mat cmat){
  arma::mat res;
  arma::vec p = rel_eff_cpp2(data, theta, psi);
  arma::mat sigma = sigma_est_cpp4(n, data, theta, psi);
  res = p.t() * cmat.t() * arma::pinv(cmat * sigma * cmat.t()) * cmat * p;
  unsigned int df_1 = arma::rank(cmat * sigma);
  return Rcpp::List::create(res, df_1);
}

// [[Rcpp::export]]
Rcpp::List q_anova_arma(arma::vec n, Rcpp::List data, arma::vec theta, Rcpp::List psi, arma::mat cmat){
  arma::mat res;
  arma::vec p = rel_eff_cpp2(data, theta, psi);
  arma::mat sigma = sigma_est_cpp4(n, data, theta, psi);
  arma::mat M = cmat.t() * arma::pinv(cmat * cmat.t()) * cmat;
  double nen = arma::trace(M * sigma);
  res = p.t() * M * p / nen;
  double df_1 = pow(arma::trace(M * sigma), 2) / arma::trace(M * sigma * M * sigma);
  return Rcpp::List::create(res, df_1, nen);
}

// [[Rcpp::export]]
arma::mat m_mat(arma::mat cmat){
  arma::mat M = cmat.t() * arma::pinv(cmat * cmat.t()) * cmat;
  return M;
}

// [[Rcpp::export]]
double nen(arma::mat M, arma::mat sigma){
  return arma::trace(M * sigma);
}

// [[Rcpp::export]]
arma::mat res(arma::vec p, arma::mat M){
  arma::mat res;
  return res = p.t() * M * p;
}

// [[Rcpp::export]]
arma::mat stat_pp(arma::vec p, arma::mat M, arma::mat sigma){
  arma::mat res;
    double nen = arma::trace(M * sigma);
  res = p.t() * M * p / nen;
  return res;
}
