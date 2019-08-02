// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector sortxy(IntegerVector x, IntegerVector y) {
  IntegerVector idx = seq_along(x) - 1;
  std::sort(idx.begin(), idx.end(), [&](int i, int j){return y[i] < y[j];});
  for(int i=0; i<2;i++){
    if((y[idx[i]]==y[idx[i+1]]) & (x[idx[i]]>x[idx[i+1]])){
      int tmp= idx[i+1];
      idx[i+1]=idx[i];
      idx[i]=tmp;
    }
  }
  for(int i=0; i<2;i++){
    if((y[idx[i]]==y[idx[i+1]]) & (x[idx[i]]>x[idx[i+1]])){
      int tmp= idx[i+1];
      idx[i+1]=idx[i];
      idx[i]=tmp;
    }
  }
  return x[idx];
}

// [[Rcpp::export]]
IntegerVector triadCensusCol(const arma::sp_mat& A, 
                   IntegerVector attr,
                   IntegerMatrix orbitClasses,
                   IntegerVector triads){
  int code=0;
  int n=attr.size();
  IntegerVector trorbits(3);
  IntegerVector orbits(3);
  IntegerVector trattr(3);
  IntegerVector attrOrder(3);
  IntegerVector idx(3);
  IntegerVector vattr(3);
  std::string orbStr;
  std::string attrStr;
  IntegerVector tritypes = {1,2,2,3,2,4,6,8,2,6,5,7,3,8,7,11,
                2,6,4,8,5,9,9,13,6,10,9,14,7,14,12,15,
                2,5,6,7,6,9,10,14,4,9,9,12,8,13,14,15,
                3,7,8,11,7,12,14,15,8,14,13,15,11,15,15,16};
  
  for(int u=0;u<n;++u){
    for(int v=0;v<n;++v){
      for(int w=0;w<n;++w){
        if((u<v) & (v<w)){
          code = A(u,v)+2*A(u,w)+4*A(v,u)+8*A(v,w)+16*A(w,u)+32*A(w,v);
          orbits = orbitClasses(code,_);
          // Rcout <<"trorbits:"<< orbits[0]<<orbits[1]<<orbits[2] <<std::endl;
          idx = {u,v,w};
          // Rcout<< "idx:" << idx[0]<<idx[1]<<idx[2] <<std::endl;
          vattr = attr[idx];
          // Rcout <<"vattr:"<< vattr[0]<<vattr[1]<<vattr[2] <<std::endl;
          // attrOrder = order_(vattr);
          // trattr = vattr[attrOrder-1];
          // trorbits = sortxy(orbits,vattr);
          // trattr = vattr.sort();
          trattr = sortxy(vattr,orbits);
          trorbits = orbits.sort();
          // Rcout <<"trattr:"<< trattr[0]<<trattr[1]<<trattr[2] <<std::endl;
          
          // trorbits = trorbits[attrOrder-1];
          
          // Rcout <<"trorbits:"<< trorbits[0]<<trorbits[1]<<trorbits[2] <<std::endl;
          orbStr = std::to_string(trorbits[0]) + std::to_string(trorbits[1])+std::to_string(trorbits[2]);
          attrStr= std::to_string(trattr[0]) + std::to_string(trattr[1])+std::to_string(trattr[2]);
          // Rcout << orbStr+"-"+attrStr <<std::endl;
          int b = triads[orbStr+"-"+attrStr];
          b+=1;
          triads[orbStr+"-"+attrStr]=b;
        }
      }
    }
  }
  return triads;
}

