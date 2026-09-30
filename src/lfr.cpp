


/*
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *                                                                               *
 *	This program is free software; you can redistribute it and/or modify         *
 *  it under the terms of the GNU General Public License as published by         *
 *  the Free Software Foundation; either version 2 of the License, or            *
 *  (at your option) any later version.                                          *
 *                                                                               *
 *  This program is distributed in the hope that it will be useful,              *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of               *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                *
 *  GNU General Public License for more details.                                 *
 *                                                                               *
 *  You should have received a copy of the GNU General Public License            *
 *  along with this program; if not, write to the Free Software                  *
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA    *
 *                                                                               *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *                                                                               *
 *  Created by Andrea Lancichinetti on 7/01/09 (email: arg.lanci@gmail.com)      *
 *	Modified on 28/05/09                                                         *
 *	Collaborators: Santo Fortunato												 *
 *  Location: ISI foundation, Turin, Italy                                       *
 *	Project: Benchmarking community detection programs                           *
 *                                                                               *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
 */

#include <Rcpp.h>
using namespace Rcpp;

#include <cmath>
#include <deque>
#include <set>
#include <map>
#include <algorithm>

using namespace std;

// uniform random number in (0, 1) drawn from R's RNG (respects set.seed).
// The RNG state is handled by the RNGScope that Rcpp attributes add to the
// exported benchmark() wrapper.
double ran4() {
	return R::unif_rand();
}

// uniform random integer in {0, ..., n}
int irand(int n) {
	if (n < 0)
		Rcpp::stop("LFR benchmark: internal error, tried to sample from an empty set");
	int r = int(ran4() * (n + 1));
	return r > n ? n : r;
}

// uniformly random element of v
template <typename T>
typename T::value_type random_element(const T & v) {
	if (v.empty())
		Rcpp::stop("LFR benchmark: internal error, tried to sample from an empty set");
	return v[irand(v.size() - 1)];
}

// uniformly random element x of v for which reject(x) is false. Rejection
// sampling is tried first; if that fails, the admissible elements are
// collected and one of them is drawn, so this always terminates.
template <typename T, typename F>
typename T::value_type random_element_except(const T & v, F reject) {
	const size_t max_tries = 10 * v.size() + 100;
	for (size_t tries = 0; tries < max_tries; tries++) {
		typename T::value_type x = random_element(v);
		if (!reject(x))
			return x;
	}
	T admissible;
	for (size_t i = 0; i < v.size(); i++)
		if (!reject(v[i]))
			admissible.push_back(v[i]);
	if (admissible.empty())
		Rcpp::stop("LFR benchmark: could not find a node to rewire a link to. Please change the parameters (e.g. increase the community sizes or the number of nodes).");
	return random_element(admissible);
}

int powerlaw (int n, int min, double tau, deque<double> &cumulative) {
	
	cumulative.clear();
	double a=0;			

	for (double h=min; h<n+1; h++)
		a+= pow((1./h),tau);
	
	
	double pf=0;
	for(double i=min; i<n+1; i++) {
	
		pf+=1/a*pow((1./(i)),tau);
		cumulative.push_back(pf);
	
	}
	
	return 0;	
	
}
// draws a value from the distribution given by cumulative, whose first entry
// corresponds to the value min
int sample_from_cumulative(const deque<double> & cumulative, int min) {
	if (cumulative.empty())
		Rcpp::stop("LFR benchmark: the range of the power law distribution is empty. Please check the degree and community size parameters.");
	long int pos = lower_bound(cumulative.begin(), cumulative.end(), ran4()) - cumulative.begin();
	// guard against rounding: the last cumulative value may be slightly below 1
	if (pos >= (long int) cumulative.size())
		pos = cumulative.size() - 1;
	return pos + min;
}

bool they_are_mate(int a, int b, const deque<deque<int> > & member_list) {


	for(long unsigned int i=0; i<member_list[a].size(); i++) {
		
		if(binary_search(member_list[b].begin(), member_list[b].end(), member_list[a][i]))
			return true;
	
	}

	return false;

}


// it computes the sum of a deque<int>

int deque_int_sum(const deque<int> & a) {
	
	int s=0;
	for(long unsigned int i=0; i<a.size(); i++)
		s+=a[i];

	return s;
}

// it computes the integral of a power law
double integral (double a, double b) {

	
	if (fabs(a+1.)>1e-10)
		return (1./(a+1.)*pow(b, a+1.));
	
	
	else
		return (log(b));

}


// it returns the average degree of a power law
double average_degree(const double &dmax, const double &dmin, const double &gamma) {

	return (1./(::integral(gamma, dmax)-::integral(gamma, dmin)))*(::integral(gamma+1, dmax)-::integral(gamma+1, dmin));

}


//bisection method to find the inferior limit, in order to have the expected average degree
double solve_dmin(const double& dmax, const double &dmed, const double &gamma) {
	
	double dmin_l=1;
	double dmin_r=dmax;
	double average_k1=average_degree(dmin_r, dmin_l, gamma);
	double average_k2=dmin_r;
	
	
	if ((average_k1-dmed>0) || (average_k2-dmed<0)) {
		
		if (average_k1-dmed>0)
			Rcpp::stop("the average degree is out of range: increase `average_degree` (to more than %g) or decrease `max_degree`.", average_k1);

		Rcpp::stop("the average degree is out of range: decrease `average_degree` (to at most %g) or increase `max_degree`.", average_k2);
	}
	
		
	while (fabs(average_k1-dmed)>1e-7) {
		
		double temp=average_degree(dmax, ((dmin_r+dmin_l)/2.), gamma);
		if ((temp-dmed)*(average_k2-dmed)>0) {
			
			average_k2=temp;
			dmin_r=((dmin_r+dmin_l)/2.);
		
		}
		else {
			
			average_k1=temp;
			dmin_l=((dmin_r+dmin_l)/2.);
			
		
		}

	}
	
	return dmin_l;
}

// it computes the correct (i.e. discrete) average of a power law
double integer_average (int n, int min, double tau) {
	
	double a=0;

	for (double h=min; h<n+1; h++)
		a+= pow((1./h),tau);
	
	
	double pf=0;
	for(double i=min; i<n+1; i++)
		pf+=1/a*pow((1./(i)),tau)*i;
	
	return pf;

}

// this function changes the community sizes merging the smallest communities
int change_community_size(deque<int> &seq) {

	
			
	if (seq.size()<=2)
		return -1;
	
	int min1=0;
	int min2=0;
	
	for (long unsigned int i=0; i<seq.size(); i++)		
		if (seq[i]<=seq[min1])
			min1=i;
	
	if (min1==0)
		min2=1;
	
	for (long unsigned int i=0; i<seq.size(); i++)		
		if (seq[i]<=seq[min2] && seq[i]>seq[min1])
			min2=i;

	seq[min1]+=seq[min2];
	
	int c=seq[0];
	seq[0]=seq[min2];
	seq[min2]=c;
	seq.pop_front();
	
	
	return 0;
}

int build_bipartite_network(deque<deque<int> >  & member_matrix, const deque<int> & member_numbers, const deque<int> &num_seq) {

	
	
	// this function builds a bipartite network with num_seq and member_numbers which are the degree sequences. in member matrix links of the communities are stored
	// this means member_matrix has num_seq.size() rows and each row has num_seq[i] elements
	
	
	
	deque<set<int> > en_in;			// this is the Ein of the subgraph
	deque<set<int> > en_out;		// this is the Eout of the subgraph
	
	
	{
		set<int> first;
		for(long unsigned int i=0; i<member_numbers.size(); i++) {
			en_in.push_back(first);
		}
	}
	
	{
		set<int> first;
		for(long unsigned int i=0; i<num_seq.size(); i++) {
			en_out.push_back(first);
		}
	}

	
	
	multimap <int, int> degree_node_out;
	deque<pair<int, int> > degree_node_in;
	
	for(long unsigned int i=0; i<num_seq.size(); i++)
		degree_node_out.insert(make_pair(num_seq[i], i));
	
	for(long unsigned int i=0; i<member_numbers.size(); i++)
		degree_node_in.push_back(make_pair(member_numbers[i], i));
	
	
	sort(degree_node_in.begin(), degree_node_in.end());
	
	
	
	deque<pair<int, int> >::iterator itlast = degree_node_in.end();
	
	
	
	
	while (itlast != degree_node_in.begin()) {
		
		itlast--;
		
		
		multimap <int, int>::iterator itit= degree_node_out.end();
		deque <multimap<int, int>::iterator> erasenda;
		
		for (int i=0; i<itlast->first; i++) {
			
			if(itit!=degree_node_out.begin()) {
				
				itit--;
				
					
				en_in[itlast->second].insert(itit->second);
				en_out[itit->second].insert(itlast->second);
					
				erasenda.push_back(itit);
				
			}
			
			else
				return -1;
		
		}
		
		
		
		for (long unsigned int i=0; i<erasenda.size(); i++) {
			
			
			if(erasenda[i]->first>1)
				degree_node_out.insert(make_pair(erasenda[i]->first - 1, erasenda[i]->second));
	
			
			degree_node_out.erase(erasenda[i]);

			
		
		}
		
		
	}
	
	
	// this is to randomize the subgraph -------------------------------------------------------------------
    
    
    deque<int> degree_list;
    for(long unsigned int kk=0; kk<member_numbers.size(); kk++){
        for(int k2=0; k2<member_numbers[kk]; k2++){
            degree_list.push_back(kk); 
		}
	}

	
	for(long unsigned int run=0; run<10; run++) for(long unsigned int node_a=0; node_a<num_seq.size(); node_a++) for(long unsigned int krm=0; krm<en_out[node_a].size(); krm++) {
				
        int random_mate=random_element(degree_list);
		
		if (en_out[node_a].find(random_mate)==en_out[node_a].end()) {
			
			deque <int> external_nodes;
			for (set<int>::iterator it_est=en_out[node_a].begin(); it_est!=en_out[node_a].end(); it_est++)
				external_nodes.push_back(*it_est);
						
										
			int	old_node=random_element(external_nodes);
					
			
			deque <int> not_common;
			for (set<int>::iterator it_est=en_in[random_mate].begin(); it_est!=en_in[random_mate].end(); it_est++)
				if (en_in[old_node].find(*it_est)==en_in[old_node].end())
					not_common.push_back(*it_est);
					
			
			if (not_common.empty())
				break;
				
			int node_h=random_element(not_common);
			
			
			en_out[node_a].insert(random_mate);
			en_out[node_a].erase(old_node);
			
			en_in[old_node].insert(node_h);
			en_in[old_node].erase(node_a);
			
			en_in[random_mate].insert(node_a);
			en_in[random_mate].erase(node_h);
			
			en_out[node_h].erase(random_mate);
			en_out[node_h].insert(old_node);
			

		}	
	}

	
	
	member_matrix.clear();
	deque <int> first;
	
	for (long unsigned int i=0; i<en_out.size(); i++) { 
		
		member_matrix.push_back(first);
		for (set<int>::iterator its=en_out[i].begin(); its!=en_out[i].end(); its++)
			member_matrix[i].push_back(*its);
	
	}
	
	
	return 0;


}

int internal_degree_and_membership (double mixing_parameter, int overlapping_nodes, int max_mem_num, int num_nodes, deque<deque<int> >  & member_matrix, 
bool excess, bool defect,  deque<int> & degree_seq, deque<int> &num_seq, deque<int> &internal_degree_seq, bool fixed_range, int nmin, int nmax, double tau2, bool verbose) {
	
	
	
	
	
	if(num_nodes< overlapping_nodes) {
		
		Rcpp::stop("there are more overlapping nodes than nodes in the network. Please decrease `on` or increase `n`.");
	}
	
	
	// 
	member_matrix.clear();
	internal_degree_seq.clear();
	
	deque<double> cumulative;
	
	// it assigns the internal degree to each node -------------------------------------------------------------------------
	int max_degree_actual=0;		// maximum internal degree

	for (long unsigned int i=0; i<degree_seq.size(); i++) {
		
		double interno=(1-mixing_parameter)*degree_seq[i];
		int int_interno=int(interno);
		
		
		if (ran4()<(interno-int_interno))
			int_interno++;
		
		if (excess) {
			
			while (   (  double(int_interno)/degree_seq[i] < (1-mixing_parameter) )  &&   (int_interno<degree_seq[i])   )
				int_interno++;
				
		
		}
		
		
		if (defect) {
			
			while (   (  double(int_interno)/degree_seq[i] > (1-mixing_parameter) )  &&   (int_interno>0)   )
				int_interno--;
				
		
		}

		
		
		
		internal_degree_seq.push_back(int_interno);
		
		
		if (int_interno>max_degree_actual)
			max_degree_actual=int_interno;
		
			
	}
	
	
	// it assigns the community size sequence -----------------------------------------------------------------------------
	
	powerlaw(nmax, nmin, tau2, cumulative);
	
	
	if (num_seq.empty()) {
		
		int _num_=0;
		if (!fixed_range && (max_degree_actual+1)>nmin) {
		
			_num_=max_degree_actual+1;			// this helps the assignment of the memberships (it assures that at least one module is big enough to host each node)
			num_seq.push_back(max_degree_actual+1);
		
		}
		
		
		while (true) {
			
			
			int nn=sample_from_cumulative(cumulative, nmin);
			
			if (nn+_num_<=num_nodes + overlapping_nodes * (max_mem_num-1) ) {
				
				num_seq.push_back(nn);				
				_num_+=nn;
			
			}
			else
				break;
			
			
		}
		
		num_seq[min_element(num_seq.begin(), num_seq.end()) - num_seq.begin()]+=num_nodes + overlapping_nodes * (max_mem_num-1) - _num_;
		
	}
	
	//int ncom=num_seq.size();
	

	// I have to assign the degree to the nodes
	
	
	deque<int> member_numbers;
	for(int i=0; i<overlapping_nodes; i++)
		member_numbers.push_back(max_mem_num);
	for(long unsigned int i=overlapping_nodes; i<degree_seq.size(); i++)
		member_numbers.push_back(1);
	
	
	if(build_bipartite_network(member_matrix, member_numbers, num_seq)==-1) {
		
		Rcpp::stop("the overlapping nodes need more communities than there are. Please increase the number of communities (e.g. decrease `max_community`) or decrease `on` or `om`.");
	
	}
		
	
	

	deque<int> available;
	for (int i=0; i<num_nodes; i++)
		available.push_back(0);
	
	for (long unsigned int i=0; i<member_matrix.size(); i++) {
		for (long unsigned int j=0; j<member_matrix[i].size(); j++)
			available[member_matrix[i][j]]+=member_matrix[i].size()-1;
	}
	
	
	
	deque<int> available_nodes;
	for (int i=0; i<num_nodes; i++)
		available_nodes.push_back(i);
	
	
	deque<int> map_nodes;				// in the position i there is the new name of the node i
	for (int i=0; i<num_nodes; i++)
		map_nodes.push_back(0);

	
	for (int i=degree_seq.size()-1; i>=0; i--) {
		
		//int & degree_here=internal_degree_seq[i];
		int try_this = irand(available_nodes.size()-1);
		
		int kr=0;
		while (internal_degree_seq[i] > available[available_nodes[try_this]]) {
		
			kr++;
			if (kr % 1000 == 0)
				Rcpp::checkUserInterrupt();
			try_this = irand(available_nodes.size()-1);
			if(kr==3*num_nodes) {
			
				if(change_community_size(num_seq)==-1) {
					
					Rcpp::stop("could not assign community memberships: the algorithm needs more than two communities. Please change the parameters (e.g. decrease `mu`, `min_community` or `max_community`).");
				
				}
				
				if (verbose) {
					Rcout<<"it took too long to decide the memberships; I will try to change the community sizes"<<endl;
					Rcout<<"new community sizes"<<endl;
					for (long unsigned int i=0; i<num_seq.size(); i++)
						Rcout<<num_seq[i]<<" ";
					Rcout<<endl<<endl;
				}
				
				return (internal_degree_and_membership(mixing_parameter, overlapping_nodes, max_mem_num, num_nodes, member_matrix, excess, defect, degree_seq, num_seq, internal_degree_seq, fixed_range, nmin, nmax, tau2, verbose));
			
			
			}
			
			
		}
		
		
		
		map_nodes[available_nodes[try_this]]=i;
		
		available_nodes[try_this]=available_nodes[available_nodes.size()-1];
		available_nodes.pop_back();
		
	
	
	}
	
	
	for (long unsigned int i=0; i<member_matrix.size(); i++) {
		for (long unsigned int j=0; j<member_matrix[i].size(); j++)
			member_matrix[i][j]=map_nodes[member_matrix[i][j]];	
	}
	
	
	
	for (long unsigned int i=0; i<member_matrix.size(); i++)
		sort(member_matrix[i].begin(), member_matrix[i].end());

		
	return 0;

}

int compute_internal_degree_per_node(int d, int m, deque<int> & a) {
	
	
	// d is the internal degree
	// m is the number of memebership 
	
	a.clear();
	int d_i= d/m;
	for (int i=0; i<m; i++)
		a.push_back(d_i);
		
	for(int i=0; i<d%m; i++)
		a[i]++;
	
	
	

	return 0;

}

// links_removed counts the multiple links that could not be rewired and were dropped
int build_subgraph(deque<set<int> > & E, const deque<int> & nodes, const deque<int> & degrees, int & links_removed) {
	
	

	if(degrees.size()<3) {
		
		Rcpp::stop("some communities would have fewer than 3 nodes. Please change the parameters (e.g. increase `min_community`).");
	
	}
	
	// this function is to build a network with the labels stored in nodes and the degree seq in degrees (correspondence is based on the vectorial index)
	// the only complication is that you don't want the nodes to have neighbors they already have
	
	
	
	// labels will be placed in the end
	deque<set<int> > en; // this is the E of the subgraph

	{
		set<int> first;
		for(long unsigned int i=0; i<nodes.size(); i++) 
			en.push_back(first);
	}
	
	
	
	multimap <int, int> degree_node;
	
	for(long unsigned int i=0; i<degrees.size(); i++)
		degree_node.insert(degree_node.end(), make_pair(degrees[i], i));
	

	while (degree_node.size() > 0) {
		
		multimap<int, int>::iterator itlast= degree_node.end();
		itlast--;
		
		multimap <int, int>::iterator itit= itlast;
		deque <multimap<int, int>::iterator> erasenda;
		
		
		for (int i=0; i<itlast->first; i++) {
			
			if(itit!=degree_node.begin()) {
			
				itit--;
				
				
				en[itlast->second].insert(itit->second);
				en[itit->second].insert(itlast->second);
				
				erasenda.push_back(itit);				
				
			}
			
			else
				break;
		
		}
		
		
		for (long unsigned int i=0; i<erasenda.size(); i++) {
			
			
			if(erasenda[i]->first>1)
				degree_node.insert(make_pair(erasenda[i]->first - 1, erasenda[i]->second));
	
			degree_node.erase(erasenda[i]);
		
		}

		degree_node.erase(itlast);
		
	}
    
    
    // ----------------------------------------------------------
    deque<int> degree_list;
    for(long unsigned int kk=0; kk<degrees.size(); kk++){
        for(int k2=0; k2<degrees[kk]; k2++){
            degree_list.push_back(kk); 
		}
	}
	
    
	// this is to randomize the subgraph -------------------------------------------------------------------
	
	for(long unsigned int run=0; run<10; run++) for(long unsigned int node_a=0; node_a<degrees.size(); node_a++) for(long unsigned int krm=0; krm<en[node_a].size(); krm++) {
	
		long unsigned int random_mate=random_element_except(degree_list, [&](int x) { return (long unsigned int) x == node_a; });
				
		
		if (en[node_a].insert(random_mate).second) {
			
			deque <int> out_nodes;
			for (set<int>::iterator it_est=en[node_a].begin(); it_est!=en[node_a].end(); it_est++) if ((*it_est)!=(const int) random_mate)
				out_nodes.push_back(*it_est);
						
					
			int old_node=random_element(out_nodes);
					
			en[node_a].erase(old_node);
			en[random_mate].insert(node_a);
			en[old_node].erase(node_a);

			deque <int> not_common;
			for (set<int>::iterator it_est=en[random_mate].begin(); it_est!=en[random_mate].end(); it_est++)
				if ((old_node!=(*it_est)) && (en[old_node].find(*it_est)==en[old_node].end()))
					not_common.push_back(*it_est);
					
						
			int node_h=random_element(not_common);
			
			en[random_mate].erase(node_h);
			en[node_h].erase(random_mate);
			en[node_h].insert(old_node);
			en[old_node].insert(node_h);
			
			
		}		
	}

	
	
	// now I try to insert the new links into the already done network. If some multiple links come out, I try to rewire them
	
	deque < pair<int, int> > multiple_edge;
	for (long unsigned int i=0; i<en.size(); i++) {
		
		for(set<int>::iterator its=en[i].begin(); its!=en[i].end(); its++) if((const int) i<*its ) {
		
			bool already = !(E[nodes[i]].insert(nodes[*its]).second);		// true is the insertion didn't take place
			if (already)
				multiple_edge.push_back(make_pair(nodes[i], nodes[*its]));			
			else
				E[nodes[*its]].insert(nodes[i]);
			
		
		}
	
	
	}
	
	
	for (long unsigned int i=0; i<multiple_edge.size(); i++) {
		
		Rcpp::checkUserInterrupt();
		int &a = multiple_edge[i].first;
		int &b = multiple_edge[i].second;
		
	
		// now, I'll try to rewire this multiple link among the nodes stored in nodes.
		long unsigned int stopper_ml=0;
		
		while (true) {
					
			stopper_ml++;
			
            
			int random_mate=nodes[random_element_except(degree_list, [&](int x) { return nodes[x] == a || nodes[x] == b; })];
			
			if(E[a].find(random_mate)==E[a].end()) {
				
				deque <int> not_common;
				for (set<int>::iterator it_est=E[random_mate].begin(); it_est!=E[random_mate].end(); it_est++)
					if ((b!=(*it_est)) && (E[b].find(*it_est)==E[b].end()) && (binary_search(nodes.begin(), nodes.end(), *it_est)))
						not_common.push_back(*it_est);
				
				if(not_common.size()>0) {
				
					int node_h=random_element(not_common);
					
					
					
					E[random_mate].insert(a);
					E[random_mate].erase(node_h);
					
					E[node_h].erase(random_mate);
					E[node_h].insert(b);
					
					E[b].insert(node_h);
					E[a].insert(random_mate);
					
					break;

				
			
				}
			
			}
			
			if(stopper_ml==2*E.size()) {
	
				links_removed++;
				break;
	
			}
			
			
			
		}
	
	
	}

	return 0;

}

int build_subgraphs(deque<set<int> > & E, const deque<deque<int> > & member_matrix, deque<deque<int> > & member_list, 
	deque<deque<int> > & link_list, const deque<int> & internal_degree_seq, const deque<int> & degree_seq, const bool excess, const bool defect, int & links_removed) {
	
	
	
	E.clear();
	member_list.clear();
	link_list.clear();
	
	int num_nodes=degree_seq.size();
	

	
	
	
	{
		
		deque<int> first;
		for (int i=0; i<num_nodes; i++)
			member_list.push_back(first);
	
	}
	
	
	
	for (long unsigned int i=0; i<member_matrix.size(); i++)
		for (long unsigned int j=0; j<member_matrix[i].size(); j++)
			member_list[member_matrix[i][j]].push_back(i);
	
	
	
	for (long unsigned int i=0; i<member_list.size(); i++) {
		
		deque<int> liin;

		
		for (long unsigned int j=0; j<member_list[i].size(); j++) {
			
			compute_internal_degree_per_node(internal_degree_seq[i], member_list[i].size(), liin);
			liin.push_back(degree_seq[i] - internal_degree_seq[i]);
		
		}
		
		link_list.push_back(liin);
		
	}
			
	
	

	// now there is the check for the even node (it means that the internal degree of each group has to be even and we want to assure that, otherwise the degree_seq has to change) ----------------------------

	
	
	
	// ------------------------ this is done to check if the sum of the internal degree is an even number. if not, the program will change it in such a way to assure that. 
	
	
			
	for (long unsigned int i=0; i<member_matrix.size(); i++) {
	
		
		int internal_cluster=0;
		for (long unsigned int j=0; j<member_matrix[i].size(); j++) {
			
			int right_index= lower_bound(member_list[member_matrix[i][j]].begin(), member_list[member_matrix[i][j]].end(), i) - member_list[member_matrix[i][j]].begin();
			
				
			
			internal_cluster+=link_list[member_matrix[i][j]][right_index];
		}
		
		
		if(internal_cluster % 2 != 0) {
			
			
			
			bool default_flag=false;
			
			if(excess)
				default_flag=true;
			else if(defect)
				default_flag=false;
			else if (ran4()>0.5)
				default_flag=true;
			
			if(default_flag) {
				
				// if this does not work in a reasonable time the degree sequence will be changed
				
				for (long unsigned int j=0; j<member_matrix[i].size(); j++) {		
					
					
					int random_mate=random_element(member_matrix[i]);
					
					int right_index= lower_bound(member_list[random_mate].begin(), member_list[random_mate].end(), i) - member_list[random_mate].begin();
					
					
					if ((link_list[random_mate][right_index]<(int) member_matrix[i].size()-1) && (link_list[random_mate][link_list[random_mate].size()-1] > 0 )) {
						
						link_list[random_mate][right_index]++;
						link_list[random_mate][link_list[random_mate].size()-1]--;

						break;
						
					}
				
				}
			
			
			}
			
			
			else {
				
				for (long unsigned int j=0; j<member_matrix[i].size(); j++) {

					int random_mate=random_element(member_matrix[i]);

					int right_index= lower_bound(member_list[random_mate].begin(), member_list[random_mate].end(), i) - member_list[random_mate].begin();
					
					if (link_list[random_mate][right_index] > 0 ) {
						
						link_list[random_mate][right_index]--;
						link_list[random_mate][link_list[random_mate].size()-1]++;

						break;
						
					}
				
				}
			
			
			}
			
			
			
					
		}
			
	
	}
	
	
	// ------------------------ this is done to check if the sum of the internal degree is an even number. if not, the program will change it in such a way to assure that. 
	
	
	
	
	{
	
		set<int> first;
		for(int i=0; i<num_nodes; i++)
			E.push_back(first);
	
	}
	
	for (long unsigned int i=0; i<member_matrix.size(); i++) {
		
		
		deque<int> internal_degree_i;
		for (long unsigned int j=0; j<member_matrix[i].size(); j++) {
		
		
			int right_index= lower_bound(member_list[member_matrix[i][j]].begin(), member_list[member_matrix[i][j]].end(), i) - member_list[member_matrix[i][j]].begin();
		
			internal_degree_i.push_back(link_list[member_matrix[i][j]][right_index]);
		
		}
		
		
		
		
		Rcpp::checkUserInterrupt();
		build_subgraph(E, member_matrix[i], internal_degree_i, links_removed);
	
	
	}

	return 0;
	
}

int connect_all_the_parts(deque<set<int> > & E, const deque<deque<int> > & member_list, const deque<deque<int> > & link_list) {

	
	deque<int> degrees;
	for(long unsigned int i=0; i<link_list.size(); i++)
		degrees.push_back(link_list[i][link_list[i].size()-1]);
	
	
		
	
	deque<set<int> > en; // this is the en of the subgraph

	{
		set<int> first;
		for(long unsigned int i=0; i<member_list.size(); i++) 
			en.push_back(first);
	}
	
	
	
	multimap <int, int> degree_node;
	
	for(long unsigned int i=0; i<degrees.size(); i++)
		degree_node.insert(degree_node.end(), make_pair(degrees[i], i));
	

	while (degree_node.size() > 0) {
		
		multimap<int, int>::iterator itlast= degree_node.end();
		itlast--;
		
		multimap <int, int>::iterator itit= itlast;
		deque <multimap<int, int>::iterator> erasenda;
		
		
		for (int i=0; i<itlast->first; i++) {
			
			if(itit!=degree_node.begin()) {
			
				itit--;
				
				
				en[itlast->second].insert(itit->second);
				en[itit->second].insert(itlast->second);
				
				erasenda.push_back(itit);				
				
			}
			
			else
				break;
		
		}
		
		
		for (long unsigned int i=0; i<erasenda.size(); i++) {
			
			
			if(erasenda[i]->first>1)
				degree_node.insert(make_pair(erasenda[i]->first - 1, erasenda[i]->second));
	
			degree_node.erase(erasenda[i]);
		
		}
		
		degree_node.erase(itlast);
		
	}
		
	// this is to randomize the subgraph -------------------------------------------------------------------
	
    // ----------------------------------------------------------
    deque<int> degree_list;
    for(long unsigned int kk=0; kk<degrees.size(); kk++){
        for(int k2=0; k2<degrees[kk]; k2++){
            degree_list.push_back(kk); 
		}
	}
    
	for(long unsigned int run=0; run<10; run++) for(long unsigned int node_a=0; node_a<degrees.size(); node_a++) for(long unsigned int krm=0; krm<en[node_a].size(); krm++) {
        
        
		long unsigned int random_mate=random_element_except(degree_list, [&](int x) { return (long unsigned int) x == node_a; });
		
		
		
		if (en[node_a].insert(random_mate).second) {
			
			deque <int> out_nodes;
			for (set<int>::iterator it_est=en[node_a].begin(); it_est!=en[node_a].end(); it_est++) if ((*it_est)!= (const int)random_mate)
				out_nodes.push_back(*it_est);
						
										
					
			int old_node=random_element(out_nodes);
					
			en[node_a].erase(old_node);
			en[random_mate].insert(node_a);
			en[old_node].erase(node_a);
										
			deque <int> not_common;
			for (set<int>::iterator it_est=en[random_mate].begin(); it_est!=en[random_mate].end(); it_est++)
				if ((old_node!=(*it_est)) && (en[old_node].find(*it_est)==en[old_node].end()))
					not_common.push_back(*it_est);
					
						
			int node_h=random_element(not_common);
			
			en[random_mate].erase(node_h);
			en[node_h].erase(random_mate);
			en[node_h].insert(old_node);
			en[old_node].insert(node_h);
			
			
		}
	
	
	

	}

	
	
	
	// now there is a rewiring process to avoid "mate nodes" (nodes with al least one membership in common) to link each other
	
	int var_mate=0;
	for(long unsigned int i=0; i<degrees.size(); i++) for(set<int>::iterator itss= en[i].begin(); itss!=en[i].end(); itss++) if(they_are_mate(i, *itss, member_list)) {
		var_mate++;
	}
	
	
	int stopper_mate=0;
	int mate_trooper=10;
	
	while(var_mate>0) {
		
		Rcpp::checkUserInterrupt();
	
		

		
		int best_var_mate=var_mate;
	
		// ************************************************  rewiring
		
		
		for(long unsigned int a=0; a<degrees.size(); a++) for(set<int>::iterator its= en[a].begin(); its!=en[a].end(); its++) if(they_are_mate(a, *its, member_list)) {
				
			
			
			long unsigned int b=*its;
			long unsigned int stopper_m=0;
			
			while (true) {
						
				stopper_m++;
				
				long unsigned int random_mate = random_element_except(degree_list, [&](int x) { return (long unsigned int) x == a || (long unsigned int) x == b; });
				
				
				if(!(they_are_mate(a, random_mate, member_list)) && (en[a].find(random_mate)==en[a].end())) {
					
					deque <int> not_common;
					for (set<int>::iterator it_est=en[random_mate].begin(); it_est!=en[random_mate].end(); it_est++)
						if (((const int) b!=(*it_est)) && (en[b].find(*it_est)==en[b].end()))
							not_common.push_back(*it_est);
					
					if(not_common.size()>0) {
					
						int node_h=random_element(not_common);
						
						
						en[random_mate].erase(node_h);
						en[random_mate].insert(a);
						
						en[node_h].erase(random_mate);
						en[node_h].insert(b);
						
						en[b].erase(a);
						en[b].insert(node_h);
						
						en[a].insert(random_mate);
						en[a].erase(b);
						
											
						
						if(!they_are_mate(b, node_h, member_list))
							var_mate-=2;
						
						
						if(they_are_mate(random_mate, node_h, member_list))
							var_mate-=2;
						
						break;

					
				
					}
				
				}
				
				if(stopper_m==en[a].size())
					break;
				
				
				
			}
				
			
			break;		// this break is done because if you erased some link you have to stop this loop (en[i] changed)
	
	
		}

		// ************************************************  rewiring
		
		
				

		if(var_mate==best_var_mate) {
			
			stopper_mate++;
			
			if(stopper_mate==mate_trooper)
				break;

		}
		else
			stopper_mate=0;
		
		
		

	
	}
	
	
	

	for (long unsigned int i=0; i<en.size(); i++) {
		
		for(set<int>::iterator its=en[i].begin(); its!=en[i].end(); its++) if((const int) i<*its) {
		
			E[i].insert(*its);
			E[*its].insert(i);
		
		}
	}
	return 0;

}

int internal_kin(deque<set<int> > & E, const deque<deque<int> > & member_list, int i) {
	
	int var_mate2=0;
	for(set<int>::iterator itss= E[i].begin(); itss!=E[i].end(); itss++) if(they_are_mate(i, *itss, member_list)) 
		var_mate2++;	

	return var_mate2;
	
}

int erase_links(deque<set<int> > & E, const deque<deque<int> > & member_list, const bool excess, const bool defect, const double mixing_parameter, const bool verbose) {

	
	int num_nodes= member_list.size();
	
	int eras_add_times=0;
	
	if (excess) {
		
		for (int i=0; i<num_nodes; i++) {
			
			
			while ( (E[i].size()>1) &&  double(internal_kin(E, member_list, i))/E[i].size() < 1 - mixing_parameter) {
			
			//---------------------------------------------------------------------------------
				
				
				++eras_add_times;
				if (verbose)
					Rcout<<"degree sequence changed to respect the option -sup ... "<<eras_add_times<<endl;
				
				deque<int> deqar;
				for (set<int>::iterator it_est=E[i].begin(); it_est!=E[i].end(); it_est++)
					if (!they_are_mate(i, *it_est, member_list))
						deqar.push_back(*it_est);
				
				
				if(deqar.size()==E[i].size()) {	// this shouldn't happen...
				
					Rcpp::stop("LFR benchmark: there is a node which does not respect the constraints (option -sup).");
				
				}
				
				int random_mate=random_element(deqar);
				
				E[i].erase(random_mate);
				E[random_mate].erase(i);
				
		
			}
		}
	
	}
	
	
	
	if (defect) {
			
		for (int i=0; i<num_nodes; i++)
			while ( (E[i].size()<E.size()) &&  double(internal_kin(E, member_list, i))/E[i].size() > 1 - mixing_parameter) {
				
				//---------------------------------------------------------------------------------
					
				
				++eras_add_times;
				if (verbose)
					Rcout<<"degree sequence changed to respect the option -inf ... "<<eras_add_times<<endl;


				int stopper_here=num_nodes;
				int stopper_=0;
				
				int random_mate=irand(num_nodes-1);
				while ( (    (they_are_mate(i, random_mate, member_list)) || E[i].find(random_mate)!=E[i].end())      &&      (stopper_<stopper_here) ) {
					
					random_mate=irand(num_nodes-1);
					stopper_++;
				
				
				}
				
				if(stopper_==stopper_here) {	// this shouldn't happen...
				
					Rcpp::stop("LFR benchmark: there is a node which does not respect the constraints (option -inf).");
				
				}
				
				
				
				E[i].insert(random_mate);
				E[random_mate].insert(i);
				
								
		
			}
			
		
	}

	//------------------------------------ Erasing links   ------------------------------------------------------

	return 0;
	
}

//[[Rcpp::export]]
Rcpp::List benchmark(bool excess, bool defect, int num_nodes, double  average_k, int  max_degree, double  tau, double  tau2, 
	double  mixing_parameter, int  overlapping_nodes, int  overlap_membership, int nmin, int nmax ,bool fixed_range, bool verbose = false) {

	double dmin=solve_dmin(max_degree, average_k, -tau);
	
	int min_degree=int(dmin);
	
	double media1=integer_average(max_degree, min_degree, tau);
	double media2=integer_average(max_degree, min_degree+1, tau);
	
	if (fabs(media1-average_k)>fabs(media2-average_k))
		min_degree++;
	
	// range for the community sizes
	if (!fixed_range) {
		nmin=max(int(min_degree), 3);
		nmax=max(max_degree, nmin);
		if (verbose)
			Rcout<<"community size range automatically set equal to ["<<nmin<<" , "<<nmax<<"]"<<endl;
	}
	if (nmin < 1 || nmin > nmax)
		Rcpp::stop("invalid community size range [%d, %d].", nmin, nmax);

	//--------------------------------------------------------------------------------------------------
	
	deque <int> degree_seq ;		//  degree sequence of the nodes
	deque <double> cumulative;
	powerlaw(max_degree, min_degree, tau, cumulative);
	
	for (int i=0; i<num_nodes; i++)
		degree_seq.push_back(sample_from_cumulative(cumulative, min_degree));
	
	sort(degree_seq.begin(), degree_seq.end());
		
	if(deque_int_sum(degree_seq) % 2!=0)
		degree_seq[max_element(degree_seq.begin(), degree_seq.end()) - degree_seq.begin()]--;
	
	
	deque<deque<int> >  member_matrix;
	deque<int> num_seq;
	deque<int> internal_degree_seq;
	
	// ********************************			internal_degree and membership			***************************************************

	internal_degree_and_membership(mixing_parameter, overlapping_nodes, overlap_membership, num_nodes, member_matrix, excess, defect, degree_seq, num_seq, internal_degree_seq, fixed_range, nmin, nmax, tau2, verbose);
	
	deque<set<int> > E;					// E is the adjacency matrix written in form of list of edges
	deque<deque<int> > member_list;		// row i cointains the memberships of node i
	deque<deque<int> > link_list;		// row i cointains degree of the node i respect to member_list[i][j]; there is one more number that is the external degree

	int links_removed=0;
	build_subgraphs(E, member_matrix, member_list, link_list, internal_degree_seq, degree_seq, excess, defect, links_removed);
	
	connect_all_the_parts(E, member_list, link_list);
	
	erase_links(E, member_list, excess, defect, mixing_parameter, verbose);

	return Rcpp::List::create(Rcpp::Named("edgelist")=E,
							  Rcpp::Named("membership")=member_list,
							  Rcpp::Named("links_removed")=links_removed);
	
}
