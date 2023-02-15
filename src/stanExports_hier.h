// Generated by rstantools.  Do not edit by hand.

/*
    test is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    test is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with test.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef MODELS_HPP
#define MODELS_HPP
#define STAN__SERVICES__COMMAND_HPP
#include <rstan/rstaninc.hpp>
// Code generated by Stan version 2.21.0
#include <stan/model/model_header.hpp>
namespace model_hier_namespace {
using std::istream;
using std::string;
using std::stringstream;
using std::vector;
using stan::io::dump;
using stan::math::lgamma;
using stan::model::prob_grad;
using namespace stan::math;
static int current_statement_begin__;
stan::io::program_reader prog_reader__() {
    stan::io::program_reader reader;
    reader.add_event(0, 0, "start", "model_hier");
    reader.add_event(30, 28, "end", "model_hier");
    return reader;
}
#include <stan_meta_header.hpp>
class model_hier
  : public stan::model::model_base_crtp<model_hier> {
private:
        int ns;
        std::vector<int> y;
        std::vector<int> n;
        double pri_sig;
public:
    model_hier(stan::io::var_context& context__,
        std::ostream* pstream__ = 0)
        : model_base_crtp(0) {
        ctor_body(context__, 0, pstream__);
    }
    model_hier(stan::io::var_context& context__,
        unsigned int random_seed__,
        std::ostream* pstream__ = 0)
        : model_base_crtp(0) {
        ctor_body(context__, random_seed__, pstream__);
    }
    void ctor_body(stan::io::var_context& context__,
                   unsigned int random_seed__,
                   std::ostream* pstream__) {
        typedef double local_scalar_t__;
        boost::ecuyer1988 base_rng__ =
          stan::services::util::create_rng(random_seed__, 0);
        (void) base_rng__;  // suppress unused var warning
        current_statement_begin__ = -1;
        static const char* function__ = "model_hier_namespace::model_hier";
        (void) function__;  // dummy to suppress unused var warning
        size_t pos__;
        (void) pos__;  // dummy to suppress unused var warning
        std::vector<int> vals_i__;
        std::vector<double> vals_r__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning
        try {
            // initialize data block variables from context__
            current_statement_begin__ = 2;
            context__.validate_dims("data initialization", "ns", "int", context__.to_vec());
            ns = int(0);
            vals_i__ = context__.vals_i("ns");
            pos__ = 0;
            ns = vals_i__[pos__++];
            check_greater_or_equal(function__, "ns", ns, 1);
            current_statement_begin__ = 3;
            validate_non_negative_index("y", "ns", ns);
            context__.validate_dims("data initialization", "y", "int", context__.to_vec(ns));
            y = std::vector<int>(ns, int(0));
            vals_i__ = context__.vals_i("y");
            pos__ = 0;
            size_t y_k_0_max__ = ns;
            for (size_t k_0__ = 0; k_0__ < y_k_0_max__; ++k_0__) {
                y[k_0__] = vals_i__[pos__++];
            }
            size_t y_i_0_max__ = ns;
            for (size_t i_0__ = 0; i_0__ < y_i_0_max__; ++i_0__) {
                check_greater_or_equal(function__, "y[i_0__]", y[i_0__], 0);
            }
            current_statement_begin__ = 4;
            validate_non_negative_index("n", "ns", ns);
            context__.validate_dims("data initialization", "n", "int", context__.to_vec(ns));
            n = std::vector<int>(ns, int(0));
            vals_i__ = context__.vals_i("n");
            pos__ = 0;
            size_t n_k_0_max__ = ns;
            for (size_t k_0__ = 0; k_0__ < n_k_0_max__; ++k_0__) {
                n[k_0__] = vals_i__[pos__++];
            }
            size_t n_i_0_max__ = ns;
            for (size_t i_0__ = 0; i_0__ < n_i_0_max__; ++i_0__) {
                check_greater_or_equal(function__, "n[i_0__]", n[i_0__], 0);
            }
            current_statement_begin__ = 5;
            context__.validate_dims("data initialization", "pri_sig", "double", context__.to_vec());
            pri_sig = double(0);
            vals_r__ = context__.vals_r("pri_sig");
            pos__ = 0;
            pri_sig = vals_r__[pos__++];
            check_greater_or_equal(function__, "pri_sig", pri_sig, 0);
            // initialize transformed data variables
            // execute transformed data statements
            // validate transformed data
            // validate, set parameter ranges
            num_params_r__ = 0U;
            param_ranges_i__.clear();
            current_statement_begin__ = 9;
            validate_non_negative_index("beta", "ns", ns);
            num_params_r__ += (1 * ns);
            current_statement_begin__ = 10;
            num_params_r__ += 1;
            current_statement_begin__ = 11;
            num_params_r__ += 1;
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }
    ~model_hier() { }
    void transform_inits(const stan::io::var_context& context__,
                         std::vector<int>& params_i__,
                         std::vector<double>& params_r__,
                         std::ostream* pstream__) const {
        typedef double local_scalar_t__;
        stan::io::writer<double> writer__(params_r__, params_i__);
        size_t pos__;
        (void) pos__; // dummy call to supress warning
        std::vector<double> vals_r__;
        std::vector<int> vals_i__;
        current_statement_begin__ = 9;
        if (!(context__.contains_r("beta")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable beta missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("beta");
        pos__ = 0U;
        validate_non_negative_index("beta", "ns", ns);
        context__.validate_dims("parameter initialization", "beta", "double", context__.to_vec(ns));
        std::vector<double> beta(ns, double(0));
        size_t beta_k_0_max__ = ns;
        for (size_t k_0__ = 0; k_0__ < beta_k_0_max__; ++k_0__) {
            beta[k_0__] = vals_r__[pos__++];
        }
        size_t beta_i_0_max__ = ns;
        for (size_t i_0__ = 0; i_0__ < beta_i_0_max__; ++i_0__) {
            try {
                writer__.scalar_unconstrain(beta[i_0__]);
            } catch (const std::exception& e) {
                stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable beta: ") + e.what()), current_statement_begin__, prog_reader__());
            }
        }
        current_statement_begin__ = 10;
        if (!(context__.contains_r("mu_beta")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable mu_beta missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("mu_beta");
        pos__ = 0U;
        context__.validate_dims("parameter initialization", "mu_beta", "double", context__.to_vec());
        double mu_beta(0);
        mu_beta = vals_r__[pos__++];
        try {
            writer__.scalar_unconstrain(mu_beta);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable mu_beta: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        current_statement_begin__ = 11;
        if (!(context__.contains_r("sigma")))
            stan::lang::rethrow_located(std::runtime_error(std::string("Variable sigma missing")), current_statement_begin__, prog_reader__());
        vals_r__ = context__.vals_r("sigma");
        pos__ = 0U;
        context__.validate_dims("parameter initialization", "sigma", "double", context__.to_vec());
        double sigma(0);
        sigma = vals_r__[pos__++];
        try {
            writer__.scalar_lb_unconstrain(0, sigma);
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(std::runtime_error(std::string("Error transforming variable sigma: ") + e.what()), current_statement_begin__, prog_reader__());
        }
        params_r__ = writer__.data_r();
        params_i__ = writer__.data_i();
    }
    void transform_inits(const stan::io::var_context& context,
                         Eigen::Matrix<double, Eigen::Dynamic, 1>& params_r,
                         std::ostream* pstream__) const {
      std::vector<double> params_r_vec;
      std::vector<int> params_i_vec;
      transform_inits(context, params_i_vec, params_r_vec, pstream__);
      params_r.resize(params_r_vec.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r(i) = params_r_vec[i];
    }
    template <bool propto__, bool jacobian__, typename T__>
    T__ log_prob(std::vector<T__>& params_r__,
                 std::vector<int>& params_i__,
                 std::ostream* pstream__ = 0) const {
        typedef T__ local_scalar_t__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // dummy to suppress unused var warning
        T__ lp__(0.0);
        stan::math::accumulator<T__> lp_accum__;
        try {
            stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
            // model parameters
            current_statement_begin__ = 9;
            std::vector<local_scalar_t__> beta;
            size_t beta_d_0_max__ = ns;
            beta.reserve(beta_d_0_max__);
            for (size_t d_0__ = 0; d_0__ < beta_d_0_max__; ++d_0__) {
                if (jacobian__)
                    beta.push_back(in__.scalar_constrain(lp__));
                else
                    beta.push_back(in__.scalar_constrain());
            }
            current_statement_begin__ = 10;
            local_scalar_t__ mu_beta;
            (void) mu_beta;  // dummy to suppress unused var warning
            if (jacobian__)
                mu_beta = in__.scalar_constrain(lp__);
            else
                mu_beta = in__.scalar_constrain();
            current_statement_begin__ = 11;
            local_scalar_t__ sigma;
            (void) sigma;  // dummy to suppress unused var warning
            if (jacobian__)
                sigma = in__.scalar_lb_constrain(0, lp__);
            else
                sigma = in__.scalar_lb_constrain(0);
            // transformed parameters
            current_statement_begin__ = 15;
            validate_non_negative_index("theta", "ns", ns);
            std::vector<local_scalar_t__> theta(ns, local_scalar_t__(0));
            stan::math::initialize(theta, DUMMY_VAR__);
            stan::math::fill(theta, DUMMY_VAR__);
            // transformed parameters block statements
            current_statement_begin__ = 16;
            for (int i = 1; i <= ns; ++i) {
                current_statement_begin__ = 17;
                stan::model::assign(theta, 
                            stan::model::cons_list(stan::model::index_uni(i), stan::model::nil_index_list()), 
                            (stan::math::exp(get_base1(beta, i, "beta", 1)) / (1 + stan::math::exp(get_base1(beta, i, "beta", 1)))), 
                            "assigning variable theta");
            }
            // validate transformed parameters
            const char* function__ = "validate transformed params";
            (void) function__;  // dummy to suppress unused var warning
            current_statement_begin__ = 15;
            size_t theta_k_0_max__ = ns;
            for (size_t k_0__ = 0; k_0__ < theta_k_0_max__; ++k_0__) {
                if (stan::math::is_uninitialized(theta[k_0__])) {
                    std::stringstream msg__;
                    msg__ << "Undefined transformed parameter: theta" << "[" << k_0__ << "]";
                    stan::lang::rethrow_located(std::runtime_error(std::string("Error initializing variable theta: ") + msg__.str()), current_statement_begin__, prog_reader__());
                }
            }
            size_t theta_i_0_max__ = ns;
            for (size_t i_0__ = 0; i_0__ < theta_i_0_max__; ++i_0__) {
                check_greater_or_equal(function__, "theta[i_0__]", theta[i_0__], 0);
                check_less_or_equal(function__, "theta[i_0__]", theta[i_0__], 1);
            }
            // model body
            current_statement_begin__ = 22;
            lp_accum__.add(normal_log<propto__>(beta, mu_beta, sigma));
            current_statement_begin__ = 23;
            lp_accum__.add(normal_log<propto__>(mu_beta, 0, 100));
            current_statement_begin__ = 24;
            lp_accum__.add(normal_log<propto__>(sigma, 0, pri_sig));
            current_statement_begin__ = 27;
            lp_accum__.add(binomial_log<propto__>(y, n, theta));
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
        lp_accum__.add(lp__);
        return lp_accum__.sum();
    } // log_prob()
    template <bool propto, bool jacobian, typename T_>
    T_ log_prob(Eigen::Matrix<T_,Eigen::Dynamic,1>& params_r,
               std::ostream* pstream = 0) const {
      std::vector<T_> vec_params_r;
      vec_params_r.reserve(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        vec_params_r.push_back(params_r(i));
      std::vector<int> vec_params_i;
      return log_prob<propto,jacobian,T_>(vec_params_r, vec_params_i, pstream);
    }
    void get_param_names(std::vector<std::string>& names__) const {
        names__.resize(0);
        names__.push_back("beta");
        names__.push_back("mu_beta");
        names__.push_back("sigma");
        names__.push_back("theta");
    }
    void get_dims(std::vector<std::vector<size_t> >& dimss__) const {
        dimss__.resize(0);
        std::vector<size_t> dims__;
        dims__.resize(0);
        dims__.push_back(ns);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dims__.push_back(ns);
        dimss__.push_back(dims__);
    }
    template <typename RNG>
    void write_array(RNG& base_rng__,
                     std::vector<double>& params_r__,
                     std::vector<int>& params_i__,
                     std::vector<double>& vars__,
                     bool include_tparams__ = true,
                     bool include_gqs__ = true,
                     std::ostream* pstream__ = 0) const {
        typedef double local_scalar_t__;
        vars__.resize(0);
        stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
        static const char* function__ = "model_hier_namespace::write_array";
        (void) function__;  // dummy to suppress unused var warning
        // read-transform, write parameters
        std::vector<double> beta;
        size_t beta_d_0_max__ = ns;
        beta.reserve(beta_d_0_max__);
        for (size_t d_0__ = 0; d_0__ < beta_d_0_max__; ++d_0__) {
            beta.push_back(in__.scalar_constrain());
        }
        size_t beta_k_0_max__ = ns;
        for (size_t k_0__ = 0; k_0__ < beta_k_0_max__; ++k_0__) {
            vars__.push_back(beta[k_0__]);
        }
        double mu_beta = in__.scalar_constrain();
        vars__.push_back(mu_beta);
        double sigma = in__.scalar_lb_constrain(0);
        vars__.push_back(sigma);
        double lp__ = 0.0;
        (void) lp__;  // dummy to suppress unused var warning
        stan::math::accumulator<double> lp_accum__;
        local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning
        if (!include_tparams__ && !include_gqs__) return;
        try {
            // declare and define transformed parameters
            current_statement_begin__ = 15;
            validate_non_negative_index("theta", "ns", ns);
            std::vector<double> theta(ns, double(0));
            stan::math::initialize(theta, DUMMY_VAR__);
            stan::math::fill(theta, DUMMY_VAR__);
            // do transformed parameters statements
            current_statement_begin__ = 16;
            for (int i = 1; i <= ns; ++i) {
                current_statement_begin__ = 17;
                stan::model::assign(theta, 
                            stan::model::cons_list(stan::model::index_uni(i), stan::model::nil_index_list()), 
                            (stan::math::exp(get_base1(beta, i, "beta", 1)) / (1 + stan::math::exp(get_base1(beta, i, "beta", 1)))), 
                            "assigning variable theta");
            }
            if (!include_gqs__ && !include_tparams__) return;
            // validate transformed parameters
            const char* function__ = "validate transformed params";
            (void) function__;  // dummy to suppress unused var warning
            current_statement_begin__ = 15;
            size_t theta_i_0_max__ = ns;
            for (size_t i_0__ = 0; i_0__ < theta_i_0_max__; ++i_0__) {
                check_greater_or_equal(function__, "theta[i_0__]", theta[i_0__], 0);
                check_less_or_equal(function__, "theta[i_0__]", theta[i_0__], 1);
            }
            // write transformed parameters
            if (include_tparams__) {
                size_t theta_k_0_max__ = ns;
                for (size_t k_0__ = 0; k_0__ < theta_k_0_max__; ++k_0__) {
                    vars__.push_back(theta[k_0__]);
                }
            }
            if (!include_gqs__) return;
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e, current_statement_begin__, prog_reader__());
            // Next line prevents compiler griping about no return
            throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }
    }
    template <typename RNG>
    void write_array(RNG& base_rng,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& params_r,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& vars,
                     bool include_tparams = true,
                     bool include_gqs = true,
                     std::ostream* pstream = 0) const {
      std::vector<double> params_r_vec(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r_vec[i] = params_r(i);
      std::vector<double> vars_vec;
      std::vector<int> params_i_vec;
      write_array(base_rng, params_r_vec, params_i_vec, vars_vec, include_tparams, include_gqs, pstream);
      vars.resize(vars_vec.size());
      for (int i = 0; i < vars.size(); ++i)
        vars(i) = vars_vec[i];
    }
    std::string model_name() const {
        return "model_hier";
    }
    void constrained_param_names(std::vector<std::string>& param_names__,
                                 bool include_tparams__ = true,
                                 bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        size_t beta_k_0_max__ = ns;
        for (size_t k_0__ = 0; k_0__ < beta_k_0_max__; ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "beta" << '.' << k_0__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        param_name_stream__.str(std::string());
        param_name_stream__ << "mu_beta";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "sigma";
        param_names__.push_back(param_name_stream__.str());
        if (!include_gqs__ && !include_tparams__) return;
        if (include_tparams__) {
            size_t theta_k_0_max__ = ns;
            for (size_t k_0__ = 0; k_0__ < theta_k_0_max__; ++k_0__) {
                param_name_stream__.str(std::string());
                param_name_stream__ << "theta" << '.' << k_0__ + 1;
                param_names__.push_back(param_name_stream__.str());
            }
        }
        if (!include_gqs__) return;
    }
    void unconstrained_param_names(std::vector<std::string>& param_names__,
                                   bool include_tparams__ = true,
                                   bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        size_t beta_k_0_max__ = ns;
        for (size_t k_0__ = 0; k_0__ < beta_k_0_max__; ++k_0__) {
            param_name_stream__.str(std::string());
            param_name_stream__ << "beta" << '.' << k_0__ + 1;
            param_names__.push_back(param_name_stream__.str());
        }
        param_name_stream__.str(std::string());
        param_name_stream__ << "mu_beta";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "sigma";
        param_names__.push_back(param_name_stream__.str());
        if (!include_gqs__ && !include_tparams__) return;
        if (include_tparams__) {
            size_t theta_k_0_max__ = ns;
            for (size_t k_0__ = 0; k_0__ < theta_k_0_max__; ++k_0__) {
                param_name_stream__.str(std::string());
                param_name_stream__ << "theta" << '.' << k_0__ + 1;
                param_names__.push_back(param_name_stream__.str());
            }
        }
        if (!include_gqs__) return;
    }
}; // model
}  // namespace
typedef model_hier_namespace::model_hier stan_model;
#ifndef USING_R
stan::model::model_base& new_model(
        stan::io::var_context& data_context,
        unsigned int seed,
        std::ostream* msg_stream) {
  stan_model* m = new stan_model(data_context, seed, msg_stream);
  return *m;
}
#endif
#endif
