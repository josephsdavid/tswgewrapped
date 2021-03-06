% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ModelCompareUnivariate.R
\name{ModelCompareUnivariate}
\alias{ModelCompareUnivariate}
\title{R6 class ModelCompareUnivariate}
\description{
R6 class ModelCompareUnivariate

R6 class ModelCompareUnivariate
}
\examples{
library(tswge)
data("airlog")

# Woodward Gray Airline Model
phi_wg = c(-0.36, -0.05, -0.14, -0.11, 0.04, 0.09, -0.02, 0.02, 0.17, 0.03, -0.10, -0.38)
d_wg = 1
s_wg = 12

# Parzen Model
phi_pz = c(0.74, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.38, -0.2812)
s_pz = 12

# Box Model
d_bx = 1
s_bx = 12  
theta_bx =  c(0.40, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.60, -0.24)

models = list("Woodward Gray Model A" = list(phi = phi_wg, d = d_wg, s = s_wg, sliding_ase = FALSE),
              "Woodward Gray Model B" = list(phi = phi_wg, d = d_wg, s = s_wg, sliding_ase = TRUE),
              "Parzen Model A" = list(phi = phi_pz, s = s_pz, sliding_ase = FALSE),
              "Parzen Model B" = list(phi = phi_pz, s = s_pz, sliding_ase = TRUE),
              "Box Model A" = list(theta = theta_bx, d = d_bx, s = s_bx, sliding_ase = FALSE),
              "Box Model B" = list(theta = theta_bx, d = d_bx, s = s_bx, sliding_ase = TRUE)
              )
              
mdl_compare = ModelCompareUnivariate$new(data = airlog, mdl_list = models,
                                         n.ahead = 36, batch_size = 72)
# Plots the historgam of the ASE values for each model.
# This is especially useful when models using a sliding window for ASE calculations. 
mdl_compare$plot_boxplot_ases()

# The following method gives 2 plots
# (1) Plots the forecasts for each model along with the realization.
# (2) Plots the upper and lower limits for each model along with the realization.
# In both cases, this marks each batch using a background color for ease of comparison.
# only_sliding = TRUE will only plot forecsts for models using sliding ASE  calculations.
mdl_compare$plot_batch_forecasts(only_sliding = TRUE)

# This method statistically compares all the models that use a sliding window ASE calculation
mdl_compare$statistical_compare()  

ASEs = mdl_compare$get_tabular_metrics(ases = TRUE)
print(ASEs)

# This method returns the metrics (ASE values) or forecasts for each model
# 'only_sliding' If set to TRUE, only the models that use a sliding window 
#                ASE calculation will be returned
# 'ases' If set to TRUE, this method will return the ASE value(s) 
#        Single value for models that don't use sliding ASEs and 
#        Multiple values (one per batch) for models that use sliding window
#        ASE calculations
#        If set to FALSE, this function will return the model forecasts and 
#        upper and lower confidence intervals. 
forecasts = mdl_compare$get_tabular_metrics(ases = FALSE)
print(forecasts)

}
\section{Super class}{
\code{\link[tswgewrapped:ModelCompareBase]{tswgewrapped::ModelCompareBase}} -> \code{ModelCompareUnivariate}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{ModelCompareUnivariate$new()}}
\item \href{#method-get_data_var_interest}{\code{ModelCompareUnivariate$get_data_var_interest()}}
\item \href{#method-plot_multiple_realizations}{\code{ModelCompareUnivariate$plot_multiple_realizations()}}
\item \href{#method-evaluate_residuals}{\code{ModelCompareUnivariate$evaluate_residuals()}}
\item \href{#method-summarize_build}{\code{ModelCompareUnivariate$summarize_build()}}
\item \href{#method-clone}{\code{ModelCompareUnivariate$clone()}}
}
}
\if{html}{
\out{<details ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="tswgewrapped" data-topic="ModelCompareBase" data-id="add_models">}\href{../../tswgewrapped/html/ModelCompareBase.html#method-add_models}{\code{tswgewrapped::ModelCompareBase$add_models()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="tswgewrapped" data-topic="ModelCompareBase" data-id="compute_metrics">}\href{../../tswgewrapped/html/ModelCompareBase.html#method-compute_metrics}{\code{tswgewrapped::ModelCompareBase$compute_metrics()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="tswgewrapped" data-topic="ModelCompareBase" data-id="get_batch_size">}\href{../../tswgewrapped/html/ModelCompareBase.html#method-get_batch_size}{\code{tswgewrapped::ModelCompareBase$get_batch_size()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="tswgewrapped" data-topic="ModelCompareBase" data-id="get_data">}\href{../../tswgewrapped/html/ModelCompareBase.html#method-get_data}{\code{tswgewrapped::ModelCompareBase$get_data()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="tswgewrapped" data-topic="ModelCompareBase" data-id="get_model_names">}\href{../../tswgewrapped/html/ModelCompareBase.html#method-get_model_names}{\code{tswgewrapped::ModelCompareBase$get_model_names()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="tswgewrapped" data-topic="ModelCompareBase" data-id="get_n.ahead">}\href{../../tswgewrapped/html/ModelCompareBase.html#method-get_n.ahead}{\code{tswgewrapped::ModelCompareBase$get_n.ahead()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="tswgewrapped" data-topic="ModelCompareBase" data-id="get_tabular_metrics">}\href{../../tswgewrapped/html/ModelCompareBase.html#method-get_tabular_metrics}{\code{tswgewrapped::ModelCompareBase$get_tabular_metrics()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="tswgewrapped" data-topic="ModelCompareBase" data-id="keep_models">}\href{../../tswgewrapped/html/ModelCompareBase.html#method-keep_models}{\code{tswgewrapped::ModelCompareBase$keep_models()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="tswgewrapped" data-topic="ModelCompareBase" data-id="plot_batch_ases">}\href{../../tswgewrapped/html/ModelCompareBase.html#method-plot_batch_ases}{\code{tswgewrapped::ModelCompareBase$plot_batch_ases()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="tswgewrapped" data-topic="ModelCompareBase" data-id="plot_batch_forecasts">}\href{../../tswgewrapped/html/ModelCompareBase.html#method-plot_batch_forecasts}{\code{tswgewrapped::ModelCompareBase$plot_batch_forecasts()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="tswgewrapped" data-topic="ModelCompareBase" data-id="plot_boxplot_ases">}\href{../../tswgewrapped/html/ModelCompareBase.html#method-plot_boxplot_ases}{\code{tswgewrapped::ModelCompareBase$plot_boxplot_ases()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="tswgewrapped" data-topic="ModelCompareBase" data-id="plot_simple_forecasts">}\href{../../tswgewrapped/html/ModelCompareBase.html#method-plot_simple_forecasts}{\code{tswgewrapped::ModelCompareBase$plot_simple_forecasts()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="tswgewrapped" data-topic="ModelCompareBase" data-id="remove_models">}\href{../../tswgewrapped/html/ModelCompareBase.html#method-remove_models}{\code{tswgewrapped::ModelCompareBase$remove_models()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="tswgewrapped" data-topic="ModelCompareBase" data-id="set_batch_size">}\href{../../tswgewrapped/html/ModelCompareBase.html#method-set_batch_size}{\code{tswgewrapped::ModelCompareBase$set_batch_size()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="tswgewrapped" data-topic="ModelCompareBase" data-id="set_verbose">}\href{../../tswgewrapped/html/ModelCompareBase.html#method-set_verbose}{\code{tswgewrapped::ModelCompareBase$set_verbose()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="tswgewrapped" data-topic="ModelCompareBase" data-id="statistical_compare">}\href{../../tswgewrapped/html/ModelCompareBase.html#method-statistical_compare}{\code{tswgewrapped::ModelCompareBase$statistical_compare()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Initialize an object to compare several Univatiate Time Series Models
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelCompareUnivariate$new(
  data = NA,
  var_interest = NA,
  mdl_list,
  n.ahead = NA,
  batch_size = NA,
  step_n.ahead = TRUE,
  verbose = 0
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{A Univariate Time Series Realization}

\item{\code{var_interest}}{If data is a dataframe with multiple columns, then what is the output variable of interest}

\item{\code{mdl_list}}{A named list of all models (see format below)}

\item{\code{n.ahead}}{The number of observations used to calculate ASE or forecast ahead}

\item{\code{batch_size}}{If any of the models used sliding ase method,
then this number indicates the batch size to use}

\item{\code{step_n.ahead}}{If using sliding window, should batches be incremented by n.ahead
(Default = TRUE)}

\item{\code{verbose}}{How much to print during the model building and other processes (Default = 0)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new `ModelCompareUnivariate` object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_data_var_interest"></a>}}
\if{latex}{\out{\hypertarget{method-get_data_var_interest}{}}}
\subsection{Method \code{get_data_var_interest()}}{
Returns the dependent variable data only
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelCompareUnivariate$get_data_var_interest()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
The dependent variable data only
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-plot_multiple_realizations"></a>}}
\if{latex}{\out{\hypertarget{method-plot_multiple_realizations}{}}}
\subsection{Method \code{plot_multiple_realizations()}}{
Creates multiple realization of each model. Useful to check model appropriateness.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelCompareUnivariate$plot_multiple_realizations(
  n.realizations = 4,
  lag.max = 25,
  seed = NA,
  plot = c("all"),
  scales = "free_y"
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{n.realizations}}{Number of realization to create (Default: 4)}

\item{\code{lag.max}}{lag.max to plot for ACF (Default: 25)}

\item{\code{seed}}{The seed to use for generating realizations}

\item{\code{plot}}{A vector of options to plot (Default = c("all"))
Other options: 'realization', 'acf', 'spectrum'}

\item{\code{scales}}{The scales argument to be passed to ggplot facet_wrap layer
(Default = 'free_y') Other appropriate options: 'fixed'}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-evaluate_residuals"></a>}}
\if{latex}{\out{\hypertarget{method-evaluate_residuals}{}}}
\subsection{Method \code{evaluate_residuals()}}{
For the models for which the residuals have been provided,
this method will check whetehr the residuals are white noise or not.
(1) Plots the residuals and the ACF values
(2) Performs the Ljung-Box test for K = 24 and K = 48
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelCompareUnivariate$evaluate_residuals(lag.max = 50)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{lag.max}}{The maximum lag to plot for the ACF}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A dataframe containing the results of the 2 Ljung-Box tests
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-summarize_build"></a>}}
\if{latex}{\out{\hypertarget{method-summarize_build}{}}}
\subsection{Method \code{summarize_build()}}{
Not applicable for the Univariate Compare, since we are passing already build models
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelCompareUnivariate$summarize_build()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelCompareUnivariate$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
