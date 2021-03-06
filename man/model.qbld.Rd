% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model.R
\name{model.qbld}
\alias{model.qbld}
\title{QBLD Sampler}
\usage{
model.qbld(fixed_formula, data, id = "id", random_formula = ~1, p = 0.25, 
                  b0 = 0, B0 = 1, c1 = 9, d1 = 10, method = c("block","unblock"), 
                  nsim, burn = 0, summarize = FALSE, verbose = FALSE)
}
\arguments{
\item{fixed_formula}{: a description of the model to be fitted of the form 
response~fixed effects predictors i.e \eqn{Xi} in the model. See vignette for more information.}

\item{data}{: data frame, NAs not allowed and should throw errors, factor variables are auto-converted, 
find airpollution.rda and locust.rda built into the package.}

\item{id}{: variable name in the dataset that specifies individual profile. By default, \code{id="id"} and
data is expected to contain an id variable. This is omitted while modelling.}

\item{random_formula}{: a description of the model to be fitted of the form 
response~random effects predictors i.e \eqn{Si} in the model. This defaults to \eqn{Si} being only an intercept.
See vignette for more information.}

\item{p}{: quantile for the AL distribution on the error term, \eqn{p=0.25} by default. For very low \eqn{(<=0.025)} or
very high \eqn{(>=0.970)} values of p, sampler forces to unblock version to avoid errors.}

\item{b0, B0}{: Prior model parameters for Beta. These are defaulted to 0 vector, and Identity matrix.}

\item{c1, d1}{: Prior model parameters for Varphi2. These are defaulted to 9,10 (arbitrary) respectively.}

\item{method}{: Choose between the "Block" vs "Unblock" sampler, Block is slower but produces lower correlation.}

\item{nsim}{: number of simultions to run the sampler.}

\item{burn}{: Burn in percentage, number between (0,1). Burn-in values are discarded and not used for summary calculations.}

\item{summarize}{: Outputs a summary table (same as \code{summary(output)}), in addition also prints Model fit
AIC/BIC/Log-likelihood values. False by default.}

\item{verbose}{: False by default. Spits out progress reports while the sampler is running.}
}
\value{
Returns `qbld' class object. `qbld' class contains the following :
\itemize{
\item {\code{Beta:}} { Matrix of MCMC samples of fixed-effects parameters.}
\item {\code{Alpha:}} { 3-dimensional matrix of MCMC samples of random-effects parameters.}
\item {\code{Varphi2:}} { Matrix of MCMC samples for varphi2.}
\item {\code{nsim:}} { Attribute; No. of simulations of chain run.}
\item {\code{burn:}} { Attribute; Whether or not burn-in used.}
\item {\code{which:}} {Attribute; "block" or "unblock" sampler used}
}
}
\description{
Runs the QBLD sampler as in Rahman and Vossmeyer(2019) and outputs a `qbld' class object which 
 consists of Markov chains for Beta(the fixed effects estimate), Alpha(the random effects estimate),
 and Varphi2 (as per the model), of which Beta and Varphi2 are of interest.
}
\details{
For a detailed information on the sampler, please check the vignette.
Data are contained in a data.frame. Each element of the data argument must be identifiable by
a name. The simplest situation occurs when all subjects are observed at the same time points. The
id variable represent the individual profiles of each subject, it is expected a variable in the
data.frame that identifies the correspondence of each component of the response variable to the
subject that it belongs, by default is named id variable. Hence NA values are not valid.
For very low \eqn{(<=0.025)} or very high \eqn{(>=0.970)} values of \eqn{p}, sampler forces to unblock version to avoid errors.
Block version in this case may lead to machine tolerance issues.

`qbld' object contains markov chains and sampler run information as attributes , and is compatible 
with S3 methods like summary,plot. make.qbld function can be used to convert a similar
type-object to `qbld' class.
}
\examples{

data(airpollution)

output <- model.qbld(fixed_formula = wheeze~smoking+I(age^2), data = airpollution, id="id", 
                     random_formula = ~1, p=0.25, nsim=1000, method="block", burn=0, 
                     summarize=TRUE, verbose=FALSE)
           
plot(output)



}
\references{
Rahman, Mohammad Arshad and Angela Vossmeyer, “Estimation and Applications of 
Quantile Regression for Binary Longitudinal Data,” 
Advances in Econometrics, 40B, 157-191, 2019.
}
\seealso{
A qbld object may be summarized by the summary function and visualized with the plot function.  

\code{\link{summary.qbld}}, \code{\link{plot.qbld}}  

Datasets : \code{\link{airpollution}}, \code{\link{locust}}
}
