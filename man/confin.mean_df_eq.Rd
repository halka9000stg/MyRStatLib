\name{confin.mean_df_eq}
\alias{confin.mean_df_eq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
confin.mean_df_eq(x_len, x_mean, x_var, y_len, y_mean, y_var, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x_len}{
%%     ~~Describe \code{x_len} here~~
}
  \item{x_mean}{
%%     ~~Describe \code{x_mean} here~~
}
  \item{x_var}{
%%     ~~Describe \code{x_var} here~~
}
  \item{y_len}{
%%     ~~Describe \code{y_len} here~~
}
  \item{y_mean}{
%%     ~~Describe \code{y_mean} here~~
}
  \item{y_var}{
%%     ~~Describe \code{y_var} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x_len, x_mean, x_var, y_len, y_mean, y_var, y) 
{
    x_dof = x_len - 1
    y_dof = y_len - 1
    alpha = 1 - y
    tval = qt(alpha, x_dof + y_dof)
    mgd_var = (x_dof * x_var + y_dof * y_var)/(x_dof + y_dof)
    lower = (x_mean - y_mean) - tval * (x_dof + y_dof) * sqrt(mgd_var * 
        ((1/x_len) + (1/y_len)))
    upper = (x_mean - y_mean) + tval * (x_dof + y_dof) * sqrt(mgd_var * 
        ((1/x_len) + (1/y_len)))
    c(lower, upper)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
