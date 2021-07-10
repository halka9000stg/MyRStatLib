\name{confin.eq_pop_var}
\alias{confin.eq_pop_var}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
confin.eq_pop_var(x_vec, y_vec, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x_vec}{
%%     ~~Describe \code{x_vec} here~~
}
  \item{y_vec}{
%%     ~~Describe \code{y_vec} here~~
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
function (x_vec, y_vec, y) 
{
    x_len = length(x_vec)
    y_len = length(y_vec)
    x_mean = mean(x_vec)
    y_mean = mean(y_vec)
    x_var = var(x_vec)
    y_var = var(y_vec)
    x_dof = x_len - 1
    y_dof = y_len - 1
    alpha = 1 - y
    al_hf = alpha/2
    fval_hf = qf(al_hf, x_dof, y_dof)
    fval_hf_m = 1/fval_hf
    lower = fval_hf_m * (x_var/y_var)
    upper = fval_hf * (x_var/y_var)
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
