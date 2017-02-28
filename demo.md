# Notes for my In-class Demo on February 27th

## Some General Advice 
- Learn to touch-type well
- Learn how to use a *real* text editor (vim, emacs, atom, etc.)
- Learn how to work *effectively* with LaTeX
- Learn version control (git/Github) and use it for *everything*
- Spend time and effort improving your writing and presentation skills
- Use open-source software (R, Julia, Python, etc.)
- Don't use Matlab! Don't use STATA! Don't use SAS!
- Practice reproducible research
- Use R/C++ and R packages!

## Resources for Learning More
- Karl Bromans' Reading List for Reproducible Research: http://kbroman.org/Tools4RR/pages/resources.html 
- R Style Guide (Hadley/Google): http://adv-r.had.co.nz/Style.html
- Matlab/R Bilingual Dictionary: http://www.math.umaine.edu/~hiebeler/comp/matlabR.pdf
- Hadley's R Packages Book: http://r-pkgs.had.co.nz
- Hadley's Advanced R Book: http://adv-r.had.co.nz
- Rcpp Gallery: http://gallery.rcpp.org
- Armadillo C++ Linear Algebra Library: http://arma.sourceforge.net/docs.html
- Norm Matloff's Parallel Programming Book: heather.cs.ucdavis.edu/~matloff/158/PLN/ParProcBook.pdf  
- Norm Matloff's Parallel R Book: https://www.amazon.com/Parallel-Computing-Data-Science-Examples/dp/1466587016/
- R Markdown (RStudio): http://rmarkdown.rstudio.com/
- Knitr: https://yihui.name/knitr/
- RcppNumerical: https://github.com/yixuan/RcppNumerical
- TikzDevice: https://cran.r-project.org/web/packages/tikzDevice/vignettes/tikzDevice.pdf
- Economical Writing (McCloskey): https://www.amazon.com/Economical-Writing-Deirdre-McCloskey/dp/1577660633
- John Cochrane's Writing Group Webpage: https://faculty.chicagobooth.edu/john.cochrane/teaching/writing/

## My Demo

### Part I - Creating a Simple R Package
1. Make sure we've installed `devtools` and `roxygen2`. On Ubuntu, you may need to add a few missing packages using `apt-get` to get `devtools` to install. I can help you with this.

2. In RStudio do: File > New Project > New Directory > R Package 

3. Give the package a name, say where to put it, create a git repository, then click "Create Project."

4. A whole bunch of files have appeared! Let's take a quick look at the key ones: Description, and the R directory. The Description contains information about your package and what other packages it depends on, while the R directory is where you put any R source files for your package.

5. There's already a function in this package, called `hello`. Let's build the package and test out the function: Build > Clean and Rebuild

6. Look at your packages list: you can see `testpkg` there! Try the command `hello()` at the R console.

7. Let's get rid of `hello` and make a more interesting function: say a simple simulation dgp. We'll put it in a new file called `dgp.R`

        dgp <- function(sample_size, intercept, slope, error_sd){
            x <- runif(sample_size)
            epsilon <- rnorm(sample_size, sd = error_sd)
            y <- intercept + slope * x + epsilon
            return(y)
        }

8. Now we'll re-build the pacakge and see that dgp is available.

9. You're going to forget what dgp does, so let's document it.  Do: Code > Insert Roxygen Skeleton. Fill out the fields. Then we're going to delete `NAMESPACE` since we'll have roxygen handle this for us. Finally, do `devtools::document()`. If everything worked, when you type `?dgp` at the console, you'll see your helpfile!

10. Talk about the difference between functions that exported and those that aren't. For your own private use, it's reasonable just to export everything. But if you're sharing a package with users, some functions are purely "internal" and you don't need to export them.

11. Let's create another function that calls dgp. The nice thing about a package is that after loading it, all functions in the package are available: you don't have to manually source them. We can either put this new functions in their own file or in the same as dgp. I'll put them in a new file: `estimator.R` You should always document everything, but in the interest of time, I'll leave the fields blank.

        linear_regression <- function(regression_data){
            x <- regression_data$x
            y <- regression_data$y
            slope_est <- cov(x, y) / var(x)
            intercept_est <- mean(y) - slope_est * mean(x)
            out <- c(slope = slope_est, intercept = intercept_est)
        }

        simulation_replication <- function(sample_size, intercept, slope, error_sd){
            regression_data <- dgp(sample_size, intercept, slope, error_sd)
            return(linear_regression(regression_data))
        }
      
        simulation_study <- function(n_reps, sample_size, intercept, slope, error_sd){
            out <- replicate(n_reps, simulation_replication(sample_size, intercept, slope, error_sd))
            return(t(out))
        }

12. What about adding *data* to our package? Every paper needs an empirical example, right? You can easily store a dataset inside your package so it's easily accessible to you as you work. If your data needs to be cleaned or pre-processed, you can document all of these steps and include them in the package. Do the following: `devtools::use_data_raw()`. Now there's a new directory for your raw data and any files used to process it. These will *not* be run every time that you build your package, which is a good thing!

13. Let's get some data and put it in our package. Create a new R script in the `data-raw` directory called `download_and_clean_data.R` or something like that. Then type the following:

        data_url <- "http://ditraglia.com/econ103/survey_clean.csv"
        survey <- read.csv(data_url, header = TRUE, row.names = NULL,
                           stringsAsFactors = FALSE)
        height_handspan <- survey[,c('height', 'handspan')]
        devtools::use_data(height_handspan)
        
Then save, source the script, and re-build the package. Now the height-handspan regression data is available in your package. Notice that there's now a `data` directory and if you type `height_handspan` at the console after loading your package, you'll see the data. 

14. We should document our data. Here's how: create a file called `data.R` in the R directory containing something like this:

        #' Height and hanspan for Econ 103 Students
        #'
        #' A dataset containing the height and handspan of Econ 103 Students
        #'
        #' @format A data frame with 69 rows and 2 variables:
        #' \describe{
        #'   \item{height}{height, in inches}
        #'   \item{handspan}{handspan, in centimeters}
        #' }
        #' @source \url{http://ditraglia.com/econ103/survey_clean.csv/}
        "height_handspan"

Then do `devtools::document()` and then clean and re-build the package. You can also set your build options so RStudio uses `roxygen` automatically. Click `More` in the `Build` menu.

15. Now let's share our package on github. Click on the `git` tab and then commit the changes. Ordinarily we'd make many commits, each of which is a small change but this is just a simple example. Now we need to get this on github. You'll need a github account. Sign up: it's free. Click `Create New Repository` and then give it the same name as our local package: `test_pkg`. Don't initialize with a README. Then follow the instructions under `push an existing repository from the command line.` You can access the git command line in R under Git > More > Shell.

16. Now let's check to see that the package is on github. Since this is a public repository, you can easily install it on your own computer by using the following command `devtools::install_github("fditraglia/testpkg")`

## Part II Building Packages with Rcpp/RcppArmadillo

## Part III A Very Brief Overview of Parallel Computing
