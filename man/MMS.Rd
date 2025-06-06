% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Inventory.R
\name{MMS}
\alias{MMS}
\title{MMS Class for Manure Management System}
\description{
An R6 class to perform manure management calculations based on User_input
parameters and emission factors from configuration files.
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{user_input}}{User_input object containing input parameters}

\item{\code{config_paths}}{Paths to configuration files}

\item{\code{debug_mode}}{Enable debug mode for more verbose output}

\item{\code{check}}{Enable mass check balance across all steps}

\item{\code{global_params}}{Global parameters loaded from configuration files}

\item{\code{results}}{Results of the simulation}

\item{\code{EF_by_stage}}{Emission factors organized by stage}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-MMS-new}{\code{MMS$new()}}
\item \href{#method-MMS-compile_emission_factors_from_input}{\code{MMS$compile_emission_factors_from_input()}}
\item \href{#method-MMS-run_inventory}{\code{MMS$run_inventory()}}
\item \href{#method-MMS-print_inventory_ini}{\code{MMS$print_inventory_ini()}}
\item \href{#method-MMS-print}{\code{MMS$print()}}
\item \href{#method-MMS-clone}{\code{MMS$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MMS-new"></a>}}
\if{latex}{\out{\hypertarget{method-MMS-new}{}}}
\subsection{Method \code{new()}}{
Create a new MMS object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MMS$new(user_input, debug_mode = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{user_input}}{User_input object containing input parameters}

\item{\code{debug_mode}}{Enable debug mode for more verbose output}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MMS-compile_emission_factors_from_input"></a>}}
\if{latex}{\out{\hypertarget{method-MMS-compile_emission_factors_from_input}{}}}
\subsection{Method \code{compile_emission_factors_from_input()}}{
Compile emission factors from user input
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MMS$compile_emission_factors_from_input(slurry_crust = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{slurry_crust}}{Logical indicating whether slurry has a crust (default: TRUE)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
List of emission factors organized by stage
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MMS-run_inventory"></a>}}
\if{latex}{\out{\hypertarget{method-MMS-run_inventory}{}}}
\subsection{Method \code{run_inventory()}}{
Run the inventory calculation
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MMS$run_inventory()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
List of results
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MMS-print_inventory_ini"></a>}}
\if{latex}{\out{\hypertarget{method-MMS-print_inventory_ini}{}}}
\subsection{Method \code{print_inventory_ini()}}{
Print a summary of the MMS object configuration
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MMS$print_inventory_ini()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Invisibly returns the MMS object for method chaining
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MMS-print"></a>}}
\if{latex}{\out{\hypertarget{method-MMS-print}{}}}
\subsection{Method \code{print()}}{
Print method (required for R6 objects)
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MMS$print()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Invisibly returns the MMS object for method chaining
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MMS-clone"></a>}}
\if{latex}{\out{\hypertarget{method-MMS-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MMS$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
