# Models

A progression model for repeated measures (PMRM) is a longitudinal
continuous-time nonlinear model of a progressive disease. The `pmrm`
package implements PMRMs from Raket (2022). This vignette defines the
models in the package.

## Common elements

This section defines the notation and assumptions common to all models
in the package.

### Data

Let the scalar $y_{ij}$ be the continuous measure of disease severity of
patient $i$ ($i = 1,\ldots,I$) at clinical visit $j$ ($j = 1,\ldots,J$).
For a progressive disease, we generally expect $y_{ij}$ to worsen from
visit to visit. The goal of treatment is usually to minimize this
worsening over time.

Some $y_{ij}$ values may be missing due to dropout or other intercurrent
events. We assume these outcomes are missing at random (MAR). Except for
the outcomes $y_{ij}$, all values in the data must be non-missing.

$t_{ij}$ is continuous time since the baseline visit of $j = 1$. At
baseline, we assume treatment has not been administered yet, so all
study arms should have the same expected outcome if randomization is
conducted properly.[¹](#fn1)

The model may include optional covariates such as age and biomarker
status.

### Likelihood

Let $y_{i} = \left( y_{i1},\ldots,y_{iJ} \right)$ be the vector of
outcomes of patient $i$. For each pair of different patients $i$ and
$i^{*}$, we assume $y_{i}$ is independent of $y_{i^{*}}$ and
$\text{Var}\left( y_{i} \right) = \text{Var}\left( y_{i^{*}} \right)$ .
For each $i$, define
$\mu_{i} = E\left( y_{i} \right) = \left( \mu_{i1},\ldots,\mu_{iJ} \right)$
and $\Sigma = \text{Var}\left( y_{i} \right)$.

If there were no missing outcomes, we would assign a multivariate normal
likelihood to each patient:

$$\begin{array}{r}
{y_{i} \sim \text{MVN}\left( \mu_{i},\Sigma \right)}
\end{array}$$

But to account for intercurrent events such as dropout, we instead
assign patient-level marginal likelihoods with missing outcomes
integrated out.

Let $Q_{i}$ be the $q \times I$ matrix such that $Q_{i}y_{i}$ is the
chronologically ordered vector of all $q$ non-missing values of $y_{i}$.
Then the marginal likelihood for patient $i$ is:

$$\begin{array}{r}
{Q_{i}y_{i} \sim \text{MVN}\left( Q_{i}\mu_{i},Q_{i}\Sigma Q_{i}^{T} \right)}
\end{array}$$

The model is fit by maximizing the product of these independent marginal
likelihoods over the parameters that define $\mu_{i}$ ($i = 1,\ldots,I$)
and $\Sigma$. These parameters are $\alpha$, $\theta$, $\gamma$, $\phi$,
and $\rho$, and they are all defined in subsequent sections of this
vignette.

### Variance

Recall that $\Sigma$ is defined as $\text{Var}\left( y_{i} \right)$
($i = 1,\ldots,I$). We parameterize $\Sigma$ as follows:

$$\begin{aligned}
\Sigma & {= D\Lambda D}
\end{aligned}$$

$D$ is a $J \times J$ diagonal matrix with diagonal
$\sigma = \left( \sigma_{1},\ldots,\sigma_{J} \right)$ (the
visit-specific standard deviation parameters). To constrain
$\sigma_{j} \geq 0$, we define a latent parameter vector
$\phi = \left( \phi_{1},\ldots,\phi_{J} \right)$ such that
$\phi_{j} = \log\left( \sigma_{j} \right)$ for $j = 1,\ldots,J$. The
model estimates $\phi$ with maximum likelihood.

$\Lambda$ is the $J \times J$ correlation matrix among visits within a
patient. Define:

$$\begin{aligned}
\Lambda & {= LL^{T}}
\end{aligned}$$

$L$ is a lower triangular Cholesky factor of the correlation matrix
$\Lambda$, and it is expressed in terms of a vector
$\rho = \left( \rho_{1},\ldots,\rho_{J{(J - 1)}/2} \right)$ of
$\frac{J(J - 1)}{2}$ latent parameters.[²](#fn2) The model estimates
$\rho$ with maximum likelihood.

### Expected value of the control group

Recall that $\mu_{ij}$ is the expected mean outcome of patient $i$ at
visit $j$. If patient $i$ is part of the control group, then we define:

$$\begin{array}{r}
{\mu_{ij} = f\left( t_{ij}|\xi,\alpha \right) + W_{i}\gamma - \langle{\overline{W}}_{i},\gamma\rangle}
\end{array}$$

Each model expresses the expected outcomes differently, but all models
reduce to the above equation for the control arm.

Above, $W_{i}$ is the $J \times V$ sparse model matrix of constant
non-missing covariates from the data, ${\overline{W}}_{i}$ is $V$-length
vector of the column means of $W_{i}$, and
$\langle{\overline{W}}_{i},\gamma\rangle$ is the inner product of
${\overline{W}}_{i}$ with model coefficient parameter vector
$\gamma$.[³](#fn3) The model estimates $\gamma$ with maximum likelihood.

The mean function $f\left( t_{ij}|\xi,\alpha \right)$ is the expected
clinical outcome at time $t_{ij}$ of the control arm prior to covariate
adjustment. It is a spline with knots
$\xi = \left( \xi_{1},\ldots,\xi_{S} \right)$ and vertical anchor points
$\alpha = \left( \alpha_{1},\ldots,\alpha_{S} \right)$.[⁴](#fn4) The
knots in $\xi$ are fixed and supplied by the user, and they are usually
the scheduled visit times specified in the study protocol.[⁵](#fn5) The
model estimates $\alpha$ with maximum likelihood.

## The decline models

In progressive disease, we usually expect patient health to decline
after baseline. The models in this section measure how well treatment
reduces this decline. Therapeutic benefit is expressed as a pointwise
reduction on the clinical outcome scale.

### The non-proportional decline model

We express the expected value $\mu_{ij}$ as follows:

$$\begin{aligned}
\mu_{ij} & {= \left( 1 - \beta_{b{(i)}j} \right)\left( f\left( t_{ij}|\xi,\alpha \right) - f\left( 0|\xi,\alpha \right) \right) + f\left( 0|\xi,\alpha \right) + W_{i}\gamma - \langle{\overline{W}}_{i},\gamma\rangle}
\end{aligned}$$

where $b(i)$ is the study arm of patient $i$, and $\beta_{kj}$ is the
reduction in decline of study arm $k$ ($k = 1,\ldots,K$) relative to the
control arm $k = 1$.

To appropriately constrain the parameter space, we express the
$\beta_{kj}$ parameters in terms of latent $\theta_{{(k - 1)}{(j - 1)}}$
parameters as follows:

$$\begin{array}{r}
{\beta_{kj} = \begin{cases}
0 & {k = 1{\mspace{6mu}\text{or}\mspace{6mu}}j = 1} \\
\theta_{{(k - 1)}{(j - 1)}} & {k \in \{ 2,\ldots,K\}{\mspace{6mu}\text{and}\mspace{6mu}}j \in \{ 2,\ldots,J\}}
\end{cases}}
\end{array}$$

The model estimates the latent parameters
$\theta_{11},\ldots,\theta_{{(K - 1)}{(J - 1)}}$ with maximum
likelihood.

### The proportional decline model

The proportional decline model is the same as the non-proportional
variant except the treatment effects are now
$\beta_{1},\ldots,\beta_{K}$, which no longer depend on the visit. In
other words, we assume that the reduction in decline due to treatment is
proportional to time. We express the expectation $\mu_{ij}$ as:

$$\begin{aligned}
\mu_{ij} & {= \left( 1 - \beta_{b{(i)}} \right)\left( f\left( t_{ij}|\xi,\alpha \right) - f\left( 0|\xi,\alpha \right) \right) + f\left( 0|\xi,\alpha \right) + W_{i}\gamma - \langle{\overline{W}}_{i},\gamma\rangle}
\end{aligned}$$

To appropriately constrain the parameter space, we introduce latent
parameters $\theta_{1},\ldots,\theta_{k - 1}$ as follows:

$$\begin{array}{r}
{\beta_{k} = \begin{cases}
0 & {k = 1} \\
\theta_{k - 1} & {k = 2,\ldots,K}
\end{cases}}
\end{array}$$

The latent parameters $\theta_{1},\ldots,\theta_{K - 1}$ are estimated
with maximum likelihood.

## The slowing models

In these models, we assume that treatment slows the passage of time
along the disease progression trajectory. All study arms share a common
disease trajectory, and treated patients may progress more slowly on
that same path than control patients. The treatment effect is on the
time scale, not the clinical outcome scale.

### The non-proportional slowing model

The expected value is:

$$\begin{aligned}
\mu_{ij} & {= f\left( u_{ij}|\xi,\alpha \right) + W_{i}\gamma - \langle{\overline{W}}_{i},\gamma\rangle}
\end{aligned}$$

where $u_{ij}$ is shifted time along the disease progression trajectory:

$$\begin{array}{r}
{u_{ij} = \left( 1 - \beta_{b{(i)}j} \right)t_{ij}}
\end{array}$$

and $\beta_{kj}$ is the relative time shift due treatment $k$ at visit
$j$.

To appropriately constrain the parameter space, we express the
$\beta_{kj}$ parameters in terms of latent $\theta_{{(k - 1)}{(j - 1)}}$
parameters as follows:

$$\begin{array}{r}
{\beta_{kj} = \begin{cases}
0 & {k = 1{\mspace{6mu}\text{or}\mspace{6mu}}j = 1} \\
\theta_{{(k - 1)}{(j - 1)}} & {k \in \{ 2,\ldots,K\}{\mspace{6mu}\text{and}\mspace{6mu}}j \in \{ 2,\ldots,J\}}
\end{cases}}
\end{array}$$

The model estimates the latent parameters
$\theta_{11},\ldots,\theta_{{(K - 1)}{(J - 1)}}$ with maximum
likelihood.

### The proportional slowing model

The proportional slowing model is the same as the non-proportional
variant except the time shifts are now $\beta_{1},\ldots,\beta_{K}$,
which no longer depend on the visit. In other words, we assume that the
slowing of disease progression due to to treatment is proportional to
time. We express the expectation $\mu_{ij}$ the same as before:

$$\begin{aligned}
\mu_{ij} & {= f\left( u_{ij}|\xi,\alpha \right) + W_{i}\gamma - \langle{\overline{W}}_{i},\gamma\rangle}
\end{aligned}$$

but the time shift uses $\beta_{b{(i)}}$:

$$\begin{array}{r}
{u_{ij} = \left( 1 - \beta_{b{(i)}} \right)t_{ij}}
\end{array}$$

To appropriately constrain the parameter space, we introduce latent
parameters $\theta_{1},\ldots,\theta_{k - 1}$ as follows:

$$\begin{array}{r}
{\beta_{k} = \begin{cases}
0 & {k = 1} \\
\theta_{k - 1} & {k = 2,\ldots,K}
\end{cases}}
\end{array}$$

The latent parameters $\theta_{1},\ldots,\theta_{K - 1}$ are estimated
with maximum likelihood.

## References

Donohue, Michael C, Oliver Langford, Philip S. Insel, Christopher H. van
Dyck, Ronald C Petersen, Suzanne Craft, Gopalan Sethuraman, Rema Raman,
and Paul S. Aisen. 2023. “Natural Cubic Splines for the Analysis of
Alzheimer’s Clinical Trials.” *Pharmaceutical Statistics* 22 (3):
508–19. <https://doi.org/10.1002/pst.2285>.

Raket, Lars Lau. 2022. “Progression Models for Repeated Measures:
Estimating Novel Treatment Effects in Progressive Diseases.” *Statistics
in Medicine* 41 (28): 5537–57. <https://doi.org/10.1002/sim.9581>.

Wang, Guoqiao, Lei Liu, Yan Li, Andrew J Aschenbrenner, Paul Delmer, Lon
S Schneidder, Richard E Kennedy, Gary R Cutter, and Chengjie Xiong.
2022. “Proportional Constrained Longitudinal Data Analysis Models for
Clinical Trials in Sporadic Alzheimer’s Disease.” *Alzheimer’s and
Dementia* 8 (1). <https://doi.org/10.1002/trc2.12286>.

------------------------------------------------------------------------

1.  This is constrained longitudinal data analysis (cLDA) as explained
    by Wang et al. (2022).

2.  In R, `RTMB::unstructured(J)$corr(rho)` maps latent parameter vector
    $\rho$ to the $J \times J$ correlation matrix $LL^{T}$.

3.  We express the covariate adjustment terms as
    $W_{i}\gamma - \langle{\overline{W}}_{i},\gamma\rangle$ to improve
    computational efficiency when dealing with sparse matrices.

4.  In other words, $f\left( \xi_{s}|\xi,\alpha \right) = \alpha_{s}$
    for $s = 1,\ldots,S$.

5.  However, both the discrete visit designations in the data and the
    spline knots in the model may need adjustment if many visits
    occurred off-schedule, as was the case for neurodegeneration studies
    during the COVID-19 pandemic. (See Donohue et al. (2023)).
