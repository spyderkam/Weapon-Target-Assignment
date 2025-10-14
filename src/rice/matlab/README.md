# Rice Distribution: Mathematical Analysis of MATLAB Code

> This document analyzes MATLAB implementations of the Rice distribution written by Sam Thompson. The code primarily computes the Rice CDF using an asymptotic approximation valid for large and normalized parameters generating lookup tables as matrices with the values representing the Rice CDF evaluated at different combinations of $\nu$ (non-centrality parameter) and $b$ (evaluation point).

## 1. Mathematical Background: The Rice Distribution

The Rice distribution (also known as the Rician distribution) arises naturally in signal processing and physics when dealing with the magnitude of a bivariate normal random variable with non-zero mean.[^bnrv]

### 1.1 Mathematical Formulation

If there exists two independent Gaussian random variables $X \sim \mathcal{N}(\mu_{x}, \sigma^2)$ and $Y \sim \mathcal{N}(\mu_{y}, \sigma^2)$, then the magnitude:

$$ R = \sqrt{X^2 + Y^2} $$

follows a Rice distribution with parameters $\nu$ and $\sigma$, where:

$$ \nu = \sqrt{\mu_{x}^2 + \mu_{y}^2} $$

represents the distance of the mean from the origin (the non-centrality parameter).

### 1.2 Probability Density Function

The Rice PDF is given by:

$$ \mathcal{P}(r; \nu, \sigma) = \frac{r}{\sigma^2} \exp\left(-\frac{r^2 + \nu^2}{2\sigma^2}\right) I_0\left(\frac{r\nu}{\sigma^2}\right) $$

where $r \geq 0$ is the radial distance of the random variable, $\nu \geq 0$ is the non-centrality parameter (offset from origin), $\sigma > 0$ is the scale parameter, and $I_0$ is the modified Bessel function of the first kind of order zero.

### 1.3 Cumulative Distribution Function

The CDF of the Rice distribution is:

$$ \mathcal{C}(r; \nu, \sigma) = \int_0^r \frac{t}{\sigma^2} \exp\left(-\frac{t^2 + \nu^2}{2\sigma^2}\right) I_0\left(\frac{t\nu}{\sigma^2}\right) dt $$

This integral generally has no closed-form solution and must be evaluated numerically.

## 2. Analysis of MATLAB Code

The provided MATLAB scripts implement: *asymptotic approximation* for the Rice CDF, *exact numerical integration* for comparison, and *lookup table generation* for efficient computation.

### 2.1 Mathematical Expressions Extracted

#### 2.1.1 Normalization and Gaussian Components

The code first normalizes the parameters with respect to $\sigma$:

$$
\begin{align*}
  &b_{\mathrm{norm}} = \frac{b}{\sigma} \\
  &\nu_{\mathrm{norm}} = \frac{\nu}{\sigma} \\
  &z = b_{\mathrm{norm}} \cdot \nu_{\mathrm{norm}} = \frac{b\nu}{\sigma^2}
\end{align*}
$$

where $b$ is the fuel budget or maximum divert capability and $z$ is a dimensionless parameter used in the asymptotic approximation.

The normalized Gaussian PDF and CDF at point $b$:

$$ 
\begin{align*}
  &\mathcal{P}_{\mathrm{norm}}(b) = \frac{1}{\sqrt{2\pi}} \exp\left[-\frac{1}{2}(b_{\mathrm{norm}} - \nu_{\mathrm{norm}})^2\right] \\
  &\mathcal{C}_{\mathrm{norm}}(b) = \frac{1}{\sqrt{2\pi}} \int_{-\infty}^{b_{\mathrm{norm}}} \exp\left[-\frac{1}{2}(x - \nu_{\mathrm{norm}})^2\right] dx 
\end{align*}
$$

These functions come from the asymptotic approximation of the Rice distribution for large $\nu/\sigma$.

When $\nu \gg \sigma$ (strong signal), the Rice distribution approaches a Gaussian distribution with mean $\nu$ and variance $\sigma^2$. This is because the 2D distribution becomes highly concentrated around $(\mu_{x}, \mu_{y})$ and that the radial distance $R$ becomes approximately normally distributed around $\nu$. The asymptotic formula:[^mqf]

$$\mathcal{C}(b) \approx \mathcal{C}_{\mathrm{norm}}(b) - \frac{\mathcal{P}_{\mathrm{norm}}(b)}{2\sqrt{z}}\left(1 + \frac{1}{8z} + \mathcal{O}\!\left(\frac{1}{z^2}\right)\!\right)$$

uses the Gaussian CDF as the leading term, with a correction factor involving the Gaussian PDF. This correction accounts for the slight asymmetry of the Rice distribution compared to a true Gaussian.

#### 2.1.2 Asymptotic and Exact Calculation[^C_vs_pq]

The key asymptotic formula for the Rice CDF is:

$$ \mathcal{C}_{\mathrm{asymp}} =\, \mathcal{C}_{\mathrm{norm}}(b) - \frac{\mathcal{P}_{\mathrm{norm}}(b)}{2\sqrt{z}} \left(1 + \frac{1}{8z}\right) $$

This approximation is valid for large values of $z = {b\nu}/{\sigma^2}$. The exact Rice CDF (in normalized coordinates):

$$ \mathcal{C}_{\mathrm{exact}} = \int_0^{b_{\mathrm{norm}}} x \exp\left(-\frac{x^2 + \nu_{\mathrm{norm}}^2}{2}\right) I_0(\nu_{\mathrm{norm}} \cdot x) \,dx $$

### 2.2 Lookup Table Generation

The `genRiceTable.m` script creates a lookup table with:

-   **$\nu$ values:** ranging from $2$ to $150$ (in steps, relative to $\sigma = 10$)
-   **$b$ values:** ranging from approximately $48.68$ to $150$
-   **Output:** $150×150$ matrix of CDF values using the asymptotic approximation

Each row corresponds to a different $\nu$ value, each column to a different $b$ value. The matrix element at position $(i,j)$ contains $\mathcal{C}(b_j; \nu_i, \sigma)$.

The `rice_cdf_table.m` creates a finer table for smaller values:

-   **$\nu$ values:** $0$ to $4$ ($200$ points, relative to $\sigma = 1$)
-   **$b$ values:** $0$ to $4$ ($200$ points)
-   **Output:** $200×200$ matrix comparing asymptotic and exact values
    -   Matrix values: $\mathcal{C}_{\mathrm{exact}}$ (from the `q` output of `calc_rice_cdf_asymp`)

Note that while it calculates both `p` (asymptotic) and `q` (exact), it only saves `q` to the table. 



[^bnrv]: A bivariate normal random variable is a 2D variable where each dimension is Gaussian (in distribution); i.e., a pair of random variables $(X, Y)$ that follow a joint normal (Gaussian) distribution together. Mathematically, it is characterized by: two means ($\mu_{x}$ and $\mu_{y}$), two variances ($\sigma_{x}^2$ and $\sigma_{y}^2$), and one correlation coefficient ($\rho$). The joint PDF is: $$ \mathcal{P}(x,y) = \frac{1}{2\pi\sigma_{x}\sigma_{y}\sqrt{1-\rho^2}} \exp\left(-\frac{1}{2(1-\rho^2)}\left[\frac{(x-\mu_{x})^2}{\sigma_{x}^2} - \frac{2\rho(x-\mu_{x})(y-\mu_{y})}{\sigma_{x}\sigma_{y}} + \frac{(y-\mu_{y})^2}{\sigma_{y}^2}\right]\right)$$ In the Rice distribution context, we have the special case where $X$ and $Y$ are *independent* (so $\rho = 0$); they have equal variances ($\sigma_{x}^2 = \sigma_{y}^2 = \sigma^2$). This represents a 2D Gaussian *cloud* centered at $(\mu_{x}, \mu_{y})$. The Rice distribution then describes the distance from the origin to a random point sampled from this cloud: $R = \sqrt{X^2 + Y^2}$. 

[^mqf]: This is a well-known asymptotic expansion for the Marcum Q-function (which is closely related to the Rice CDF). The formula comes from asymptotic analysis of the Bessel function $I_0$ for large arguments.

[^C_vs_pq]: In the MATLAB code, $\mathcal{C}_{\mathrm{asymp}}$ and $\mathcal{C}_{\mathrm{exact}}$ are `p` and `q` respectively.
