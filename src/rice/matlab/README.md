rdmamc
rdmamc
# Rice Distribution: Mathematical Analysis of MATLAB Code

> This document analyzes MATLAB implementations of the Rice distribution written by Sam Thompson. The code primarily computes the Rice CDF using an asymptotic approximation valid for large and normalized parameters generating lookup tables as matrices with the values representing the Rice CDF evaluated at different combinations of $\nu$ (non-centrality parameter) and $b$ (evaluation point).

## 1. Mathematical Background: The Rice Distribution

The Rice distribution (also known as the Rician distribution) arises naturally in signal processing and physics when dealing with the magnitude of a bivariate normal random variable with non-zero mean.[^bnrv]

### 1.1 Mathematical Formulation

If there exists two independent Gaussian random variables $x \sim \mathcal{N}(\mu_{x}, \sigma^2)$ and $y \sim \mathcal{N}(\mu_{y}, \sigma^2)$, then the magnitude:

$$ r = \sqrt{x^2 + y^2} $$

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

where $t$ is a dummy variable. This integral generally has no closed-form solution and must be evaluated numerically.

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
  &\mathcal{C}_{\mathrm{norm}}(b) = \frac{1}{\sqrt{2\pi}} \int_{-\infty}^{b_{\mathrm{norm}}} \exp\left[-\frac{1}{2}(t - \nu_{\mathrm{norm}})^2\right] dt
\end{align*}
$$

These functions come from the asymptotic approximation of the Rice distribution for large $\nu/\sigma$.

When $\nu \gg \sigma$ (strong signal), the Rice distribution approaches a Gaussian distribution with mean $\nu$ and variance $\sigma^2$. This is because the 2D distribution becomes highly concentrated around $(\mu_{x}, \mu_{y})$ and that the radial distance $r$ becomes approximately normally distributed around $\nu$. The asymptotic formula:[^mqf]

$$\mathcal{C}(b) \approx \mathcal{C}_{\mathrm{norm}}(b) - \frac{\mathcal{P}_{\mathrm{norm}}(b)}{2\sqrt{z}}\left(1 + \frac{1}{8z} + \mathcal{O}\!\left(\frac{1}{z^2}\right)\!\right)$$

uses the Gaussian CDF as the leading term, with a correction factor involving the Gaussian PDF. This correction accounts for the slight asymmetry of the Rice distribution compared to a true Gaussian.

#### 2.1.2 Asymptotic and Exact Calculation[^C_vs_pq]

To re-write the key asymptotic formula for the Rice CDF:

$$ \mathcal{C}_{\mathrm{asymp}} =\, \mathcal{C}_{\mathrm{norm}}(b) - \frac{\mathcal{P}_{\mathrm{norm}}(b)}{2\sqrt{z}} \left(1 + \frac{1}{8z}\right) $$

This approximation is valid for large values of $z = {b\nu}/{\sigma^2}$. The exact Rice CDF (in normalized coordinates):

$$ \mathcal{C}_{\mathrm{exact}} = \int_0^{b_{\mathrm{norm}}} t \exp\left(-\frac{t^2 + \nu_{\mathrm{norm}}^2}{2}\right) I_0(\nu_{\mathrm{norm}} \cdot t) \,dt $$

### 2.2 Lookup Table Generation

The `genRiceTable.m` script creates a lookup table with:

-   **$\nu$ values:** ranging from $2$ to $150$ (in steps, relative to $\sigma = 10$)
-   **$b$ values:** ranging from approximately $48.68$ to $150$
-   **Output:** $150×150$ matrix of CDF values using the asymptotic approximation

Each row corresponds to a different $\nu$ value, each column to a different $b$ value. The matrix element at position $(i,j)$ contains $\mathcal{C}(b_j; \nu_i, \sigma)$.

The `rice_cdf_table.m` script creates a finer table for smaller values:[^finer]

-   **$\nu$ values:** $0$ to $4$ ($200$ points, relative to $\sigma = 1$)
-   **$b$ values:** $0$ to $4$ ($200$ points)
-   **Output:** $200×200$ matrix comparing asymptotic and exact values
    -   Matrix values: $\mathcal{C}_{\mathrm{exact}}$ (from the `q` output of `calc_rice_cdf_asymp`)

Note that while it calculates both $\mathcal{C}_{\mathrm{asymp}}$ and $\mathcal{C}_{\mathrm{exact}}$, it only saves $\mathcal{C}_{\mathrm{exact}}$ to the table. 



[^bnrv]: A bivariate normal random variable is a 2D variable where each dimension is Gaussian (in distribution); i.e., a pair of random variables $(X, Y)$ that follow a joint normal (Gaussian) distribution together. Mathematically, it is characterized by: two means ($\mu_{x}$ and $\mu_{y}$), two variances ($\sigma_{x}^2$ and $\sigma_{y}^2$), and one correlation coefficient ($\rho$). The joint PDF is: $$ \mathcal{P}(x,y) = \frac{1}{2\pi\sigma_{x}\sigma_{y}\sqrt{1-\rho^2}} \exp\left(-\frac{1}{2(1-\rho^2)}\left[\frac{(x-\mu_{x})^2}{\sigma_{x}^2} - \frac{2\rho(x-\mu_{x})(y-\mu_{y})}{\sigma_{x}\sigma_{y}} + \frac{(y-\mu_{y})^2}{\sigma_{y}^2}\right]\right)$$ In the Rice distribution context, we have the special case where $X$ and $Y$ are *independent* (so $\rho = 0$); they have equal variances ($\sigma_{x}^2 = \sigma_{y}^2 = \sigma^2$). This represents a 2D Gaussian *cloud* centered at $(\mu_{x}, \mu_{y})$. The Rice distribution then describes the distance from the origin to a random point sampled from this cloud: $r = \sqrt{x^2 + y^2}$. 

[^mqf]: This is a well-known asymptotic expansion for the Marcum Q-function (which is closely related to the Rice CDF). The formula comes from asymptotic analysis of the Bessel function $I_0$ for large arguments.

[^C_vs_pq]: In the MATLAB code, $\mathcal{C}_{\mathrm{asymp}}$ and $\mathcal{C}_{\mathrm{exact}}$ are `p` and `q` respectively.

[^finer]: "Finer" here means higher resolution; i.e., more densely sampled. The $200×200$ table covers a smaller range $[0,4]$ with $200$ points, giving a spacing of $0.02$ between points. The $150×150$ table covers a much larger range $[2,150]$ with only $150$ points, giving a spacing of about $0.99$. So the small-value table has $~50×$ finer resolution.​​​​​​​​​​​​​​​​

Rice Distribution: Mathematical Analysis of MATLAB Code
This document analyzes MATLAB implementations of the Rice distribution written by Sam Thompson. The code primarily computes the Rice CDF using an asymptotic approximation valid for large and normalized parameters generating lookup tables as matrices with the values representing the Rice CDF evaluated at different combinations of 
ν
ν (non-centrality parameter) and 
b
b (evaluation point).

1. Mathematical Background: The Rice Distribution
The Rice distribution (also known as the Rician distribution) arises naturally in signal processing and physics when dealing with the magnitude of a bivariate normal random variable with non-zero mean.1

1.1 Mathematical Formulation
If there exists two independent Gaussian random variables 
x
∼
N
(
μ
x
,
σ
2
)
x∼N(μ 
x
​
 ,σ 
2
 ) and 
y
∼
N
(
μ
y
,
σ
2
)
y∼N(μ 
y
​
 ,σ 
2
 ), then the magnitude:

r
=
x
2
+
y
2
r= 
x 
2
 +y 
2
 
​
 

follows a Rice distribution with parameters 
ν
ν and 
σ
σ, where:

ν
=
μ
x
2
+
μ
y
2
ν= 
μ 
x
2
​
 +μ 
y
2
​
 
​
 

represents the distance of the mean from the origin (the non-centrality parameter).

1.2 Probability Density Function
The Rice PDF is given by:

P
(
r
;
ν
,
σ
)
=
r
σ
2
exp
⁡
(
−
r
2
+
ν
2
2
σ
2
)
I
0
(
r
ν
σ
2
)
P(r;ν,σ)= 
σ 
2
 
r
​
 exp(− 
2σ 
2
 
r 
2
 +ν 
2
 
​
 )I 
0
​
 ( 
σ 
2
 
rν
​
 )

where 
r
≥
0
r≥0 is the radial distance of the random variable, 
ν
≥
0
ν≥0 is the non-centrality parameter (offset from origin), 
σ
>
0
σ>0 is the scale parameter, and 
I
0
I 
0
​
  is the modified Bessel function of the first kind of order zero.

1.3 Cumulative Distribution Function
The CDF of the Rice distribution is:

C
(
r
;
ν
,
σ
)
=
∫
0
r
t
σ
2
exp
⁡
(
−
t
2
+
ν
2
2
σ
2
)
I
0
(
t
ν
σ
2
)
d
t
C(r;ν,σ)=∫ 
0
r
​
  
σ 
2
 
t
​
 exp(− 
2σ 
2
 
t 
2
 +ν 
2
 
​
 )I 
0
​
 ( 
σ 
2
 
tν
​
 )dt

where 
t
t is a dummy variable. This integral generally has no closed-form solution and must be evaluated numerically.

2. Analysis of MATLAB Code
The provided MATLAB scripts implement: asymptotic approximation for the Rice CDF, exact numerical integration for comparison, and lookup table generation for efficient computation.

2.1 Mathematical Expressions Extracted
2.1.1 Normalization and Gaussian Components
The code first normalizes the parameters with respect to 
σ
σ:

b
n
o
r
m
=
b
σ
ν
n
o
r
m
=
ν
σ
z
=
b
n
o
r
m
⋅
ν
n
o
r
m
=
b
ν
σ
2
​
  
b 
norm
​
 = 
σ
b
​
 
ν 
norm
​
 = 
σ
ν
​
 
z=b 
norm
​
 ⋅ν 
norm
​
 = 
σ 
2
 
bν
​
 
​
 

where 
b
b is the fuel budget or maximum divert capability and 
z
z is a dimensionless parameter used in the asymptotic approximation.

The normalized Gaussian PDF and CDF at point 
b
b:

P
n
o
r
m
(
b
)
=
1
2
π
exp
⁡
[
−
1
2
(
b
n
o
r
m
−
ν
n
o
r
m
)
2
]
C
n
o
r
m
(
b
)
=
1
2
π
∫
−
∞
b
n
o
r
m
exp
⁡
[
−
1
2
(
t
−
ν
n
o
r
m
)
2
]
d
t
​
  
P 
norm
​
 (b)= 
2π
​
 
1
​
 exp[− 
2
1
​
 (b 
norm
​
 −ν 
norm
​
 ) 
2
 ]
C 
norm
​
 (b)= 
2π
​
 
1
​
 ∫ 
−∞
b 
norm
​
 
​
 exp[− 
2
1
​
 (t−ν 
norm
​
 ) 
2
 ]dt
​
 

These functions come from the asymptotic approximation of the Rice distribution for large 
ν
/
σ
ν/σ.

When 
ν
≫
σ
ν≫σ (strong signal), the Rice distribution approaches a Gaussian distribution with mean 
ν
ν and variance 
σ
2
σ 
2
 . This is because the 2D distribution becomes highly concentrated around 
(
μ
x
,
μ
y
)
(μ 
x
​
 ,μ 
y
​
 ) and that the radial distance 
r
r becomes approximately normally distributed around 
ν
ν. The asymptotic formula:2

C
(
b
)
≈
C
n
o
r
m
(
b
)
−
P
n
o
r
m
(
b
)
2
z
(
1
+
1
8
z
+
O
(
1
z
2
)
)
C(b)≈C 
norm
​
 (b)− 
2 
z
​
 
P 
norm
​
 (b)
​
 (1+ 
8z
1
​
 +O( 
z 
2
 
1
​
 ))

uses the Gaussian CDF as the leading term, with a correction factor involving the Gaussian PDF. This correction accounts for the slight asymmetry of the Rice distribution compared to a true Gaussian.

2.1.2 Asymptotic and Exact Calculation3
To re-write the key asymptotic formula for the Rice CDF:

C
a
s
y
m
p
=
C
n
o
r
m
(
b
)
−
P
n
o
r
m
(
b
)
2
z
(
1
+
1
8
z
)
C 
asymp
​
 =C 
norm
​
 (b)− 
2 
z
​
 
P 
norm
​
 (b)
​
 (1+ 
8z
1
​
 )

This approximation is valid for large values of 
z
=
b
ν
/
σ
2
z=bν/σ 
2
 . The exact Rice CDF (in normalized coordinates):

C
e
x
a
c
t
=
∫
0
b
n
o
r
m
t
exp
⁡
(
−
t
2
+
ν
n
o
r
m
2
2
)
I
0
(
ν
n
o
r
m
⋅
t
)
d
t
C 
exact
​
 =∫ 
0
b 
norm
​
 
​
 texp(− 
2
t 
2
 +ν 
norm
2
​
 
​
 )I 
0
​
 (ν 
norm
​
 ⋅t)dt

2.2 Lookup Table Generation
The genRiceTable.m script creates a lookup table with:

ν
ν values: ranging from 
2
2 to 
150
150 (in steps, relative to 
σ
=
10
σ=10)
b
b values: ranging from approximately 
48.68
48.68 to 
150
150
Output: 
150
×
150
150×150 matrix of CDF values using the asymptotic approximation
Each row corresponds to a different 
ν
ν value, each column to a different 
b
b value. The matrix element at position 
(
i
,
j
)
(i,j) contains 
C
(
b
j
;
ν
i
,
σ
)
C(b 
j
​
 ;ν 
i
​
 ,σ).

The rice_cdf_table.m script creates a finer table for smaller values:4

ν
ν values: 
0
0 to 
4
4 (
200
200 points, relative to 
σ
=
1
σ=1)
b
b values: 
0
0 to 
4
4 (
200
200 points)
Output: 
200
×
200
200×200 matrix comparing asymptotic and exact values
Matrix values: 
C
e
x
a
c
t
C 
exact
​
  (from the q output of calc_rice_cdf_asymp)
Note that while it calculates both 
C
a
s
y
m
p
C 
asymp
​
  and 
C
e
x
a
c
t
C 
exact
​
 , it only saves 
C
e
x
a
c
t
C 
exact
​
  to the table.

A bivariate normal random variable is a 2D variable where each dimension is Gaussian (in distribution); i.e., a pair of random variables 
(
X
,
Y
)
(X,Y) that follow a joint normal (Gaussian) distribution together. Mathematically, it is characterized by: two means (
μ
x
μ 
x
​
  and 
μ
y
μ 
y
​
 ), two variances (
σ
x
2
σ 
x
2
​
  and 
σ
y
2
σ 
y
2
​
 ), and one correlation coefficient (
ρ
ρ). The joint PDF is:
P
(
x
,
y
)
=
1
2
π
σ
x
σ
y
1
−
ρ
2
exp
⁡
(
−
1
2
(
1
−
ρ
2
)
[
(
x
−
μ
x
)
2
σ
x
2
−
2
ρ
(
x
−
μ
x
)
(
y
−
μ
y
)
σ
x
σ
y
+
(
y
−
μ
y
)
2
σ
y
2
]
)
P(x,y)= 
2πσ 
x
​
 σ 
y
​
  
1−ρ 
2
 
​
 
1
​
 exp(− 
2(1−ρ 
2
 )
1
​
 [ 
σ 
x
2
​
 
(x−μ 
x
​
 ) 
2
 
​
 − 
σ 
x
​
 σ 
y
​
 
2ρ(x−μ 
x
​
 )(y−μ 
y
​
 )
​
 + 
σ 
y
2
​
 
(y−μ 
y
​
 ) 
2
 
​
 ])
In the Rice distribution context, we have the special case where 
X
X and 
Y
Y are independent (so 
ρ
=
0
ρ=0); they have equal variances (
σ
x
2
=
σ
y
2
=
σ
2
σ 
x
2
​
 =σ 
y
2
​
 =σ 
2
 ). This represents a 2D Gaussian cloud centered at 
(
μ
x
,
μ
y
)
(μ 
x
​
 ,μ 
y
​
 ). The Rice distribution then describes the distance from the origin to a random point sampled from this cloud: 
r
=
x
2
+
y
2
r= 
x 
2
 +y 
2
 
​
 . ↩︎

This is a well-known asymptotic expansion for the Marcum Q-function (which is closely related to the Rice CDF). The formula comes from asymptotic analysis of the Bessel function 
I
0
I 
0
​
  for large arguments. ↩︎

In the MATLAB code, 
C
a
s
y
m
p
C 
asymp
​
  and 
C
e
x
a
c
t
C 
exact
​
  are p and q respectively. ↩︎

“Finer” here means higher resolution; i.e., more densely sampled. The 
200
×
200
200×200 table covers a smaller range 
[
0
,
4
]
[0,4] with 
200
200 points, giving a spacing of 
0.02
0.02 between points. The 
150
×
150
150×150 table covers a much larger range 
[
2
,
150
]
[2,150] with only 
150
150 points, giving a spacing of about 
0.99
0.99. So the small-value table has 
 
50
×
 50× finer resolution.​​​​​​​​​​​​​​​​ ↩︎

Markdown 7225 bytes 912 words 112 lines Ln 82, Col 150HTML 7296 characters 899 words 63 paragraphs