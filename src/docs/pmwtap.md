# Physical Modeling of the Weapon Target Assignment Problem

Claude Sonnet 4, Kam Modjtahedzadeh  
Lockheed Martin & Northrop Grumman  
September 16, 2025 &ndash; September 19, 2025

> The weapon target assignment problem requires a comprehensive physical model that captures the fundamental relationships between weapon systems, target characteristics, and engagement dynamics through quantitative mathematical expressions.

## Target Vulnerability Modeling

Target vulnerability is characterized through a damage assessment function that relates weapon effects to target survivability. For a target $\jmath$ engaged by weapon $\imath$, the probability of kill function takes the form:

$$P_{\mathrm{kill}}(\imath,\jmath) = 1 - e^{-\alpha_{\imath\jmath} \cdot E_{\mathrm{delivered}}(\imath,\jmath)}$$

where $\alpha_{\imath\jmath}$ represents the target-weapon vulnerability coefficient and $E_{\mathrm{delivered}}(\imath,\jmath)$ denotes the effective energy transfer. The vulnerability coefficient incorporates target-specific parameters including armor effectiveness $A_{\jmath}$, critical component density $\rho_{\jmath}$,[^ccd] and structural integrity factor $S_{\jmath}$ through the relationship:

$$\alpha_{\imath\jmath} = \frac{\beta_{\imath}}{A_{\jmath} \cdot S_{\jmath}} \cdot f(\rho_{\jmath})$$

Here, $\beta_{\imath}$ represents the weapon's intrinsic lethality parameter, while $f(\rho_{\jmath})$ serves as a spatial distribution modifier that translates the raw density of critical components into a vulnerability factor.

## Weapon Effectiveness Physics

The delivered energy model depends fundamentally on weapon type and engagement geometry. For kinetic energy weapons, the energy transfer follows:

$$E_{\mathrm{delivered}} = \left(\frac{1}{2}m_{\mathrm{p}} v_{\mathrm{impact}}^2\right) \cdot \eta_{\mathrm{penetration}} \cdot P_{\mathrm{hit}}$$

where $m_{\mathrm{p}}$ represents projectile mass, $v_{\mathrm{impact}}$ denotes impact velocity after accounting for drag losses, $\eta_{\mathrm{penetration}}$ captures the fraction of kinetic energy effectively transferred to the target, and $P_{\mathrm{hit}}$ represents the geometric hit probability.

The hit probability incorporates weapon accuracy characteristics through a two-dimensional Gaussian distribution:

$$P_{\mathrm{hit}} = \int\int_{A_{\mathrm{target}}} \frac{dxdy}{2\pi\sigma_x\sigma_y} \exp\left(-\frac{x^2}{2\sigma_x^2} - \frac{y^2}{2\sigma_y^2}\right) $$

where $\sigma_x$ and $\sigma_y$ define the weapon's accuracy envelope (the standard deviations of the weapon's accuracy in the $x$ and $y$ directions respectively). The integration occurs over the target's presented cross-sectional area $A_{\mathrm{target}}$.

## Engagement Geometry and Constraints

The spatial relationship between weapons and targets introduces fundamental physical constraints. The engagement range constraint ensures weapons operate within their effective envelope:

$$r_{\imath\jmath} \leq R_{\mathrm{max},{\imath}}$$

where $r_{\imath\jmath}$ is the distance between $(x_{\imath}, y_{\imath}, z_{\imath})$ and $(x_{\jmath}, y_{\jmath}, z_{\jmath})$, and $R_{\mathrm{max},{\imath}}$ denotes the maximum effective range for weapon $\imath$.

Time-dependent engagement windows arise from relative motion dynamics. For targets moving with velocity vector $\vec{v}_{\jmath}$ and weapons with engagement preparation time $t_{\mathrm{prep},\imath}$, the intercept geometry requires:

$$\vec{r}_{\jmath}(t) = \vec{r}_{\jmath}(0) + \vec{v}_{\jmath} \cdot t$$

The engagement window exists when the predicted target position remains within the weapon's engagement envelope throughout the interval $[t_{\mathrm{prep},\imath},\,  t_{\mathrm{prep},\imath} + t_{\mathrm{engagement},\imath}]$ where $t_{\mathrm{engagement},\imath}$ represents the duration of time that weapon $\imath$ requires to complete its engagement sequence against a target.

## Resource Depletion Dynamics

Weapon system limitations introduce discrete resource constraints that couple individual engagements. The ammunition constraint follows:

$$\sum_{\jmath=1}^{N_{\mathrm{targets}}} \delta_{\imath\jmath} \cdot n_{\mathrm{rounds},\imath\jmath} \leq C_{\mathrm{available},\imath}$$

where $\delta_{\imath\jmath}$ represents the binary assignment variable,[^bav] $n_{\mathrm{rounds},\imath\jmath}$ denotes the number of rounds required for engagement, and $C_{\mathrm{available},\imath}$ represents available ammunition for weapon $\imath$.

Thermal management constraints for rapid-fire systems introduce duty cycle limitations:[^bvi]

$$\sum_{\jmath} \delta_{\imath\jmath} \cdot t_{\mathrm{engagement},\imath\jmath} \cdot P_{\mathrm{thermal},\imath} \leq \frac{Q_{\mathrm{max},\imath}}{k_{\mathrm{cooling},\imath}}$$

where $P_{\mathrm{thermal},\imath}$ represents thermal power generation per engagement, $Q_{\mathrm{max},\imath}$ denotes maximum thermal capacity, and $l_{\mathrm{cooling},\imath}$ captures cooling system effectiveness.

This mathematical framework establishes the physical foundation necessary for formulating the subsequent optimization problem, ensuring that all constraint relationships reflect realistic operational limitations and measurable system parameters.

[^ccd]: Critical component density, $\rho_{\jmath}$, represents the spatial concentration or distribution of vital systems within target $\jmath$. Higher $\rho_{\jmath}$ indicates critical components (engines, command centers, fuel systems, etc.) are densely packed or concentrated in specific areas. Lower $\rho_{\jmath}$ suggests critical systems are more dispersed throughout the target. This affects weapon effectiveness as high density implies easier to achieve catastrophic damage with a single hit to a critical area and low density requires more precise targeting or multiple hits to disable essential systems.

[^bav]: The binary assignment variable, $\delta_{\imath\jmath}$ is a decision variable that indicates whether weapon $\imath$ is assigned to engage target $\jmath$; $$\delta_{\imath\jmath} = \begin{cases}1 &\text{if weapon $\imath$ is assigned to target $\jmath$} \\ 0 &\text{if weapon $\imath$ is not assigned to target $\jmath$}\end{cases}$$ This variable represents the assignment decision in the optimization problem. For example, if you have $3$ weapons and $4$ targets, you'd have a $3×4$ matrix of these binary variables. The optimization algorithm determines which $\delta_{\imath\jmath}$ values should be $1$ (make the assignment) and which should be $0$ (do not make the assignment).

[^bvi]: The binary variable implies a summation limit of $N_{\mathrm{target}}$ (in both equations that is appears).
