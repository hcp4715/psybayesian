"""lec5.py

Python script aligned with the examples in lec5_new_v2.qmd.
The slide deck uses R as the executable source; this file provides Python
counterparts for the same grid-approximation and Beta-Binomial MCMC examples.
"""

import numpy as np
import pandas as pd
import scipy.stats as st
import matplotlib.pyplot as plt
import seaborn as sns


sns.set_theme(style="white")


# ---------------------------------------------------------------------------
# Grid approximation: Beta-Binomial model, 11 grid points
# ---------------------------------------------------------------------------

pi_grid = np.linspace(0.5, 1, 11)
print("从 0.5 到 1 内的连续变量 pi 中取出 11 个值:")
print(pi_grid)

prior = st.beta.pdf(pi_grid, a=70, b=30)
likelihood = st.binom.pmf(k=90, n=100, p=pi_grid)
posterior = prior * likelihood / np.sum(prior * likelihood)

plot_data = pd.DataFrame({"pi_grid": pi_grid, "posterior": posterior})

plt.figure(figsize=(8, 5))
plt.vlines(plot_data["pi_grid"], 0, plot_data["posterior"], linewidth=0.8)
plt.scatter(plot_data["pi_grid"], plot_data["posterior"], s=30)
plt.ylim(0, 1)
plt.xlabel("pi")
plt.title("Posterior")
sns.despine()
plt.show()


# Step 4: draw posterior samples
np.random.seed(84735)
posterior_sample = np.random.choice(
    pi_grid,
    size=10000,
    p=posterior,
    replace=True,
)
posterior_sample = pd.DataFrame({"pi_sample": posterior_sample})

result = (
    posterior_sample.value_counts(normalize=True)
    .reset_index(name="proportion")
    .sort_values("pi_sample")
)
print(posterior_sample.info())
print(result.head())

plt.figure(figsize=(8, 5))
plt.hist(posterior_sample["pi_sample"], bins=10)
plt.xlabel("pi_sample")
plt.ylabel("Count")
sns.despine()
plt.show()


# Compare grid samples with theoretical Beta(160, 40) posterior
x_beta = np.linspace(0, 1, 10000)
y_beta = st.beta.pdf(x_beta, a=160, b=40)

plt.figure(figsize=(10, 5))
plt.plot(
    x_beta,
    y_beta,
    color="#4169E1",
    linewidth=2,
    label="posterior distribution (theoretical)",
)
plt.hist(
    posterior_sample["pi_sample"],
    bins=40,
    density=True,
    color="#E28903",
    edgecolor="black",
    alpha=0.5,
    label="posterior distribution (grid, 11 points)",
)
plt.legend()
sns.despine()
plt.show()


# ---------------------------------------------------------------------------
# Grid approximation: Beta-Binomial model, 101 grid points
# ---------------------------------------------------------------------------

pi_grid = np.linspace(0, 1, 101)

prior = st.beta.pdf(pi_grid, a=70, b=30)
likelihood = st.binom.pmf(k=90, n=100, p=pi_grid)
unstd_posterior = prior * likelihood
posterior = unstd_posterior / np.sum(unstd_posterior)

df = pd.DataFrame({"pi": pi_grid, "posterior": posterior})

plt.figure(figsize=(8, 5))
plt.vlines(df["pi"], 0, df["posterior"], color="#1f77b4", linewidth=0.8)
plt.scatter(df["pi"], df["posterior"], color="#1f77b4", s=18)
plt.ylim(0, 0.3)
plt.title("Grid Approximation (101 points)")
sns.despine()
plt.show()


np.random.seed(84735)
posterior_sample = np.random.choice(
    pi_grid,
    size=10000,
    p=posterior,
    replace=True,
)
posterior_sample = pd.DataFrame({"pi_sample": posterior_sample})

result = (
    posterior_sample.value_counts(normalize=True)
    .reset_index(name="relative_frequency")
    .sort_values("pi_sample")
)
print(result.head(20))

plt.figure(figsize=(8, 5))
plt.hist(posterior_sample["pi_sample"], bins=50)
plt.xlabel("pi_sample")
plt.ylabel("Count")
sns.despine()
plt.show()


x_beta = np.linspace(0, 1, 10000)
y_beta = st.beta.pdf(x_beta, a=160, b=40)
posterior_sample = pd.DataFrame(
    {"pi_sample": st.beta.rvs(a=155, b=38, size=10000, random_state=84735)}
)

plt.figure(figsize=(10, 5))
plt.plot(
    x_beta,
    y_beta,
    color="#4169E1",
    linewidth=2,
    label="posterior distribution (theoretical)",
)
plt.hist(
    posterior_sample["pi_sample"],
    bins=160,
    density=True,
    color="#E28903",
    edgecolor="black",
    alpha=0.7,
    label="Grid Approximation (101 points)",
)
plt.legend()
sns.despine()
plt.show()


# ---------------------------------------------------------------------------
# Exercise: one-dimensional grid approximation for a normal mean
# ---------------------------------------------------------------------------

np.random.seed(0)
data = np.random.normal(loc=550, scale=80, size=10)
print(np.round(data, 2))

n_step = 200
mu_grid = np.linspace(200, 800, n_step)

prior_mean = 500
prior_std = 100
prior_prob = st.norm.pdf(mu_grid, loc=prior_mean, scale=prior_std)

likelihood = np.array([
    np.prod(st.norm.pdf(data, loc=mu, scale=80)) for mu in mu_grid
])

posterior_prob = prior_prob * likelihood
posterior_prob = posterior_prob / np.sum(posterior_prob)

max_posterior = np.argmax(posterior_prob)
print("最大后验概率对应的参数值：", mu_grid[max_posterior])

plt.figure(figsize=(10, 5))
plt.plot(
    mu_grid,
    prior_prob / np.sum(prior_prob),
    color="orange",
    linewidth=1.5,
    label="prior",
)
plt.plot(
    mu_grid,
    posterior_prob,
    color="blue",
    linewidth=1.5,
    label="posterior of grid method",
)
plt.axvline(np.mean(data), color="red", linewidth=1.5, label="data mean")
plt.title("Grid search posterior distribution")
plt.xlabel(r"$\mu$")
plt.ylabel("Density")
plt.xlim(200, 800)
plt.ylim(0, np.max(posterior_prob) * 1.1)
plt.legend()
sns.despine()
plt.show()


# Normal-Normal conjugate posterior for the same data
x = np.linspace(200, 800, 10000)
prior_mean = 500
prior_variance = 200**2
sigma2 = 80**2
n = len(data)

posterior_mean = (
    prior_mean / prior_variance + np.sum(data) / sigma2
) / (1 / prior_variance + n / sigma2)
posterior_std = np.sqrt(1 / (1 / prior_variance + n / sigma2))
posterior_conjugate = st.norm.pdf(x, loc=posterior_mean, scale=posterior_std)

plt.figure(figsize=(10, 5))
plt.plot(
    x,
    posterior_conjugate,
    color="blue",
    linewidth=1.5,
    label="posterior of conjugated method",
)
plt.axvline(np.mean(data), color="red", linewidth=1.5, label="data mean")
plt.title("Conjugated posterior distribution")
plt.xlabel(r"$\mu$")
plt.ylabel("Density")
plt.xlim(200, 800)
plt.ylim(0, np.max(posterior_conjugate) * 1.1)
plt.legend()
sns.despine()
plt.show()


# ---------------------------------------------------------------------------
# Exercise: two-dimensional grid approximation for normal mean and standard
# deviation
# ---------------------------------------------------------------------------

n_step = 20
mean_grid = np.linspace(200, 800, n_step)
std_grid = np.linspace(20, 200, n_step)
grid_data = pd.MultiIndex.from_product(
    [mean_grid, std_grid],
    names=["mean", "std"],
).to_frame(index=False)
mean_mesh = grid_data["mean"].to_numpy().reshape(n_step, n_step)
std_mesh = grid_data["std"].to_numpy().reshape(n_step, n_step)

prior_mean_mean = 500
prior_mean_std = 200
prior_std_mean = 100
prior_std_std = 50

mean_mesh_prior = np.linspace(0, 1000, 20)
std_mesh_prior = np.linspace(0, 200, 20)

prior_mean = st.norm.pdf(
    mean_mesh_prior,
    loc=prior_mean_mean,
    scale=prior_mean_std,
)
prior_std = st.norm.pdf(
    std_mesh_prior,
    loc=prior_std_mean,
    scale=prior_std_std,
)
prior_grid = np.outer(prior_mean, prior_std)
print(prior_grid.shape)

likelihood_grid = np.zeros((n_step, n_step))
for mean_index, mean_value in enumerate(mean_grid):
    for std_index, std_value in enumerate(std_grid):
        likelihood_i = np.prod(st.norm.pdf(data, loc=mean_value, scale=std_value))
        likelihood_grid[mean_index, std_index] = likelihood_i

print(likelihood_grid.shape)

posterior_grid = prior_grid * likelihood_grid
posterior_grid = posterior_grid / np.sum(posterior_grid)
print(posterior_grid.shape)

max_idx = np.unravel_index(np.argmax(posterior_grid), posterior_grid.shape)
estimated_mean = mean_grid[max_idx[0]]
estimated_std = std_grid[max_idx[1]]

print(f"Estimated Mean: {estimated_mean:f}")
print(f"Estimated Standard Deviation: {estimated_std:f}")

plot_data = pd.DataFrame(
    {
        "mean": [prior_mean_mean, np.mean(data), estimated_mean],
        "std": [prior_std_mean, np.std(data, ddof=1), estimated_std],
        "type": ["prior", "data", "max_posterior"],
    }
)

colors = {"prior": "orange", "data": "black", "max_posterior": "red"}
plt.figure(figsize=(8, 6))
for label, subset in plot_data.groupby("type"):
    plt.scatter(
        subset["mean"],
        subset["std"],
        color=colors[label],
        s=55,
        label=label,
    )
plt.xlabel("Mean")
plt.ylabel("Standard Deviation")
plt.title("Posterior Distribution")
plt.xlim(400, 800)
plt.ylim(20, 200)
plt.legend()
sns.despine()
plt.show()


# ---------------------------------------------------------------------------
# MCMC counterpart: Beta-Binomial model via PyMC
# ---------------------------------------------------------------------------

n_trials = 100
n_successes = 90

try:
    import arviz as az
    import pymc as pm

    with pm.Model() as bb_model:
        p = pm.Beta("p", alpha=70, beta=30)
        pm.Binomial("n_successes", n=n_trials, p=p, observed=n_successes)

        trace = pm.sample(
            draws=2000,
            tune=1000,
            chains=4,
            cores=2,
            random_seed=84735,
            return_inferencedata=True,
        )

    print(az.summary(trace, var_names=["p"]))

    az.plot_posterior(trace, var_names=["p"])
    plt.show()

    alpha_prior = 70
    beta_prior = 30
    alpha_posterior = alpha_prior + n_successes
    beta_posterior = beta_prior + (n_trials - n_successes)

    x = np.linspace(0.5, 1, 100)
    posterior_samples = trace.posterior["p"].values.ravel()
    hdi_low, hdi_high = az.hdi(posterior_samples, hdi_prob=0.94)
    mean_p = np.mean(posterior_samples)

    plt.figure(figsize=(10, 5))
    plt.plot(
        x,
        st.beta.pdf(x, a=alpha_prior, b=beta_prior),
        color="green",
        label=f"Prior Beta({alpha_prior},{beta_prior})",
    )
    plt.plot(
        x,
        st.beta.pdf(x, a=alpha_posterior, b=beta_posterior),
        color="red",
        linestyle="--",
        label=(
            f"Posterior Beta({alpha_posterior},{beta_posterior}) "
            "from conjugated prior"
        ),
    )
    sns.kdeplot(
        posterior_samples,
        color="blue",
        linewidth=2,
        label="MCMC posterior",
    )
    plt.hlines(y=0, xmin=hdi_low, xmax=hdi_high, color="black", linewidth=3)
    plt.text((hdi_low + hdi_high) / 2, 0.5, "94% HDI", ha="center")
    plt.text(hdi_low, -0.3, f"{hdi_low:.2f}", ha="center")
    plt.text(hdi_high, -0.3, f"{hdi_high:.2f}", ha="center")
    plt.text(
        mean_p,
        st.beta.pdf(x, a=alpha_posterior, b=beta_posterior).max() + 1,
        f"mean={mean_p:.2f}",
        color="blue",
        ha="center",
    )
    plt.xlim(0.5, 1)
    plt.title("Posterior results")
    plt.legend()
    sns.despine()
    plt.show()

except ImportError:
    print(
        "PyMC or ArviZ is not installed. "
        "Install them to run the MCMC section: pip install pymc arviz"
    )
