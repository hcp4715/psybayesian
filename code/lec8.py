"""lec8.py

Python counterpart of the executable R chunks in lec8.qmd (the qmd is the
source of truth for the slides). It reproduces:

  1. Beta-Binomial model (prior Beta(2, 2), data N = 118, Y = 99) with PyMC
  2. HDI / ROPE-based inference
  3. six framings of hypothesis testing (point-null, null-region, directional)
  4. Savage-Dickey density ratio (analytic, conjugate model)
  5. bridge-sampling target: the marginal likelihood (computed exactly here,
     because Python has no direct counterpart of bridgesampling::bridge_sampler)
"""

import warnings

import numpy as np
import scipy.stats as st
import matplotlib.pyplot as plt
import seaborn as sns
import pymc as pm
import arviz as az

warnings.filterwarnings("ignore")

sns.set_theme(style="white", context="paper")
plt.rcParams.update({
    "axes.spines.top": False,
    "axes.spines.right": False,
    "axes.grid": False,
    "figure.dpi": 120,
})


def fmt(x):
    if 1e-2 <= abs(x) < 1e6:
        return f"{x:.2f}"
    return f"{x:.2e}"


# ---------------------------------------------------------------------------
# 1. Data
# ---------------------------------------------------------------------------

# 数据来自 data/beta-binmial.csv（118 次判断、正确 99 次，ACC 列求和 = 99）
N, Y = 118, 99
print(f"N = {N}, Y = {Y}, observed proportion = {Y / N:.3f}")

# ---------------------------------------------------------------------------
# 2. Beta-Binomial model and posterior sampling
# ---------------------------------------------------------------------------

with pm.Model() as bb_model:
    theta = pm.Beta("theta", alpha=2, beta=2)
    y = pm.Binomial("y", n=N, p=theta, observed=Y)
    trace = pm.sample(draws=4000, tune=1000, chains=4, cores=4,
                      random_seed=84735, progressbar=False)

az.summary(trace, var_names=["theta"])

theta_post = trace.posterior["theta"].values.reshape(-1)
theta_prior = st.beta.rvs(2, 2, size=theta_post.size, random_state=84735)

# ---------------------------------------------------------------------------
# 3. HDI and ROPE
# ---------------------------------------------------------------------------

hdi = az.hdi(theta_post, prob=0.95)
print(f"95% HDI: [{hdi[0]:.2f}, {hdi[1]:.2f}]")
print(f"Posterior mean: {theta_post.mean():.3f} | median: {np.median(theta_post):.3f}")

inside_rope = (theta_post >= 0.4) & (theta_post <= 0.6)
p_inside_rope = inside_rope.mean()
print(f"P(theta in ROPE [0.4, 0.6] | Y) = {p_inside_rope:.2e}")

if hdi[0] > 0.6:
    print("HDI lies entirely above the ROPE -> theta is practically different from 0.5")
elif hdi[1] < 0.4:
    print("HDI lies entirely below the ROPE -> theta is practically different from 0.5")
elif hdi[0] >= 0.4 and hdi[1] <= 0.6:
    print("HDI lies entirely inside the ROPE -> theta is practically equivalent to 0.5")
else:
    print("HDI overlaps the ROPE -> undecided")

# ---------------------------------------------------------------------------
# 4. Six framings of hypothesis testing
# ---------------------------------------------------------------------------

def make_test(samples, name, null_desc, interest_desc, null_spans, vlines=()):
    """null_spans 为空表示点零假设（后验下概率质量为 0）。"""
    in_null = np.zeros_like(samples, dtype=bool)
    for lo, hi in null_spans:
        in_null |= (samples >= lo) & (samples <= hi)
    return {
        "name": name,
        "null": null_desc,
        "interest": interest_desc,
        "p_null": float(in_null.mean()),
        "p_interest": float(1 - in_null.mean()),
        "spans": null_spans,
        "vlines": vlines,
    }


# 六个面板与 lec8.qmd §2.3 的表格逐行对应
tests = [
    make_test(theta_post, "1. Two-sided NHST",
              "θ = 0.5 (point); P(θ = 0.5 | Y) = 0, see BF in 3.1",
              "θ ≠ 0.5",
              [], (0.5,)),
    make_test(theta_post, "2. One-sided NHST",
              "θ ≤ 0.5", "θ > 0.5",
              [(-np.inf, 0.5)], (0.5,)),
    make_test(theta_post, "3. Two-sided Minimal Effect Test",
              "0.4 ≤ θ ≤ 0.6 (trivial effect)",
              "θ < 0.4 or θ > 0.6 (meaningful effect)",
              [(0.4, 0.6)], (0.4, 0.6)),
    make_test(theta_post, "4. One-sided Minimal Effect Test",
              "θ ≤ 0.6", "θ > 0.6",
              [(-np.inf, 0.6)], (0.6,)),
    make_test(theta_post, "5. Equivalence Test",
              "θ < 0.4 or θ > 0.6 (outside ROPE)",
              "0.4 ≤ θ ≤ 0.6 (inside ROPE)",
              [(-np.inf, 0.4), (0.6, np.inf)], (0.4, 0.6)),
    make_test(theta_post, "6. Non-inferiority Test",
              "θ < 0.4", "θ ≥ 0.4",
              [(-np.inf, 0.4)], (0.4,)),
]

for t in tests:
    print(f"{t['name']:<36s} p(null) = {t['p_null']:.3f} | p(interest) = {t['p_interest']:.3f}")

# six panels: blue shading = null region
fig, axes = plt.subplots(3, 2, figsize=(12, 8), sharex=True)
for ax, t in zip(axes.ravel(), tests):
    sns.kdeplot(x=theta_post, fill=True, color="grey", alpha=0.4, linewidth=1, ax=ax)
    for lo, hi in t["spans"]:
        ax.axvspan(max(lo, 0), min(hi, 1), color="steelblue", alpha=0.18, linewidth=0)
    for v in t["vlines"]:
        ax.axvline(v, linestyle=":", color="steelblue", linewidth=0.8)
    ax.set_xlim(0, 1)
    ax.set_title(t["name"], fontsize=10, fontweight="bold")
    ax.set_xlabel("θ")
    ax.set_ylabel("Posterior density")
    ax.text(0.02, 0.95,
            f"Null: {t['null']}\nInterest: {t['interest']}\n"
            f"p(null) = {t['p_null']:.3f} | p(interest) = {t['p_interest']:.3f}",
            transform=ax.transAxes, va="top", fontsize=6.5)
fig.suptitle("Blue shading = null hypothesis region", fontsize=9)
fig.tight_layout()
plt.show()

# ---------------------------------------------------------------------------
# 5. Savage-Dickey density ratio (point null: theta = 0.5)
# ---------------------------------------------------------------------------

prior_at_null = st.beta.pdf(0.5, 2, 2)
posterior_at_null = st.beta.pdf(0.5, 101, 21)

BF01 = posterior_at_null / prior_at_null
BF10 = 1 / BF01
print(f"BF_10 = {fmt(BF10)} | BF_01 = {fmt(BF01)}")

# ---------------------------------------------------------------------------
# 6. Prior vs posterior (APA-style figure)
# ---------------------------------------------------------------------------

fig, ax = plt.subplots(figsize=(8, 4))
sns.kdeplot(x=theta_prior, fill=True, color="steelblue", alpha=0.5, label="Prior", ax=ax)
sns.kdeplot(x=theta_post, fill=True, color="darkorange", alpha=0.5, label="Posterior", ax=ax)
ax.set_xlim(0, 1)
ax.set_xlabel("θ")
ax.set_ylabel("Density")
ax.legend(title="Distribution", loc="upper center", bbox_to_anchor=(0.5, 1.12), ncol=2,
          frameon=False)
ax.set_title("Prior and Posterior Distribution Comparison")
plt.tight_layout()
plt.show()

# ---------------------------------------------------------------------------
# 7. Marginal likelihood and the interval null (ROPE)
# ---------------------------------------------------------------------------

# H0: theta = 0.5 (point null) -> the marginal likelihood is the likelihood at 0.5
logml_H0 = st.binom.logpmf(Y, N, 0.5)

# H1: theta ~ Beta(2, 2) -> integrate prior x likelihood over [0, 1]
# (this is exactly what bridge sampling estimates from posterior draws)
from scipy.integrate import quad
marginal_H1 = quad(lambda t: st.beta.pdf(t, 2, 2) * st.binom.pmf(Y, N, t),
                   0, 1, epsabs=1e-14, limit=200)[0]
BF01_marginal = np.exp(logml_H0) / marginal_H1
print(f"[point null]     BF_01 = {fmt(BF01_marginal)} | BF_10 = {fmt(1 / BF01_marginal)}")

# Interval null: H0: theta in [0.4, 0.6], H1: theta outside
p_rope_post = st.beta.cdf(0.6, 101, 21) - st.beta.cdf(0.4, 101, 21)
p_rope_prior = st.beta.cdf(0.6, 2, 2) - st.beta.cdf(0.4, 2, 2)
BF01_rope = (p_rope_post / (1 - p_rope_post)) * ((1 - p_rope_prior) / p_rope_prior)
print(f"P(theta in ROPE | Y)      = {fmt(p_rope_post)}")
print(f"P(theta in ROPE) (prior)  = {fmt(p_rope_prior)}")
print(f"[interval null]  BF_01 = {fmt(BF01_rope)} | BF_10 = {fmt(1 / BF01_rope)}")

# ---------------------------------------------------------------------------
# 8. Exercise: directional hypothesis (fill in the blanks)
# ---------------------------------------------------------------------------

# H1: theta > 0.5. Reuse the helper logic above:
#   region = (-inf, 0.5)
#   prior_odds = ...
#   posterior_odds = ...
#   BF_10 = posterior_odds / prior_odds
#
# Hint: the prior for H1 is P(theta > 0.5) under Beta(2, 2),
#       the posterior one uses Beta(101, 21).
