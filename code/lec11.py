"""第 11 讲：贝叶斯回归模型的评估与比较。

在项目根目录运行 python code/lec11.py。加 --exercise 运行模型 4–6 的参考解答。
核验环境使用 PyMC 5.16.2、ArviZ 0.20.0 与 statsmodels。
"""

from __future__ import annotations

import argparse
import hashlib
import inspect
import json
import tempfile
import warnings
from pathlib import Path

import arviz as az
import numpy as np
import pandas as pd
import pymc as pm
import scipy.special as special
import scipy.stats as stats
import statsmodels.api as sm

_ROOT = Path(__file__).resolve().parents[1]
DATA = _ROOT / "data"
CACHE = _ROOT / "tmpdata" / "lec11-python"
DRAWS = 2000
TUNE = 1000
CHAINS = 4
SEED = 84735


def read_rt() -> pd.DataFrame:
    """汇总每名被试在各标签和匹配条件下的平均反应时。"""
    trials = pd.read_csv(DATA / "Kolvoort_2020_HBM_Exp1_Clean.csv")
    rt = trials.groupby(["Subject", "Label", "Matching"], as_index=False).agg(
        RT_sec=("RT_sec", lambda values: values.mean(skipna=False))
    )
    rt["Label"] = pd.Categorical.from_codes(
        rt["Label"].astype(int) - 1, categories=["Self", "Friend", "Stranger"]
    )
    rt["Matching"] = pd.Categorical(
        rt["Matching"], categories=["Matching", "Nonmatching"]
    )
    return rt


def rt_designs(rt: pd.DataFrame) -> dict[str, pd.DataFrame]:
    """按课件的处理对比建立三个嵌套设计矩阵。"""
    friend = rt["Label"].eq("Friend").astype(int)
    stranger = rt["Label"].eq("Stranger").astype(int)
    nonmatching = rt["Matching"].eq("Nonmatching").astype(int)
    model1 = pd.DataFrame(
        {"(Intercept)": 1, "LabelFriend": friend, "LabelStranger": stranger},
        index=rt.index,
    )
    model2 = model1.assign(MatchingNonmatching=nonmatching)
    model3 = model2.assign(
        **{
            "LabelFriend:MatchingNonmatching": friend * nonmatching,
            "LabelStranger:MatchingNonmatching": stranger * nonmatching,
        }
    )
    return {"model1": model1, "model2": model2, "model3": model3}


def read_exercise() -> pd.DataFrame:
    """保留清华站点的有效观测，并以不吸烟为基线。"""
    data = pd.read_csv(DATA / "Data_Sum_HPP_Multi_Site_Share.csv")
    valid = (
        data["Site"].eq("Tsinghua")
        & data["smoke"].isin([1, 2])
        & np.isfinite(data["stress"])
        & np.isfinite(data["scontrol"])
    )
    exercise = data.loc[valid, ["stress", "scontrol", "smoke"]].reset_index(drop=True)
    exercise["smoke"] = exercise["smoke"].eq(1).astype(int)
    return exercise


def exercise_designs(exercise: pd.DataFrame) -> dict[str, pd.DataFrame]:
    """保留原始截距，依次纳入压力、吸烟状态和交互项。"""
    model4 = pd.DataFrame({"(Intercept)": 1, "stress": exercise["stress"]})
    model5 = model4.assign(smoke=exercise["smoke"])
    model6 = model5.assign(**{"stress:smoke": exercise["stress"] * exercise["smoke"]})
    return {"model4": model4, "model5": model5, "model6": model6}


def fit_gaussian(
    name: str,
    design: pd.DataFrame,
    observed: pd.Series,
    intercept_mean: float,
    intercept_sd: float,
    slope_sd: float,
    sigma_rate: float,
) -> az.InferenceData:
    """拟合课件的高斯回归，保存后验预测抽样和逐点对数似然。"""
    x = design.to_numpy(dtype=float)
    y = observed.to_numpy(dtype=float)
    if not design.index.equals(observed.index):
        raise ValueError("设计矩阵和观测值的索引不一致。")
    prior_mean = np.r_[intercept_mean, np.zeros(x.shape[1] - 1)]
    prior_sd = np.r_[intercept_sd, np.full(x.shape[1] - 1, slope_sd)]

    # 数据、先验、采样设置或实现变化时使用不同的缓存。
    metadata = json.dumps(
        [design.columns.tolist(), x.shape, sigma_rate, DRAWS, TUNE, CHAINS, SEED,
         pm.__version__, az.__version__]
    )
    key = hashlib.sha256(inspect.getsource(fit_gaussian).encode("utf-8"))
    key.update(metadata.encode("utf-8"))
    for array in (x, y, prior_mean, prior_sd):
        key.update(array.tobytes())
    CACHE.mkdir(parents=True, exist_ok=True)
    filename = CACHE / f"{name}-{key.hexdigest()[:20]}.nc"
    if filename.is_file():
        return az.from_netcdf(filename)

    coords = {"coefficient": design.columns.tolist(), "observation": design.index}
    with pm.Model(coords=coords):
        beta = pm.Normal("beta", mu=prior_mean, sigma=prior_sd, dims="coefficient")
        sigma = pm.Exponential("sigma", lam=sigma_rate)
        pm.Normal("y", mu=pm.math.dot(x, beta), sigma=sigma, observed=y, dims="observation")
        fit = pm.sample(
            draws=DRAWS, tune=TUNE, chains=CHAINS, cores=1,
            random_seed=SEED, idata_kwargs={"log_likelihood": True},
        )
        pm.sample_posterior_predictive(
            fit, var_names=["y"], extend_inferencedata=True, random_seed=SEED
        )

    with tempfile.NamedTemporaryFile(dir=CACHE, suffix=".nc", delete=False) as file:
        temp = Path(file.name)
    try:
        fit.to_netcdf(temp)
        temp.replace(filename)
    finally:
        temp.unlink(missing_ok=True)
    return fit


def calculate_medae(fit: az.InferenceData, observed: pd.Series) -> float:
    """计算观测值与后验预测均值之间的样本内绝对误差中位数。"""
    predicted = fit.posterior_predictive["y"].mean(dim=("chain", "draw"))
    return float(np.median(np.abs(observed.to_numpy() - predicted.values)))


def calculate_loo(fit: az.InferenceData) -> pd.DataFrame:
    """使用各观测似然的相对 ESS 计算逐行 PSIS-LOO。"""
    log_lik = fit.log_likelihood["y"].transpose("observation", "chain", "draw")
    sample_count = log_lik.sizes["chain"] * log_lik.sizes["draw"]
    # 在保留链维度时先计算相对 ESS。按观测缩放似然，避免指数下溢。
    lik = np.exp(log_lik - log_lik.max(dim=("chain", "draw")))
    r_eff = az.ess(lik, method="mean", relative=True)["y"].values
    samples = log_lik.values.reshape(-1, sample_count)
    elpd = np.empty(len(samples))
    pareto_k = np.empty(len(samples))
    for i, row in enumerate(samples):
        weights, pareto_k[i] = az.psislw(-row, reff=float(r_eff[i]))
        elpd[i] = special.logsumexp(row + weights)
    threshold = min(0.7, 1.0 - 1.0 / np.log10(sample_count))
    if np.any(pareto_k > threshold):
        warnings.warn("部分观测的 Pareto k 超过阈值，需检查 PSIS 近似。", stacklevel=2)

    lppd = special.logsumexp(samples, axis=1) - np.log(sample_count)
    return pd.DataFrame(
        {"elpd_loo": elpd, "p_loo": lppd - elpd, "looic": -2 * elpd, "pareto_k": pareto_k},
        index=log_lik["observation"].values,
    )


def compare_loo(model_loo: dict[str, pd.DataFrame]) -> pd.DataFrame:
    """以逐点配对差计算相对首个模型的 ELPD 差及标准误。"""
    points = pd.DataFrame({name: loo["elpd_loo"] for name, loo in model_loo.items()})
    ranked = points.sum().sort_values(ascending=False).index
    difference = points.loc[:, ranked].subtract(points[ranked[0]], axis=0)
    return pd.DataFrame(
        {"elpd_diff": difference.sum(), "se_diff": difference.std(ddof=1) * np.sqrt(len(points))}
    )


def calculate_dic(fit: az.InferenceData, design: pd.DataFrame, observed: pd.Series) -> float:
    """计算后验平均偏差加上有效参数个数。"""
    log_lik = fit.log_likelihood["y"]
    mean_deviance = float(-2 * log_lik.sum("observation").mean())
    coefficient_mean = fit.posterior["beta"].mean(dim=("chain", "draw")).values
    sigma_mean = float(fit.posterior["sigma"].mean())
    plug_in_mean = design.to_numpy() @ coefficient_mean
    plug_in_deviance = float(
        -2 * stats.norm.logpdf(observed, loc=plug_in_mean, scale=sigma_mean).sum()
    )
    return 2 * mean_deviance - plug_in_deviance


def summarize_regression(
    fit: az.InferenceData, design: pd.DataFrame, observed: pd.Series
) -> None:
    """用同一设计矩阵比较贝叶斯回归和 OLS 系数。"""
    ols = sm.OLS(observed, design).fit()
    beta = fit.posterior["beta"]
    beta_ci = beta.quantile([0.025, 0.975], dim=("chain", "draw")).values
    ols_ci = ols.conf_int().to_numpy()
    coef = pd.DataFrame(
        {
            "后验均值": beta.mean(dim=("chain", "draw")).values,
            "后验SD": beta.std(dim=("chain", "draw"), ddof=1).values,
            "可信区间下限": beta_ci[0],
            "可信区间上限": beta_ci[1],
            "OLS估计": ols.params.to_numpy(),
            "OLS标准误": ols.bse.to_numpy(),
            "置信区间下限": ols_ci[:, 0],
            "置信区间上限": ols_ci[:, 1],
        },
        index=design.columns,
    )
    print(coef.round(3).to_string())

    sigma = fit.posterior["sigma"].values
    sigma_ci = np.quantile(sigma, [0.025, 0.975])
    print(
        f"残差SD的后验均值 = {sigma.mean():.3f}，后验标准差 = {sigma.std(ddof=1):.3f}，"
        f"95% 等尾可信区间 = [{sigma_ci[0]:.3f}, {sigma_ci[1]:.3f}]"
    )
    # R 的 AIC、BIC 额外计入残差方差这一参数。
    parameter_count = len(ols.params) + 1
    aic = -2 * ols.llf + 2 * parameter_count
    bic = -2 * ols.llf + np.log(ols.nobs) * parameter_count
    print(pd.Series(
        {"R2": ols.rsquared, "调整后R2": ols.rsquared_adj, "F": ols.fvalue,
         "p": ols.f_pvalue, "对数似然": ols.llf, "AIC": aic, "BIC": bic}
    ).round(4).to_string())


def summarize_predictions(
    fits: dict[str, az.InferenceData], designs: dict[str, pd.DataFrame], observed: pd.Series
) -> None:
    """输出 MedAE、ELPD 差与标准误、DIC 及 Pareto k。"""
    model_loo = {name: calculate_loo(fit) for name, fit in fits.items()}
    loo_comparison = compare_loo(model_loo)
    metrics = pd.DataFrame(
        {
            "MedAE": {name: calculate_medae(fit, observed) for name, fit in fits.items()},
            "DIC": {
                name: calculate_dic(fit, designs[name], observed)
                for name, fit in fits.items()
            },
            "最大Pareto k": {name: loo["pareto_k"].max() for name, loo in model_loo.items()},
        }
    )
    print(metrics.join(loo_comparison).round(4).to_string())
    if "model3" in model_loo:
        estimates = model_loo["model3"][["elpd_loo", "p_loo", "looic"]]
        print(pd.DataFrame(
            {"Estimate": estimates.sum(), "SE": estimates.std(ddof=1) * np.sqrt(len(estimates))}
        ).round(4).to_string())


def main() -> None:
    """运行课堂示例，按需运行练习参考解答。"""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--exercise", action="store_true", help="运行模型 4–6 的参考解答")
    args = parser.parse_args()

    rt = read_rt()
    designs = rt_designs(rt)
    observed = rt["RT_sec"]
    print(rt.head(3).to_string(index=False))
    print(designs["model3"].head(3).to_string(index=False))
    fits = {
        name: fit_gaussian(name, x, observed, 5, 2, 1, 0.3)
        for name, x in designs.items()
    }
    summarize_regression(fits["model3"], designs["model3"], observed)
    predicted = fits["model1"].posterior_predictive["y"].mean(dim=("chain", "draw"))
    print(pd.DataFrame(
        {"observed": observed, "predicted": predicted.values}
    ).head(4).to_string(index=False))
    summarize_predictions(fits, designs, observed)

    exercise = read_exercise()
    print(exercise.head(4).to_string(index=False))
    # Part 5–6：用 PyMC 拟合课件中的三个高斯回归模型。
    if args.exercise:
        exercise_matrices = exercise_designs(exercise)
        exercise_observed = exercise["scontrol"]
        exercise_fits = {
            name: fit_gaussian(
                name, design, exercise_observed,
                intercept_mean=50, intercept_sd=10, slope_sd=10, sigma_rate=0.6,
            )
            for name, design in exercise_matrices.items()
        }

        # Part 6：查看等尾可信区间、Rhat 与有效样本量。
        for name, fit in exercise_fits.items():
            summary = az.summary(
                fit,
                var_names=["beta", "sigma"],
                stat_funcs={
                    "2.5%": lambda draws: np.quantile(draws, 0.025),
                    "97.5%": lambda draws: np.quantile(draws, 0.975),
                },
            )
            columns = ["mean", "sd", "2.5%", "97.5%", "r_hat", "ess_bulk", "ess_tail"]
            print(f"\n{name}\n{summary[columns].to_string()}")

        model4 = exercise_fits["model4"]
        y_rep4 = model4.posterior_predictive["y"].stack(sample=("chain", "draw"))
        log_lik4 = model4.log_likelihood["y"].stack(sample=("chain", "draw"))
        # 合并链后与 brms 的矩阵一致，每行是一次抽样，每列是一位被试。
        print(pd.Series({
            "y_rep4": y_rep4.transpose("sample", "observation").shape,
            "log_lik4": log_lik4.transpose("sample", "observation").shape,
            "n": len(exercise),
        }).to_string())
        predicted5 = exercise_fits["model5"].posterior_predictive["y"].mean(
            dim=("chain", "draw")
        )
        print(pd.Series(predicted5.values, name="predicted5").head(4).to_string(index=False))

        # Part 5：用各拟合对象的后验预测与逐点对数似然计算指标。
        summarize_predictions(exercise_fits, exercise_matrices, exercise_observed)


if __name__ == "__main__":
    main()


# Python 代码核验说明
# Part 5–6 使用与 brms 相同的原始截距、数据筛选、先验和采样次数。
# PyMC 的逐点对数似然与后验预测分别用于 ELPD-LOO、DIC 和 MedAE。
# PyMC 与 brms 的 Stan 后端、ArviZ 与 loo 的 PSIS 数值实现存在差异。
# 相同随机种子下，各自的后验抽样及统计结果仍可有小幅差异。
