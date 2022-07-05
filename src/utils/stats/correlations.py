import itertools as it
from collections import defaultdict

import altair as alt
import numpy as np
import pandas as pd
from scipy import stats


class Correlation:
    def __init__(self, df: pd.DataFrame, ignore_errors=True) -> None:
        self.df = df
        self.cor_matrix = None
        self.ignore_errors = ignore_errors

    @staticmethod
    def infer_type(series):

        n_values = len(series.unique())
        if n_values == 1:
            return "invalid"

        if series.dtype == "object":
            return "categorical"
        if series.dtype == bool:
            return "binary"

        if n_values == 2:
            return "binary"
        elif 2 < n_values < 5:
            return "ordinal"
        elif 5 <= n_values:
            return "metric"

        raise Exception("Could not infer type for {}".format(series.name))

    @staticmethod
    def pointbiserialr(x: pd.Series, y: pd.Series, xtype: str, ytype: str):

        if xtype == "binary" and ytype == "metric":
            binary = x
            metric = y
        elif ytype == "binary" and xtype == "metric":
            binary = y
            metric = x
        else:
            raise Exception(
                "Wrong variable types for pointbiserial correlation: {}".format(
                    (xtype, ytype),
                ),
            )
        cor, p_val = stats.stats.pointbiserialr(binary, metric)
        return cor

    @staticmethod
    def listwise_nan_exlusion(x: pd.Series, y: pd.Series):
        not_nan = ~np.logical_or(np.isnan(x.values), np.isnan(y.values))

        x = x.loc[np.where(not_nan)]
        y = y.loc[np.where(not_nan)]

        return x, y

    @staticmethod
    def pearsonr(x, y):
        cor, p_val = stats.stats.pearsonr(x, y)
        return cor

    @staticmethod
    def spearmanr(x, y):
        cor, p_val = stats.stats.spearmanr(x, y)
        return cor

    @staticmethod
    def contingency_coefficient(x, y):
        confusion_matrix = pd.crosstab(x, y).to_numpy()
        chi2 = stats.chi2_contingency(confusion_matrix)[0]

        n = confusion_matrix.sum()

        C = np.sqrt(chi2 / (n + chi2))
        m = min(confusion_matrix.shape)
        C_max = np.sqrt((m - 1) / m)

        C_corrected = C / C_max

        return C_corrected

    @property
    def correlation_matrix(self):
        if self.cor_matrix is not None:
            return self.cor_matrix

        correlations = defaultdict(dict)

        columns = list(self.df.columns)

        for i, j in it.combinations(columns, r=2):

            x = self.df[i]
            y = self.df[j]

            x, y = self.listwise_nan_exlusion(x, y)

            x_type = self.infer_type(x)
            y_type = self.infer_type(y)

            type_set = set([x_type, y_type])

            if type_set == {"metric"}:
                cor_type = "pearsonr"
                cor = self.pearsonr(x, y)
            elif type_set == {"metric", "binary"}:
                cor_type = "pointbiserialr"
                cor = self.pointbiserialr(x, y, x_type, y_type)
            elif type_set in [
                {"ordinal"},
                {"ordinal", "metric"},
                {"ordinal", "binary"},
            ]:
                cor_type = "spearmanr"
                cor = self.spearmanr(x, y)
            elif type_set in [{"binary"}, {"binary", "categorical"}, {"categorical"}]:
                cor_type = "C_corr"
                cor = self.contingency_coefficient(x, y)
            else:
                if self.ignore_errors:
                    continue
                else:
                    raise Exception(
                        "Correlation not implemented yet for {}".format(
                            (x_type, y_type),
                        ),
                    )

            correlations[i][j] = (cor, cor_type, len(x))
            correlations[j][i] = (cor, cor_type, len(x))

        df = pd.DataFrame(correlations).sort_index(axis=0).sort_index(axis=1)
        np.fill_diagonal(df.values, 1)

        self.cor_matrix = df
        return df

    @staticmethod
    def stack_correlation_matrix(df):

        df = df.copy()
        df = (
            df.stack()
            .reset_index()
            .rename(columns={0: "correlation", "level_0": "var", "level_1": "var2"})
        )

        def unpack(x):
            try:
                return x[0], x[1], x[2]
            except Exception:
                return x, None, 0

        df["correlation"], df["cor_type"], df["n_valid"] = zip(*df["correlation"].apply(unpack))
        df["correlation_label"] = round(df["correlation"], 2)

        return df

    def heatmap(self, width=300, height=300, scheme="blueorange", domain=(-1, 1)):

        df = self.stack_correlation_matrix(self.correlation_matrix)
        base = alt.Chart(df).encode(
            x="var2:O",
            y="var:O",
        )

        text = base.mark_text(baseline="middle").encode(
            text="correlation_label",
            color=alt.condition(
                alt.datum.correlation > 0.5,
                alt.value("white"),
                alt.value("black"),
            ),
            tooltip=[
                alt.Tooltip("var:N", title="Var"),
                alt.Tooltip("var2:N", title="Var2"),
                alt.Tooltip("cor_type:N", title="Type"),
                alt.Tooltip("n_valid:Q", title="N Valid"),
                alt.Tooltip("correlation:Q", title="Correlation"),
            ],
        )

        cor_plot = base.mark_rect().encode(
            color=alt.Color(
                "correlation:Q",
                scale=alt.Scale(scheme=scheme, domain=domain),
            ),
            tooltip=[
                alt.Tooltip("var:N", title="Var"),
                alt.Tooltip("var2:N", title="Var2"),
                alt.Tooltip("cor_type:N", title="Type"),
                alt.Tooltip("n_valid:Q", title="N Valid"),
                alt.Tooltip("correlation:Q", title="Correlation"),
            ],
        )

        return (cor_plot + text).properties(width=width, height=height)
