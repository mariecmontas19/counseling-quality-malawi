"""Build the wild-cluster-bootstrap (WCB) robustness appendix table.

Reads:  /tmp/wcb_results.csv  (from results_wildbootstrap.do)
Writes: tables/iv_wildbootstrap.tex

Reports the WCB p-value (Webb weights, clustered on the six counselors) for the
pooled / postpartum / pregnant effect of each of the 19 outcomes. Point estimates
are identical to Table 5 and are shown for orientation.
"""
import csv

with open('/tmp/wcb_results.csv') as f:
    rows = list(csv.DictReader(f))

LABELS = {
    'share_same_counselor': 'Same counselor', 'total_sessions': 'Total sessions',
    'visit_rate_mo': 'Sessions per month', 'husband_present': 'Husband present',
    'etp1_fp': 'FP information', 'etp1_spacing': 'Birth spacing',
    'etp1_preg': 'Pregnancy info', 'etp1_partner': 'Partner engagement',
    'etp1_childhealth': 'Child health', 'etp1_breast': 'Breastfeeding',
    'admin_fp_any': 'Any FP service', 'admin_inject_any': 'Any injectable',
    'n_admin_fp': 'Number of FP services', 'fp_inj_coverage_frac': 'Injectable coverage',
    'husband_knows_fp': 'Husband knows FP',
    'birth_within_24mo': 'Birth within 24 months', 'preg_within_12mo': 'Pregnancy within 12 months',
    'preg_within_24mo': 'Pregnancy within 24 months', 'mistimed_preg_expost': 'Mistimed pregnancy',
}
PANEL_TITLE = {'A': 'Panel A: Counseling continuity', 'B': 'Panel B: Counseling topics',
               'C': 'Panel C: FP coverage', 'D': 'Panel D: Reproductive'}

def st(p):
    p = float(p)
    return '$^{***}$' if p < 0.01 else '$^{**}$' if p < 0.05 else '$^{*}$' if p < 0.10 else ''

def cell(b, p):
    return f"{float(b):.3f} ({float(p):.3f}{st(p)})"

out = []
out.append(r"\begin{table}[H]")
out.append(r"\centering")
out.append(r"\caption{Wild-Cluster-Bootstrap Inference (Clustering on the Six Counselors)}")
out.append(r"\label{tab:iv_wildbootstrap}")
out.append(r"\small")
out.append(r"\setlength{\tabcolsep}{6pt}")
out.append(r"\begin{tabular}{lccc}")
out.append(r"\toprule")
out.append(r" & Pooled & Postpartum & Pregnant \\")
out.append(r" & \multicolumn{3}{c}{\emph{Coefficient (WCB $p$-value)}} \\")
out.append(r"\cmidrule(lr){2-4}")
last_panel = None
for r in rows:
    if r['panel'] != last_panel:
        out.append(r"\addlinespace[2pt]")
        out.append(f"\\multicolumn{{4}}{{l}}{{\\textit{{{PANEL_TITLE[r['panel']]}}}}} \\\\")
        last_panel = r['panel']
    lab = LABELS.get(r['outcome'], r['outcome'])
    out.append(f"\\quad {lab} & {cell(r['pool_b'], r['pool_pwcb'])} & "
               f"{cell(r['pp_b'], r['pp_pwcb'])} & {cell(r['pr_b'], r['pr_pwcb'])} \\\\")
out.append(r"\bottomrule")
out.append(r"\end{tabular}")
out.append(r"\begin{minipage}{\linewidth}")
out.append(
    r"\footnotesize \textit{Notes:} Each cell reports the 2SLS coefficient (identical to "
    r"Tables~\ref{tab:iv_mechanism} and~\ref{tab:iv_outcomes}) and, in parentheses, the wild-cluster-bootstrap $p$-value for the "
    r"null that the coefficient is zero. Bootstrap clustering is on the six counselors using Webb "
    r"six-point weights and $9{,}999$ replications \citep{cameron2008,mackinnon2017wild}; subgroup "
    r"effects are recovered as direct coefficients from a fully-interacted reparameterization. "
    r"Because the Counselor Quality Tendency instrument varies essentially at the counselor level, "
    r"with only six clusters the wild-cluster bootstrap is conservative and cannot separately "
    r"distinguish the counselor-level mechanism channels (Panels~A and~B) from cluster-level noise, "
    r"even where the point estimates are large; the family-planning take-up effects in Panel~C "
    r"nonetheless remain significant under this most conservative small-sample procedure. We "
    r"therefore treat the wild-cluster bootstrap as a complement to, not a substitute for, the "
    r"woman-level robust standard errors, the leave-one-counselor-out exercise, and the "
    r"summary-index aggregation, on which our inference primarily rests. Stars: $^{*}$ p$<$0.10, "
    r"$^{**}$ p$<$0.05, $^{***}$ p$<$0.01."
)
out.append(r"\end{minipage}")
out.append(r"\end{table}")

with open('tables/iv_wildbootstrap.tex', 'w') as f:
    f.write('\n'.join(out) + '\n')
print("Wrote tables/iv_wildbootstrap.tex")
