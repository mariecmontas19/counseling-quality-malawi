"""Rebuild LaTeX tables with full labels via \\makecell{...\\\\...} 2-line headers."""
import csv

with open('/tmp/phase5_v2_results.csv') as f:
    reader = csv.DictReader(f)
    rows = list(reader)

for r in rows:
    for k in ['pool_b','pool_se','pool_p','pp_b','pp_se','pp_p','pr_b','pr_se','pr_p','pdiff','n','mn']:
        try: r[k] = float(r[k])
        except: r[k] = None

by_outcome = {r['outcome']: r for r in rows}

# Merge in subgroup means (PP and Preg) for each outcome
sub_means = {}
with open('/tmp/subgroup_means_table9_10.csv') as f:
    for row in csv.DictReader(f):
        sub_means[row['outcome']] = {
            'mn_pp':   float(row['mn_pp'])   if row['mn_pp']   not in ('', '.') else None,
            'mn_preg': float(row['mn_preg']) if row['mn_preg'] not in ('', '.') else None,
        }
for outc, r in by_outcome.items():
    r['mn_pp']   = sub_means.get(outc, {}).get('mn_pp')
    r['mn_preg'] = sub_means.get(outc, {}).get('mn_preg')

PANELS = {
    'A': {'name': 'Counseling continuity',
          'outcomes': ['total_sessions','share_same_counselor','visit_rate_mo','husband_present'],
          'labels': [
              r'\makecell{Total \\ sessions}',
              r'\makecell{Same \\ counselor}',
              r'\makecell{Couns sessions \\ per month}',
              r'\makecell{Husband \\ present}',
          ]},
    'B': {'name': 'Counseling topics (post-1st-session)',
          'outcomes': ['etp1_fp','etp1_spacing','etp1_preg','etp1_partner','etp1_childhealth','etp1_breast'],
          'labels': [
              r'\makecell{FP \\ information}',
              r'\makecell{Birth \\ spacing}',
              r'\makecell{Pregnancy \\ info}',
              r'\makecell{Partner \\ engagement}',
              r'\makecell{Child \\ health}',
              r'Breastfeeding',
          ]},
    'C': {'name': 'FP coverage',
          'outcomes': ['admin_fp_any','admin_inject_any','n_admin_fp','fp_inj_coverage_frac','husband_knows_fp'],
          'labels': [
              r'\makecell{Any FP \\ service}',
              r'\makecell{Any \\ injectable}',
              r'\makecell{Number of \\ FP services}',
              r'\makecell{Injectable \\ coverage}',
              r'\makecell{Husband \\ knows FP}',
          ]},
    'D': {'name': 'Reproductive',
          'outcomes': ['birth_within_24mo','preg_within_12mo','preg_within_24mo','mistimed_preg_expost'],
          'labels': [
              r'\makecell{Birth within \\ 24 months}',
              r'\makecell{Pregnancy within \\ 12 months}',
              r'\makecell{Pregnancy within \\ 24 months}',
              r'\makecell{Mistimed \\ pregnancy}',
          ]},
}

def stars(p):
    if p is None: return ''
    if p < 0.01: return '$^{***}$'
    if p < 0.05: return '$^{**}$'
    if p < 0.10: return '$^{*}$'
    return ''

def fmt_b(b, p, dec=3):
    if b is None: return ''
    return f"{b:.{dec}f}{stars(p)}"

def fmt_se(se, dec=3):
    if se is None: return ''
    return f"({se:.{dec}f})"

def fmt_p(p, dec=3):
    return f"{p:.{dec}f}" if p is not None else ''

def fmt_n(n):
    return f"{int(n):,}" if n is not None else ''

def fmt_mn(m, dec=3):
    return f"{m:.{dec}f}" if m is not None else ''

def build_table(panel_keys, table_label, table_caption, outfile, landscape=False):
    all_outcomes = []
    all_labels = []
    panel_breaks = []
    for pk in panel_keys:
        p = PANELS[pk]
        panel_breaks.append((pk, len(p['outcomes']), p['name']))
        all_outcomes.extend(p['outcomes'])
        all_labels.extend(p['labels'])
    n_cols = len(all_outcomes)

    out = []
    if landscape:
        out.append(r"\begin{sidewaystable}")
    else:
        out.append(r"\begin{table}[H]")
    out.append(r"\centering")
    out.append(r"\caption{" + table_caption + "}")
    out.append(r"\label{" + table_label + "}")
    if landscape:
        out.append(r"\footnotesize")
        out.append(r"\setlength{\tabcolsep}{4pt}")
        out.append(r"\renewcommand{\arraystretch}{1.05}")
    else:
        out.append(r"\small")
        out.append(r"\setlength{\tabcolsep}{5pt}")
        out.append(r"\renewcommand{\arraystretch}{1.05}")
    col_spec = "l" + ("c" * n_cols)
    if landscape:
        out.append(r"\resizebox{\linewidth}{!}{%")
    else:
        out.append(r"\resizebox{\textwidth}{!}{%")
    out.append(r"\begin{tabular}{" + col_spec + "}")
    out.append(r"\toprule")

    header_parts = []
    for pk, ncol, pname in panel_breaks:
        header_parts.append(f"\\multicolumn{{{ncol}}}{{c}}{{\\emph{{Panel {pk}: {pname}}}}}")
    out.append(" & " + " & ".join(header_parts) + r" \\")

    cmidrules = []
    cum = 2
    for pk, ncol, _ in panel_breaks:
        cmidrules.append(f"\\cmidrule(lr){{{cum}-{cum+ncol-1}}}")
        cum += ncol
    out.append(" ".join(cmidrules))

    out.append(" & " + " & ".join(all_labels) + r" \\")
    out.append(r"\midrule")

    def block(label, b_key, se_key, p_key):
        out.append(f"\\multicolumn{{{n_cols+1}}}{{l}}{{\\textit{{{label}}}}} \\\\")
        row_b = ["\\quad Estimate"]
        row_se = ["\\quad"]
        for o in all_outcomes:
            r = by_outcome[o]
            row_b.append(fmt_b(r[b_key], r[p_key]))
            row_se.append(fmt_se(r[se_key]))
        out.append(" & ".join(row_b) + r" \\")
        out.append(" & ".join(row_se) + r" \\")

    block("Pooled", 'pool_b', 'pool_se', 'pool_p')
    out.append(r"\addlinespace[3pt]")
    block("Postpartum (preg=0)", 'pp_b', 'pp_se', 'pp_p')
    out.append(r"\addlinespace[3pt]")
    block("Pregnant (preg=1)", 'pr_b', 'pr_se', 'pr_p')

    row_diff = ["\\quad p(PP=Preg)"]
    for o in all_outcomes:
        row_diff.append(fmt_p(by_outcome[o]['pdiff']))
    out.append(" & ".join(row_diff) + r" \\")
    out.append(r"\midrule")

    row_n      = ["N"]
    row_mn     = ["Mean DV (pooled)"]
    row_mn_pp  = ["Mean DV (PP)"]
    row_mn_pr  = ["Mean DV (Preg)"]
    for o in all_outcomes:
        r = by_outcome[o]
        row_n.append(fmt_n(r['n']))
        row_mn.append(fmt_mn(r['mn']))
        row_mn_pp.append(fmt_mn(r.get('mn_pp')))
        row_mn_pr.append(fmt_mn(r.get('mn_preg')))
    out.append(" & ".join(row_n) + r" \\")
    out.append(" & ".join(row_mn) + r" \\")
    out.append(" & ".join(row_mn_pp) + r" \\")
    out.append(" & ".join(row_mn_pr) + r" \\")

    out.append(r"\bottomrule")
    out.append(r"\end{tabular}")
    out.append(r"}")  # close resizebox
    out.append(r"\begin{minipage}{\linewidth}" if landscape else r"\begin{minipage}{0.9\linewidth}")
    out.append(r"\footnotesize \textit{Notes:} Two-stage least squares estimates of higher counseling quality on each outcome. Endogenous variable: an above-median indicator for the woman's average satisfaction-and-topics quality across all attended sessions. Instrument: the leave-one-out, scheduling-residualized Counselor Quality Tendency (CQT) of the woman's first counselor. Heteroskedasticity-robust standard errors in parentheses below each estimate. The row \emph{p(PP=Preg)} reports the p-value testing equality of the postpartum and pregnant subgroup effects. Stars: $^{*}$ p$<$0.10, $^{**}$ p$<$0.05, $^{***}$ p$<$0.01.")
    out.append(r"\end{minipage}")
    if landscape:
        out.append(r"\end{sidewaystable}")
    else:
        out.append(r"\end{table}")

    with open(outfile, 'w') as f:
        f.write('\n'.join(out) + '\n')
    print(f"Wrote {outfile}")

def build_subtabular(panel_keys):
    """Build only the tabular block (no \\begin{table}, no caption, no notes).
    Used inside build_stacked_table() to stack two sub-tables under one caption."""
    all_outcomes = []
    all_labels = []
    panel_breaks = []
    for pk in panel_keys:
        p = PANELS[pk]
        panel_breaks.append((pk, len(p['outcomes']), p['name']))
        all_outcomes.extend(p['outcomes'])
        all_labels.extend(p['labels'])
    n_cols = len(all_outcomes)

    out = []
    col_spec = "l" + ("c" * n_cols)
    out.append(r"\resizebox{\textwidth}{!}{%")
    out.append(r"\begin{tabular}{" + col_spec + "}")
    out.append(r"\toprule")

    header_parts = []
    for pk, ncol, pname in panel_breaks:
        header_parts.append(f"\\multicolumn{{{ncol}}}{{c}}{{\\emph{{Panel {pk}: {pname}}}}}")
    out.append(" & " + " & ".join(header_parts) + r" \\")

    cmidrules = []
    cum = 2
    for pk, ncol, _ in panel_breaks:
        cmidrules.append(f"\\cmidrule(lr){{{cum}-{cum+ncol-1}}}")
        cum += ncol
    out.append(" ".join(cmidrules))

    out.append(" & " + " & ".join(all_labels) + r" \\")
    out.append(r"\midrule")

    def block(label, b_key, se_key, p_key):
        out.append(f"\\multicolumn{{{n_cols+1}}}{{l}}{{\\textit{{{label}}}}} \\\\")
        row_b = ["\\quad Estimate"]
        row_se = ["\\quad"]
        for o in all_outcomes:
            r = by_outcome[o]
            row_b.append(fmt_b(r[b_key], r[p_key]))
            row_se.append(fmt_se(r[se_key]))
        out.append(" & ".join(row_b) + r" \\")
        out.append(" & ".join(row_se) + r" \\")

    block("Pooled", 'pool_b', 'pool_se', 'pool_p')
    out.append(r"\addlinespace[2pt]")
    block("Postpartum (preg=0)", 'pp_b', 'pp_se', 'pp_p')
    out.append(r"\addlinespace[2pt]")
    block("Pregnant (preg=1)", 'pr_b', 'pr_se', 'pr_p')

    row_diff = ["\\quad p(PP=Preg)"]
    for o in all_outcomes:
        row_diff.append(fmt_p(by_outcome[o]['pdiff']))
    out.append(" & ".join(row_diff) + r" \\")
    out.append(r"\midrule")

    row_n   = ["N"]
    row_mn  = ["Mean DV (pooled)"]
    for o in all_outcomes:
        r = by_outcome[o]
        row_n.append(fmt_n(r['n']))
        row_mn.append(fmt_mn(r['mn']))
    out.append(" & ".join(row_n) + r" \\")
    out.append(" & ".join(row_mn) + r" \\")

    out.append(r"\bottomrule")
    out.append(r"\end{tabular}")
    out.append(r"}")  # close resizebox
    return out


def build_stacked_table(top_panels, bottom_panels, table_label, table_caption, outfile):
    """Build ONE table environment with two tabular blocks stacked vertically.
    Used to render Table 5 as 'mechanism on top, behavioral outcomes on bottom'."""
    out = []
    out.append(r"\begin{table}[H]")
    out.append(r"\centering")
    out.append(r"\caption{" + table_caption + "}")
    out.append(r"\label{" + table_label + "}")
    out.append(r"\footnotesize")
    out.append(r"\setlength{\tabcolsep}{4pt}")
    out.append(r"\renewcommand{\arraystretch}{0.92}")

    out.extend(build_subtabular(top_panels))
    out.append(r"")
    out.append(r"\medskip")
    out.append(r"")
    out.extend(build_subtabular(bottom_panels))

    out.append(r"\begin{minipage}{\linewidth}")
    out.append(r"\scriptsize \textit{Notes:} Two-stage least squares estimates of higher counseling quality on each outcome. The upper block reports the mechanism-channel panels (Panels~A and~B); the lower block reports the behavioral-outcome panels (Panels~C and~D). Endogenous variable: an above-median indicator for the woman's average satisfaction-and-topics quality across all attended sessions. Instrument: the leave-one-out, scheduling-residualized Counselor Quality Tendency (CQT) of the woman's first counselor. Heteroskedasticity-robust standard errors in parentheses below each estimate. The row \emph{p(PP=Preg)} reports the p-value testing equality of the postpartum and pregnant subgroup effects. Stars: $^{*}$ p$<$0.10, $^{**}$ p$<$0.05, $^{***}$ p$<$0.01.")
    out.append(r"\end{minipage}")
    out.append(r"\end{table}")

    with open(outfile, 'w') as f:
        f.write('\n'.join(out) + '\n')
    print(f"Wrote {outfile}")


build_table(['A','B'], 'tab:iv_mechanism',
            'IV Estimates: Counseling Quality on Mechanism Outcomes (Continuity and Topics)',
            'tables/iv_main_flipped_mechanism.tex')
build_table(['C','D'], 'tab:iv_outcomes',
            'IV Estimates: Counseling Quality on FP Coverage and Reproductive Outcomes',
            'tables/iv_main_flipped_outcomes.tex')
build_stacked_table(['A','B'], ['C','D'], 'tab:iv_main',
            'Main IV Results: Counseling Quality on Mechanism Channels and Behavioral Outcomes',
            'tables/iv_main_combined.tex')
# (Legacy single-landscape variant retained for reference; not used in main text)
build_table(['A','B','C','D'], 'tab:iv_main_landscape',
            'Main IV Results: Counseling Quality on Mechanism Channels and Behavioral Outcomes (Landscape)',
            'tables/iv_main_landscape.tex', landscape=True)
