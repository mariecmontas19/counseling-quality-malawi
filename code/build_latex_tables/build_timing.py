"""Build the timing-mechanism LaTeX table in the FLIPPED layout (outcomes as
columns, estimates/SE/N/Mean DV as rows), matching the format of Tables 9 and 10.

Two panels stacked:
  Panel A: Postpartum women, split by months postpartum at baseline (recent vs older)
  Panel B: Pregnant women, split by months until delivery at baseline (3rd-tri vs earlier)
"""
import csv

with open('/tmp/phase6b_results.csv') as f:
    reader = csv.DictReader(f)
    rows = list(reader)

for r in rows:
    for k in ['grp','b','se','p','n','hq_mn','lq_mn']:
        try: r[k] = float(r[k])
        except: r[k] = None

idx = {(r['panel'], r['outcome'], int(r['grp'])): r for r in rows}

# Subgroup overall means (computed separately in /tmp/subgroup_means_table11.csv)
sub_means = {}
with open('/tmp/subgroup_means_table11.csv') as f:
    for r in csv.DictReader(f):
        sub_means[r['outcome']] = {
            k: float(r[k]) for k in ('recent_pp','older_pp','third_tri_preg','earlier_preg')
        }

# Outcome columns: same first five outcomes for both panels, last one differs
# (birth_within_24mo for PP, preg_within_24mo for Preg). We use a single column
# per panel for the "outcome-specific" final column.
COMMON_OUTCOMES = [
    ('admin_fp_any',          r'\makecell{Any FP \\ service}'),
    ('admin_inject_any',      r'\makecell{Any \\ injectable}'),
    ('n_admin_fp',            r'\makecell{Number of \\ FP services}'),
    ('fp_inj_coverage_frac',  r'\makecell{Injectable \\ coverage}'),
    ('fp_inj_protected_day',  r'\makecell{Inj.\ protected \\ days}'),
]
PP_LAST_OUTCOME   = ('birth_within_24mo',  r'\makecell{Birth within \\ 24 months}')
PREG_LAST_OUTCOME = ('preg_within_24mo',   r'\makecell{Pregnancy within \\ 24 months}')

PP_OUTCOMES   = COMMON_OUTCOMES + [PP_LAST_OUTCOME]
PREG_OUTCOMES = COMMON_OUTCOMES + [PREG_LAST_OUTCOME]
N_COLS = len(PP_OUTCOMES)  # 6

def stars(p):
    if p is None: return ''
    if p < 0.01: return '$^{***}$'
    if p < 0.05: return '$^{**}$'
    if p < 0.10: return '$^{*}$'
    return ''

def fmt_b(b, p, dec=3):
    if b is None: return ''
    if abs(b) >= 10: dec = 1
    return f"{b:.{dec}f}{stars(p)}"

def fmt_se(se, dec=3):
    if se is None: return ''
    if abs(se) >= 10: dec = 1
    return f"({se:.{dec}f})"

def fmt_n(n):
    return f"{int(n)}" if n is not None else ''

def fmt_mn(m, dec=3):
    if m is None: return ''
    if abs(m) >= 10: dec = 1
    return f"{m:.{dec}f}"

def build_panel_block(panel_letter, panel_title,
                      subgroup_a_label, subgroup_a_grp_value, panel_data_key,
                      subgroup_b_label, subgroup_b_grp_value,
                      outcomes, mean_key_a, mean_key_b):
    """Emit the panel block: heading + estimate rows for both subgroups + N + Mean DV."""
    lines = []
    lines.append(f"\\multicolumn{{{N_COLS+1}}}{{l}}{{\\textit{{{panel_title}}}}} \\\\")
    # Subgroup A: estimate row + SE row
    row_b_a  = [f"\\quad {subgroup_a_label}: Estimate"]
    row_se_a = [r"\quad"]
    for outc, _ in outcomes:
        r = idx.get((panel_data_key, outc, subgroup_a_grp_value))
        if r is None:
            row_b_a.append('');  row_se_a.append('')
        else:
            row_b_a.append(fmt_b(r['b'], r['p']))
            row_se_a.append(fmt_se(r['se']))
    lines.append(" & ".join(row_b_a) + r" \\")
    lines.append(" & ".join(row_se_a) + r" \\")
    lines.append(r"\addlinespace[3pt]")
    # Subgroup B: estimate row + SE row
    row_b_b  = [f"\\quad {subgroup_b_label}: Estimate"]
    row_se_b = [r"\quad"]
    for outc, _ in outcomes:
        r = idx.get((panel_data_key, outc, subgroup_b_grp_value))
        if r is None:
            row_b_b.append('');  row_se_b.append('')
        else:
            row_b_b.append(fmt_b(r['b'], r['p']))
            row_se_b.append(fmt_se(r['se']))
    lines.append(" & ".join(row_b_b) + r" \\")
    lines.append(" & ".join(row_se_b) + r" \\")
    # Mean DV rows for each subgroup
    lines.append(r"\addlinespace[3pt]")
    row_mn_a = [f"\\quad Mean DV ({subgroup_a_label})"]
    row_mn_b = [f"\\quad Mean DV ({subgroup_b_label})"]
    for outc, _ in outcomes:
        m_a = sub_means.get(outc, {}).get(mean_key_a)
        m_b = sub_means.get(outc, {}).get(mean_key_b)
        row_mn_a.append(fmt_mn(m_a))
        row_mn_b.append(fmt_mn(m_b))
    lines.append(" & ".join(row_mn_a) + r" \\")
    lines.append(" & ".join(row_mn_b) + r" \\")
    # N rows omitted from the table body: same value across all admin outcomes
    # per subgroup, and the last-column outcome (birth/preg_within_24mo) is at
    # most a few observations smaller. Sample sizes are reported in the notes.
    return lines


def get_panel_n(panel_data_key, outcomes, grp_value):
    """Return (n_admin, n_last) for the panel's two distinct sample sizes."""
    n_admin = idx.get((panel_data_key, outcomes[0][0], grp_value), {}).get('n')
    n_last  = idx.get((panel_data_key, outcomes[-1][0], grp_value), {}).get('n')
    return int(n_admin) if n_admin else None, int(n_last) if n_last else None


out = []
out.append(r"\begin{table}[H]")
out.append(r"\centering")
out.append(r"\caption{Timing of FP Coverage: IV Heterogeneity by Baseline Reproductive Timing}")
out.append(r"\label{tab:iv_timing_mechanism}")
out.append(r"\small")
out.append(r"\setlength{\tabcolsep}{4pt}")
out.append(r"\renewcommand{\arraystretch}{1.05}")
out.append(r"\resizebox{\textwidth}{!}{%")
col_spec = "l" + ("c" * N_COLS)
out.append(r"\begin{tabular}{" + col_spec + "}")
out.append(r"\toprule")

# Column header row (outcome labels)
header_labels = [r'\makecell{Any FP \\ service}',
                 r'\makecell{Any \\ injectable}',
                 r'\makecell{Number of \\ FP services}',
                 r'\makecell{Injectable \\ coverage}',
                 r'\makecell{Inj.\ protected \\ days}',
                 r'\makecell{Birth within 24mo \\ \emph{or} Preg within 24mo \\ \scriptsize(panel-specific)}']
out.append(" & " + " & ".join(header_labels) + r" \\")
out.append(r"\midrule")

# === Panel A: PP timing split ===
out.extend(build_panel_block(
    panel_letter='A',
    panel_title='Panel A: Postpartum women, by months postpartum at baseline (last column = Birth within 24 months)',
    subgroup_a_label='Recent-PP ($\\le$2.9 mo)',
    subgroup_a_grp_value=0,
    panel_data_key='PP',
    subgroup_b_label='Older-PP ($>$2.9 mo)',
    subgroup_b_grp_value=1,
    outcomes=PP_OUTCOMES,
    mean_key_a='recent_pp',
    mean_key_b='older_pp',
))
out.append(r"\midrule")

# === Panel B: Preg timing split ===
out.extend(build_panel_block(
    panel_letter='B',
    panel_title='Panel B: Pregnant women, by months until delivery at baseline (last column = Pregnancy within 24 months)',
    subgroup_a_label='3rd-tri Preg ($\\le$2.6 mo)',
    subgroup_a_grp_value=0,
    panel_data_key='Preg',
    subgroup_b_label='Earlier-gest Preg ($>$2.6 mo)',
    subgroup_b_grp_value=1,
    outcomes=PREG_OUTCOMES,
    mean_key_a='third_tri_preg',
    mean_key_b='earlier_preg',
))

out.append(r"\bottomrule")
out.append(r"\end{tabular}")
out.append(r"}")  # close resizebox

# Compute sample sizes for the notes
n_a_pp_admin,   n_a_pp_last   = get_panel_n('PP',   PP_OUTCOMES,   0)
n_b_pp_admin,   n_b_pp_last   = get_panel_n('PP',   PP_OUTCOMES,   1)
n_a_preg_admin, n_a_preg_last = get_panel_n('Preg', PREG_OUTCOMES, 0)
n_b_preg_admin, n_b_preg_last = get_panel_n('Preg', PREG_OUTCOMES, 1)

out.append(r"\begin{minipage}{\linewidth}")
out.append(
    r"\footnotesize \textit{Notes:} Two-stage least squares estimates of higher counseling quality (sat+topics) "
    r"on each outcome, run separately within each baseline-timing subgroup. Median splits within each "
    r"pregnancy-status group: 2.9 months postpartum at baseline for Panel~A; 2.6 months until delivery for Panel~B. "
    r"Heteroskedasticity-robust standard errors in parentheses below each estimate. ``Mean DV'' rows report the "
    r"within-subgroup mean of the outcome among treatment women. The last outcome column is panel-specific: "
    r"\emph{birth within 24 months} in Panel~A, \emph{pregnancy within 24 months} in Panel~B. "
    r"Sample sizes per subgroup (same across columns except for the last reproductive column due to small "
    r"differences in missingness): "
    rf"Panel~A Recent-PP $N = {n_a_pp_admin}$, Older-PP $N = {n_b_pp_admin}$; "
    rf"Panel~B 3rd-tri Preg $N = {n_a_preg_admin}$ (Pregnancy-within-24-months $N = {n_a_preg_last}$), "
    rf"Earlier-gest Preg $N = {n_b_preg_admin}$ (Pregnancy-within-24-months $N = {n_b_preg_last}$). "
    r"Stars: $^{*}$ p$<$0.10, $^{**}$ p$<$0.05, $^{***}$ p$<$0.01."
)
out.append(r"\end{minipage}")
out.append(r"\end{table}")

with open('tables/iv_timing_mechanism.tex', 'w') as f:
    f.write('\n'.join(out) + '\n')
print('Wrote tables/iv_timing_mechanism.tex')
