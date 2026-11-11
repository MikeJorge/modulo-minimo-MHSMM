#!/usr/bin/env python3
"""
MM Development Architecture Figure v1.0.2

Targeted remediation of Figure v1.0.0 findings F01-F03 and publication-facing
alignment with the frozen Nature Computational Science figure requirements profile.

Deterministic Python/Matplotlib line-art generator.
No generative-image model or external image-generation service is used.

Outputs:
  - editable-text SVG
  - vector PDF
  - PNG fallback at 300 dpi
  - exact-byte manifest
"""

from __future__ import annotations

import argparse
import datetime as _dt
import hashlib
import json
import os
from pathlib import Path
import re

os.environ.setdefault("SOURCE_DATE_EPOCH", "1794355200")  # 2026-11-11 00:00:00 UTC

import matplotlib as mpl
mpl.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch, FancyArrowPatch, Rectangle

VERSION = "1.0.2"
FIGURE_ID = "MM-DEVELOPMENT-ARCHITECTURE-FIGURE"
WIDTH_MM = 180.0
HEIGHT_MM = 165.0
PNG_DPI = 300
FONT_MIN_PT = 5.0
FONT_MAX_PT = 7.0
TITLE = "Gate-governed scientific software development architecture of the Module Minimum (MM) system"

# Determinism and vector-text controls.
mpl.rcParams["svg.hashsalt"] = "mm-development-architecture-v1.0.2"
mpl.rcParams["svg.fonttype"] = "none"        # preserve SVG text as text
mpl.rcParams["pdf.fonttype"] = 42            # TrueType/selectable text
mpl.rcParams["pdf.use14corefonts"] = True    # Helvetica core-font semantics where feasible
mpl.rcParams["font.family"] = "sans-serif"
mpl.rcParams["font.sans-serif"] = ["DejaVu Sans"]
mpl.rcParams["savefig.transparent"] = False
mpl.rcParams["axes.unicode_minus"] = False

USED_FONT_SIZES = set()
USED_LINE_WIDTHS = set()

# Nature-facing accessible colour system.
# Colour is a secondary grouping cue only; labels, position, line style and borders
# remain the primary semantic carriers. Avoids red-green contrast and rainbow scales.
PALETTE = {
    "authority_fill": "#DCEAF7",
    "authority_edge": "#285A7A",
    "method_fill": "#E8EDF2",
    "method_edge": "#4B5563",
    "governance_fill": "#EAF2F8",
    "governance_edge": "#2F5D7C",
    "data_fill": "#DDF3F0",
    "data_edge": "#2A756D",
    "module_fill": "#FFF1CC",
    "module_edge": "#7A6518",
    "contract_fill": "#ECE6F6",
    "contract_edge": "#6B4C8A",
    "verification_fill": "#DCEAF7",
    "validation_fill": "#FBE7D4",
    "assurance_fill": "#E8E2F2",
    "runtime_fill": "#EFEFEF",
    "runtime_edge": "#4B4B4B",
    "lifecycle_fill": "#F7F7F7",
    "lifecycle_edge": "#5B5B5B",
    "historical_fill": "#F4F4F4",
    "historical_edge": "#6B7280",
}


def _font(size: float) -> float:
    USED_FONT_SIZES.add(float(size))
    return float(size)

def _lw(width: float) -> float:
    USED_LINE_WIDTHS.add(float(width))
    return float(width)

def add_box(ax, x, y, w, h, text, *, fontsize=5.4, weight="normal",
            rounded=True, lw=0.65, linestyle="-", align="center", zorder=2,
            facecolor="white", edgecolor="black"):
    if rounded:
        patch = FancyBboxPatch(
            (x, y), w, h,
            boxstyle="round,pad=0.8,rounding_size=1.1",
            linewidth=_lw(lw), edgecolor=edgecolor, facecolor=facecolor,
            linestyle=linestyle, zorder=zorder,
        )
    else:
        patch = Rectangle(
            (x, y), w, h,
            linewidth=_lw(lw), edgecolor=edgecolor, facecolor=facecolor,
            linestyle=linestyle, zorder=zorder,
        )
    ax.add_patch(patch)
    ha = {"left": "left", "center": "center", "right": "right"}[align]
    tx = x + (1.5 if align == "left" else (w - 1.5 if align == "right" else w / 2))
    ax.text(tx, y + h / 2, text, ha=ha, va="center",
            fontsize=_font(fontsize), fontweight=weight, zorder=zorder + 1,
            linespacing=1.12)
    return patch

def add_arrow(ax, x1, y1, x2, y2, *, lw=0.65, linestyle="-",
              mutation_scale=7.5, zorder=1, connectionstyle="arc3"):
    a = FancyArrowPatch(
        (x1, y1), (x2, y2),
        arrowstyle="-|>",
        mutation_scale=mutation_scale,
        linewidth=_lw(lw), linestyle=linestyle,
        color="black", shrinkA=1.0, shrinkB=1.0,
        zorder=zorder, connectionstyle=connectionstyle,
    )
    ax.add_patch(a)
    return a

def add_line(ax, x1, y1, x2, y2, *, lw=0.55, linestyle="-", zorder=1):
    ax.plot([x1, x2], [y1, y2], linewidth=_lw(lw), linestyle=linestyle,
            color="black", zorder=zorder)

def add_header(ax, x, y, text):
    ax.text(x, y, text, ha="left", va="center",
            fontsize=_font(6.6), fontweight="bold")

def render(outdir: Path) -> dict:
    global USED_FONT_SIZES, USED_LINE_WIDTHS
    USED_FONT_SIZES = set()
    USED_LINE_WIDTHS = set()



    outdir.mkdir(parents=True, exist_ok=True)
    fig = plt.figure(figsize=(WIDTH_MM / 25.4, HEIGHT_MM / 25.4), dpi=PNG_DPI)
    ax = fig.add_axes([0, 0, 1, 1])
    fig.patch.set_facecolor("white")
    ax.set_xlim(0, WIDTH_MM)
    ax.set_ylim(0, HEIGHT_MM)
    ax.axis("off")

    # Small in-asset identifier only; full title/caption lives in the external legend.
    ax.text(5, 160.6, "MM development architecture · v1.0.2",
            ha="left", va="center", fontsize=_font(6.8), fontweight="bold")
    ax.text(175, 160.6,
            "non-normative explanatory derivative · certified MM authorities control",
            ha="right", va="center", fontsize=_font(5.0))

    # ------------------------------------------------------------------
    # A. Superior authorities and controlled methodology
    # ------------------------------------------------------------------
    add_header(ax, 5, 154.6, "Superior certified authorities")
    authority_y = 143.0
    authority_h = 8.8
    authority_w = 52.0
    authority_x = [5.0, 64.0, 123.0]
    authority_labels = [
        "Scientific Execution Architecture\nv1.0.2",
        "Software Development & Assurance Manual\nv1.0.1 RC3",
        "Project Evidence Base\nv1.0.0",
    ]
    for x, label in zip(authority_x, authority_labels):
        add_box(ax, x, authority_y, authority_w, authority_h, label,
                fontsize=5.5, weight="bold", lw=0.75,
                facecolor=PALETTE["authority_fill"], edgecolor=PALETTE["authority_edge"])

    methodology_x, methodology_y, methodology_w, methodology_h = 58.0, 129.8, 64.0, 8.7
    add_box(ax, methodology_x, methodology_y, methodology_w, methodology_h,
            "Controlled Methodology v1.0.1\nhuman-readable baseline — non-normative",
            fontsize=5.5, weight="bold", lw=0.75,
            facecolor=PALETTE["method_fill"], edgecolor=PALETTE["method_edge"])

    # Authority convergence, stopping above the methodology box.
    junction_y = 140.0
    for x in [31.0, 90.0, 149.0]:
        add_line(ax, x, authority_y, x, junction_y, lw=0.55)
        add_line(ax, x, junction_y, 90.0, junction_y, lw=0.55)
    add_arrow(ax, 90.0, junction_y, 90.0, methodology_y + methodology_h + 0.6, lw=0.65)

    # ------------------------------------------------------------------
    # B. Execution-control governance + local adapter inset
    # ------------------------------------------------------------------
    gov_x, gov_y, gov_w, gov_h = 5.0, 69.0, 47.0, 53.0
    add_header(ax, gov_x, 125.5, "Execution-control governance")
    add_box(ax, gov_x, gov_y, gov_w, gov_h, "", rounded=False, lw=0.65,
            facecolor="#FFFFFF", edgecolor=PALETTE["governance_edge"])

    plane_h = 7.4
    plane_gap = 1.3
    py = gov_y + gov_h - plane_h - 2.0
    planes = [
        ("CONTROL PLANE", "Gates · scope · permissions · adjudication"),
        ("EXECUTION PLANE", "Explicitly authorized mutable work"),
        ("AUTHORITY PLANE", "Immutable identities · no implicit latest"),
        ("EVIDENCE / PROVENANCE", "Claims · receipts · readbacks · gaps"),
    ]
    for head, sub in planes:
        add_box(ax, gov_x + 2.0, py, gov_w - 4.0, plane_h,
                f"{head}\n{sub}", fontsize=5.05, weight="bold", lw=0.55,
                facecolor=PALETTE["governance_fill"], edgecolor=PALETTE["governance_edge"])
        py -= plane_h + plane_gap

    add_box(ax, gov_x + 2.0, 77.3, gov_w - 4.0, 7.5,
            "DATA / CATALOG GOVERNANCE\ncross-cutting layer\ncontracts · metadata · quality",
            fontsize=5.0, weight="bold", linestyle="--", lw=0.55,
            facecolor=PALETTE["data_fill"], edgecolor=PALETTE["data_edge"])

    # Historical adapter inset — explicitly not architecture.
    add_box(ax, gov_x + 2.0, 69.8, gov_w - 4.0, 6.5,
            "Local execution adapter — historical view\nWindows → WSL2 → /opt/mm\nadapter ≠ architecture",
            fontsize=5.0, linestyle="--", lw=0.5,
            facecolor=PALETTE["historical_fill"], edgecolor=PALETTE["historical_edge"])

    # ------------------------------------------------------------------
    # C. Bounded scientific modules
    # ------------------------------------------------------------------
    mod_x, mod_y, mod_w, mod_h = 57.0, 69.0, 63.0, 53.0
    add_header(ax, mod_x, 125.5, "Bounded scientific modules")
    add_box(ax, mod_x, mod_y, mod_w, mod_h, "", rounded=False, lw=0.65,
            facecolor="#FFFFFF", edgecolor=PALETTE["module_edge"])

    module_w, module_h = 27.2, 10.1
    module_xs = [60.2, 89.6]
    module_ys = [108.8, 95.9, 83.0]
    modules = [
        ("MM-ENVIRONMENT", "climate · soil\ngeospatial / GEE"),
        ("MM-PLANT", "DSSAT · forage\nplant response"),
        ("MM-ANIMAL", "production · biological\nconstraints"),
        ("MM-ECONOMIC", "economics · risk\ndecision outputs"),
        ("MM-INTEGRATION-CORE", "contract-bound\norchestration"),
        ("MM-DATA-CATALOG", "contracts · metadata\nprovenance"),
    ]
    for i, (head, sub) in enumerate(modules):
        col = i % 2
        row = i // 2
        add_box(ax, module_xs[col], module_ys[row], module_w, module_h,
                f"{head}\n{sub}", fontsize=5.15,
                weight="bold" if head == "MM-INTEGRATION-CORE" else "normal",
                linestyle="--" if head == "MM-DATA-CATALOG" else "-",
                lw=0.55,
                facecolor=PALETTE["data_fill"] if head == "MM-DATA-CATALOG" else PALETTE["module_fill"],
                edgecolor=PALETTE["data_edge"] if head == "MM-DATA-CATALOG" else PALETTE["module_edge"])

    add_box(ax, 60.2, 71.8, 56.6, 7.6,
            "BOUNDARY RULE\nmodule internal state ≠ inter-module API",
            fontsize=5.2, weight="bold", linestyle="--", lw=0.55,
            facecolor="#FFFFFF", edgecolor=PALETTE["module_edge"])

    # ------------------------------------------------------------------
    # D. Prescribed inter-module contract path
    # ------------------------------------------------------------------
    chain_x, chain_y, chain_w, chain_h = 125.0, 69.0, 50.0, 53.0
    add_header(ax, chain_x, 125.5, "Permitted inter-module path")
    add_box(ax, chain_x, chain_y, chain_w, chain_h, "", rounded=False, lw=0.65,
            facecolor="#FFFFFF", edgecolor=PALETTE["contract_edge"])

    chain_items = [
        "MODULE OUTPUT",
        "VERSIONED DATA CONTRACT",
        "CATALOG RECORD",
        "PROVENANCE / EVIDENCE BINDING",
        "INTEGRATION ADAPTER",
        "CONSUMER MODULE",
    ]
    ch = 6.3
    gap = 1.95
    cys = []
    y = 112.8
    for i, item in enumerate(chain_items):
        yy = y - i * (ch + gap)
        cys.append(yy)
        add_box(ax, chain_x + 4.0, yy, chain_w - 8.0, ch, item,
                fontsize=5.1, weight="bold" if i in (1, 3, 4) else "normal", lw=0.55,
                facecolor=PALETTE["contract_fill"], edgecolor=PALETTE["contract_edge"])
        if i < len(chain_items) - 1:
            add_arrow(ax, chain_x + chain_w / 2, yy,
                      chain_x + chain_w / 2, yy - gap + 0.2,
                      lw=0.55, mutation_scale=6.5)

    # F01 correction: module region connects ONLY to the chain entry point.
    # The arrow starts from the module container boundary and terminates at MODULE OUTPUT.
    entry_center_y = cys[0] + ch / 2
    add_arrow(ax, mod_x + mod_w, entry_center_y,
              chain_x + 4.0 - 1.2, entry_center_y,
              lw=0.75, mutation_scale=7.5)

    # Governance applies to both modules and interface path.
    add_arrow(ax, gov_x + gov_w, 105.0, mod_x - 1.2, 105.0, lw=0.6, mutation_scale=6.8)

    # ------------------------------------------------------------------
    # E. Evidence-producing lifecycle (3×3 serpentine, final-size readable)
    # ------------------------------------------------------------------
    add_header(ax, 5.0, 62.4, "Evidence-producing development lifecycle · every transition is Gate-governed")

    life_x = [5.0, 42.0, 79.0]
    box_w, box_h = 31.0, 8.0
    row_y = [50.4, 39.2, 28.0]
    stage_defs = [
        ("1  Scientific need /\nintended use", 0, 0),
        ("2  Requirements", 1, 0),
        ("3  Architecture &\ninterfaces", 2, 0),
        ("4  Detailed design", 2, 1),
        ("5  Implementation", 1, 1),
        ("6  Verification", 0, 1),
        ("7  Validation", 0, 2),
        ("8  Independent readback /\nadversarial assurance", 1, 2),
        ("9  Freeze / release\ncandidate", 2, 2),
    ]
    centers = {}
    for label, col, row in stage_defs:
        x, y = life_x[col], row_y[row]
        stage_fill = PALETTE["lifecycle_fill"]
        stage_edge = PALETTE["lifecycle_edge"]
        if label.startswith("6 "):
            stage_fill = PALETTE["verification_fill"]
            stage_edge = PALETTE["authority_edge"]
        elif label.startswith("7 "):
            stage_fill = PALETTE["validation_fill"]
            stage_edge = "#9A6324"
        elif label.startswith("8 "):
            stage_fill = PALETTE["assurance_fill"]
            stage_edge = PALETTE["contract_edge"]
        add_box(ax, x, y, box_w, box_h, label,
                fontsize=5.25,
                weight="bold" if label.startswith(("1 ", "6 ", "7 ", "8 ", "9 ")) else "normal",
                lw=0.55, facecolor=stage_fill, edgecolor=stage_edge)
        stage_num = int(label.split()[0])
        centers[stage_num] = (x, y, x + box_w, y + box_h)

    def gate_arrow(n1, n2, mode):
        a = centers[n1]; b = centers[n2]
        if mode == "right":
            x1, y1 = a[2], (a[1]+a[3])/2
            x2, y2 = b[0]-1.0, (b[1]+b[3])/2
            gx, gy = (x1+x2)/2, y1+1.8
        elif mode == "left":
            x1, y1 = a[0], (a[1]+a[3])/2
            x2, y2 = b[2]+1.0, (b[1]+b[3])/2
            gx, gy = (x1+x2)/2, y1+1.8
        elif mode == "down":
            x1, y1 = (a[0]+a[2])/2, a[1]
            x2, y2 = (b[0]+b[2])/2, b[3]+0.8
            gx, gy = x1+3.0, (y1+y2)/2
        else:
            raise ValueError(mode)
        add_arrow(ax, x1, y1, x2, y2, lw=0.55, mutation_scale=6.3)
        ax.text(gx, gy, "Gate", ha="center", va="center",
                fontsize=_font(5.0), fontweight="bold")

    gate_arrow(1,2,"right")
    gate_arrow(2,3,"right")
    gate_arrow(3,4,"down")
    gate_arrow(4,5,"left")
    gate_arrow(5,6,"left")
    gate_arrow(6,7,"down")
    gate_arrow(7,8,"right")
    gate_arrow(8,9,"right")

    # V&V semantics, separated from runtime boundary.
    vv_x, vv_w = 116.0, 59.0
    add_box(ax, vv_x, 47.2, vv_w, 10.4,
            "VERIFICATION\nDid we build the system correctly?",
            fontsize=5.25, weight="bold", linestyle="--", lw=0.55,
            facecolor=PALETTE["verification_fill"], edgecolor=PALETTE["authority_edge"])
    add_box(ax, vv_x, 34.4, vv_w, 10.4,
            "VALIDATION\nDid we build the correct system\nfor intended scientific use?",
            fontsize=5.1, weight="bold", linestyle="--", lw=0.55,
            facecolor=PALETTE["validation_fill"], edgecolor="#9A6324")
    add_box(ax, vv_x, 24.8, vv_w, 6.8,
            "Independent MM assurance ≠ formal external IV&V",
            fontsize=5.0, linestyle="--", lw=0.5,
            facecolor=PALETTE["assurance_fill"], edgecolor=PALETTE["contract_edge"])

    # ------------------------------------------------------------------
    # F. Runtime authorization boundary — separate from assurance region
    # ------------------------------------------------------------------
    runtime_y = 7.2
    runtime_h = 13.3
    add_box(ax, 5.0, runtime_y, 170.0, runtime_h, "", rounded=False, lw=0.8,
            facecolor=PALETTE["runtime_fill"], edgecolor=PALETTE["runtime_edge"])
    ax.text(90.0, runtime_y + runtime_h - 3.3,
            "FROZEN / CERTIFIED  ≠  RUNTIME AUTHORIZED",
            ha="center", va="center", fontsize=_font(6.3), fontweight="bold")

    runtime_labels = [
        "local verification",
        "primary scientific runtime",
        "oracle runtime",
        "GEE runtime",
        "production runtime",
    ]
    rw = 30.4
    rx = 10.0
    for label in runtime_labels:
        add_box(ax, rx, runtime_y + 1.5, rw, 5.2, label,
                fontsize=5.0, linestyle="--", lw=0.5,
                facecolor="#FFFFFF", edgecolor=PALETTE["runtime_edge"])
        rx += 32.4

    # Footer identity only.
    ax.text(175.0, 3.6, f"{FIGURE_ID} v{VERSION}",
            ha="right", va="center", fontsize=_font(5.0))

    # ------------------------------------------------------------------
    # Export at exact final dimensions. Do NOT use bbox_inches='tight'.
    # ------------------------------------------------------------------
    svg_path = outdir / "mm-development-architecture-v1.0.2.svg"
    pdf_path = outdir / "mm-development-architecture-v1.0.2.pdf"
    png_path = outdir / "mm-development-architecture-v1.0.2.png"

    fixed_dt = _dt.datetime(2026, 11, 11, 0, 0, 0, tzinfo=_dt.timezone.utc)
    common_meta = {
        "Title": TITLE,
        "Creator": "MM deterministic Python/Matplotlib figure generator",
        "Subject": "Non-normative explanatory derivative; certified MM authorities control.",
    }

    fig.savefig(svg_path, format="svg", metadata={
        "Title": TITLE,
        "Creator": "MM deterministic Python/Matplotlib figure generator",
        "Description": "Non-normative explanatory derivative; certified MM authorities control.",
        "Date": "2026-11-11",
    })
    fig.savefig(pdf_path, format="pdf", metadata={
        **common_meta,
        "CreationDate": fixed_dt,
        "ModDate": fixed_dt,
        "Producer": "MM deterministic Python/Matplotlib figure generator",
    })
    fig.savefig(png_path, format="png", dpi=PNG_DPI, metadata={
        "Software": "MM deterministic Python/Matplotlib figure generator",
        "Title": TITLE,
    })
    plt.close(fig)

    # Publication-safe SVG font declaration while preserving text as text.
    svg = svg_path.read_text(encoding="utf-8")
    svg = svg.replace("DejaVu Sans", "Arial, Helvetica, sans-serif")
    svg_path.write_text(svg, encoding="utf-8", newline="\n")

    def sha256(path: Path) -> str:
        h = hashlib.sha256()
        with path.open("rb") as f:
            for chunk in iter(lambda: f.read(1024 * 1024), b""):
                h.update(chunk)
        return h.hexdigest()

    # Structural readback for exact geometry and text/vector properties.
    svg_text = svg_path.read_text(encoding="utf-8")
    width_match = re.search(r'width="([0-9.]+)pt"', svg_text)
    height_match = re.search(r'height="([0-9.]+)pt"', svg_text)
    svg_width_pt = float(width_match.group(1)) if width_match else None
    svg_height_pt = float(height_match.group(1)) if height_match else None
    pt_to_mm = 25.4 / 72.0

    manifest = {
        "figure_id": FIGURE_ID,
        "version": VERSION,
        "title": TITLE,
        "generator": "Python + Matplotlib",
        "generative_ai_image_used": False,
        "nature_profile": {
            "target_width_mm": WIDTH_MM,
            "target_height_mm": HEIGHT_MM,
            "standard_label_font_pt_min": FONT_MIN_PT,
            "standard_label_font_pt_max": FONT_MAX_PT,
            "png_dpi": PNG_DPI,
        },

        "colour_accessibility": {
            "rgb": True,
            "red_green_semantic_dependence": False,
            "rainbow_scale_used": False,
            "colour_is_redundant_grouping_cue": True,
            "black_text_on_light_fills": True,
            "palette": PALETTE,
        },
        "generator_observed": {
            "used_font_sizes_pt": sorted(USED_FONT_SIZES),
            "used_line_widths_pt": sorted(USED_LINE_WIDTHS),
        },
        "svg_structural_readback": {
            "width_pt": svg_width_pt,
            "height_pt": svg_height_pt,
            "width_mm": svg_width_pt * pt_to_mm if svg_width_pt else None,
            "height_mm": svg_height_pt * pt_to_mm if svg_height_pt else None,
            "text_elements": svg_text.count("<text"),
            "image_elements": svg_text.count("<image"),
            "path_elements": svg_text.count("<path"),
            "publication_font_stack_declared": "Arial, Helvetica, sans-serif" in svg_text,
        },
        "outputs": {
            svg_path.name: {"bytes": svg_path.stat().st_size, "sha256": sha256(svg_path)},
            pdf_path.name: {"bytes": pdf_path.stat().st_size, "sha256": sha256(pdf_path)},
            png_path.name: {"bytes": png_path.stat().st_size, "sha256": sha256(png_path)},
        },
        "targeted_findings": {
            "F01": "connector enters contract chain at MODULE OUTPUT only",
            "F02": "IV&V non-claim contained in assurance region; runtime boundary spatially separate",
            "F03": "authored directly at 180 x 165 mm with standard labels 5-7 pt",
        },
    }

    manifest_path = outdir / "MM_DEVELOPMENT_ARCHITECTURE_FIGURE_MANIFEST_v1.0.2.json"
    manifest_path.write_text(json.dumps(manifest, indent=2, ensure_ascii=False) + "\n", encoding="utf-8", newline="\n")
    return manifest

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--outdir", default=".")
    args = parser.parse_args()
    manifest = render(Path(args.outdir))
    print(json.dumps(manifest, indent=2, ensure_ascii=False))

if __name__ == "__main__":
    main()
