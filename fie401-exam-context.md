# Context: FIE401 Hjemmeeksamen — Markedsfragmentering og aksjelikviditet

**Last updated:** 2026-05-12
**Status:** Under arbeid — R-kode revidert og fikset, LaTeX-rapport gjenstår

---

## Overview

Hjemmeeksamen i FIE401 (NHH, vår 2026). Gruppebasert, 3–4 personer.
Innlevering: 13. mai 2026 kl. 14:00 via WiseFlow.
Oral eksamen: 2.–4. juni 2026 (5 min presentasjon + 20 min spørsmål).
Innleveres som: R-fil (.R), rapport (.pdf), presentasjon (.pdf).

---

## Goals / Research Questions

**Hovedspørsmål:** How does market fragmentation affect stock's liquidity?

**Delspørsmål (fra eksamensoppgaven):**
- Tabell 1: Beskriv data og variabler
- Tabell 2: Er det forskjell mellom SMI og CAC40/DAX30 FØR hendelsen?
- Tabell 3: Hadde konsolideringen effekt på aksjelikviditet?
- Diskuter uobserverte konfunderende faktorer
- Diskuter fundamentale forskjeller mellom SMI og CAC40/DAX30

---

## Empirisk strategi

**Metode:** Difference-in-Differences (DiD) med two-way fixed effects
- **Behandlingsgruppe:** SMI (25 sveitsiske aksjer)
- **Kontrollgruppe:** CAC40 (51) + DAX30 (47 aksjer)
- **Hendelse 1:** 1. juli 2019 — EU-ekvivalens utløper → konsolidering
- **Hendelse 2:** 8. februar 2021 — UK gjeninnfører ekvivalens → re-fragmentering
- **Avhengig variabel:** Bid-ask spread = (ask−bid)/midpris
- **DiD-variabel:** SMI_post = SMI × post (1 kun for SMI etter 1. juli 2019)
- **Fixed effects:** Firma-FE (fjerner tidsinvariante forskjeller) + Tid-FE (fjerner felles sjokk)
- **Standardfeil:** Klynget per aksje (cluster = "group")

---

## Current Status

- ✅ Pakker lastet (require)
- ✅ Data lastet inn (read.csv, setwd via rstudioapi)
- ✅ Datoer parset (DD/MM/YYYY format)
- ✅ isins renset (junk-rad fjernet)
- ✅ Merge (firm_info + isins + chf_eur)
- ✅ Datarensing (volum, negative priser, stock filter ≥100 obs, pris ≥1)
- ✅ Valutakonvertering (CHF → EUR for SMI)
- ✅ Bid-ask spread beregnet
- ✅ Kontrollvariabler (log_mcap, turnover, btm, leverage)
- ✅ DiD-variabler (SMI, post, SMI_post)
- ✅ Winsorize (0.5%/99.5%)
- ✅ Tabell 1 — describe() + writeLines → Table1.tex (ett panel m/ Panel A: SMI, Panel B: CAC40/DAX30)
- ✅ Tabell 2 — OLS pre-periode (var ~ SMI) med vcovCL clustret per ISIN → Table2.tex
- ✅ Tabell 3 — DiD plm() two-way FE + vcovHC clustret per group → Table3.tex
- ✅ DAX30 volumkorreksjon lagt inn (×10, linje 58)
- ✅ Amihud-koeffisienter rettet (10× lavere etter volumfiks)
- 🔲 Mangler kode for Table_Robustness.tex i Solution.R (KRITISK — filen finnes men har ingen R-kode)
- 🔲 Tabell 1 er i Analysis-seksjonen — skal være i Data-seksjonen iflg. oppgaven
- 🔲 LaTeX-rapport (Abstract, Intro, Data, Analysis, Conclusion — kun section-stubs)
- 🔲 Presentasjon (.pdf)

---

## Progress Log

| Date | Update |
|------|--------|
| 2026-05-11 | Startet eksamen, leste oppgaven og etablerte empirisk strategi |
| 2026-05-11 | Gjennomgikk forelesningsmateriell (Week 11 DiD, Week 8 Panel, 2020-fasit) |
| 2026-05-11 | Bygget Solution.R: datarensing, variabler, winsorize, Tabell 1 |
| 2026-05-11 | Fullførte Tabell 2 (pre-periode OLS) og Tabell 3 (DiD plm) i Solution.R |
| 2026-05-12 | Gjennomgikk Solution.R mot eksamensoppgaven — identifiserte feil og mangler |
| 2026-05-12 | Lagt inn DAX30 volumkorreksjon (×10) per forelesers e-post — påvirker amihud og turnover |
| 2026-05-12 | Reviderte Tabell 1: erstattet Table1A/B.tex med én kombinert Table1.tex via writeLines |
| 2026-05-12 | Vurderte log(Amihud) — forkastet, foreleseren bruker råverdier i DiD-regresjoner |
| 2026-05-12 | Verifiserte at DescTools::Winsorize val= fungerer korrekt i installert versjon |

---

## Decisions Made

- **Likviditetsmål:** Bid-ask spread (primær) + Amihud illiquidity (robusthet, modell 5–6 i Tabell 3)
- **Amihud — ingen log-transformasjon:** Foreleseren bruker råverdier i DiD-regresjoner (QTE_DVOL rå i 2020-fasit, AVOL rå i uke 11-lab). Log er teorietisk forsvarlig men bryter med kurskonvensjonen.
- **Winsorize:** 0.5%/99.5% — følger 2020-fasiten
- **Stock filter:** ≥100 handelsdager og snittpris ≥1 EUR — unngår penny stocks og datafeil
- **Valutakurs:** CHF/pris delt på chf_eur (CHF per EUR) for å konvertere til EUR
- **Post-dummy:** 1. juli 2019 (Event 1 - konsolidering)
- **Kontrollvariabler:** log_mcap, turnover, btm, leverage
- **Clustered SE Tabell 2:** vcovCL(cluster = df_pre$ISIN) — lm-modeller krever direkte vektor
- **Clustered SE Tabell 3:** vcovHC(cluster = "group", type = "sss") — identisk med 2020-fasit
- **Tabell 2 struktur:** Kun OLS-regresjoner (var ~ SMI i pre-perioden) — ingen separate describe()-tabeller
- **Tabell 3 struktur:** To modeller — uten og med kontrollvariabler, følger 2020-fasitens stil
- **reformulate():** Brukes istedenfor as.formula(paste()) for å unngå stargazer NA-feil

---

## Open Questions / Blockers

- **KRITISK:** Kode for Table_Robustness.tex mangler i Solution.R — filen finnes i LaTeX men er ikke reproduserbar
- Tabell 1 bør flyttes til Data-seksjonen (nå i Analysis) — krever LaTeX-endring i seksjonsfiler
- LaTeX-rapport ikke startet — innlevering 13. mai kl. 14:00

---

## Key Files & Resources

- `Exam/Solution.R` — R-kode (komplett)
- `Exam/Latex/Inputs/Table1.tex` — Tabell 1, kombinert panel (genereres fra R)
- `Exam/Latex/Inputs/Table2.tex` — Tabell 2, pre-periode sammenligning
- `Exam/Latex/Inputs/Table3.tex` — Tabell 3, DiD-regresjon
- `Exam/FIE401 Hjemmeeksamen (12t +) V.2026.pdf` — eksamensoppgaven
- `Exam/FIE401 exam attach 2 firm_info.csv` — daglige aksjedata (92 565 rader)
- `Exam/FIE401 exam attach 1 chf_eur.csv` — CHF/EUR valutakurs
- `Exam/FIE401 EXAM attach 3 isins.csv` — 123 aksjer (25 SMI, 51 CAC40, 47 DAX30)
- `Foreleser/Previous Exams/Spring 2020/Suggested-coding-solution-Spring-2020.pdf` — fasit 2020

---

## Notes

- Data: 108 aksjer etter filter, 80 761 obs, datoer ca. jan 2019 – aug 2021
- Tabell 2-funn etter DAX30-fiks: SMI hadde lavere spread (***), lavere btm (**) — men turnover-forskjellen er nå IKKE signifikant (0.133, t=0.61). Styrker parallel trends-argumentet.
- Tabell 3-funn: Bid-ask spread økte ~2.7 bps for SMI etter konsolidering (sign. med kontroller). Re-fragmentering ga enda større økning (~4.4 bps***). Amihud-koeffisienter ~1–2 (var ~12–20 før volumfiks).
- Regime-modell (modell 3–4): inkludert i Tabell 3 som robusthet for hele perioden
- Negative priser finnes ikke i dette datasettet (0 tilfeller), men håndteres uansett
- `rstudioapi::getActiveDocumentContext()$path` brukes for portabel setwd
- DescTools::Winsorize bruker `val = quantile(...)` på denne R-versjonen (ikke `probs =`)
- stargazer + lapply: bruk `reformulate("SMI", response = v)` — ikke `as.formula(paste(...))`
- stargazer LaTeX-noter: bruk `notes.append = FALSE` + `$^{*}$p$<$0.1` for korrekt rendering
