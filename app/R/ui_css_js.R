# ui_css_js.R
# Inline CSS and JS injected into the page header (layout, plot menus, Run/Try-it
# button colors, Results-dropdown close helpers, KPI card navigation).
# Driven by every page; not a chart file.

app_header_tags <- function() {
  tagList(
    tags$style(HTML("
    .shiny-plot-output {
      width: 100% !important;
      height: clamp(520px, 60vh, 720px) !important;
      min-height: 520px !important;
    }
    /* Bottleneck downtime card sits beside the 420px waits girafe; keep it short. */
    #plot_down.shiny-plot-output {
      height: 420px !important;
      min-height: 420px !important;
      max-height: 420px !important;
    }
    /* Page scrolls as a normal document. Fillable nesting was trapping overflow. */
    html, body {
      height: auto !important;
      max-height: none !important;
      overflow-y: auto !important;
      overflow-x: hidden !important;
    }
    .bslib-page-fill, .html-fill-container {
      height: auto !important;
      min-height: 0 !important;
      max-height: none !important;
      overflow: visible !important;
    }
    .bslib-sidebar-layout {
      height: auto !important;
      max-height: none !important;
      overflow: visible !important;
    }
    .bslib-sidebar-layout > .main,
    .bslib-sidebar-layout > .sidebar {
      overflow: visible !important;
      max-height: none !important;
    }
    /* Sub-tab pills wrap instead of collapsing into one run-on label. */
    #capacity_tabs.nav-pills,
    #flow_tabs.nav-pills,
    #lead_tabs.nav-pills {
      display: flex;
      flex-wrap: wrap;
      gap: 0.5rem;
    }
    #capacity_tabs.nav-pills > .nav-item,
    #flow_tabs.nav-pills > .nav-item,
    #lead_tabs.nav-pills > .nav-item {
      flex: 0 0 auto;
      width: auto;
    }
    .two-set-study .nav-tabs,
    .two-set-study .nav-pills {
      display: flex;
      flex-wrap: wrap;
      gap: 0.35rem;
    }
    .two-set-study .nav-item {
      flex: 0 0 auto;
      width: auto;
    }
    .provenance-warning {
      border-left: 6px solid #8a4b08;
      font-weight: 500;
    }
    .run-settings summary { cursor: pointer; font-weight: 600; }
    /* Single illustrative-results note under the guided example. */
    .fine-print { font-size: 0.8rem; color: #1a1a1a; }
    /* Headline cards: every card fills its grid cell, so all four match. */
    #headline_kpis .bslib-grid { align-items: stretch; grid-auto-rows: 1fr; }
    #headline_kpis .kpi-nav-card { height: 100%; display: flex; }
    #headline_kpis .kpi-nav-card > .bslib-value-box {
      flex: 1 1 auto; height: 100%; min-height: 170px; margin-bottom: 0;
    }
    .text-muted,
    .text-body-secondary,
    .form-text,
    .help-block,
    caption,
    .caption {
      color: #1a1a1a !important;
    }
    #headline_kpis, #plot_down, #plot_wip, #plot_queue, #plot_cycle,
    #occupancy_summary_table, #labor_table, #reliability_losses,
    #reliability_table, #table_metrics,
    #scenario_table_wrap, #timing_table_wrap {
      transition: opacity 0.15s ease;
    }
    /* Sidebar section jumps look like collapsed accordion headers. */
    .param-jump {
      display: block;
      width: 100%;
      text-align: left;
      border: 1px solid rgba(0,0,0,.125);
      border-radius: 0.375rem;
      background: #f8f9fa;
      color: #1a1a1a;
      padding: 0.65rem 0.85rem;
      margin: 0.35rem 0;
      font-weight: 600;
      text-decoration: none !important;
    }
    .param-jump:hover, .param-jump:focus {
      background: #e9ecef;
      color: #1a1a1a;
    }
    .param-jump .jump-hint {
      display: block;
      font-weight: 400;
      font-size: 0.8rem;
      color: #333;
      margin-top: 0.15rem;
    }
    .params-section {
      scroll-margin-top: 5.5rem;
    }
    /* Compact plot-option menus on chart pages. */
    .plot-options-btn { white-space: nowrap; }
    .plot-options-popover { min-width: 16rem; max-width: 22rem; }

    /* Hide Shiny's default progress UI; the slim top status bar is the only
       progress surface. withProgress remains only as a websocket flush pump. */
    .shiny-progress-container {
      display: none !important;
    }
    .shiny-notification:has(.shiny-progress-notification),
    .shiny-notification .shiny-progress-notification {
      display: none !important;
    }
  ")),
    tags$script(HTML("
      function setSimProgressVisible(on) {
        var panel = document.getElementById('sim_progress_panel');
        var idle = document.getElementById('sim_progress_idle');
        if (panel) panel.classList.toggle('d-none', !on);
        if (idle) idle.classList.toggle('d-none', !!on);
        document.body.classList.toggle('sim-running', !!on);
      }
      Shiny.addCustomMessageHandler('simProgress', function(msg) {
        var el = document.getElementById('sim_progress_panel');
        if (!el) return;
        if (msg && msg.show) {
          setSimProgressVisible(true);
          var detail = el.querySelector('.sim-progress-detail');
          var bar = el.querySelector('.progress-bar');
          if (detail) detail.textContent = msg.detail || '';
          if (bar && msg.value != null) {
            var pct = Math.max(0, Math.min(1, msg.value)) * 100;
            bar.style.width = pct + '%';
            bar.textContent = Math.round(pct) + '%';
          } else if (bar) {
            bar.style.width = '100%';
            bar.textContent = 'In progress';
          }
        } else {
          setSimProgressVisible(false);
        }
      });
      function showSimProgressLocal(detail) {
        var el = document.getElementById('sim_progress_panel');
        if (!el) return;
        setSimProgressVisible(true);
        var d = el.querySelector('.sim-progress-detail');
        if (d) d.textContent = detail || '';
        var bar = el.querySelector('.progress-bar');
        if (bar) { bar.style.width = '100%'; bar.textContent = 'In progress'; }
      }
      function repsLabel(extra) {
        var reps = (document.getElementById('n_reps') || {}).value || '5';
        var base = reps + ' replications';
        return extra ? (base + ' · ' + extra) : base;
      }
      // Paint Run / Try it like Compare: amber Running… instantly, no fade.
      function paintSimRunBtn(btn, state) {
        if (!btn) return;
        var isTry = (btn.id === 'guide_try');
        btn.classList.remove('btn-primary', 'btn-warning', 'btn-success');
        btn.classList.add('sim-run-btn');
        btn.style.setProperty('opacity', '1', 'important');
        btn.style.setProperty('filter', 'none', 'important');
        btn.querySelectorAll('i, .fa, .fas, svg').forEach(function(n) { n.remove(); });
        var label = btn.querySelector('.sim-run-label');
        if (!label) {
          label = document.createElement('span');
          label.className = 'sim-run-label';
          btn.appendChild(label);
        }
        if (state === 'running') {
          btn.classList.add('btn-warning');
          btn.disabled = true;
          btn.style.setProperty('background-color', '#e67e00', 'important');
          btn.style.setProperty('border-color', '#c96e00', 'important');
          btn.style.setProperty('color', '#ffffff', 'important');
          btn.style.setProperty('font-weight', '700', 'important');
          var spin = document.createElement('i');
          spin.className = 'fa fa-spinner fa-spin';
          spin.setAttribute('aria-hidden', 'true');
          spin.style.marginRight = '0.35rem';
          btn.insertBefore(spin, btn.firstChild);
          label.textContent = 'Running…';
        } else if (state === 'done') {
          btn.classList.add('btn-success');
          btn.disabled = false;
          btn.style.setProperty('background-color', '#198754', 'important');
          btn.style.setProperty('border-color', '#146c43', 'important');
          btn.style.setProperty('color', '#ffffff', 'important');
          btn.style.setProperty('font-weight', '700', 'important');
          var check = document.createElement('i');
          check.className = 'fa fa-check';
          check.setAttribute('aria-hidden', 'true');
          check.style.marginRight = '0.35rem';
          btn.insertBefore(check, btn.firstChild);
          label.textContent = isTry ? 'Done – try again' : 'Done – run again';
        } else {
          btn.classList.add('btn-primary');
          btn.disabled = false;
          btn.style.removeProperty('background-color');
          btn.style.removeProperty('border-color');
          btn.style.removeProperty('color');
          btn.style.removeProperty('font-weight');
          var play = document.createElement('i');
          play.className = 'fa fa-play';
          play.setAttribute('aria-hidden', 'true');
          play.style.marginRight = '0.35rem';
          btn.insertBefore(play, btn.firstChild);
          label.textContent = isTry ? 'Try it' : 'Run simulation';
        }
      }
      function paintAllSimRunBtns(state) {
        paintSimRunBtn(document.getElementById('run'), state);
        paintSimRunBtn(document.getElementById('guide_try'), state);
      }
      function registerSimRunButtons() {
        if (window.__simRunButtons) return;
        if (!window.Shiny || !Shiny.addCustomMessageHandler) return;
        window.__simRunButtons = true;
        Shiny.addCustomMessageHandler('simRunButtons', function(message) {
          var state = (message && message.state) || 'idle';
          // The startup run is automatic: keep the buttons idle until the
          // user clicks Run or Try it, so the page never opens on Done.
          if (!window.__simUserRan && state !== 'idle') state = 'idle';
          paintAllSimRunBtns(state);
        });
      }
      document.addEventListener('shiny:connected', registerSimRunButtons);
      if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', registerSimRunButtons);
      else registerSimRunButtons();
      var _srb = 0;
      var _srbTimer = window.setInterval(function() {
        registerSimRunButtons();
        if (window.__simRunButtons || ++_srb > 50) window.clearInterval(_srbTimer);
      }, 100);
      // Open the panel immediately on click so it is visible while Shiny blocks.
      $(document).on('click', '#run', function() {
        window.__simUserRan = true;
        paintSimRunBtn(this, 'running');
        showSimProgressLocal(repsLabel());
      });
      $(document).on('click', '#guide_try', function() {
        window.__simUserRan = true;
        paintSimRunBtn(this, 'running');
        // Mirror on the main Run button so both signal the active sim.
        paintSimRunBtn(document.getElementById('run'), 'running');
        showSimProgressLocal(repsLabel('guided example'));
      });
      $(document).on('click', '#compare', function() {
        showSimProgressLocal(repsLabel('staffing comparison'));
      });
      $(document).on('click', '#compare_timing', function() {
        showSimProgressLocal(repsLabel('timing comparison'));
      });
      Shiny.addCustomMessageHandler('scrollToSection', function(msg) {
        var id = (msg && msg.id) ? msg.id : msg;
        var tries = 0;
        function go() {
          var el = document.getElementById(id);
          if (el) {
            el.scrollIntoView({behavior: 'smooth', block: 'start'});
            return;
          }
          if (tries++ < 40) setTimeout(go, 50);
        }
        setTimeout(go, 50);
      });
      function closeResultsNavChrome() {
        document.querySelectorAll('.navbar .nav-item.dropdown, .navbar .dropdown').forEach(function(d) {
          var toggle = d.querySelector('[data-bs-toggle=dropdown], .dropdown-toggle');
          if (toggle && window.bootstrap && bootstrap.Dropdown) {
            try { bootstrap.Dropdown.getOrCreateInstance(toggle).hide(); } catch (err) {}
          }
          d.classList.remove('show');
          if (toggle) {
            toggle.classList.remove('show');
            toggle.setAttribute('aria-expanded', 'false');
            try { toggle.blur(); } catch (e) {}
          }
        });
        document.querySelectorAll('.navbar .dropdown-menu.show').forEach(function(m) {
          m.classList.remove('show');
        });
        var collapse = document.querySelector('.navbar-collapse.show');
        if (collapse) {
          if (window.bootstrap && bootstrap.Collapse) {
            try { bootstrap.Collapse.getOrCreateInstance(collapse, {toggle: false}).hide(); }
            catch (err) {
              collapse.classList.remove('show');
            }
          } else {
            collapse.classList.remove('show');
          }
        }
      }
      function kpiNavigate(el) {
        var page = el && el.getAttribute('data-page');
        if (!page) return;
        closeResultsNavChrome();
        Shiny.setInputValue('kpi_nav', page, {priority: 'event'});
        // Nav may re-open the menu while switching; close again after paint.
        setTimeout(closeResultsNavChrome, 0);
        setTimeout(closeResultsNavChrome, 120);
      }
      $(document).on('click', '.kpi-nav-card', function(e) {
        kpiNavigate(this);
      });
      $(document).on('keydown', '.kpi-nav-card', function(e) {
        if (e.key === 'Enter' || e.key === ' ') {
          e.preventDefault();
          kpiNavigate(this);
        }
      });
      // Results menu item click: close the dropdown after nav switches pages.
      $(document).on('click', '.navbar .dropdown-menu .dropdown-item', function() {
        [0, 50, 120, 250, 450].forEach(function(ms) {
          setTimeout(closeResultsNavChrome, ms);
        });
      });
      Shiny.addCustomMessageHandler('closeResultsNav', function(msg) {
        closeResultsNavChrome();
        [0, 50, 150, 300, 500].forEach(function(ms) {
          setTimeout(closeResultsNavChrome, ms);
        });
      });
      // If a Results nav item becomes active, force the dropdown/toggler shut.
      $(document).on('shown.bs.tab shown.bs.dropdown', function() {
        var active = document.querySelector('.navbar .nav-link.active, .navbar .dropdown-item.active');
        if (!active) return;
        var val = active.getAttribute('data-value') || '';
        if (val.indexOf('results_') === 0 || val === 'production_schedule') {
          setTimeout(closeResultsNavChrome, 0);
          setTimeout(closeResultsNavChrome, 100);
        }
      });
    "))

  )
}
