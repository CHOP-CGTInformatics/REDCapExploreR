document.addEventListener("DOMContentLoaded", function () {
  const tabs = document.querySelectorAll(".redcap-quality-report-tab");

  function showSection(group, sectionId) {
    document
      .querySelectorAll(
        `.redcap-quality-report-tab[data-tab-group="${group}"]`
      )
      .forEach(function (tab) {
        const active = tab.getAttribute("data-section") === sectionId;
        tab.classList.toggle("is-active", active);
        tab.setAttribute("aria-selected", active ? "true" : "false");
      });

    document
      .querySelectorAll(`[data-panel-group="${group}"]`)
      .forEach(function (section) {
        section.classList.toggle("is-active", section.id === sectionId);
      });

    if (window.HTMLWidgets && window.HTMLWidgets.staticRender) {
      window.HTMLWidgets.staticRender();
    }

    if (window.jQuery && window.jQuery.fn.dataTable) {
      window.jQuery(".dataTable").each(function () {
        if (window.jQuery.fn.dataTable.isDataTable(this)) {
          window.jQuery(this).DataTable().columns.adjust();
        }
      });
    }
  }

  tabs.forEach(function (tab) {
    tab.addEventListener("click", function (event) {
      event.preventDefault();
      const group = tab.getAttribute("data-tab-group");
      showSection(group, tab.getAttribute("data-section"));

      if (group === "primary") {
        document
          .querySelector(".redcap-quality-report-header")
          .scrollIntoView({ behavior: "auto", block: "start" });
      }
    });
  });

  document.addEventListener("click", function (event) {
    const row = event.target.closest(".redcap-quality-report-table tbody tr");
    if (!row) return;

    row.parentElement
      .querySelectorAll("tr.redcap-quality-report-row-selected")
      .forEach(function (selectedRow) {
        selectedRow.classList.remove("redcap-quality-report-row-selected");
      });
    row.classList.add("redcap-quality-report-row-selected");
  });
});
