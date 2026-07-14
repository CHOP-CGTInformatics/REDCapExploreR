document.addEventListener("DOMContentLoaded", function () {
  const tabs = document.querySelectorAll(".redcap-codebook-tab");
  const sections = document.querySelectorAll(".redcap-codebook-section");

  function showSection(sectionId) {
    tabs.forEach(function (tab) {
      const active = tab.getAttribute("data-section") === sectionId;
      tab.classList.toggle("is-active", active);
      tab.setAttribute("aria-selected", active ? "true" : "false");
    });

    sections.forEach(function (section) {
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
      showSection(tab.getAttribute("data-section"));
      document
        .querySelector(".redcap-codebook-header")
        .scrollIntoView({ behavior: "auto", block: "start" });
    });
  });

  document.addEventListener("click", function (event) {
    const row = event.target.closest(".redcap-codebook-table tbody tr");
    if (!row) return;

    row.parentElement
      .querySelectorAll("tr.redcap-codebook-row-selected")
      .forEach(function (selectedRow) {
        selectedRow.classList.remove("redcap-codebook-row-selected");
      });
    row.classList.add("redcap-codebook-row-selected");
  });

  if (window.location.hash) {
    const sectionId = window.location.hash.replace("#", "");
    if (document.getElementById(sectionId)) showSection(sectionId);
  }
});
