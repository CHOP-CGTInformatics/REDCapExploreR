document.addEventListener("DOMContentLoaded", function () {
  document.querySelectorAll(".redcap-codebook-viewer").forEach(function (root) {
    const tabs = Array.from(root.querySelectorAll(".redcap-codebook-tab"));
    const sections = root.querySelectorAll(".redcap-codebook-section");

    function showSection(sectionId) {
      tabs.forEach(function (tab) {
        const active = tab.getAttribute("data-section") === sectionId;
        tab.classList.toggle("is-active", active);
        tab.setAttribute("aria-selected", active ? "true" : "false");
        tab.setAttribute("tabindex", active ? "0" : "-1");
      });
      sections.forEach(function (section) {
        section.classList.toggle("is-active", section.getAttribute("data-section") === sectionId);
      });
      if (window.HTMLWidgets && window.HTMLWidgets.staticRender) window.HTMLWidgets.staticRender();
      if (window.jQuery && window.jQuery.fn.dataTable) {
        window.jQuery(root).find(".dataTable").each(function () {
          if (window.jQuery.fn.dataTable.isDataTable(this)) window.jQuery(this).DataTable().columns.adjust();
        });
      }
    }

    tabs.forEach(function (tab, index) {
      tab.addEventListener("click", function () {
        showSection(tab.getAttribute("data-section"));
        root.querySelector(".redcap-codebook-header").scrollIntoView({ behavior: "auto", block: "start" });
      });
      tab.addEventListener("keydown", function (event) {
        let next;
        if (event.key === "ArrowRight") next = (index + 1) % tabs.length;
        if (event.key === "ArrowLeft") next = (index + tabs.length - 1) % tabs.length;
        if (event.key === "Home") next = 0;
        if (event.key === "End") next = tabs.length - 1;
        if (next !== undefined) {
          event.preventDefault();
          tabs[next].focus();
          showSection(tabs[next].getAttribute("data-section"));
        }
      });
    });

    root.addEventListener("click", function (event) {
      const row = event.target.closest(".redcap-codebook-table tbody tr");
      if (!row) return;
      row.parentElement.querySelectorAll("tr.redcap-codebook-row-selected").forEach(function (selected) {
        selected.classList.remove("redcap-codebook-row-selected");
      });
      row.classList.add("redcap-codebook-row-selected");
    });
    const hash = window.location.hash.slice(1);
    const target = Array.from(sections).find(function (section) {
      return section.id === hash || section.getAttribute("data-section") === hash;
    });
    if (target) showSection(target.getAttribute("data-section"));
  });
});
