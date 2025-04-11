document.addEventListener("DOMContentLoaded", function () {
  const links = document.querySelectorAll("a[href^='http']");
  links.forEach(function (link) {
    // Only apply to external links
    if (link.hostname !== window.location.hostname) {
      link.setAttribute("target", "_blank");
      link.setAttribute("rel", "noopener noreferrer");
    }
  });
});