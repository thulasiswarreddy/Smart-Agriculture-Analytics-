document.addEventListener("shiny:connected", function () {
  document.body.classList.add("app-loaded");
});

document.addEventListener("shiny:inputchanged", function (event) {
  if (event.name === "get_started") {
    setTimeout(function () {
      document.querySelector(".content-area")?.classList.add("page-animated");
    }, 100);
  }
});