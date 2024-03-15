document.getElementById("query")?.addEventListener("keyup", ({ key }) => {
  if (key === "Enter") {
    document.getElementById("search-button")?.click();
  }
});

function onResetList() {
  document.getElementById("recipe-ingredients").value = "";
}
