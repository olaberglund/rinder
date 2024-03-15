function addIngredient(ingredient) {
  var list = document.getElementById("recipe-ingredients");
  if (list) {
    const ingredients = list.value.split(/\r?\n/);
    if (!ingredients.includes(ingredient)) {
      const newList = ingredient + "\n" + list.value;
      list.value = newList;
    } else {
      alert("Ingrediensen är redan tillagd");
    }
  }
}

document.getElementById("query")?.addEventListener("keyup", ({ key }) => {
  if (key === "Enter") {
    document.getElementById("search-button")?.click();
  }
});

function onResetList() {
  document.getElementById("recipe-ingredients").value = "";
}
