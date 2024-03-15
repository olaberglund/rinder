function addIngredient(ingredient, url) {
  var list = document.getElementById("recipe-ingredients");
  if (list) {
    if (list.value === "") {
      list.value = ingredient;
    } else {
      const ingredients = list.value.split("\n");
      if (!ingredients.includes(ingredient)) {
        const newList = ingredients.concat(ingredient).join("\n");
        list.value = newList;
      } else {
        alert("Ingrediensen är redan tillagd");
        return;
      }
    }
    const urlInput = document.createElement("input");
    urlInput.type = "hidden";
    urlInput.name = "urls";
    urlInput.value = url;

    const ingredientInput = document.createElement("input");
    ingredientInput.type = "hidden";
    ingredientInput.name = "ingredients";
    ingredientInput.value = ingredient;

    document.getElementById("recipe-form").appendChild(urlInput);
    document.getElementById("recipe-form").appendChild(ingredientInput);
  }
}

// input_ [type_ "hidden", name_ "ingredients", id_ "recipe-urls"]

document.getElementById("query")?.addEventListener("keyup", ({ key }) => {
  if (key === "Enter") {
    document.getElementById("search-button")?.click();
  }
});

function onResetList() {
  document.getElementById("recipe-form").reset();
}
