function onAddIngredient() {
  var input = document.getElementById("chosen-product");
  var list = document.getElementById("recipe-ingredients");
  if (!input.value) {
    alert("Du måste skriva in en ingrediens");
    return;
  }

  if (list) {
    const ingredients = list.value.split(/\r?\n/);
    if (!ingredients.includes(input.value)) {
      const newList = input.value + "\n" + list.value;
      list.value = newList;
      input.value = "";
    } else {
      alert("Ingrediensen är redan tillagd");
    }
  }
}

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

document
  .getElementById("chosen-product")
  ?.addEventListener("keyup", ({ key }) => {
    if (key === "Enter") {
      onAddIngredient();
    }
  });

function onResetList() {
  document.getElementById("recipe-ingredients").value = "";
}

function onSubmit() {}
