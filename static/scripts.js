function onAddIngredient() {
  var input = document.getElementById("chosen-product");
  var list = document.getElementById("recipe-ingredients");
  if (!input.value) {
    alert("Du måste skriva in en ingrediens");
    return;
  }

  if (list) {
    // split on newline,
    // see if ingredient is already in the list
    // and if so, dont add it
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

function onResetList() {
  document.getElementById("recipe-ingredients").value = "";
}

function onSubmit() {}
