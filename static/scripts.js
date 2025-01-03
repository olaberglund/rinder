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
    ingredientInput.name = "names";
    ingredientInput.value = ingredient;

    document.getElementById("recipe-form").appendChild(urlInput);
    document.getElementById("recipe-form").appendChild(ingredientInput);
  }
}

document.getElementById("query")?.addEventListener("keyup", ({ key }) => {
  if (key === "Enter") {
    document.getElementById("search-button")?.click();
  }
});

function onResetList() {
  document.getElementById("recipe-form").reset();
}

function toggleHidden(checkbox) {
  if (checkbox.checked) {
    document.getElementById("checkbox-url-" + checkbox.id).checked = true;
  } else {
    document.getElementById("checkbox-url-" + checkbox.id).checked = false;
  }
}
document.body.addEventListener("htmx:afterSwap", function (_evt) {
  document.querySelector("#split-form")?.reset();
});

function openTab(evt, divId) {
  // Declare all variables
  var i, tabcontent, tablinks;

  // Get all elements with class="tabcontent" and hide them
  tabcontent = document.getElementsByClassName("tabcontent");
  for (i = 0; i < tabcontent.length; i++) {
    tabcontent[i].style.display = "none";
  }

  // Get all elements with class="tablinks" and remove the class "active"
  tablinks = document.getElementsByClassName("tab");
  for (i = 0; i < tablinks.length; i++) {
    tablinks[i].className = tablinks[i].className.replace(" active", "");
  }

  // Show the current tab, and add an "active" class to the button that opened the tab
  document.getElementById(divId).style.display = "block";
  evt.currentTarget.className += " active";
}
document?.getElementById("default-open")?.click();

function stripLanguage(href) {
  return href.slice(href.indexOf("/", 1));
}

// whenever the html document has been parsed, i.e. on page load
document.addEventListener("DOMContentLoaded", () => {
  const inputs = document.querySelectorAll("input[name='paidBy']");
  const storageKey = "paidBy";

  const loadSettings = () => {
    const savedValue = localStorage.getItem(storageKey);
    inputs.forEach((el) => {
      el.checked = el.value === savedValue;
    });
  };

  inputs.forEach((el) => {
    el.addEventListener("change", (e) => {
      localStorage.setItem(storageKey, e.target.value);
    });
  });

  loadSettings();
});

// inject the current href into hrefs of all a tags with class "language-link"
document.querySelectorAll(".language-link").forEach((el) => {
  el.href
    ? (el.href = el.href + stripLanguage(window.location.pathname))
    : (el.href = stripLanguage(window.location.pathname));
});
