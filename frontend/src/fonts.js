const fontfacesPath = "/css/font-faces.css"

const setFontfacesStylesheet = () => {
  const newLink = document.createElement("link");
  newLink.rel = "stylesheet";
  newLink.href = fontfacesPath;

  document.head.appendChild(newLink);
}

export const linkFontFaces = () => {
  const xhr = new XMLHttpRequest();
  xhr.open('GET', fontfacesPath, true);
  xhr.onreadystatechange = function () {
    if (xhr.readyState == 4 && xhr.status == 200) {
      setFontfacesStylesheet();
    }
  };
  xhr.send();
}
