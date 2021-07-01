import { Elm } from "./src/Main.elm";

const SETTINGS_STORAGE_KEY = "Minesweeper";

function loadValue(key) {
  if (key == null) {
    console.warn("Trying to load value with empty key");
    return;
  }
  const nskey = `${SETTINGS_STORAGE_KEY}.${key}`;
  console.debug(`Trying to load value with key ${nskey}`);
  try {
    const json = localStorage.getItem(nskey);
    return json ? JSON.parse(json) : null;
  } catch (err) {
    try {
      localStorage.removeItem(nskey);
    } catch (_err) {}
    console.warn({ err: err, key: key });
  }
  return;
}

function saveValue({ key, value }) {
  if (key == null) {
    console.warn("Trying to save value with empty key");
    return;
  }
  const nskey = `${SETTINGS_STORAGE_KEY}.${key}`;
  console.debug(`Trying to load value with key ${nskey}`);
  try {
    localStorage.setItem(nskey, JSON.stringify(value));
  } catch (err) {
    console.warn(err);
  }
}

const theme = loadValue("theme");

const flags = {
  theme: theme,
  themes: theme ? [theme] : null,
  level: loadValue("level"),
};

const app = Elm.Main.init({
  node: document.getElementById("root"),
  flags,
});

document.documentElement.oncontextmenu = (e) => {
  e.preventDefault();
};

window.addEventListener("load", () => {
  document.body.classList.add("Theme__Solarized", "Theme__Solarized__Dark");
});

window.addEventListener("blur", (e) => {
  app.ports.windowBlurred.send("GotBlurred");
});

app.ports.saveValue.subscribe(saveValue);
