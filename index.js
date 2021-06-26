import { Elm } from "./src/Main.elm";
import * as serviceWorker from './serviceWorker';

const SETTINGS_STORAGE_KEY = "Minesweeper.settings";

function validateType(type) {
  if (!["string", "number", "boolean", "object"].includes(type)) {
    throw new TypeError("Unable to save value of type '" + type);
  }
}

function loadValue(type, key) {
  const nskey = SETTINGS_STORAGE_KEY + "." + key;
  try {
    validateType(type);
    const json = localStorage.getItem(nskey);
    switch (type) {
      case "number": {
        const v = Number(json);
        return isNaN(v) ? undefined : v;
      }
      case "string":
        return json;
      case "boolean":
      case "object": {
        const parsed = json != null ? JSON.parse(json) : undefined;
        if (type === "boolean") {
          return parsed === true;
        }
        return typeof parsed === "object" ? parsed : undefined;
      }
      default:
        console.warn("Unhandled type " + type);
    }
  } catch (err) {
    try {
      localStorage.removeItem(nskey);
    } catch (_err) {}
    console.warn({ err: err, type: type, key: key });
  }
  return;
}

function saveValue(type, key, value) {
  try {
    validateType(type);
    localStorage.setItem(
      SETTINGS_STORAGE_KEY + "." + key,
      type === "object" ? JSON.stringify(value) : value
    );
  } catch (err) {
    console.warn(err);
  }
}

const theme = loadValue("string", "theme") || "Solarized";

const flags = {
  theme: theme,
  themes: [theme],
};

const app = Elm.Main.init({
  node: document.getElementById("root"),
  flags,
});

document.documentElement.oncontextmenu = (e) => {
  e.preventDefault();
};
window.addEventListener('blur', () => {
  app.ports.windowBlurred.send("GotBlurred");
});

// app.ports.saveValue.subscribe(saveValue);
// app.ports.loadValue.subscribe(loadValue);

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.register();
