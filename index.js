import { Elm } from "./src/Main.elm";
import { Field, Solver, GameOver, GameWon } from "./solver";
/**
 * @typedef {Object} Level
 * @property {number} cols
 * @property {number} rows
 * @property {"Toroid"|"Plane"} topology
 * @property {"Hex"|"Square"} type_
 * @property {number} mines
 *
 *
 * @typedef {Object} CellsUncoveredPayloadContent
 * @property {number} index
 * @property {number} threats
 * @property {bool} mined
 * @property {bool} flagged
 * @property {[number]} neighbours
 *
 *
 * @typedef {Object} CellsUncoveredPayload
 * @property {[CellsUncoveredPayloadContent]} cells
 *
 * @typedef {Object} CellsUncoveredMsg
 * @property {"CellsUncovered"} tag
 * @property {CellsUncoveredPayload} payload
 *
 * @typedef {Object} SaveValuePayload
 * @property {string} key
 * @property {*} value
 *
 * @typedef {Object} SaveValueMsg
 * @property {"SaveValue"} tag
 * @property {SaveValuePayload} payload
 *
 * @typedef {Object} GetStepPayload
 * @property {"Slow"|"Medium"|"AsFastAsPossible"} speed
 *
 * @typedef {Object} GetStepMsg
 * @property {"GetStep"} tag
 * @property {GetStepPayload} payload
 *
 * @typedef {CellsUncoveredMsg|SaveValueMsg|GetStepMsg} IncomingMessage
 */

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
    const value = json ? JSON.parse(json) : null;
    return value;
  } catch (err) {
    try {
      localStorage.removeItem(nskey);
    } catch (_err) {}
    console.warn({ err: err, key: key });
  }
  return;
}

/**
 *
 * @param {{key: string, value: any}} param0
 * @returns
 */
function saveValue({ key, value }) {
  if (key == null) {
    throw TypeError("Trying to save value with empty key");
  }
  const nskey = `${SETTINGS_STORAGE_KEY}.${key}`;
  console.debug(`Trying to save value with key ${nskey}`, {value});
  try {
    localStorage.setItem(nskey, JSON.stringify(value));
  } catch (err) {
    console.error(err);
  }
}

const theme = loadValue("theme");

/**
 * @type {{theme: string?, themes: [string]?, level: Level?}}
 */
const flags = {
  theme: theme,
  themes: theme ? [theme] : null,
  level: loadValue("level"),
  actor: loadValue("actor"),
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

/**
 * @type {SolverRunner | undefined}
 */
let runner;

app.ports.send.subscribe(handleIncoming);
app.ports.stateChanged.subscribe(handleStateChanged);

function cleanup() {
  console.debug("Cleanup!");
  runner = undefined;
}

function handleStateChanged(x) {
  console.log("stateChanged", x);
  if (x.new.startsWith("Done")) {
    // cleanup();
    if (x.new === "Done GameOver") {
      console.error("BOOOM!");
    } else if (x.new === "Done Completed") {
      console.info("Yay!!!");
    } else {
      console.warn("Unknown state", x.new);
    }
  }
}

/**
 *
 * @param {IncomingMessage}
 */
function handleIncoming({ tag, payload }) {
  switch (tag) {
    case "SaveValue":
      saveValue(payload);
      break;
    case "StartSolver":
      console.info("handleIncoming", { tag, payload });
      if (runner != null) {
        console.warn("Already running");
      }
      const { cols, rows, mines } = payload;
      runner = new SolverRunner(new Solver(new Field(cols, rows, mines)));
      app.ports.receive.send({ tag: "SolverStarted" });
      requestAnimationFrame(() => app.ports.receive.send(runner.step()));

      break;
    case "CellsUncovered":
      try {
        runner.solver.onUncovered(payload);
      } catch (err) {
        if (err instanceof GameOver) {
          app.ports.receive.send(
            runner.getSendMessage({
              flag: [],
              poke: [],
              state: "GameOver",
            })
          );
          console.info(`Game over: ${err.message}`);
        } else if (err instanceof GameWon) {
          app.ports.receive.send(
            runner.getSendMessage({
              flag: [],
              poke: [],
              state: "Completed",
            })
          );
          console.info(`Game won: ${err.message}`);
        } else {
          console.error(err.message || err);
        }
        cleanup();
      }
      break;
    case "GetStep":
      if (runner == null) {
        logger.warn("Runner is undefined.");
      } else {
        const f = () => app.ports.receive.send(runner.step());
        switch (payload.speed) {
          case "Slow":
            setTimeout(f, 300);
            break;
          case "Medium":
            setTimeout(f, 100);
            break;
          case "AsFastAsPossible":
            requestAnimationFrame(f);
            break;
          default:
            console.warn("Unknown speed", speed)
            setTimeout(f, 1000);
        }
      }
      break;
    default:
      break;
  }
}

class SolverRunner {
  /**
   * @param {Solver} solver
   */
  constructor(solver) {
    /**
     * @type {Solver} solver
     */
    this.solver = solver;
  }
  cleanup() {
    console.warn("TODO");
  }
  step() {
    try {
      const { poke, flag } = this.solver.step();
      if (poke.length === 0) {
        throw new Error("Solver has no suggestions");
      } else {
        return this.getSendMessage({ poke, flag });
      }
    } catch (err) {
      console.error(err);
      this.cleanup();
      return { tag: "Solution", error: err.toString() };
    }
  }
  getSendMessage({ poke, flag, state }) {
    return {
      tag: "Solution",
      payload: {
        poke,
        flag,
        state,
      },
    };
  }
}
