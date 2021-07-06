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
 * @typedef {CellsUncoveredMsg|SaveValueMsg} IncomingMessage
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
  console.debug(`Trying to save value with key ${nskey}`);
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
  state = x.new;
  if (x.new.startsWith("Done")) {
    // cleanup();
    if (x.new === "Done GameOver") {
      console.error("BOOOM!");
      // runner.getSendMessage({ flag: [], poke: [], state: "GameOver" });
    } else if (x.new === "Done Completed") {
      console.info("Yay!!!");
      // runner.getSendMessage({ flag: [], poke: [], state: "Completed" });
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
      // startDemo(payload);
      // return;
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
        requestAnimationFrame(() => app.ports.receive.send(runner.step()));
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
      console.error("aksdaksldk", err);
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

/**
 *
 * @param {Level} param0
 */
async function startDemo({ cols, rows, mines }) {
  console.debug("startDemo!");
  const solver = new Solver(new Field(cols, rows, mines));

  app.ports.stateChanged.subscribe(handleStateChanged);
  app.ports.send.subscribe(handleIncoming);

  requestAnimationFrame(stepper);

  function handleStateChanged(x) {
    console.log("stateChanged", x);
    state = x.new;
    if (x.new.startsWith("Done")) {
      if (x.new === "Done GameOver") {
        console.error("BOOOM!");
      } else if (x.new === "Done Completed") {
        console.info("Yay!!!");
      } else {
        console.warn("Unknown state", x.new);
      }
      cleanup();
    } else if (x.new === "Playing InProgress" && x.old === "Playing Paused") {
      console.info("Was paused, continuing");
      requestAnimationFrame(stepper);
    } else if (x.old.startsWith("Playing") && !x.new.startsWith("Playing")) {
      console.warn(`Going from "${x.old}" to ${x.new}. Stopping solver.`);
      cleanup();
    }
  }

  /**
   *
   * @param {[CellsUncoveredPayloadContent]} cells
   */
  function handleCellsUncovered(cells) {
    try {
      solver.onUncovered(cells);
      requestAnimationFrame(stepper);
    } catch (err) {
      if (err instanceof GameOver) {
        console.debug(err.message);
      } else {
        console.error(err);
      }
      cleanup();
    }
  }
  /**
   *
   * @param {IncomingMessage} msg
   */
  function handleIncoming({ tag, payload }) {
    switch (tag) {
      case "CellsUncovered":
        handleCellsUncovered(payload);
        break;
      default:
        break;
    }
  }

  function cleanup() {
    console.debug("Cleanup!");
    app.ports.send.unsubscribe(handleIncoming);
    app.ports.stateChanged.unsubscribe(handleStateChanged);
    // solver.field = null;
  }

  function stepper() {
    try {
      const { poke, flag } = solver.step();
      if (poke.length === 0) {
        throw Error("Solver has no suggestions");
      } else {
        sendCells({ poke, flag });
      }
    } catch (err) {
      console.error(err);
      cleanup();
    }
  }
}

function sendCells({ poke, flag }) {
  app.ports.receive.send({
    tag: "IncomingCells",
    payload: {
      poke: poke,
      flag: flag,
    },
  });
}
