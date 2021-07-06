// Copied and adapted from https://github.com/timjb/minesweeper-solver/blob/gh-pages/solver.js

/*
 * Copyright © 2012 Tim Baumann, http://timbaumann.info
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the “Software”), to deal in the Software without
 * restriction, including without limitation the rights to use, copy,
 * modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

function repeat(el, times) {
  const arr = [];
  for (let i = 0; i < times; i++) {
    arr[i] = el;
  }
  return arr;
}

function randomInt(n) {
  return Math.floor(Math.random() * n);
}

function randomElement(arr) {
  return arr[randomInt(arr.length)];
}

function range(s, e) {
  const arr = [];
  while (s <= e) {
    arr.push(s++);
  }
  return arr;
}

export class GameOver extends Error {}
export class GameWon extends Error {}

export class Field {
  /**
   *
   * @param {number} width
   * @param {number} height
   * @param {number} minesCount
   */
  constructor(width, height, minesCount) {
    /**
     * @type {number}
     */
    this.width = width;
    /**
     * @type {number}
     */
    this.height = height;
    /**
     * @type {number}
     */
    this.minesCount = minesCount;
    /**
     * @type {number}
     */
    this.left = width * height - minesCount;
    /**
     * @type {[boolean]}
     */
    this.covered = repeat(true, width * height);
  }
  /**
   *
   * @param {number} index
   * @returns {boolean}
   */
  isCovered(index) {
    return this.covered[index];
  }
  /**
   *
   * @param {number} index
   * @returns {void}
   */
  uncover(index) {
    delete this.covered[index];
    this.left--;
  }
  blowUp() {
    this.field = null;
    throw new GameOver("KAAAWUMMMMM!");
  }
}

export class Solver {
  /**
   * @param {Field} field
   */
  constructor(field) {
    /**
     * @type {Field}
     */
    this.field = field;
    /**
     * @type {Set<number>}
     */
    this.frontier = new Set();
    const n = this.field.width * this.field.height;

    this.cells = [];
    for (let i = 0; i < n; i++) {
      this.cells[i] = [];
    }

    this.createConstraint(field.minesCount, range(0, n - 1));
    /**
     * @type {Set<number>}
     */
    this.flagged = new Set();
  }
  createConstraint(mines, cells) {
    if (mines === 0) {
      this.uncoverLater(cells);
    }
    const constraint = new Constraint(mines, cells);
    for (let i = 0, l = cells.length; i < l; i++) {
      const cell = this.cells[cells[i]];
      if (cell) {
        cell.push(constraint);
      }
    }
    const overlapping = this.getOverlappingConstraints(constraint);
    for (let i = 0, l = overlapping.length; i < l; i++) {
      if (this.unifyConstraints(constraint, overlapping[i])) {
        break;
      }
    }
  }
  removeConstraint(constraint) {
    for (let i = 0, l = constraint.cells.length; i < l; i++) {
      const cell = this.cells[constraint.cells[i]];
      if (cell) {
        const index = cell.indexOf(constraint);
        cell.splice(index, 1);
      }
    }
  }
  getOverlappingConstraints(constraint) {
    const set = new Set();
    for (let i = 0, l = constraint.cells.length; i < l; i++) {
      const cell = this.cells[constraint.cells[i]];
      if (cell) {
        for (let j = 0, k = cell.length; j < k; j++) {
          if (cell[j] !== constraint) {
            set.add(cell[j]);
          }
        }
      }
    }
    return [...set.values()];
  }
  unifyConstraints(a, b) {
    if (a.cells.length < b.cells.length) {
      const tmp = a;
      a = b;
      b = tmp;
    }
    a.unifiedWith.push(b);
    b.unifiedWith.push(a);

    const inA = [];
    const inB = [];
    const inAB = [];

    for (let i = 0, l = a.cells.length; i < l; i++) {
      if (b.cells.indexOf(a.cells[i]) !== -1) {
        inAB.push(a.cells[i]);
      } else {
        inA.push(a.cells[i]);
      }
    }

    for (let i = 0, l = b.cells.length; i < l; i++) {
      if (inAB.indexOf(b.cells[i]) === -1) {
        inB.push(b.cells[i]);
      }
    }

    const aMore = a.mines - b.mines;
    if (aMore === inA.length) {
      this.removeConstraint(a);
      this.removeConstraint(b);
      this.createConstraint(aMore, inA);
      this.createConstraint(b.mines, inAB);
      this.createConstraint(0, inB);
      return true;
    }
    if (b.mines - a.mines === inB.length) {
      this.removeConstraint(a);
      this.removeConstraint(b);
      this.createConstraint(b.mines - a.mines, inB);
      this.createConstraint(a.mines, inAB);
      this.createConstraint(0, inA);
      return true;
    }
    if (inB.length === 0) {
      this.removeConstraint(a);
      this.removeConstraint(b);
      this.createConstraint(aMore, inA);
      // Re-add b
      this.createConstraint(b.mines, b.cells);
      return true;
    }
    if (b.mines === b.cells.length) {
      this.removeConstraint(a);
      this.removeConstraint(b);
      this.createConstraint(a.mines - inAB.length, inA);
      this.createConstraint(b.mines, b.cells);
      return true;
    }
    if (a.mines === a.cells.length) {
      this.removeConstraint(b);
      this.removeConstraint(a);
      this.createConstraint(b.mines - inAB.length, inB);
      this.createConstraint(a.mines, a.cells);
      return true;
    }
    return false;
  }
  /**
   *
   * @param {{index: number, mined: boolean, threats: number}} cells
   * @returns {void}
   */
  onUncovered(cells) {
    cells.forEach(({ index, mined, threats, neighbours }) => {
      if (!this.field.isCovered(index)) {
        console.warn(`${index} was already opened. Race condition, anyone?`);
        // throw new Error();
      }
      this.field.uncover(index);
      if (mined) {
        this.field.blowUp();
      }
      this.createConstraint(0, [index]);
      this.createConstraint(
        threats,
        neighbours.filter((i) => this.field.isCovered(i))
      );
    });
    if (this.field.left === 0) {
      throw new GameWon("All clear!");
    }
    this.flagged.clear();
    cells.forEach(({ neighbours }) => {
      neighbours
        .filter((i) => this.field.isCovered(i))
        .forEach((i) => {
          if (this.getProbability(i) === 1) {
            this.flagged.add(i);
          }
        });
    });
  }
  /**
   *
   * @param {number} index
   * @returns {number}
   */
  getProbability(index) {
    const constraints = this.cells[index];
    let p = 0;
    for (let i = 0, l = constraints.length; i < l; i++) {
      const constraint = constraints[i];
      p = Math.max(p, constraint.mines / constraint.cells.length);
    }
    return p;
  }
  /**
   *
   * @returns {number | undefined}
   */
  guess() {
    const best = [];
    let bestP = 1;
    this.field.covered.forEach((_, i) => {
      const p = this.getProbability(i);
      if (p === bestP) {
        best.push(i);
      } else if (p < bestP) {
        bestP = p;
        best.length = 0;
        best.push(i);
      }
    });
    return randomElement(best);
  }
  uncoverLater(cells) {
    for (var i = 0, l = cells.length; i < l; i++) {
      if (this.field.isCovered([cells[i]])) {
        this.frontier.add(cells[i]);
      }
    }
  }
  /**
   * @typedef {Object} StepResult
   * @property {[number]} poke
   * @property {[number]} flag
   *
   * @returns {StepResult}
   */
  step() {
    if (this.field.left === 0) {
      throw new GameWon("All clear");
    }
    const result = (poke) => {
      const flag = [...this.flagged.values()];
      return { poke, flag };
    };
    if (this.frontier.size > 0) {
      const xs = [...this.frontier.values()].filter((i) =>
        this.field.isCovered(i)
      );
      this.frontier.clear();
      return xs.length > 0 ? result(xs) : this.step();
    }
    const index = this.guess();
    if (index == null) {
      throw Error("No guess!!!");
    }
    return result([index]);
  }
}

class Constraint {
  constructor(mines, cells) {
    this.mines = mines;
    this.cells = cells;
    this.unifiedWith = [];
  }
}
