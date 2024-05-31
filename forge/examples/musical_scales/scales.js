require('d3')
require('tone')

// Written by Mia Santomauro with advice from Tim Nelson in 2021.
// Minor changes to match 2024 Forge by Tim in February, 2024.

// NOTE WELL: the require lines MUST be the first lines in the script, since 
// this is an "old-style" custom visualizer. It is not an actual JavaScript require, 
// but a directive to Sterling, listing modules to load from the JsDelivr CDN.

// clear the svg
d3.selectAll("svg > *").remove();
// set up tone synth
const synth = new tone.Synth().toDestination();
// used for rendering and as a base for offset
const lowestNote = "C4";

/* 
  ___   _ _____ _     ___ _   _ _  _  ___ _____ ___ ___  _  _ ___ 
 |   \ /_\_   _/_\   | __| | | | \| |/ __|_   _|_ _/ _ \| \| / __|
 | |) / _ \| |/ _ \  | _|| |_| | .` | (__  | |  | | (_) | .` \__ \
 |___/_/ \_\_/_/ \_\ |_|  \___/|_|\_|\___| |_| |___\___/|_|\_|___/
                                                                                                                                                          
*/

/**
 * a function to parse the note notation for the letter, accidental, and octave
 * @param {string} note 
 * @return {string[]} [letter, accidental, octave]
 */
function unpackNote(note) {
    if (note.length === 2) {
        return [note[0], "", note[1]];
    } else if (note.length === 3) {
        return [note[0], note[1], note[2]];
    } else {
        console.log("unsupported note format found in unpackNote: " + note);
    }
}

/**
 * a function to remove the octave notation from the given note
 * @param {string} note
 */
function truncNote(note) {
    const [letter, accidental, octave] = unpackNote(note) 
    return letter + accidental;
}

/**
 * a function to extract out the id from the Forge Interval sigs
 * @param {Interval[]} intervals the given intervals
 * @param {Number[]} simpleIntervals an empty array to populate
 */
function constructSimpleIntervals(intervals, simpleIntervals) {
    let i;
    for (i = 0; i < intervals.length; i++) {
      simpleIntervals.push(parseInt(intervals[i].hs._id));
    }
}

/**
 * a function to put a given array of intervals in order
 * @param {Interval[]} intervals the given intervals
 * @param {Interval[]} orderedIntervals  an empty array to populate
 */
function constructOrderedIntervals(intervals, orderedIntervals) {
    const firstInterval = Start0.start;
    let currInterval = firstInterval;
    orderedIntervals.push(firstInterval);
  
    let i;
    for (i = 0; i < intervals.length - 1; i++) {
      currInterval = currInterval.next;
      orderedIntervals.push(currInterval);
    }
}

/**
 * a function to order and simplify a given array of Intervals
 * @param {Interval[]} intervals the given intervals
 * @param {Number[]} simpleOrderedIntervals an empty array to populate
 */
function constructSimpleOrderedIntervals(intervals, simpleOrderedIntervals) {
    const orderedIntervals = [];
    constructOrderedIntervals(intervals, orderedIntervals);
    constructSimpleIntervals(orderedIntervals, simpleOrderedIntervals)
}

/*
  __  __ _   _ ___ ___ ___   ___ _   _ _  _  ___ _____ ___ ___  _  _ ___
 |  \/  | | | / __|_ _/ __| | __| | | | \| |/ __|_   _|_ _/ _ \| \| / __|
 | |\/| | |_| \__ \| | (__  | _|| |_| | .` | (__  | |  | | (_) | .` \__ \
 |_|  |_|\___/|___/___\___| |_|  \___/|_|\_|\___| |_| |___\___/|_|\_|___/

*/

// index corresponds to number of sharps
const SHARP_KEYS = ["G", "D", "A", "E", "B", "F#", "C#"];
// index corresponds to number of flats
const FLAT_KEYS = ["F", "Bb", "Eb", "Ab", "Db", "Gb"];
// index is arbitrary 
const VALID_KEYS = ["C"] + SHARP_KEYS + FLAT_KEYS;
// index corresponds to position of tonic of relative major key
const MODES = ["ionian", "locrian", "aeolian", "mixolydian", "lydian", "phrygian", "dorian"];

/**
 * a function to get the next letter alphabetically after the given letter,
 * wrapping around back to A after G
 * @param {string} letter
 */
function nextLetter(letter) {
    return (letter === "G") ? "A" : String.fromCharCode(letter.charCodeAt(0) + 1);
}

/**
 * a function to get the previous letter alphabetically before the given letter,
 * wrapping around back to G after A
 * @param {string} letter
 */
function previousLetter(letter) {
    return (letter === "A") ? "G" : String.fromCharCode(letter.charCodeAt(0) - 1);
}

/**
 * a function to get the note a half-step up from the given note
 * @param {string} note
 */
function halfStep(note) {
    let [letter, accidental, octave] = unpackNote(note);
    // no accidental
    if (accidental === "") {
        if (letter === "B") {
            let newOctave = parseInt(octave) + 1;
            return "C" + newOctave;
        } else if (letter === "E") {
            return "F" + octave;
        } else {
            return letter + "#" + octave;
        }
    // sharp
    } else if (accidental === "#") {
        if (letter === "B") {
            octave = parseInt(octave) + 1;
            return "C#" + octave;
        } else if (letter === "E") {
            return "F#" + octave; 
        } else {
            return nextLetter(letter) + octave; 
        }
    // flat
    } else if (accidental === "b") {
        return letter + octave;
    } else {
        console.log("unsupported note format found in halfStep: " + note);
    }
}

/**
 * a function to get the note a whole step up from the given note
 * @param {string} note
 */
function wholeStep(note) {
    return halfStep(halfStep(note))
}

/**
 * a function to determine if the given interval represents a half step
 * @param {Number} interval either 1 (half step) or 2 (whole step)
 */
function isHalfStep(interval) {
    return interval === 1;
}

/**
 * a function to, given a starting note and a sequence of intervals, construct a scale
 * @param {Number[]} intervals 
 * @param {string} startNote 
 * @param {string[]} notes the array to populate with the resulting scale.
 */
function getNotesFromIntervals(intervals, startNote, notes) {

    notes.push(startNote);
  
    let currNote = startNote;
    let i;
    for (i = 0; i < intervals.length; i++) {
      // if the interval is a half-step...
      if (isHalfStep(intervals[i])) {
        currNote = halfStep(currNote);   
      } else { // if the interval is a whole-step...
        currNote = wholeStep(currNote);
      }
      notes.push(currNote);
    }
}

/**
 * a function to, given a sequence of intervals, 
 * determine the index of the note which represents 
 * the corresponding major key
 * @param {Number[]} intervals 
 */
function getMajorKeyIndex(intervals) {
  // first interval is half step
  if (isHalfStep(intervals[0])) {
    // either phrygian or locrian
    if (isHalfStep(intervals[3])) {
      return 1; //locrian
    } else {
      return 5; // phrygian
    }
    // first interval is whole step
  } else {
    // either dorian, aeolian, ionian, lydian, or mixolydian
    if (isHalfStep(intervals[1])) {
      // either dorian, or aeolian
      if (isHalfStep(intervals[4])) {
        return 2; // aeolian
      } else {
        return 6; // dorian
      }
    } else {
      // either ionian, lydian, or mixolydian
      if (isHalfStep(intervals[2])) {
        // ionian or mixolydian
        if (isHalfStep(intervals[6])) {
          return 0; // ionian
        } else {
          return 3; // mixolydian
        }
      } else {
        return 4; //lydian
      }
    }
  }
}

/**
 * given a sequence of intervals, 
 * determines the index of the note which 
 * represents the corresponding minor key
 * @param {Interval[]} intervals
 */
function getMinorKeyIndex(intervals) {
    const maj = getMajorKeyIndex(intervals);
    return (maj + 5) % 7;
}

/**
 * Computes the equivalent note of opposite accidental for a given note.
 * For example, D# ~= Eb and B# ~= C
 * @param {string} note 
 */
function getNoteEquiv(note) {
    const [letter, accidental, octave] = unpackNote(note);
    if (accidental === "") {
        // TODO: check if it's E/F/B/C ?
        return note;
    } else {
        if (accidental === "#") {
            if (letter === "E") {
              return "F" + octave;
            } else if (letter === "B") {
              const newOctave = parseInt(octave) + 1;
              return "C" + newOctave;
            } else {
              return nextLetter(letter) + "b" + octave;
            }
          } else if (accidental === "b") {
            if (letter === "F") {
              return "E" + octave;
            } else if (letter === "C") {
              const newOctave = parseInt(octave) - 1;
              return "B" + newOctave;
            } else {
              return previousLetter(letter) + "#" + octave;
            }
        }
    }
}

/**
 * given an arbitrary key, return an equivalent one that is one the circle of fifths
 * @param {string} key
 */
function getValidKey(key) {
    const truncatedKey = truncNote(key);
  
    if (VALID_KEYS.includes(truncatedKey)) {
      return key;
    } else {
        // TODO: double check that note equiv is a valid key
        return getNoteEquiv(key);
    }
}

/**
 * a function to determine whether the given key uses sharps in its key signature
 * @param {string} key 
 */
function keyUsesSharps(key) {
    const truncatedKey = truncNote(key);
    return SHARP_KEYS.includes(truncatedKey);
}

/**
 * a function to determine whether the given key uses flats in its key signature
 * @param {string} key 
 */
function keyUsesFlats(key) {
    const truncatedKey = truncNote(key);
    return FLAT_KEYS.includes(truncatedKey);
}

/**
 * a function to, given some notes and a key, fit the notes to that key 
 * such that they use the proper accidentals
 * @param {string} key 
 * @param {*} notesApprox 
 * @param {*} notes an empty array to populate
 */
function fitNotesToKey(key, notesApprox, notes) {

    // TODO: this logic could be different to ensure note letters are incremental
    // but this idea may only work for church modes and SOME other scales

    let toReplace = [];
    if (keyUsesSharps(key)) {
      toReplace.push("b");
    } else if (keyUsesFlats(key)) {
      toReplace.push("#")
    } else {
      toReplace.push("b");
      toReplace.push("#");
    }
  
    let i;
    for (i = 0; i < notesApprox.length; i++) {
      let currNote = notesApprox[i];
      if (currNote.length === 3) {
        if (toReplace.includes(currNote[1])) {
          currNote = getNoteEquiv(currNote);
        }
      }
      notes.push(currNote);
    }
}

/**
 * a function to determine if the given sequence of intervals represents a church mode
 * @param {*} intervals 
 */
function isChurchMode(intervals) {

    const firstHalfStep = intervals.indexOf(1);
  
    if (firstHalfStep < 0 || firstHalfStep > 3) {
      return false;
    }
  
    const restOfIntervals = intervals.slice(firstHalfStep + 1);
    const secondHalfStep = restOfIntervals.indexOf(1);
  
    if (secondHalfStep < 0 || secondHalfStep > 3) {
      return false;
    }
  
    if (secondHalfStep === 2) {
      return [1, 2, 3].includes(firstHalfStep);
    } else if (secondHalfStep === 3) {
      return [0, 1, 2].includes(firstHalfStep);
    } else {
      return false;
    }
}

/*
  ___  ___  _   _ _  _ ___    ___ _   _ _  _  ___ _____ ___ ___  _  _ ___
 / __|/ _ \| | | | \| |   \  | __| | | | \| |/ __|_   _|_ _/ _ \| \| / __|
 \__ \ (_) | |_| | .` | |) | | _|| |_| | .` | (__  | |  | | (_) | .` \__ \
 |___/\___/ \___/|_|\_|___/  |_|  \___/|_|\_|\___| |_| |___\___/|_|\_|___/

*/

/**
 * a function to play the given scale aloud
 * @param {string[]} notes the notes to play (in order)
 */
function playScale(notes) {
    console.log(`playScale: ${notes}`)
    const now = tone.now()
    let i;
    for (i = 0; i < notes.length; i++) {
      synth.triggerAttackRelease(notes[i], "8n", now + (i * .5));
    }
}

/*
 __   _____ ____  ___ _   _ _  _  ___ _____ ___ ___  _  _ ___
 \ \ / /_ _|_  / | __| | | | \| |/ __|_   _|_ _/ _ \| \| / __|
  \ V / | | / /  | _|| |_| | .` | (__  | |  | | (_) | .` \__ \
   \_/ |___/___| |_|  \___/|_|\_|\___| |_| |___\___/|_|\_|___/

*/

const bottom = 400;
const left = 100;
const w = 25;
const margin = 30;

/**
 * a function to render the staff
 */
function drawStaff() {
    let i;
    for (i = 0; i < 5; i++) {
      let y = bottom - (i * w) - w;
      d3.select(svg)
         .append("line")
         .attr("x1", left)
         .attr("y1", y)
         .attr("x2", left + 500)
         .attr("y2", y)
         .attr("stroke", "black")
    }
}

/**
 * a function to compute y value for a given note
 * @param {string} note 
 */
function noteToY(note) {

    let offset = 0;
    let letter;
    let octave;
  
    if (note.length === 2) {
      letter = note[0];
      octave = note[1];
    } else if (note.length === 3) {
      letter = note[0];
      octave = note[2];
    }
  
    const lowestOctave = parseInt(lowestNote[1]);
    offset += (7 * (octave - lowestOctave));
  
    const letterCharCode = letter.charCodeAt(0);
    const lowestCharCode = lowestNote.charCodeAt(0);
  
    if (letterCharCode > lowestCharCode) {
      offset += letterCharCode - lowestCharCode;
    } else if (letterCharCode < lowestCharCode) {
      offset += 7 - (lowestCharCode - letterCharCode)
    }
  
    return bottom - (w * (offset / 2))
  
}

/**
 * a function to render the notes on the staff
 * @param {string[]} notes 
 */
function drawNotes(notes) {
    console.log(`drawNotes: ${notes}`)
    
    const x = (note, index) => {
      return index * (w + margin) + left + 20; // todo replace 20 with width of treble clef
    }
  
    const y = (note, index) => {
      return noteToY(note);
    }
  
    const accX = (note, index) => {
        return x(note, index) - 36;
    }
  
    const accY = (note, index) => {
        return y(note, index) + 5;
    }
  
    const acc = (note) => {
      if (note.length === 3) {
        const accidental = note[1];
        if (accidental === "#") {
          return "#"
        } else if (accidental === "b") {
          return "♭"
        }
      }
      return ""
    }
  
    const circles = d3.select(svg)
    .selectAll('ellipse')
    .data(notes)
    .join('ellipse')
    .attr('rx', w / 2 + 5)
    .attr('ry', w / 2)
    .attr('cx', x)
    .attr('cy', noteToY)
    .style('stroke', 'black')
    .style('fill', 'white');
  
    const accidentals = d3.select(svg)
    .selectAll('text')
    .data(notes)
    .join('text')
    .attr('x', accX)
    .attr('y', accY)
    .style("font", "24px times")
    .text(acc);
}

/**
 * a function to display information about the scale
 * @param {*} notes 
 * @param {*} majIndex 
 * @param {*} majKey 
 * @param {*} churchMode 
 */
function printResult(notes, majIndex, majKey, churchMode) {

    if (churchMode) {
      d3.select(svg)
      .append("text")
      .style("fill", "black")
      .attr("x", 50)
      .attr("y", 50)
      .text("This is " + truncNote(notes[0]) + " " + MODES[majIndex]);
      
      d3.select(svg)
      .append("text")
      .style("fill", "black")
      .attr("x", 50)
      .attr("y", 70)
      .text("which is in the key of " + truncNote(majKey) + " Major");
    } else {
      d3.select(svg)
      .append("text")
      .style("fill", "black")
      .attr("x", 50)
      .attr("y", 50)
      .text("This is not a church mode");
    }
}

/**
 * a function to construct the whole visualization
 * @param {*} notes 
 * @param {*} majIndex 
 * @param {*} majKey 
 * @param {*} churchMode 
 */
function constructVisualization(notes, majIndex, majKey, churchMode) {
    drawStaff();
    drawNotes(notes);
    printResult(notes, majIndex, majKey, churchMode);
}

/*
  __  __   _   ___ _  _   ___ _   _ _  _  ___ _____ ___ ___  _  _ 
 |  \/  | /_\ |_ _| \| | | __| | | | \| |/ __|_   _|_ _/ _ \| \| |
 | |\/| |/ _ \ | || .` | | _|| |_| | .` | (__  | |  | | (_) | .` |
 |_|  |_/_/ \_\___|_|\_| |_|  \___/|_|\_|\___| |_| |___\___/|_|\_|
                                                                  
*/

function go(startNote) {
    console.log(`startNote: ${startNote}`)

    // constructing ordered array of intervals
    const intervals = Interval.atoms(true);
    const simpleOrderedIntervals = []
    constructSimpleOrderedIntervals(intervals, simpleOrderedIntervals);

    // determine if this is a church mode
    const churchMode = isChurchMode(simpleOrderedIntervals);
  
    // constructing the notes array
    const notesApprox = [];
    getNotesFromIntervals(simpleOrderedIntervals, startNote, notesApprox);
  
    // finding the relative major key
    const majIndex = getMajorKeyIndex(simpleOrderedIntervals);
    const majKeyApprox = notesApprox[majIndex];
    const majKey = getValidKey(majKeyApprox);
  
    // reconstructing the notes array
    const notes = [];
    fitNotesToKey(majKey, notesApprox, notes);
  
    // playing the scale out loud
    playScale(notes);
  
    // rendering the scale on the staff
    constructVisualization(notes, majIndex, majKey, churchMode);
  
}

const off = Start0.offset._id;

let i;
let startNote = lowestNote;
for (i = 0; i < off; i++) {
    startNote = halfStep(startNote);
}

console.log(`Starting. startNote=${startNote}`)
go(startNote);
  


